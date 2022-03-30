###########
## SETUP ##
###########

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "shiny", "shinythemes", "DT", "tippy", "skimr", "visNetwork",
              "dbplyr", "DBI", "fulltext", "microdemic", "rcrossref", "rentrez", "data.table",
              "tidytext", "glue", "cleanNLP", "wordcloud2", "shinybusy", "RMariaDB", "rlang", "stringi",
              "pool", "stringdist")

ipak(packages)

source('app_source/utils.R')
source('app_source/connect.R')


raw_cols <- tibble(
  Id = numeric(),
  Ti = character(),
  Pt = character(),
  DOI = character(),
  Y = numeric(),
  CC = numeric(),
  RId = list(),
  AA = list(),
  J.JN = character()
)

col_format <- tibble(
  ID = numeric(),
  Title = character(),
  Year = numeric(),
  Authors = character(),
  Journal = character(),
  Pub_type = character(),
  DOI = character(),
  Citations = numeric(),
  References = numeric()
)

key_vec <- c(
  "Unknown",
  "Journal Article",
  "Patent",
  "Conference",
  "Book Chapter",
  "Book",
  "Book Reference",
  "Dataset",
  "Repository"
)


#########################
## ID Search Functions ##
#########################

stopwords <- read_csv("data/stopwords_mariadb.csv")$stopwords
word_freqs <- cleanNLP::word_frequency %>% 
  filter(language == "en", !word %in% stopwords) %>% 
  pull(frequency, word)
get_word_freqs <- function(words) {
  freqs <- coalesce(word_freqs[words], numeric(length(words)))
  names(freqs) <- words
  sort(freqs)
}
PAPER_INFO.template <- data.frame(
  PaperID = NA,
  OriginalTitle = NA,
  Year = NA,
  DocType = NA,
  Doi = NA
)

# by title
title.strip <- function(title){
  tolower(str_squish(gsub("[^[:alpha:] ]", " ", title)))
}


make.title.query <- function(title) {
  # strip diacritics
  title_stripped <- str_to_lower(str_squish(str_replace_all(title, "[^[:alpha:]\\s]", " ")))
  titles_latin <- stringi::stri_trans_general(title_stripped, "Latin-ASCII")
  keywords <- vapply(titles_latin, function(x) {
    title_words <- str_split(x, "\\s+")[[1]]
    title_content_words <- unique(title_words[!(title_words %in% stopwords) & nchar(title_words) > 3])
    # Phrase chunking step
    title_keywords <- get_word_freqs(title_content_words)
    str_flatten(paste0("+", names(title_keywords), "*"), collapse = ", ")
  }, character(1), USE.NAMES = FALSE)
  paste0("MATCH(OriginalTitle) AGAINST (\'", keywords, "\' IN BOOLEAN MODE)")
}

make.year.query <- function(year, fuzzy = 3L) {
  year <- as.integer(year)
  ifelse(
    !is.na(year),
    paste0("AND Year BETWEEN ", year - fuzzy, " AND ", year + fuzzy, " ORDER BY ABS(YEAR - ", year, ")"),
    ""
  )
}

# New db query functions
make.query.direct <- function(title, year = NA, limit = 20){
  title_query <- make.title.query(title)
  year_query <- make.year.query(year)
  paste("SELECT * FROM PAPER_INFO WHERE", title_query, year_query, "LIMIT", limit, ";")
}
augment.query.direct <- function(queries) {
  ifelse(
    str_detect(queries, "ORDER BY"),
    str_replace(queries, " LIMIT \\d+", ", CHARACTER_LENGTH(OriginalTitle) LIMIT 1"),
    str_replace(queries, " LIMIT \\d+", " ORDER BY CHARACTER_LENGTH(OriginalTitle) LIMIT 1")
  )
}
## Not vectorized
send.query.direct <- function(query) {
  result <- dbGetQuery(pool, query)
  if (nrow(result) == 20L) {
    result <- dbGetQuery(pool, augment.query.direct(query))
  }
  result
}
format.query.direct <- function(results, title) {
  n_matches <- map_int(results, nrow)
  results[n_matches > 1L] <- map(which(n_matches > 1L), function(idx) {
    target <- title[idx]
    matched <- results[[idx]]$OriginalTitle
    results[[idx]][which.min(stringdist::stringdist(target, matched)), ]
  })
  results[n_matches == 0] <- list(PAPER_INFO.template)
  cleaned <- bind_rows(results)
  colnames(cleaned) <- c("ID", "Title", "Year", "Pub_type", "DOI")
  cleaned$ID <- as.numeric(cleaned$ID)
  as_tibble(cleaned)
}
# For debugging
query.direct <- function(title, year = NA, limit = 20) {
  queries <- make.query.direct(title, year, limit)
  results <- imap(queries, ~{
    cat(.y, "\n")
    send.query.direct(.x)
  })
  format.query.direct(results, title)
}

# TODO
# - join supplied title/year info to query results
# - make.query.direct should pass down opts to title and year query fns
# - make sure the AWS can handle {stringdist} installation


title.search <- function(title){
  
  if (!is.null(title) && !is.na(title)) {
    
    searched <- ma_evaluate(
      query = paste0('Ti=', "'", title.strip(title), "'"),
      atts = c("Id", "Ti", "Y", "AA.AuN", "J.JN", "Pt", "RId", "CC", "DOI")
    )
    
    if (nrow(searched) == 0) {
      list(Ti = title.strip(title))
    } else if (nrow(searched) > 1) {
      arrange(searched, desc(Y), desc(Pt))[1,] %>% 
        select(-(1:2))
    } else {
      select(searched, -(1:2))
    }
    
  }
}

# by doi
doi.search <- function(doi){
  
  if (!is.null(doi) && !is.na(doi)) {
    
    searched <- ma_evaluate(
      query = paste0('DOI=', "'", str_to_upper(doi), "'"),
      atts = c("Id", "Ti", "Y", "AA.AuN", "J.JN", "Pt", "RId", "CC", "DOI")
    )
    
    if (nrow(searched) == 0) {
      list(DOI = str_to_upper(doi))
    } else if (nrow(searched) > 1) {
      arrange(searched, desc(Y), desc(Pt))[1,] %>% 
        select(-(1:2))
    } else {
      select(searched, -(1:2))
    }
    
  }
}

# by PubMed IDs
pubmed.search <- function(pubmedID, type = "pubmed"){
  
  if (!is.null(pubmedID) && !is.na(pubmedID)) {
    
    info <- suppressWarnings(possibly(entrez_summary, otherwise = NULL)(id = pubmedID, db = type)$articleids)
    
    if (!is.null(info) && "doi" %in% info$idtype) {
      doi.search(info$value[info$idtype == "doi"])
    } else {
      list2(!!switch(type, "pubmed" = "PMID", "pmc" = "PMCID") := pubmedID)
    }
    
  }
}


###################################
## Template Processing Functions ##
###################################

# not vectorized to allow for progress tracking in staged_file_searched()
fill.template.row <- function(row){
  
  doi.search(row[["DOI"]]) %!Id%
    title.search(row[["Title"]]) %!Id%
    pubmed.search(row[["PMID"]], type = "pubmed") %!Id%
    pubmed.search(row[["PMCID"]], type = "pmc") %!Id%
    row
  
}

# vectorized formatting function
format.tidy <- function(searched) {
  
  original_titles <- fast.scrape(searched$Id) %>% 
    select(Id = PaperID, OriginalTitle, DocType) %>% 
    mutate(Id = as.double(Id))
  
  bind_rows(raw_cols, searched) %>% 
    inner_join(original_titles, by = "Id") %>% 
    select(
      ID = Id,
      Title = OriginalTitle,
      Year = Y,
      Authors = AA,
      Journal = J.JN,
      Pub_type = DocType,
      DOI,
      Citations = CC,
      References = RId
    ) %>%
    filter(!is.na(ID)) %>% 
    rowwise() %>% 
    mutate(
      Authors = paste(Authors$AuN, collapse = ", "),
      References = References %0% NA
    ) %>% 
    ungroup()
  
}




########################
## Snowball Functions ##
########################

# backward search (references)
backward.search <- function(ID){
  tbl(pool, "REFS") %>% 
    filter(paperid %in% ID) %>% 
    select(Backward_References = refid, ID = paperid) %>% 
    dplyr::collect()
}

# forward search (citations)
forward.search <- function(ID){
  tbl(pool, "REFS") %>% 
    filter(refid %in% ID) %>% 
    select(ID = refid, Forward_Citations = paperid) %>% 
    dplyr::collect()
}

# snowball
snowball <- function(ID){
  unique(c(backward.search(ID)$Backward_References, forward.search(ID)$Forward_Citations))
}

# snowball with duplicates
snowball_full <- function(ID){
  c(backward.search(ID)$Backward_References, forward.search(ID)$Forward_Citations)
}

# snowball connections
snowball_connections <- function(ID){
  f <- forward.search(ID) %>%
    rename(from = ID, to = Forward_Citations) %>% 
    mutate(direction = "forward")
  b <- backward.search(ID) %>%
    rename(from = ID, to = Backward_References) %>% 
    mutate(direction = "backward")
  bind_rows(b, f) %>% 
    relocate(from)
}

######################
## Scrape Functions ##
######################

# paper info
scrape <- function(ID){
  article <- ma_evaluate(
    query = paste0('Id=', ID),
    atts = c("Id", "Ti", "Y", "AA.AuN", "J.JN", "Pt", "RId", "CC", "DOI")
  )
  if (!rlang::is_empty(article)){
    select(article, -c('logprob', 'prob'))
  } else {
    tibble(ID = ID) # TODO maybe make this return output from fast.scrape()
  }
}

# pubkey lookup vector
pub.key <- function(Pub){
  key_vec[as.numeric(Pub) + 1]
}


# abstract
## microsoft academic (MAG ID)
scrape.abst.ID <- function(IDs){
  abstracts <- map_chr(IDs, ~ {
    abst <- ma_abstract(query = paste0("Id=", .x))[["abstract"]]
    if (length(abst) == 0) NA else abst
  })
}

## other databases (semantic scholar, plos, crossref, scopus)
scrape.abst.DOI <- function(DOI, db) {
  possibly(ft_abstract, otherwise = NULL)(DOI, from = db)[[db]][[1]][["abstract"]]
}

# local db search
fast.scrape <- function(ID){
  tbl(pool, "PAPER_INFO") %>% 
    filter(PaperID %in% ID) %>% 
    dplyr::collect()
}

fast.scrape.squish <- function(ID){
  fast.scrape(ID) %>% 
    mutate(
      DocType = DocType %||% "Unknown",
      OriginalTitle = paste0("[", DocType, "] ", OriginalTitle)
    ) %>% 
    select(-DocType)
}

orig.title <- function(ID){
  fast.scrape(ID)$OriginalTitle
}
