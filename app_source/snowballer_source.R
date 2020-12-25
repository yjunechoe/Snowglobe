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
              "tidytext", "glue", "cleanNLP", "wordcloud2", "shinybusy", "RMariaDB")

ipak(packages)

# microsoft academic API key
Sys.setenv(MICROSOFT_ACADEMIC_KEY = "1cb802560edf4e9a81dc2ed363531287")
# scopus api key // check remaining with `verbose = TRUE` argument
Sys.setenv(ELSEVIER_SCOPUS_KEY = "9c9423562dfa9cef97f2e80c236a5ff1") # dd017ab5c552d4af6089cc6182758186
scopusopts <- list(key = Sys.getenv("ELSEVIER_SCOPUS_KEY"))

# connect to database
con <- dbConnect(
  drv = MariaDB(),
  host = "nortonlab.coeodvofteoq.us-east-2.rds.amazonaws.com",
  username = 'nortonadmin',
  password = 'z87knhs5bb',
  port = 3306
)
dbSendQuery(con, "use snowglobe")

paper_info_db <- tbl(con, "PAPER_INFO")
refs_db <- tbl(con, "REFS")

### mix of frontend/backend

col_format <- tibble(ID = numeric(), Title = character(), Year = numeric(), Authors = character(), Journal = character(),
                     Pub_type = character(), DOI = character(), Citations = numeric(), References = numeric())

# check null infix
"%||%" <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

# check length 0 infix
"%0%" <- function(lhs, rhs) {
  len_lhs <- length(lhs)
  if (len_lhs == 0) rhs else len_lhs
}

# possibly() otherwise NULL
possibly_null <- function(f,x){
  possibly(f, otherwise = NULL)(x)
}


#########################
## ID Search Functions ##
#########################

# search ID by title
title.strip <- function(title){
  tolower(str_squish(gsub("[^[:alnum:] ]", " ", title)))
}

title.search <- function(title){
  
  searched <- ma_evaluate(
    query = paste0('Ti=', "'", title.strip(title), "'"),
    atts = c("Id", "Ti", "Y", "AA.AuN", "J.JN", "Pt", "RId", "CC", "DOI")
  )
  
  if (nrow(searched) == 0) {
    tibble(Id = NA, Ti = title.strip(title))
  } else if (nrow(searched) > 1) {
    arrange(searched, desc(Y), desc(Pt))[1,] %>% 
      select(-(1:2))
  } else {
    select(searched, -(1:2))
  }
  
}

title.search.tidy <- function(titles){
  
  map_dfr(titles, title.search) %>% 
    select(
      ID = Id,
      Title = Ti,
      Year = Y,
      Authors = AA,
      Journal = J.JN,
      Pub_type = Pt,
      Citations = CC,
      References = RId
    ) %>%
    filter(!is.na(ID)) %>% 
    rowwise() %>% 
    mutate(
      Title = fast.scrape(ID)$OriginalTitle,
      Authors = ifelse(length(Authors) == 0, NA, paste(unique(flatten_chr(Authors)), collapse = ', ')),
      References = References %0% NA,
    ) %>% 
    ungroup() %>% 
    mutate(Pub_type = pub.key(Pub_type))
  
}

# search ID by doi
doi.search <- function(doi){
  
  searched <- ma_evaluate(
    query = paste0('DOI=', "'", str_to_upper(doi), "'"),
    atts = c("Id", "Ti", "Y", "AA.AuN", "J.JN", "Pt", "RId", "CC", "DOI")
  )
  
  if (nrow(searched) == 0) {
    tibble(Id = NA, DOI = str_to_upper(doi))
  } else if (nrow(searched) > 1) {
    arrange(searched, desc(Y), desc(Pt))[1,] %>% 
      select(-(1:2))
  } else {
    select(searched, -(1:2))
  }
  
}

doi.search.tidy <- function(dois){
  
  map_dfr(dois, doi.search) %>% 
    select(
      ID = Id,
      Title = Ti,
      Year = Y,
      Authors = AA,
      Journal = J.JN,
      Pub_type = Pt,
      Citations = CC,
      References = RId
    ) %>%
    filter(!is.na(ID)) %>% 
    rowwise() %>% 
    mutate(
      Title = fast.scrape(ID)$OriginalTitle,
      Authors = ifelse(length(Authors) == 0, NA, paste(unique(flatten_chr(Authors)), collapse = ', ')),
      References = References %0% NA,
    ) %>% 
    ungroup() %>% 
    mutate(Pub_type = pub.key(Pub_type))
  
}

# not vectorized to allow for progress tracking in staged_file_searched()
fill.template.row <- function(row){
  
  if (is.null(row$ID)) {
    possibly_null(doi.search.tidy, row$DOI) %||%
      possibly_null(title.search.tidy, row$Title) %||%
      possibly_null(doi.search.tidy, PMID.search(row$PMID)$DOI) %||%
      mutate(row, ID = NA)
  }

}

# combined
search.IDs <- function(df){
  orig <- df
  df <- bind_rows(col_format, df %>% select(Title, DOI))
  for (i in 1:nrow(df)) {
    if (!is.na(df[i, "DOI"]) & !is.na(df[i, "Title"])) {
      doi <- df[i, "DOI"]
      temp <- doi.search.tidy(df[i, "DOI"])
      if (is.na(temp$ID)) {temp <- title.search.tidy(df[i, "Title"])}
      df[i,] <- temp
      df[i, "DOI"] <- doi
    }
    else if (is.na(df[i, "DOI"]) & !is.na(df[i, "Title"])) {df[i,] <- title.search.tidy(df[i, "Title"])}
    else if (!is.na(df[i, "DOI"]) & is.na(df[i, "Title"])) {df[i,] <- doi.search.tidy(df[i, "DOI"])}
    else {}
  }
  df <- mutate(df, Pub_type = pub.key(Pub_type))
  as_tibble(cbind(select(orig, -c(Title, DOI)), df))
}

# PubMed ID
PMID.info <- function(PMID, type = "pubmed"){
  tryCatch({entrez_summary(db = type, id = PMID)}, warning = function(cond){PMID})
}

PMID.search <- function(PMIDs, type = "pubmed"){
  format <- tibble(PMID = numeric(), ID = numeric(), Title = character(), Year = numeric(),
                   Authors = character(), Journal = character(), Pub_type = character(),
                   DOI = character(), Citations = numeric(), References = numeric())
  df <- tibble(PMID = numeric(), Title = character(), DOI = character())
  for (PMID in PMIDs) {
    PMinfo <- PMID.info(PMID, type = type)
    if (length(PMinfo) != 1) {
      PMtitle <- PMinfo$title
      PMdoi <- PMinfo$articleids[PMinfo$articleids$idtype == "doi", "value"]
      PMdoi <- ifelse(length(PMdoi) == 0, NA, PMdoi)
    } else {
      PMtitle <- NA
      PMdoi <- NA
    }
    df <- rbind(df, tibble(PMID = PMID, Title = PMtitle, DOI = PMdoi))
  }
  colnames(df)[1] <- ifelse(type == "pubmed", "PMID", paste0(toupper(type), "ID"))
  df
}

########################
## Snowball Functions ##
########################

# backward search (references)
backward.search <- function(ID){
  refs_db %>% 
    filter(paperid %in% ID) %>% 
    select(Backward_References = refid, ID = paperid) %>% 
    dplyr::collect()
}

# forward search (citations)
forward.search <- function(ID){
  refs_db %>% 
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
  article <- ma_evaluate(query = paste0('Id=', ID),
                         atts = c("Id", "Ti", "Y", "AA.AuN", "J.JN", "Pt", "RId", "CC", "DOI"))
  if (nrow(article) != 0){select(article, -c('logprob', 'prob'))} 
}

# pubkey lookup vector
pub.key <- function(Pub){
  key_vec <- c("Unknown", "Journal Article", "Patent", "Conference",
               "Book Chapter", "Book", "Book Reference", "Dataset", "Repository")
  key_vec[as.numeric(Pub) + 1]
}

scrape.tidy <- function(IDs){
  data <- tibble(Id = numeric(), Ti = character(), Pt = character(), DOI = character(), Y = numeric(),
                 CC = numeric(), RId = numeric(), AA = character(), J.JN = character())
  for (ID in IDs) {
    df <- scrape(ID)
    if(!is.null(df)){
      df$RId <- ifelse(length(df$RId[[1]]) == 0, NA, length(df$RId[[1]]))
      df$AA <-  paste(unique(unlist(df$AA)), collapse = ', ')
      data <- bind_rows(data, df)
    }
  }
  
  data <- data %>%
    rename(ID = Id, Title = Ti, Year = Y, Authors = AA, Journal = J.JN,
           Pub_type = Pt, Citations = CC, References = RId) %>% 
    select(ID, Title, Year, Authors, Journal, Pub_type, DOI, Citations, References) %>% 
    mutate(Pub_type = pub.key(Pub_type))
  
  # Grab correctly formatted titles and merge
  
  original_titles <- fast.scrape(data$ID)$OriginalTitle
  
  data %>% 
    mutate(Title = coalesce(original_titles, Title)) %>% 
    relocate(ID, Title)
  
}

# abstract
## microsoft academic (MAG ID)
scrape.abst.ID <- function(IDs){
  abstracts <- map_chr(IDs, ~ {
    abst <- ma_abstract(query = paste0("Id=", .x))$abstract
    if (length(abst) == 0) {
      NA
    } else {
      abst
    }
  })
  tibble(
    ID = IDs,
    Abstract = str_squish(str_remove_all(str_remove(abstracts, "^Abstract[ ]*[NA ]*"), "(NA)+"))
  )
}

## scopus (DOI)
scrape.abst.DOI <- function(DOIs){
  abst <- NULL
  data <- tibble(DOI = character(), Abstract = character())
  for (d in DOIs){
    ft <- tryCatch({ft <- ft_abstract(x = d, from = "scopus", scopusopts = opts)$scopus[[1]]},
                   error = function(cond){ft <- NULL})
    if (is_null(ft)) {abst <- tibble(DOI = d, Abstract = NA)}
    else {abst <- tibble(DOI = ft$doi, Abstract = ifelse(is_null(ft$abstract), NA, ft$abstract))}
    data <- rbind(data, abst)
  }
  data
}

## crossref (DOI)
scrape.abst.DOI.cr <- function(DOIs) {
  data <- tibble(DOI = character(), Abstract = character())
  for (d in DOIs) {
    abst <- tryCatch({abst <- cr_abstract(d)},
                     error = function(cond){abst <- NA})
    if (is.na(abst)) {abst <- tibble(DOI = d, Abstract = NA)}
    else {abst <- tibble(DOI = d, Abstract = str_squish(abst))}
    data <- rbind(data, abst)
  }
  data
}

# local db search
fast.scrape <- function(ID){
  paper_info_db %>% 
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
