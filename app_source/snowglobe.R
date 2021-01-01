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
              "tidytext", "glue", "cleanNLP", "wordcloud2", "shinybusy", "RMariaDB", "rlang")

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

# by title
title.strip <- function(title){
  tolower(str_squish(gsub("[^[:alnum:] ]", " ", title)))
}

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
      output <- list() # TODO switch out for list2() and tidyeval with `:=`
      col_name <- switch(type, "pubmed" = "PMID", "pmc" = "PMCID")
      output[[col_name]] <- pubmedID
      output
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
  tbl(con, "REFS") %>% 
    filter(paperid %in% ID) %>% 
    select(Backward_References = refid, ID = paperid) %>% 
    dplyr::collect()
}

# forward search (citations)
forward.search <- function(ID){
  tbl(con, "REFS") %>% 
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
  if (!is.null(article)){
    select(article, -c('logprob', 'prob'))
  } else {
    tibble(ID = NA)
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
  tbl(con, "PAPER_INFO") %>% 
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
