###########
## SETUP ##
###########

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "skimr", "microdemic", "RSQLite", "DBI", "shiny", "shinythemes", "DT", "microbenchmark", "fulltext")
ipak(packages)

Sys.setenv(MICROSOFT_ACADEMIC_KEY = "1cb802560edf4e9a81dc2ed363531287")
Sys.setenv(ELSEVIER_SCOPUS_KEY = "9c9423562dfa9cef97f2e80c236a5ff1") # dd017ab5c552d4af6089cc6182758186
opts <- list(key = Sys.getenv("ELSEVIER_SCOPUS_KEY"))
# citation(package='fulltext')

# connect to database (paper.db file)
con <- dbConnect(SQLite(), "paper.db")

######################
## Search Functions ##
######################

# search by title
title.strip <- function(title){tolower(str_squish(gsub("[^[:alnum:] ]", " ", title)))}
title.search <- function(title){
  searched <- ma_evaluate(query=paste0('Ti=', "'", title.strip(title), "'"),
              atts = c("Id", "Ti", "Y", "AA.AuN", "J.JN", "Pt", "RId", "CC", "DOI")) %>% 
    select(-(1:2))
  if (nrow(searched) == 0) {tibble(Id = NA, Ti = title.strip(title))}
  else if (nrow(searched) > 1) {arrange(searched, desc(Y), desc(Pt))[1,]}
  else {searched}
}

title.search.tidy <- function(titles){
  data <- tibble(Id = numeric(), Ti = character(), Pt = character(), DOI = character(), Y = numeric(),
                 CC = numeric(), RId = numeric(), AA = character(), J.JN = character())
  for (title in titles) {
    df <- title.search(title)
    df$RId <- ifelse(length(df$RId[[1]]) == 0, NA, length(df$RId[[1]]))
    df$AA <- paste(unique(unlist(df$AA)), collapse = ', ')
    data <- bind_rows(data, df)
  }
  data %>% 
    rename(ID = Id, Title = Ti, Year = Y, Authors = AA, Journal = J.JN,
           Pub_type = Pt, Citations = CC, References = RId) %>% 
    select(ID, Title, Year, Authors, Journal, Pub_type, DOI, Citations, References) 
}

# search by doi
doi.search <- function(doi){
  searched <- ma_evaluate(query=paste0('DOI=', "'", doi, "'"),
                          atts = c("Id", "Ti", "Y", "AA.AuN", "J.JN", "Pt", "RId", "CC", "DOI")) %>% 
    select(-(1:2))
  if (nrow(searched) == 0) {tibble(Id = NA, DOI = doi)}
  else if (nrow(searched) > 1) {arrange(searched, desc(Y), desc(Pt))[1,]}
  else {searched}
}

doi.search.tidy <- function(dois){
  data <- tibble(Id = numeric(), Ti = character(), Pt = character(), DOI = character(), Y = numeric(),
                 CC = numeric(), RId = numeric(), AA = character(), J.JN = character())
  for (doi in dois) {
    df <- doi.search(doi)
    df$RId <- ifelse(length(df$RId[[1]]) == 0, NA, length(df$RId[[1]]))
    df$AA <- paste(unique(unlist(df$AA)), collapse = ', ')
    data <- bind_rows(data, df)
  }
  data %>% 
    rename(ID = Id, Title = Ti, Year = Y, Authors = AA, Journal = J.JN,
           Pub_type = Pt, Citations = CC, References = RId) %>% 
    select(ID, Title, Year, Authors, Journal, Pub_type, DOI, Citations, References) 
}

########################
## Snowball Functions ##
########################

# backward search (references)
backward.search <- function(ID){
  dbGetQuery(con, paste0("select paperid, refid from refs where paperid in (",
                         paste(ID, collapse = ","), ")")) %>% 
    rename(Backward_References = refid, ID = paperid) %>% 
    select(Backward_References, ID) %>%
    mutate(Backward_References = as.numeric(Backward_References))
}

# forward search (citations)
forward.search <- function(ID){
  dbGetQuery(con, paste0("select paperid, refid from refs where refid in (",
                         paste(ID, collapse = ","), ")")) %>% 
    rename(ID = refid, Forward_Citations = paperid) %>%
    select(ID, Forward_Citations) %>% 
    mutate(Forward_Citations = as.numeric(Forward_Citations))
}

# both search
snowball <- function(ID){
  unique(c(backward.search(ID)$Backward_References, forward.search(ID)$Forward_Citations))
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

scrape.tidy <- function(IDs){
  data <- tibble(Id = numeric(), Ti = character(), Pt = character(), DOI = character(), Y = numeric(),
                   CC = numeric(), RId = numeric(), AA = character(), J.JN = character())
  for (ID in IDs) {
    df <- scrape(ID)
    df$RId <- ifelse(length(df$RId[[1]]) == 0, NA, length(df$RId[[1]]))
    df$AA <-  paste(unique(unlist(df$AA)), collapse = ', ')
    data <- bind_rows(data, df)
  }
  data %>%
    rename(ID = Id, Title = Ti, Year = Y, Authors = AA, Journal = J.JN,
           Pub_type = Pt, Citations = CC, References = RId) %>% 
    select(ID, Title, Year, Authors, Journal, Pub_type, DOI, Citations, References) 
}

# abstract
## microsoft academic (MAG ID)
scrape.abst.ID <- function(IDs){
  data <- tibble(Id = numeric(), abstract = character())
  for (ID in IDs){data <- bind_rows(data, ma_abstract(query = paste0("Id=", ID)))}
  data %>% rename(ID = Id, Abstract = abstract) %>% 
    mutate(Abstract = ifelse(Abstract == "", NA, str_remove(Abstract, "Abstract [NA ]*")))
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

# local db search
fast.scrape <- function(ID){
  res <- dbGetQuery(con, paste("select * from paper_info where PaperID in (", paste(ID, collapse = ", "), ")"))
  res$DocType[is.na(res$DocType)] = "Unknown"
  as_tibble(res) %>%
    mutate(OriginalTitle = paste(paste0("[", DocType, "]"), OriginalTitle)) %>%
    select(-DocType)
}

