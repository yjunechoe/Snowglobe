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
              "RSQLite", "DBI", "fulltext", "microdemic", "rcrossref", "rentrez", "data.table")
ipak(packages)

# microsoft academic API key
Sys.setenv(MICROSOFT_ACADEMIC_KEY = "1cb802560edf4e9a81dc2ed363531287")
# scopus api key // check remaining with `verbose = TRUE` argument
Sys.setenv(ELSEVIER_SCOPUS_KEY = "9c9423562dfa9cef97f2e80c236a5ff1") # dd017ab5c552d4af6089cc6182758186
opts <- list(key = Sys.getenv("ELSEVIER_SCOPUS_KEY"))

# connect to database (paper.db file)
con <- dbConnect(SQLite(), "paper.db")

#########################
## ID Search Functions ##
#########################

# search ID by title
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
    df$AA <- ifelse(length(df$AA[[1]]) == 0, NA, paste(unique(unlist(df$AA)), collapse = ', '))
    data <- bind_rows(data, df)
  }
  data %>% 
    rename(ID = Id, Title = Ti, Year = Y, Authors = AA, Journal = J.JN,
           Pub_type = Pt, Citations = CC, References = RId) %>% 
    select(ID, Title, Year, Authors, Journal, Pub_type, DOI, Citations, References) 
}

# search ID by doi
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
    df$AA <- ifelse(length(df$AA[[1]]) == 0, NA, paste(unique(unlist(df$AA)), collapse = ', '))
    data <- bind_rows(data, df)
  }
  data %>% 
    rename(ID = Id, Title = Ti, Year = Y, Authors = AA, Journal = J.JN,
           Pub_type = Pt, Citations = CC, References = RId) %>% 
    select(ID, Title, Year, Authors, Journal, Pub_type, DOI, Citations, References) 
}

# combined
search.IDs <- function(df){
  format <- tibble(ID = numeric(), Title = character(), Year = numeric(), Authors = character(), Journal = character(),
                   Pub_type = character(), DOI = character(), Citations = numeric(), References = numeric())
  orig <- df
  df <- bind_rows(format, df %>% select(Title, DOI))
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
  format <- tibble(PMID = numeric(), ID = numeric(), Title = character(), Year = numeric(), Authors = character(),
                   Journal = character(), Pub_type = character(), DOI = character(), Citations = numeric(), References = numeric())
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

# snowball
snowball <- function(ID){
  unique(c(backward.search(ID)$Backward_References, forward.search(ID)$Forward_Citations))
}

# snowball with duplicates
snowball_full  <- function(ID){
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
  as_tibble(rbind(b, f)) %>% select(from, to, direction)
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

pub.key <- function(Pub){
  mapdf <- tibble(old = 0:8,
                  new = c("Unknown", "Journal Article", "Patent", "Conference",
                          "Book Chapter", "Book", "Book Reference", "Dataset", "Repository"))
  mapdf[as.numeric(Pub) + 1,]$new
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
    select(ID, Title, Year, Authors, Journal, Pub_type, DOI, Citations, References) %>% 
    mutate(Pub_type = pub.key(Pub_type))
}

# abstract
## microsoft academic (MAG ID)
scrape.abst.ID <- function(IDs){
  data <- tibble(Id = numeric(), abstract = character())
  for (ID in IDs){data <- bind_rows(data, ma_abstract(query = paste0("Id=", ID)))}
  data %>% rename(ID = Id, Abstract = abstract) %>% 
    mutate(Abstract = ifelse(Abstract == "", NA,
                             str_squish(str_remove_all(str_remove(Abstract, "^Abstract[ ]*[NA ]*"), "(NA)+"))))
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
  as_tibble(dbGetQuery(con, paste("select * from paper_info where PaperID in (", paste(ID, collapse = ", "), ")"))) %>% 
    rename(ID = PaperID, Title = OriginalTitle, Pub_type = DocType, DOI = Doi)
}

fast.scrape.squish <- function(ID){
  res <- dbGetQuery(con, paste("select * from paper_info where PaperID in (", paste(ID, collapse = ", "), ")"))
  res$DocType[is.na(res$DocType)] = "Unknown"
  as_tibble(res) %>%
    mutate(OriginalTitle = paste(paste0("[", DocType, "]"), OriginalTitle)) %>%
    select(-DocType)
}
