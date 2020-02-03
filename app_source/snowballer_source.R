# load packages

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "skimr", "microdemic", "RSQLite", "DBI", "shiny", "shinythemes", "DT", "microbenchmark")
ipak(packages)

Sys.setenv(MICROSOFT_ACADEMIC_KEY = "1cb802560edf4e9a81dc2ed363531287")

# connect to database (paper.db file)
con <- dbConnect(SQLite(), "/Users/nortonlab/Desktop/MAG/paper.db") # changed for MAG testing

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

# scraping functions
scrape <- function(ID) {
  article <- ma_evaluate(query = paste('Id=', ID, sep=''),
                         atts = c("Id", "Ti", "Y", "AA.AuN", "J.JN", "Pt", "RId", "CC"))
  if (nrow(article) != 0){select(article, -c('logprob', 'prob'))} 
}

scrape.tidy <- function(IDs) {
  data <- tibble(Id = numeric(), Ti = character(), Pt = character(), Y = numeric(),
                   CC = numeric(), RId = numeric(), AA = character(), J.JN = character())
  for (ID in IDs) {
    df <- scrape(ID)
    df$RId <- length(df$RId[[1]])
    df$AA <- paste(unlist(df$AA), collapse = ', ')
    data <- bind_rows(data, df)
  }
  data %>%
    rename(ID = Id, Title = Ti, Year = Y, Authors = AA, Journal = J.JN,
           Pub_type = Pt, Citations = CC, References = RId) %>% 
    select(ID, Title, Year, Authors, Journal, Pub_type, Citations, References) 
}

scrape.abst.tidy <- function(IDs) {
  data <- tibble(Id = numeric(), abstract = character())
  for (ID in IDs) {data <- bind_rows(data, ma_abstract(query = paste0("Id=", ID)))}
  data %>% rename(ID = Id, Abstract = abstract) %>% mutate(Abstract = ifelse(Abstract == "", NA, Abstract))
}

# quick db search
fast.scrape <- function(ID){
  res <- dbGetQuery(con, paste("select * from paper_info where PaperID in (", paste(ID, collapse = ", "), ")"))
  res$DocType[is.na(res$DocType)] = "Unknown"
  as_tibble(res) %>%
    mutate(OriginalTitle = paste(paste0("[", DocType, "]"), OriginalTitle)) %>%
    select(-DocType)
}
