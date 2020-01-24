# load packages

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "skimr", "microdemic", "RSQLite", "DBI", "shiny", "DT")
ipak(packages)

Sys.setenv(MICROSOFT_ACADEMIC_KEY = "1cb802560edf4e9a81dc2ed363531287")

# connect to database (paper.db file)
con <- dbConnect(RSQLite::SQLite(),"./paper.db")

# backward search (references)
backward.search <- function(ID){
  rs <- dbSendQuery(con,paste("select paperid, refid from refs where paperid in (",paste(ID,collapse=","),")"))
  rename(fetch(rs, -1), Backward_References = refid, ID = paperid) %>% select(Backward_References, ID)
}
# forward search (citations)
forward.search <- function(ID){
  rs <- dbSendQuery(con,paste("select paperid, refid from refs where refid in (",paste(ID,collapse=","),")"))
  rename(fetch(rs, -1), ID = refid, Forward_Citations = paperid) %>% select(ID, Forward_Citations)
}

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
  data
}

scrape.abst.tidy <- function(IDs) {
  data <- tibble(Id = numeric(), abstract = character())
  for (ID in IDs) {data <- bind_rows(data, ma_abstract(query = paste0("Id=", ID)))}
  data %>% rename(ID = Id, Abstract = abstract)
}



