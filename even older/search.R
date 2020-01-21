############
## README ##
############

# Query papers.db using SQLite3 for backwards and forwards search

###########
## SETUP ##
###########

library(tidyverse)
library(RSQLite)
library(DBI)

# connect to database (paper.db file)
con <- dbConnect(RSQLite::SQLite(),"./paper.db")

###############
## FUNCTIONS ##
###############

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

############
## SEARCH ##
############

# set input
input <- c() # read_csv() ...

# run searches and store results
b.data <- backward.search(input)
f.data <- forward.search(input)


##################
## WRITE OUTPUT ##
##################

write_csv(b.data, 'BACKWARDS.csv')
write_csv(f.data, 'FORWARDS.csv')

#########
## END ##
#########

# disconnect
dbDisconnect(con)


