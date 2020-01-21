##################
## useful links ##
##################

# api server - https://msr-apis.portal.azure-api.net/
# paper entity attributes - https://docs.microsoft.com/en-us/azure/cognitive-services/academic-knowledge/paperentityattributes
# query syntax - https://docs.microsoft.com/en-us/azure/cognitive-services/academic-knowledge/queryexpressionsyntax 
# ma_evalate() documentation - https://docs.ropensci.org/microdemic/reference/ma_evaluate.html 

###########
## setup ##
###########
library(microdemic)
library(tidyverse)

# setting environment variable for my API key
Sys.setenv(MICROSOFT_ACADEMIC_KEY = "1cb802560edf4e9a81dc2ed363531287")

# scraping function
scrape <- function(ID) {
  ma_evaluate(query = paste('Id=', ID, sep=''), # search article by ID
              # grab entity attributes
              atts = c("Id", # article ID
                       "Ti", # title
                       "Y", # year published
                       "AA.AuN", # author name
                       "J.JN", # journal
                       "Pt", # publication type (relevant= 1:Journal article, 3:Conference paper, 4:Book chapter, 5:Book)
                       "RId", # articles it's cited by
                       "CitCon", # articles it cites and sentence contexts in which they're cited
                       "CC") # number of citations
                 ) %>% select(-c('logprob', 'prob')) # remove irrelevant query stats
}

# edge class
setClass(
  "Edge",
  slots = list(
    v1 = "character",
    v2 = "character",
    con = "list"
  )
)

# set up variables
all_edges <- c()
found <- c()
iteration_data <- tibble(Id = numeric(), Ti = character(), Pt = character(), Y = numeric(), CC = numeric(),
                         RId = character(), DOI = character(), AA = character(), J.JN = character(),
                         search_count = numeric())

#####################
## input goes here ##
#####################

# test_list <- c("147082445",  "1996569541", "2128091892", "1591598348", "2467982299", "2018893201", "1569246963")
# test <- c(1968882288) # badian 1992

# set iteration number
iteration_num <- 0
# read in vector of article ID's
ID_list <- refs

####################
## automatic part ##
####################

for (ID in ID_list) {
  df <- scrape(ID)
  df$iteration <- iteration_num
  citcon <- df %>% 
    select(contains("CitCon."))
  article_cite_contexts <- names(citcon) %>% 
    str_remove("CitCon.")
  df$article_cite_contexts <- list(article_cite_contexts)
  edges <- list()
  references <- unique(c(unlist(df$article_cite_contexts), unlist(df$RId)))
  for (article in references) {
    context <- select(df, contains(article))
    if (ncol(context) == 0) {context <- list()}
    else {context <- context[[1]]}
    edge <- new("Edge", v1 = toString(ID), v2 = article, con = context)
    edges <- append(edges, edge)
  }
  df$search_count <- length(edges)
  found <- c(found, references)
  df$RId <- paste(references, collapse = ', ')
  df$J.JN <- paste(unlist(df$J.JN), collapse = ', ')
  df$AA <- paste(unlist(df$AA), collapse = ', ')
  df <- df %>% 
    select(-contains('CitCon'))
  all_edges <- c(all_edges, edges)
  iteration_data <- bind_rows(iteration_data, df)
}

# edge data
isCitationEdge <- function(x) 
{if (length(x@con) > 0) return(TRUE) else return(FALSE)}
citation_edges <- Filter(isCitationEdge, all_edges)

# tidy up output (FIX)
iteration_data <- iteration_data %>% 
  rename(Iteration = iteration_num,
         ID = Id,
         Title = Ti,
         Year = Y,
         Authors = AA,
         Journal = J.JN,
         Pub_type = Pt,
         Citation_count = CC,
         References = RId,
         References_count = search_count
         ) %>% 
  select(iteration, ID, Title, Year, Authors, Journal, Pub_type,
         Citation_count, References, References_count)

# write output
# write_csv(iteration_data, 'RESULTS WITHOUT ABSTRACTS.csv')

#####################################
#####################################

#################################
## REMOVE NON-JOURNAL ARTICLEs ##
#################################

iteration_data <- filter(iteration_data, Pub_type == 1)

###################
## ADD ABSTRACTS ##
###################

abs_ID_list <- refs
abstracts <- tibble(Id = numeric(), abstract = character())

for (ID in abs_ID_list){
  abst <- ma_abstract(query = paste("Id=", ID, sep = ""))
  abstracts <- bind_rows(abstracts, abst)
}

iteration_data_abstracts <- inner_join(iteration_data, rename(abstracts, ID = Id), by = 'ID')
write_csv(iteration_data_abstracts, 'ITERATION OUTPUT.csv')

############################
## PULL CITATION CONTEXTS ##
############################

citcon_ID_list <- c()

contexts <- Filter( function(x) x@v2 %in% citcon_ID_list, citation_edges)
contexts <- lapply(contexts, function(z) data.frame(v1 = z@v1, v2 = z@v2, con = paste(unlist(z@con),collapse='\n')))
contexts <- Reduce(rbind, contexts) %>% as_tibble()
contexts <- contexts %>%
  rename(cited_in = v1, ID = v2, context = con) %>%
  select(ID, cited_in, context) %>%
  mutate(ID = as.numeric(ID), cited_in = as.numeric(cited_in)) %>% 
  arrange(ID)

write_csv(contexts, 'CITATION CONTEXTS.csv')

