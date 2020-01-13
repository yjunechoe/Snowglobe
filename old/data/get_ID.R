library(microdemic)
library(tidyverse)
library(stringr)

# setting environment variable for my API key
Sys.setenv(MICROSOFT_ACADEMIC_KEY = "1cb802560edf4e9a81dc2ed363531287") 

# load in all papers
all_data <- read_csv('duplicate_check/data/Duplicates_removed_4100.csv')

# strip character formats of titles
all_data$Title <- tolower(gsub('[[:punct:] ]+',' ',all_data$Title))
all_data$Title <- trimws(gsub("-", "", all_data$Title))
all_data$Title <- trimws(gsub("’", "", all_data$Title))
all_data <- all_data %>% select(`Publication Year`, Author, Title, Key) %>% rename(Year = `Publication Year`)

# set as intput
query_input <- all_data

# scraping function
scrape <- function(article) {
  ma_evaluate(query = paste("And(Y=", article$Year, ", Ti='", article$Title ,"')", sep = ''),
              atts = c('Id', 'Ti', 'Y', 'AA.AuN', 'J.JN', 'Pt')
  ) #%>% select(-c('logprob', 'prob')) # remove irrelevant query stats
}

# initialize output df
output <- tibble(Id = numeric(), Ti = character(), Y = numeric(), AA = list(),
                 J.JN = character(), Pt = character(), Key = character())

# loop through papers in the input
for (i in 1:nrow(query_input)) {
  article <- query_input[i,]
  article_info <- scrape(article) %>% mutate(Key = article$Key)
  output <- bind_rows(output, article_info)
}

# add ID information and 
result <- full_join(all_data, output, key = Key)
result <- result %>% 
  rename(ID = Id,
         Pub_Type = Pt) %>% 
  select(Key, ID, Title, Year, Author, Pub_Type)

##################
## WRITE OUTPUT ##
##################

# write data for ID matches
result_with_ID <- result %>% filter(!is.na(ID))
write_csv(result_with_ID, 'duplicate_check/Duplicates_Removed_with_ID.csv', col_names = TRUE)

# write data for title matches
result_no_ID <- result %>% filter(is.na(ID))

result_no_ID <- result_no_ID[!str_detect(result_no_ID$Title, 'abstract'),]
result_no_ID <- result_no_ID[!str_detect(result_no_ID$Title, 'award'),]
result_no_ID <- result_no_ID %>% filter(!is.na(Year))
result_no_ID <- result_no_ID %>% filter(!is.na(Author))
result_no_ID <- result_no_ID %>% filter(!Year >= 2018)

write_csv(result_no_ID, 'duplicate_check/Duplicates_Removed_no_ID.csv', col_names = TRUE)
