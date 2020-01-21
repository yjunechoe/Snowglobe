############
## README ##
############

# two-step process for checking for duplicates against the original ~4100 papers already screened:
#   1) Match IDs for papers that have them (~3500) - 100% accurate at catching duplicates
#   2) Fuzzy match titles for candidate papers that don't have IDs (~400) - **must manually check matches in step 2**

############
## SETUP  ##
############
library(microdemic)
library(tidyverse)
library(stringr)

ID_match <- read_csv('data/Duplicates_Removed_with_ID.csv') # 3500 w/ ID
title_match <- read_csv('data/Duplicates_Removed_no_ID.csv') # ~600 w/o ID

############
## INPUT  ##
############

# read in search output
refs <- read_csv('iteration_1_June_output/iteration_1_papers.csv')

#######################
## STEP 1: ID match  ##
#######################

# remove ID matches
refs_minus_duplicates <- refs %>% filter(!ID %in% ID_match$ID)

# pull ID duplicates
ID_duplicates <- refs %>% filter(ID %in% ID_match$ID)

########################################
## STEP 2: title match (semi-manual)  ##
########################################

# remove title matches
title_matches <- c()
for (title in title_match$Title){
  title_matches <- c(title_matches, agrep(title, refs_minus_duplicates$Title, max.distance = 0.2)[1])
}
exclude <- title_matches[!is.na(title_matches)]

# pull title duplicates and check if they actually match
from_original <- title_match[!is.na(title_matches),]$Title
from_search <- refs_minus_duplicates[exclude,]$Title

title_duplicates <- tibble(row_num = exclude, searched = from_search, original = from_original)
title_duplicates %>% print(n = nrow(title_duplicates))

# enter in the row_num of any incorrect matches
incorrect_matches <- c(562, 1095, 1234, 87, 85, 13, 31) # enter here

# update list of title duplicates and exclude them
exclude <- exclude[-incorrect_matches]
refs_minus_duplicates <- refs_minus_duplicates[-exclude]

#############
## OUTPUT  ##
#############

write_csv(refs_minus_duplicates, 'DUPLICATES_REMOVED.csv', col_names = TRUE)


