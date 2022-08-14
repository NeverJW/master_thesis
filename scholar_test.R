###################### test scholar package ################################
# to see which information we need to keep in the database

# Define the id for Richard Feynman
id <- 'B7vSqZsAAAAJ'

# Get his profile and print his name
l <- get_profile(id)
l$h_index
l$total_cites  # these two variables gonna be keeped 

get_num_articles(id) 

get_num_top_journals(id) 

get_oldest_article(id)

get_num_distinct_journals(id)

# Get his citation history, i.e. citations to his work in a given year 
get_citation_history(id)   # from here get the mean of citations each year for certain author

# Get his publications (a large data frame)
get_publications(id)

## Predict h-index of original method author
predict_h_index(id)   # because this is designed for neuroscientist, it doesn't be included 

#### compare the citations of different scholars 
# Compare Feynman and Stephen Hawking
ids <- c('B7vSqZsAAAAJ', 'qj74uXkAAAAJ')

# Get a data frame comparing the number of citations to their work in
# a given year 
compare_scholars(ids)

# Compare their career trajectories, based on year of first citation
compare_scholar_careers(ids)

