# Source script setting up all libraries
source("setup.R")

# load excel base data from endnote
base_data <- read_excel("data/data_endnote.xlsx")

# remove na column
base_data <-
  base_data[, colSums(is.na(base_data)) < nrow(base_data)]

# remove unnecessary column
base_data <- base_data  # journal type
base_data <-
  subset(
    base_data,
    select = -c(
      language,
      url,
      ISSN,
      volume,
      issue,
      `alternate journal`,
      `author address`
    )
  )

# remove duplicates row
base_data <- base_data[!duplicated(base_data), ]

# create the author numbers variable
base_data$author_number <-
  lengths(gregexpr(";",  base_data$author)) + 1
base_data$author_number <-
  ifelse(str_detect(base_data$author, ";"), base_data$author_number, 1)

# see how many authors does this paper have
max(base_data$author_number, na.rm = TRUE)   # 9 authors are the maximal

# calculate total pages of paper
base_data$page1 <- str_split_fixed(base_data$pages, "-", 2)[, 1]
base_data$page2 <- str_split_fixed(base_data$pages, "-", 2)[, 2]
base_data$page <-
  as.numeric(base_data$page2) - as.numeric(base_data$page1)

base_data <- subset(base_data, select = -c(page1, page2))

# extract citation counts and references counts
base_data$citations <-
  str_split_fixed(base_data$notes, ":", 3)[, 2] %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
base_data$reference_count <-
  str_split_fixed(base_data$notes, ":", 3)[, 3] %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric

# remove reference_count equal to 0
base_data <- base_data %>% filter(reference_count != 0)

### add 2 years impact factor of journals
source("data/get_journal_features.R")

# merge 2 years impact factor data frame to our base data
base_data <-
  left_join(base_data, impactf_df, by = c("year", "journal"))

# add the variable which indicate the length of title
base_data$title_length <- str_count(base_data$title, " ") + 1

# add the variable which indicate whether keywords contain the most popular words of marketing papers
base_data$keyword_pop <-
  str_detect(
    base_data$keywords,
    "brands loyalty|augmented reality|digital marketing|influencer marketing|artificial intelligence|machine learning|big data|personalization|multi-channel|video marketing"
  )

# see how many papers does include popular words in the keyword part
length(base_data$keyword_pop[base_data$keyword_pop == TRUE])  # 213 papers

# add the variable which indicate whether abstract contain the most popular words of marketing papers
base_data$abstract_pop <-
  str_detect(
    base_data$abstract,
    "brands loyalty|augmented reality|digital marketing|influencer marketing|artificial intelligence|machine learning|big data|personalization|multi-channel|video marketing"
  )

# see how many papers does include popular words in the abstract part
length(base_data$abstract_pop[base_data$abstract_pop == TRUE])  # 200 papers

################################### get infos of author ##################
# remove na of variable DOI and author number
base_data <- base_data %>% drop_na(DOI)
base_data <- base_data %>% drop_na(author_number)

# create a driver from Rselenium
port <- random_port(min_port = 49152, max_port = 65536)
rD <- rsDriver(browser = "chrome", port = port, chromever="103.0.5060.24")

# get the client
remDr <- rD$client

# set time outs to give the page the change to first fully load before
# we try to get information form it
remDr$setTimeout(type = "implicit", milliseconds = 10000)
remDr$setTimeout(type = "page load", milliseconds = 10000)

# create 4 columns of author infos (because there are maximal 4 authors which has individual website link)
base_data$author_id1 <- NA
base_data$author_id2 <- NA
base_data$author_id3 <- NA
base_data$author_id4 <- NA

# get infos of authors
for (i in 1:nrow(base_data)) {
  # split the doi to construct the url 
  doi1 <- str_split(base_data$DOI, "/")[[i]][1]
  doi2 <- str_split(base_data$DOI, "/")[[i]][2]
  if (is.na(str_split(base_data$DOI, "/")[[i]][3]) != TRUE) { # if there is a third part of doi
    doi3 <- str_split(base_data$DOI, "/")[[i]][3]
  }
  
  # n equal to author number
  n <- base_data$author_number[[i]]
  
  # get the author ids
  author_id_data_frame <- scrape_google_author_id(doi1, doi2, doi3, n, port)
  
  # fill the base data
  base_data[i,21:24] <- author_id_data_frame
}


#### after getting the id of author
# add variables of authors through scholar_test package
# need 2 infos each author, so add 8 blank columns 
namevector <- c("hindex_au1", "hindex_au2", "hindex_au3", "hindex_au4", "totalcite_au1", "totalcite_au2", "totalcite_au3", "totalcite_au4")
base_data[ ,namevector] <- NA

# get h_index of each author and total number of citations of each author
for (i in 1:nrow(base_data)){
  if(is.na(base_data$author_id1[[i]])!= TRUE){
    base_data[i,25] <- get_profile(base_data$author_id1[[i]])$h_index
    base_data[i,29] <- get_profile(base_data$author_id1[[i]])$total_cites
    
  }
  
  if(is.na(base_data$author_id2[[i]])!= TRUE){
    base_data[i,26] <- get_profile(base_data$author_id2[[i]])$h_index
    base_data[i,30] <- get_profile(base_data$author_id2[[i]])$total_cites
    
  }
  
  if(is.na(base_data$author_id3[[i]])!= TRUE){
    base_data[i,27] <- get_profile(base_data$author_id3[[i]])$h_index
    base_data[i,31] <- get_profile(base_data$author_id3[[i]])$total_cites
    
  }
  
  if(is.na(base_data$author_id4[[i]])!= TRUE){
    base_data[i,28] <- get_profile(base_data$author_id4[[i]])$h_index
    base_data[i,32] <- get_profile(base_data$author_id4[[i]])$total_cites
    
  }
}

get_citation_history(author_id)
get_profile(author_id)$total_cites


