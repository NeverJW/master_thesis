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

# add the variable which indicate whether abstract contain the most popular words of marketing papers
base_data$abstract_pop <-
  str_detect(
    base_data$abstract,
    "brands loyalty|augmented reality|digital marketing|influencer marketing|artificial intelligence|machine learning|big data|personalization|multi-channel|video marketing"
  )

# see how many papers does include popular words in the abstract part
length(base_data$abstract_pop[base_data$abstract_pop == TRUE])

################################### get infos of author ##################
# remove na of variable DOI and author number
base_data <- base_data %>% drop_na(DOI)
base_data <- base_data %>% drop_na(author_number)

# create a driver from Rselenium
port <- random_port(min_port = 49152, max_port = 65536)
rD <-
  rsDriver(browser = "chrome",
           port = port,
           chromever = "105.0.5195.19")

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
  if (is.na(str_split(base_data$DOI, "/")[[i]][3]) != TRUE) {
    # if there is a third part of doi
    doi3 <- str_split(base_data$DOI, "/")[[i]][3]
  }
  
  # n equal to author number
  n <- base_data$author_number[[i]]
  
  # get the author ids
  author_id_data_frame <-
    scrape_google_author_id(doi1, doi2, doi3, n, port)
  
  # fill the base data
  base_data[i, 21:24] <- author_id_data_frame
}


#### after getting the id of author
# add variables of authors through scholar_test package
# need 2 infos each author, so add 8 blank columns
namevector <-
  c(
    "hindex_au1",
    "hindex_au2",
    "hindex_au3",
    "hindex_au4",
    "totalcite_au1",
    "totalcite_au2",
    "totalcite_au3",
    "totalcite_au4"
  )
base_data[, namevector] <- NA

# get h_index of each author and total number of citations of each author
for (i in 1:nrow(base_data)) {
  if (is.na(base_data$author_id1[[i]]) != TRUE) {
    base_data[i, 25] <- get_profile(base_data$author_id1[[i]])$h_index
    base_data[i, 29] <-
      get_profile(base_data$author_id1[[i]])$total_cites
    
  }
  
  if (is.na(base_data$author_id2[[i]]) != TRUE) {
    base_data[i, 26] <- get_profile(base_data$author_id2[[i]])$h_index
    base_data[i, 30] <-
      get_profile(base_data$author_id2[[i]])$total_cites
    
  }
  
  if (is.na(base_data$author_id3[[i]]) != TRUE) {
    base_data[i, 27] <- get_profile(base_data$author_id3[[i]])$h_index
    base_data[i, 31] <-
      get_profile(base_data$author_id3[[i]])$total_cites
    
  }
  
  if (is.na(base_data$author_id4[[i]]) != TRUE) {
    base_data[i, 28] <- get_profile(base_data$author_id4[[i]])$h_index
    base_data[i, 32] <-
      get_profile(base_data$author_id4[[i]])$total_cites
    
  }
}

# check whether all data is scraped
which(is.na(base_data$totalcite_au1), arr.ind = TRUE)
which(is.na(base_data$author_id1), arr.ind = TRUE)
# equal, the infos of author are all scraped

# create a new variable which is the max value of h_index of all authors and the mean value of
# total citations of all authors
base_data$m_hindex <- rowMeans(base_data[, 25:28], na.rm = TRUE)
base_data$max_cite <- apply(base_data[, 29:32], 1, max, na.rm = TRUE)
base_data$max_hindex <- apply(base_data[, 25:28], 1, max, na.rm = TRUE)
base_data$m_cite <- rowMeans(base_data[, 29:32], na.rm = TRUE)


### scrape citations from google scholar
# create a driver from Rselenium
port <- random_port(min_port = 49152, max_port = 65536)
rD <-
  rsDriver(browser = "chrome",
           port = port,
           chromever = "105.0.5195.19")

# get the client
remDr <- rD$client

# set time outs to give the page the change to first fully load before
# we try to get information form it
remDr$setTimeout(type = "implicit", milliseconds = 10000)
remDr$setTimeout(type = "page load", milliseconds = 10000)

# get google citations
base_data$citation <- NA

for (i in 1:nrow(base_data)) {
  # split the doi to construct the url
  doi1 <- str_split(base_data$DOI, "/")[[i]][1]
  doi2 <- str_split(base_data$DOI, "/")[[i]][2]
  if (is.na(str_split(base_data$DOI, "/")[[i]][3]) != TRUE) {
    # if there is a third part of doi
    doi3 <- str_split(base_data$DOI, "/")[[i]][3]
  }
  
  # get the author ids
  citations <- scrape_google_citations(doi1, doi2, doi3, port)
  
  # fill the base data
  base_data[i, 38] <- citations
}

# delect the na value and zero value of variable citations
base_data <- base_data[!is.na(base_data$citations), ]
base_data <- base_data %>% filter(citations != 0)

# create the new variable as recency
base_data_with_date$month <- NA
base_data_with_date <- base_data[!is.na(base_data$date), ]

# create a new variable as month
for (i in 1:nrow(base_data_with_date)) {
  if (str_detect(base_data_with_date$date[i], "Jan")) {
    base_data_with_date$month[i] <- 1
  } else if (str_detect(base_data_with_date$date[i], "Feb")) {
    base_data_with_date$month[i] <- 2
  } else if (str_detect(base_data_with_date$date[i], "Mar")) {
    base_data_with_date$month[i] <- 3
  } else if (str_detect(base_data_with_date$date[i], "Apr")) {
    base_data_with_date$month[i] <- 4
  } else if (str_detect(base_data_with_date$date[i], "May")) {
    base_data_with_date$month[i] <- 5
  } else if (str_detect(base_data_with_date$date[i], "Jun")) {
    base_data_with_date$month[i] <- 6
  } else if (str_detect(base_data_with_date$date[i], "Jul")) {
    base_data_with_date$month[i] <- 7
  } else if (str_detect(base_data_with_date$date[i], "Aug")) {
    base_data_with_date$month[i] <- 8
  } else if (str_detect(base_data_with_date$date[i], "Sep")) {
    base_data_with_date$month[i] <- 9
  } else if (str_detect(base_data_with_date$date[i], "Oct")) {
    base_data_with_date$month[i] <- 10
  } else if (str_detect(base_data_with_date$date[i], "Nov")) {
    base_data_with_date$month[i] <- 11
  } else if (str_detect(base_data_with_date$date[i], "Dec")) {
    base_data_with_date$month[i] <- 12
  } else if (is.na(base_data_with_date$date[i])) {
    base_data_with_date$month[i] <- NA
  }
  
  
}

# create a new variable as year interval to 2022
base_data_with_date$interval <- NA
base_data_with_date <-
  base_data_with_date[!is.na(base_data_with_date$year), ]

final_data <- base_data_with_date
for (i in 1:nrow(final_data)) {
  if (final_data$year[i] == 2010) {
    final_data$interval[i] <- 12
  } else if (final_data$year[i] == 2011) {
    final_data$interval[i] <- 11
  } else if (final_data$year[i] == 2012) {
    final_data$interval[i] <- 10
  } else if (final_data$year[i] == 2013) {
    final_data$interval[i] <- 9
  } else if (final_data$year[i] == 2014) {
    final_data$interval[i] <- 8
  } else if (final_data$year[i] == 2015) {
    final_data$interval[i] <- 7
  } else if (final_data$year[i] == 2016) {
    final_data$interval[i] <- 6
  } else if (final_data$year[i] == 2017) {
    final_data$interval[i] <- 5
  } else if (final_data$year[i] == 2018) {
    final_data$interval[i] <- 4
  } else if (final_data$year[i] == 2019) {
    final_data$interval[i] <- 3
  } else if (final_data$year[i] == 2020) {
    final_data$interval[i] <- 2
  } else if (final_data$year[i] == 2021) {
    final_data$interval[i] <- 1
  }
  
}

final_data$recency <- final_data$interval * 12 + final_data$month

# include minimal hindex
final_data$minhindex <- NA
final_data$minhindex <-
  apply(final_data[, 25:28], 1, min, na.rm = TRUE)

# citation of each author at recent year
final_data$cite21_1 <- NA
final_data$cite21_2 <- NA
final_data$cite21_3 <- NA
final_data$cite21_4 <- NA

for (i in 1:nrow(final_data)) {
  if (is.na(final_data$author_id1[[i]]) != TRUE) {
    final_data[i, 56] <-
      get_citation_history(final_data$author_id1[[i]]) %>% filter(year == 2021) %>% select(cites)
  }
  
  if (is.na(final_data$author_id2[[i]]) != TRUE) {
    final_data[i, 57] <-
      get_citation_history(final_data$author_id2[[i]]) %>% filter(year == 2021) %>% select(cites)
    
  }
  
  if (is.na(final_data$author_id3[[i]]) != TRUE) {
    final_data[i, 58] <-
      get_citation_history(final_data$author_id3[[i]]) %>% filter(year == 2021) %>% select(cites)
    
  }
  
  if (is.na(final_data$author_id4[[i]]) != TRUE) {
    final_data[i, 59] <-
      get_citation_history(final_data$author_id4[[i]]) %>% filter(year == 2021) %>% select(cites)
    
  }
}

final_data$cinew_min <-
  apply(final_data[, 37:40], 1, min, na.rm = TRUE)
final_data$cinew_max <-
  apply(final_data[, 37:40], 1, max, na.rm = TRUE)

# # dummy variable superstar
# for (i in 1:nrow(final_data)) {
#   if (final_data$cinew_max[i] > 1500 &
#       is.na(final_data$cinew_max[i]) == FALSE) {
#     final_data$superstar3[i] <- 1
#   } else if (final_data$cinew_max[i] > 1500 &
#              is.na(final_data$cinew_max[i]) == FALSE) {
#     final_data$superstar3[i] <- 1
#   } else if (final_data$cinew_max[i] > 1500 &
#              is.na(final_data$cinew_max[i]) == FALSE) {
#     final_data$superstar3[i] <- 1
#   } else if (final_data$cinew_max[i] > 1500 &
#              is.na(final_data$cinew_max[i]) == FALSE) {
#     final_data$superstar3[i] <- 1
#   } else {
#     final_data$superstar3[i] <- 0
#   }
#
# }

# minimum of hindex within authors
final_data$minhindex <-
  apply(final_data[, 18:21], 1, min, na.rm = TRUE)


# scrape the citation infos for each year of every paper from google scholar
final_data$ci2010 <- NA
final_data$ci2011 <- NA
final_data$ci2012 <- NA
final_data$ci2013 <- NA
final_data$ci2014 <- NA
final_data$ci2015 <- NA
final_data$ci2016 <- NA
final_data$ci2017 <- NA
final_data$ci2018 <- NA
final_data$ci2019 <- NA
final_data$ci2020 <- NA
final_data$ci2021 <- NA

# create a driver from Rselenium
port <- random_port(min_port = 49152, max_port = 65536)
rD <-
  rsDriver(browser = "chrome",
           port = port,
           chromever = "107.0.5304.62")

# get the client
remDr <- rD$client

# set time outs to give the page the change to first fully load before
# we try to get information form it
remDr$setTimeout(type = "implicit", milliseconds = 10000)
remDr$setTimeout(type = "page load", milliseconds = 10000)

final_data <- final_data %>% filter(year != 2022)
for (i in 1:nrow(final_data)) {
  year_ab <- final_data$year[i]
  # split the doi to construct the url
  doi1 <- str_split(final_data$DOI, "/")[[i]][1]
  doi2 <- str_split(final_data$DOI, "/")[[i]][2]
  if (is.na(str_split(final_data$DOI, "/")[[i]][3]) != TRUE) {
    # if there is a third part of doi
    doi3 <- str_split(final_data$DOI, "/")[[i]][3]
  }
  for (year in year_ab:2021) {
    # get the author ids
    citations_year <-
      scrape_google_citations_year(doi1, doi2, doi3, year, port)
    
    # fill the base data
    final_data[i, 61 - 2021 + year] <- citations_year
  }
}

# select the data which has >0 page
final_data <- final_data %>% filter(page > 0)
