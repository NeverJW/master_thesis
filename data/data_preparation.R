# Source script setting up all libraries
source("setup.R")

# load excel base data from endnote 
base_data <- read_excel("data/data_endnote.xlsx")

# remove na column
base_data <- base_data[, colSums(is.na(base_data)) < nrow(base_data)]

# remove unnecessary column 
base_data <- base_data  # journal type
base_data <- subset(base_data, select = -c(language,url, ISSN, volume, issue, `alternate journal`, `author address`))

# remove duplicates row
base_data <- base_data[!duplicated(base_data),]

# create the author numbers variable
base_data$author_number <- lengths(gregexpr(";",  base_data$author)) + 1
base_data$author_number <-
  ifelse(str_detect(base_data$author, ";"), base_data$author_number, 1)

# calculate total pages of paper
base_data$page1 <- str_split_fixed(base_data$pages, "-", 2)[,1]
base_data$page2 <- str_split_fixed(base_data$pages, "-", 2)[,2]
base_data$page <- as.numeric(base_data$page2) - as.numeric(base_data$page1)

base_data <- subset(base_data, select = -c(page1, page2))

# extract citation counts and references counts
base_data$citations <-  str_split_fixed(base_data$notes, ":",3)[,2] %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
base_data$reference_count <-  str_split_fixed(base_data$notes, ":",3)[,3] %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric

# remove reference_count equal to 0
base_data <- base_data %>% filter(reference_count!=0)

### add 2 years impact factor of journals 
source("data/get_journal_features.R")

# merge 2 years impact factor data frame to our base data
base_data <- left_join(base_data, impactf_df, by = c("year", "journal"))

# add the variable which indicate the length of title 
base_data$title_length <-
  if_else(
    str_count(base_data$title, " ") + 1 < 5 ,
    "short",
    if_else(str_count(base_data$title, " ") + 1 < 15,
    "normal",
    "long")
  )

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

