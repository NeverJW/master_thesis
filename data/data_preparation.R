# Source script setting up all libraries
source("setup.R")

# load excel base data from endnote 
base_data <- read_excel("data/data_endnote.xlsx")

# remove na column
base_data <- base_data[, colSums(is.na(base_data)) < nrow(base_data)]

# remove unnecessary column 
base_data <- base_data  # journal type
base_data <- subset(base_data, select = -c(language,url, ISSN, DOI, volume, issue, `alternate journal`, `author address`))

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


