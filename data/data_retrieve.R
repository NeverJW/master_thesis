library(tidyverse)
library(xml2)
library(XML)
library(pander)
path <- "C:/Users/Jingwen/Documents/My%20EndNote%20Library.htm"
openFileInOS(path)

id <- "GAi23ssAAAAJ"
id <- tidy_id(id)

url_template <- "http://scholar.google.com/citations?hl=en&user=%s&pagesize=100&view_op=list_works"
url <- sprintf(url_template, id)

doc <- htmlParse(url)
cites <- xpathApply(doc, '//tr[@class="cit-table item"]')