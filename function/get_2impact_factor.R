### get 2 years of impact factor for four journals 

get_2impact_factor <- function(url, impactfin2022, journal){
url <-
  read_html(url)
impactf <-
  url %>% html_nodes(
    "li:nth-child(2) span , li:nth-child(3) span, li:nth-child(4) span, li:nth-child(5) span, li:nth-child(6) span, li:nth-child(7) span, li:nth-child(8) span, li:nth-child(9) span, li:nth-child(10) span, li:nth-child(11) span, li:nth-child(12) span, li:nth-child(1) span"
  ) %>% html_text()

# add year 
year <- 2022:2010

# add journal
journal <- journal

# create the data frame
marketingif <-
  data.frame(journal, year, impactf[c(1, 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35)])

# add 2022 impact factor
marketingif[1, 3] <- impactfin2022

# rename the column
names(marketingif)[2] <- "year"
names(marketingif)[3] <- "impactf"

return(marketingif)
}
