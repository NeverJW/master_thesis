### get 2 years impact factor for four jounals
# journal of Marketing
url <-
  "https://www.scijournal.org/impact-factor-of-journal-of-marketing.shtml"
impactfin2022 <- 15.360
marketing_if <-
  get_2impact_factor(url, impactfin2022, "Journal of Marketing")

# journal of Marketing research
url2 <-
  "https://www.scijournal.org/impact-factor-of-journal-of-marketing-research.shtml"
impactfin2022 <- 6.664
marketingresearch_if <-
  get_2impact_factor(url2, impactfin2022, "Journal of Marketing Research")

# marketing science
url3 <-
  "https://www.scijournal.org/impact-factor-of-marketing-science.shtml"
impactfin2022 <- 3.716
marketingscience_if <-
  get_2impact_factor(url3, impactfin2022, "Marketing Science")

# journal of consumer research
url4 <-
  "https://www.scijournal.org/impact-factor-of-journal-of-consumer-research.shtml"
impactfin2022 <- 7.0
consumerresearch_if <-
  get_2impact_factor(url4, impactfin2022, "Journal of Consumer Research")

# merge four data frame
impactf_df <-
  rbind.data.frame(marketing_if,
                   marketingresearch_if,
                   marketingscience_if,
                   consumerresearch_if)
