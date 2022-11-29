##### not be used

scrape_citations_pubandforward <- function(year) {
  for (i in 1:nrow(data_with_date)) {
    if (is.na(data_with_date$author_id1[[i]]) != TRUE) {
      data_with_date[i, 56] <-
        get_citation_history(data_with_date$author_id1[[i]]) %>% filter(year ==
                                                                          year) %>% select(cites)
    }
    
    if (is.na(data_with_date$author_id2[[i]]) != TRUE) {
      data_with_date[i, 57] <-
        get_citation_history(data_with_date$author_id2[[i]]) %>% filter(year ==
                                                                          year) %>% select(cites)
      
    }
    
    if (is.na(data_with_date$author_id3[[i]]) != TRUE) {
      data_with_date[i, 58] <-
        get_citation_history(data_with_date$author_id3[[i]]) %>% filter(year ==
                                                                          year) %>% select(cites)
      
    }
    
    if (is.na(base_data_with_date$author_id4[[i]]) != TRUE) {
      data_with_date[i, 59] <-
        get_citation_history(data_with_date$author_id4[[i]]) %>% filter(year ==
                                                                          year) %>% select(cites)
      
    }
  }
}
