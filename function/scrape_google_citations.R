# scrape the author id of google scholar 
# for the purpose that getting information of authors via package scholars

scrape_google_citations <-
  function(doi1, doi2, doi3, port = 800L) {
    
    # if there is no third part of doi
    if (is.na(str_split(base_data$DOI, "/")[[i]][3]) == TRUE) {
      url <-
        paste0(
          "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=",
          doi1,
          "%2F",
          doi2,
          "&btnG="
        )
    } else {
      # if there is third part of doi
      url <-
        paste0(
          "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=",
          doi1,
          "%2F",
          doi2,
          "%2F",
          doi3,
          "&btnG="
        )
    }
    
    # navigate to the created url
    remDr$navigate(url)
    
    Sys.sleep(2)
    
    # get the url of the citations
    url_ci <- read_html(remDr$getPageSource()[[1]]) %>%
      html_elements(css="a:nth-child(3)")
    
    citations <- 
      str_split(tail(url_ci,2)[1], ">")[[1]][2] %>%
      str_extract_all("\\(?[0-9,.]+\\)?")
    
      citations <- as.numeric(citations[[1]][1])
    return(citations)
  }
