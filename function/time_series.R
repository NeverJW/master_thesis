### scrape citations from google scholar
# create a driver from Rselenium
port <- random_port(min_port = 49152, max_port = 65536)
rD <- rsDriver(browser = "chrome", port = port, chromever="107.0.5304.62")

# get the client
remDr <- rD$client

# set time outs to give the page the change to first fully load before
# we try to get information form it
remDr$setTimeout(type = "implicit", milliseconds = 10000)
remDr$setTimeout(type = "page load", milliseconds = 10000)


scrape_google_citations_year <-
  function(doi1, doi2, doi3, year, port = 800L) {
    
    # if there is no third part of doi
    if (is.na(str_split(data_with_date$DOI, "/")[[i]][3]) == TRUE) {
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
    
    Sys.sleep(1)
    # click the url of the citations
    url_ci <- read_html(remDr$getPageSource()[[1]]) %>%
      html_elements(css="a:nth-child(3)")
    
    citations_url <- xml_attrs(url_ci[[5]])[["href"]]
    citations_url <-
      paste0(
        "https://scholar.google.com",
        citations_url,
        "&scipsc=&as_ylo=",
        year,
        "&as_yhi=",
        year
      )
    
    
    # navigate to the created url
    remDr$navigate(citations_url)
    Sys.sleep(1)
    
    num_citation <- read_html(remDr$getPageSource()[[1]]) %>%
      html_elements(css="#gs_ab_md .gs_ab_mdw")
    
    num_citation <- as.numeric(str_extract(num_citation[1], "\\d+"))
    
    
    return(num_citation)
  }


  
