# scrape the author id of google scholar 
# for the purpose that getting information of authors via package scholars

scrape_google_author_id <- function(doi1, doi2, doi3, n, port = 800L){
  
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
  } else { # if there is third part of doi
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
  
  # create a driver from Rselenium
  rD <- rsDriver(browser = "chrome", port = port, chromever="103.0.5060.24")
  
  # get the client
  remDr <- rD$client
  
  # set time outs to give the page the change to first fully load before
  # we try to get information form it
  remDr$setTimeout(type = "implicit", milliseconds = 10000)
  remDr$setTimeout(type = "page load", milliseconds = 10000)
  
  # navigate to the created url
  remDr$navigate(url)
  
  Sys.sleep(2)
  
  if (1 <= n) {
    tryCatch({
      # click the first author
      click_author <-
        remDr$findElement(using = 'css',  ".gs_a a:nth-child(1)")
      click_author$clickElement()
    }, error = function(e) {
      # when there is a error which means there is no individual website link of the author
      # then skip
    })
  Sys.sleep(2)
  
  # extract the html of the page
  page_html <- read_html(remDr$getPageSource()[[1]])
  
  # extract the author id via web link
  link <- page_html %>%
    html_nodes(xpath = "//link") %>%
    html_attr("href") 
  
  author_id1 <- str_split(link[2], "=")[[1]][2]
  
  Sys.sleep(2)
}
  if (2 <= n) {  
    # navigate to the created url, scrape the second author id
    remDr$navigate(url)
    Sys.sleep(2)

    tryCatch({
      # click the second author
      click_author <-
        remDr$findElement(using = 'css',  ".gs_a a:nth-child(2)")
      click_author$clickElement()
    }, error = function(e) {
    })
  
  # extract the html of the page
  page_html <- read_html(remDr$getPageSource()[[1]])
  
  # extract the author id via web link
  link <- page_html %>%
    html_nodes(xpath = "//link") %>%
    html_attr("href") 
  
  author_id2 <- str_split(link[2], "=")[[1]][2]
  
  Sys.sleep(2)
  }
  
  
  if (3 <= n){  
  # navigate to the created url, scrape the third author id
  remDr$navigate(url)
  Sys.sleep(2)
  # click the third author
  tryCatch({
    click_author <-
      remDr$findElement(using = 'css',  ".gs_a a:nth-child(3)")
    click_author$clickElement()
  }, error = function(e) {
  })
  
  # extract the html of the page
  page_html <- read_html(remDr$getPageSource()[[1]])
  
  # extract the author id via web link
  link <- page_html %>%
    html_nodes(xpath = "//link") %>%
    html_attr("href") 
  
  author_id3 <- str_split(link[2], "=")[[1]][2]
  
  Sys.sleep(2)
  }
  
  # navigate to the created url, scrape the fouth author id
  remDr$navigate(url)
  
  if (4 <= n){
    Sys.sleep(2)
    
    tryCatch({
    # click the fouth author
    click_author <- remDr$findElement(using = 'css',  ".gs_a a:nth-child(4)")
    click_author$clickElement()
    }, error = function(e) {
    })
    
  # extract the html of the page
  page_html <- read_html(remDr$getPageSource()[[1]])
  
  # extract the author id via web link
  link <- page_html %>%
    html_nodes(xpath = "//link") %>%
    html_attr("href") 
  
  author_id4 <- str_split(link[2], "=")[[1]][2]
  
  Sys.sleep(2)
  }
  
}

get_citation_history(author_id)
get_profile(author_id)$h_index
get_profile(author_id)$total_cites
get_publications(author_id)
