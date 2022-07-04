# journal of marketing 
# use selenium to get id of author
get_download_time <- function(doi1, doi2, port = 400L){
  url <- paste0("https://journals.sagepub.com/doi/metrics/", doi1, "/",
                doi2)
 
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
  
  # # access the "ACCEPT ALL" button
  # cookie_frame_accept_button <- remDr$findElement(using = 'xpath', 
  #                                                 "//button[@title='ACCEPT ALL']")
  # # now click the accept all button
  # cookie_frame_accept_button$clickElement()
  
  # # click one particular paper
  # click_paper <- remDr$findElement(using = 'xpath',  "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'heading-title', ' ' ))]")
  # click_paper$clickElement()
  
  # extract the html of the page
  page_html <- read_html(remDr$getPageSource()[[1]])
  
  # extract the download times of the paper
  download_time <- page_html %>%
    html_nodes(xpath = "//*[(@id = '3a8b6866-bca2-4174-8207-d5503f46c431')]//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'body-compact-horizontal', ' ' ))]") %>%
    html_text() %>% 
    str_split(": ") 
  
  download_time <- download_time[[1]][2]
    

  
  }