# scrape the author id of google scholar
# for the purpose that getting information of authors via package scholars

scrape_google_author_id <-
  function(doi1, doi2, doi3, n, port = 800L) {
    # # create a driver from Rselenium
    # rD <- rsDriver(browser = "chrome", port = port, chromever="103.0.5060.24")
    #
    # # get the client
    # remDr <- rD$client
    #
    # # set time outs to give the page the change to first fully load before
    # # we try to get information form it
    # remDr$setTimeout(type = "implicit", milliseconds = 10000)
    # remDr$setTimeout(type = "page load", milliseconds = 10000)
    
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
    
    if (1 <= n) {
      tryCatch({
        # click the first author
        click_author <-
          remDr$findElement(using = 'css',  ".gs_a a:nth-child(1)")
        click_author$clickElement()
      }, error = function(e) {
        # when there is a error which means there is no individual website link of the author
        # then skip
        author_id1 <- NA
      }, finally = {
        # extract the html of the page
        page_html <- read_html(remDr$getPageSource()[[1]])
        
        # extract the author id via web link
        link <- page_html %>%
          html_nodes(xpath = "//link") %>%
          html_attr("href")
        
        author_id1 <- str_split(link[2], "=")[[1]][2]
      })
    } else {
      author_id1 <- NA
    }
    
    # navigate to the created url, scrape the second author id
    if (2 <= n) {
      remDr$navigate(url)
      Sys.sleep(2)
      
      tryCatch({
        # click the second author
        click_author <-
          remDr$findElement(using = 'css',  ".gs_a a:nth-child(2)")
        click_author$clickElement()
      }, error = function(e) {
        author_id2 <- NA
      }, finally = {
        # if there is a second author then scrape the id
        # extract the html of the page
        page_html <- read_html(remDr$getPageSource()[[1]])
        
        # extract the author id via web link
        link <- page_html %>%
          html_nodes(xpath = "//link") %>%
          html_attr("href")
        
        author_id2 <- str_split(link[2], "=")[[1]][2]
      })
    } else {
      author_id2 <- NA   # if there is no author anymore then NA
    }
    
    # navigate to the created url, scrape the third author id
    if (3 <= n) {
      remDr$navigate(url)
      Sys.sleep(2)
      # click the third author
      tryCatch({
        click_author <-
          remDr$findElement(using = 'css',  ".gs_a a:nth-child(3)")
        
        click_author$clickElement()
      }, error = function(e) {
        author_id3 <- NA
      }, finally = {
        # extract the html of the page
        page_html <- read_html(remDr$getPageSource()[[1]])
        
        # extract the author id via web link
        link <- page_html %>%
          html_nodes(xpath = "//link") %>%
          html_attr("href")
        
        # get the author id
        author_id3 <- str_split(link[2], "=")[[1]][2]
        
        Sys.sleep(2)
      })
    } else {
      author_id3 <- NA  # if there is no author anymore then NA
    }
    
    # navigate to the created url, scrape the fourth author id
    
    if (4 <= n) {
      remDr$navigate(url)
      Sys.sleep(2)
      tryCatch({
        # click the fouth author
        click_author <-
          remDr$findElement(using = 'css',  ".gs_a a:nth-child(4)")
        click_author$clickElement()
      }, error = function(e) {
        author_id4 <- NA
      }, finally = {
        # extract the html of the page
        page_html <- read_html(remDr$getPageSource()[[1]])
        
        # extract the author id via web link
        link <- page_html %>%
          html_nodes(xpath = "//link") %>%
          html_attr("href")
        
        author_id4 <- str_split(link[2], "=")[[1]][2]
        
        Sys.sleep(2)
      })
    } else {
      author_id4 <- NA # if there is no author anymore then NA
    }
    
    # return a data frame which contains all the scraped author ids
    id_df <-
      data.frame(
        author_id1 = author_id1,
        author_id2 = author_id2,
        author_id3 = author_id3,
        author_id4 = author_id4
      )
    
    return(id_df)
  }
