#### Loading libraries needed ####
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("xml2")) install.packages("xml2")
if (!require("XML")) install.packages("XML")
if (!require("RSelenium")) install.packages("RSelenium")
if (!require("readxl")) install.packages("readxl")
if (!require("scholar")) install.packages("scholar")
if (!require("rvest")) install.packages("rvest")
if (!require("miceadds")) install.packages("miceadds")
if (!require("pbdZMQ")) install.packages("pbdZMQ")


library(tidyverse)
library(xml2)
library(XML)
library(RSelenium)
library(readxl)
library(scholar)
library(rvest)
library(miceadds)
library(pbdZMQ)

# Source all R-files in the Functions folder
source.all("function/", grepstring="\\.R")

