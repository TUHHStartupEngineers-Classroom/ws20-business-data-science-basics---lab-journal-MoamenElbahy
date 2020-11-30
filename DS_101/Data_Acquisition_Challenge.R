################################ Business Data Science Basics ################################
# Chapter 3 Data Acquisition: Challenge.

library(RSQLite)
library(dplyr)
library(httr)
library(glue)
library(jsonlite)
library(keyring)
library(rvest)
library(stringr)
library(purrr)
library(xopen)
library(stringi)
library(tibble)

################################################## Task_1 ##################################################

resp = GET("http://api.openweathermap.org/data/2.5/weather?q=London,uk&APPID=a1e8fec679a9d4e32d525dc165c0af27")

#convert the raw Unicode into a character vector that resembles the JSON format
 rawToChar(resp$content)

#Lists can be accessed
resp %>%
  .$content %>%
   rawToChar() %>%
   fromJSON()


################################################## Task_2 ##################################################

url= "https://www.radon-bikes.de/"

#Wrap it into a function ----
  
get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".catalog-category-bikes__title-text") %>%
    html_text()%>%
    enframe(name = "Number", value = "Bike.Name")
  
  # Get the descriptions
  bike_database_tbl<-bike_url_tbl%>% 
    mutate(price=html_bike_category%>%
             html_nodes(css =".catalog-category-bikes__price-title")%>%
             html_text())
}

bike_tableout<-get_bike_data(url)
bike_tableout
saveRDS(bike_tableout,"Data_Acquisition_Challenge.rds")