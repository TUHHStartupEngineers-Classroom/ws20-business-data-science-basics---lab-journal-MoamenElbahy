################################ Business Data Science Basics ################################
# Chapter 3 Data Acquisition: Challenge.

library(RSQLite)
library(dplyr)
library(httr)
library(glue)
library(jsonlite)
library(keyring)
library(rvest)
library(owmr)
library(stringr)
library(purrr)
library(xopen)
library(stringi)
library(tibble)

################################################## Task_1 ##################################################
resp = GET("http://api.openweathermap.org/data/2.5/weather?q=London,uk&APPID=a1e8fec679a9d4e32d525dc165c0af27")

rawToChar(resp$content)

resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()

# first of all you have to set up your api key
OWM_API_KEY="a1e8fec679a9d4e32d525dc165c0af27"
owmr_settings(OWM_API_KEY)

(Alexandria_Egypt <- search_city_list("Alexandria","EG")) %>%
  as.list()

# get forecast
Alexandria_EG_forecast <- get_forecast("London", units = "metric") %>%
  owmr_as_tibble()

Alexandria_EG_forecast[, 1:10]

################################################## Task_2 ##################################################

#Wrap it into a function ----

url= "https://www.rosebikes.de/fahrräder/mtb"
  
get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".catalog-category-bikes__title-text") %>%
    html_text()%>%
    enframe(name = "No.", value = "Bike.Name")
  
  bike_database_tbl<-bike_url_tbl%>%
    mutate(price=html_bike_category%>%
             html_nodes(css =".catalog-category-bikes__price-title")%>%
             html_text())
}
bike_tableout<-get_bike_data(url) 

bike_tableout
saveRDS(bike_tableout,"Data_Acquisition_Challenge.rds")