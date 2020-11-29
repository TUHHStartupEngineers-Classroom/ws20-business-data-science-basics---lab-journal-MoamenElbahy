################################ Business Data Science Basics ################################
# Chapter 2 Intro to the tidyverse: Challenge.

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)

setwd("D:/TUHH/Current_SEMESTER/DataScience/ws20-business-data-science-basics---lab-journal-MoamenElbahy/DS_101")

# 3.0 Examining Data ----

orderlines_tbl
glimpse(orderlines_tbl)

# 4.0 Joining Data ----

left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----

bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

bike_orderlines_wrangled_city_separated_tbl <- bike_orderlines_joined_tbl %>%
 
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>% 
  
  separate(col    = location,
           into   = c("City", "State"),
           sep    = ", ") %>% 
  
  mutate(total.price = price * quantity)%>%
  
  select(-...1, -gender)%>%
  
  select(-ends_with(".id"))%>%
  
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
  
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  
  rename(bikeshop = name) %>%
  
  set_names(names(.) %>% 
              
  str_replace_all("\\.", "_"))

# 6.0 Business Insights ----

# 6.1 Analyze the sales by location (state) ----

library(lubridate)
# Step 1 - Manipulate
sales_by_location_tbl <- bike_orderlines_wrangled_city_separated_tbl %>%
  
  select(State, City, total_price) %>%
  
  group_by(State) %>%
  
  summarize(state_sales = sum(total_price)) %>%
  
  mutate(sales_formatted = scales::dollar(state_sales, big.mark = ".", 
                                          decimal.mark = ",", 
                                          prefix = "", 
                                          suffix = " €"))

# Step 2 - Visualize
sales_by_location_tbl %>%
  
  ggplot(aes(x = State, y = state_sales)) +
  
  geom_col(fill = "#2DC6D6") + 
  
  geom_label(aes(label = sales_formatted)) + 
  
  geom_smooth(method = "lm", se = FALSE) + 
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  labs(
    title    = "States revenue by year",
    x = "",
    y = "Revenue"
  )

# 6.2 Analyze the sales by location and year  ----

# Step 1 - Manipulate
sales_by_state_year_tbl <- bike_orderlines_wrangled_city_separated_tbl %>%
  
  select(order_date, total_price, State) %>%
  mutate(year = year(order_date)) %>%
  
  group_by(State, year) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(sales_formatted = scales::dollar(sales, big.mark = ".", 
                                          decimal.mark = ",", 
                                          prefix = "", 
                                          suffix = " €"))

# Step 2 - Visualize
sales_by_state_year_tbl %>%
  
  ggplot(aes(x = year, y = sales, fill = State)) +
  
  geom_col() +
  
  facet_wrap(~ State) +
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and state",
    subtitle = "Each State has a dynamic trend",
    fill = "States"
  )

# 7.0 Writing Files ----

# 7.1 Excel ----
install.packages("writexl")
library("writexl")
bike_orderlines_wrangled_city_separated_tbl %>%
  write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_city_separated_tbl %>% 
  write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
bike_orderlines_wrangled_city_separated_tbl %>% 
  write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
