################################ Business Data Science Basics ################################
# Chapter 3 Data Wrangling: Challenge.
library(data.table)
library(vroom)
library(tidyverse)
library(dbplyr)
library(tictoc)


col_types <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

patentassignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)


col_types2 <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types2,
  na         = c("", "NA", "NULL")
)


class(patentassignee_table)
setDT(patentassignee_table)
class(assignee_table)
setDT(assignee_table)






