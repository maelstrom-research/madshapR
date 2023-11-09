## code to prepare `valueType_list` dataset goes here
library(tidyverse)
library(fabR)
library(opalr)
library(mlstrOpalr)
library(madshapR)

DEMO_files <-
  file_index_create('../DEMO/DEMO_V10_madshapR/') %>%
  file_index_read()

DEMO_files$`data_dict_PARIS - flatten` <-
  list(Variables =
         DEMO_files$`data_dict_PARIS - flatten`)

usethis::use_data(DEMO_files, overwrite = TRUE)


View(DEMO_files)



