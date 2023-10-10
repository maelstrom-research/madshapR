## code to prepare `valueType_list` dataset goes here
library(tidyverse)
library(fabR)
library(opalr)
library(mlstrOpalr)
library(madshapR)

DEMO_files <-
  file_index_create('../DEMO/DEMO_V8_madshapR/') %>%
  file_index_read()

DEMO_files$dd_PARIS_format_flatten <-
  list(Variables =
         DEMO_files$dd_PARIS_format_flatten)

DEMO_files$`dd_PARIS_format_flatten - ERROR` <-
  list(Variables =
         DEMO_files$`dd_PARIS_format_flatten - ERROR`)


opal <- opal.login('gfabre','gfabre1234',
                   url='https://opal.maelstrom-research.org/')
DEMO_files$taxonomy_opal <- taxonomy_opal_get(opal)
DEMO_files$taxonomy_opal_mlstr <- taxonomy_opal_mlstr_get(opal)
dataset <-
  DEMO_files$dataset_TOKYO %>%
  select(height)

data_dict <-
  DEMO_files$dd_TOKYO_format_maelstrom %>%
  as_data_dict_mlstr() %>%
  data_dict_filter('name == "height"')

.summary_var <- dataset_summarize(
  dataset = dataset,
  data_dict = data_dict,
  valueType_guess = TRUE)

DEMO_files$summary_var <- .summary_var

usethis::use_data(DEMO_files, overwrite = TRUE)



View(DEMO_files)



