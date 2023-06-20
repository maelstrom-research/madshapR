# ## code to prepare `valueType_list` dataset goes here
# DEMO_files <-
#   file_index_create('data-raw/DEMO_V8_madshapR/') %>%
#   file_index_read()
#
# DEMO_files$dd_PARIS_format_flatten <-
#   list(Variables =
#          DEMO_files$dd_PARIS_format_flatten)
# 
# DEMO_files$`dd_PARIS_format_flatten - ERROR` <-
#   list(Variables =
#          DEMO_files$`dd_PARIS_format_flatten - ERROR`)
# 
# DEMO_files$`DEMO_data_processing_elements - final`<-
#   list(dataProcessing =
#          DEMO_files$`DEMO_data_processing_elements - final`)
# 
# DEMO_files$`DEMO_data_processing_elements - with error`<-
#   list(dataProcessing =
#          DEMO_files$`DEMO_data_processing_elements - with error`)
# 
# DEMO_files$`DEMO_data_processing_elements - work in progress` <-
#   list(dataProcessing =
#          DEMO_files$`DEMO_data_processing_elements - work in progress`)
#
# library(tidyverse)
# library(opalr)
# library(madshapR)
# opal <- opal.login('xxxx','xxxx',
#                    url='https://opal.maelstrom-research.org/')
# DEMO_files$taxonomy_opal <- opal_taxonomy_get(opal) 
# DEMO_files$taxonomy_opal_mlstr <- opal_mlstr_taxonomy_get(opal)
# dataset <-
#   DEMO_files$dataset_TOKYO %>%
#   select(height)
# 
# data_dict <-
#   DEMO_files$dd_TOKYO_format_maelstrom_tagged %>%
#   as_data_dict_mlstr() %>%
#   data_dict_filter('name == "height"')
# 
# .summary_var <- dataset_summarize(
#   dataset = dataset,
#   data_dict = data_dict,
#   valueType_guess = TRUE)
# 
# DEMO_files$summary_var <- .summary_var
# 
# usethis::use_data(DEMO_files, overwrite = TRUE)








