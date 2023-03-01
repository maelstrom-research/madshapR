# ## code to prepare `valueType_list` dataset goes here
# DEMO_files <-
#   file_index_create('data-raw/DEMO_V8_datashapR/') %>%
#   file_index_read()
#
# DEMO_files$dd_PARIS_format_flatten                            <- list(Variables = DEMO_files$dd_PARIS_format_flatten)
# DEMO_files$`dd_PARIS_format_flatten - ERROR`                  <- list(Variables = DEMO_files$`dd_PARIS_format_flatten - ERROR`)
# DEMO_files$`DEMO_data_processing_elements - final`            <- list(dataProcessing = DEMO_files$`DEMO_data_processing_elements - final`)
# DEMO_files$`DEMO_data_processing_elements - with error`       <- list(dataProcessing = DEMO_files$`DEMO_data_processing_elements - with error`)
# DEMO_files$`DEMO_data_processing_elements - work in progress` <- list(dataProcessing = DEMO_files$`DEMO_data_processing_elements - work in progress`)
#
# o <- opal.login('gfabre','gfabre1234',url='https://opal.maelstrom-research.org/')
# DEMO_files$Opal_taxonomy <- opal_taxonomy_get(o) %>% as_tibble() %>% as_taxonomy()
# DEMO_files$Mlstr_taxonomy <- opal_taxonomy_get(o) %>% as_tibble() %>% as_taxonomy()
# usethis::use_data(DEMO_files, overwrite = TRUE)
