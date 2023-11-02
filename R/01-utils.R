#' @title
#' Call the help center for full documentation
#'
#' @description
#' This function is a direct call to the documentation in the repository hosting
#' the package. The user can access the description of the latest version of the
#' package, the vignettes, and the list of functions.
#' 
#' @returns
#' Nothing to be returned. The function opens a web page.
#'
#' @examples
#' {
#'
#' madshapR_help()
#' 
#' }
#'
#' @importFrom utils browseURL
#'
#' @export
madshapR_help <- function(){

  browseURL("https://maelstrom-research.github.io/madshapR-documentation/")

}

#' @title
#' Built-in tibble of allowed valueType values
#'
#' @description
#' Provides a built-in tibble showing the list of allowed Opal valueType values
#' and their corresponding R data types. This tibble is mainly used for 
#' internal processes and programming.
#'
#' @details
#' The valueType is a property of a variable and is required in certain 
#' functions to determine the handling of the variables. The valueType refers 
#' to the OBiBa-internal type of a variable. It is specified in a data 
#' dictionary in a column `valueType` and can be associated with variables as 
#' attributes. Acceptable valueTypes include 'text', 'integer', 'decimal', 
#' 'boolean', datetime', 'date'). The full list of OBiBa valueType 
#' possibilities and their correspondence with R data types are available using 
#' [madshapR::valueType_list].
#'
#' @seealso
#' [Opal documentation](https://opaldoc.obiba.org/en/dev/magma-user-guide/value/type.html)
#'
#' @format ## `tibble`
#' A data frame with 12 rows and 7 columns:
#' \describe{
#'   \item{valueType}{data type as described in Opal}
#'   \item{typeof}{data type provided by base::typeof}
#'   \item{class}{data class provided by base::class}
#'   \item{call}{function to transpose object according base::do.call function}
#'   \item{toValueType}{ensemble data type as described in Opal}
#'   \item{toTypeof}{ensemble data type provided by base::typeof}
#'   \item{genericType}{ensemble data type which valueType belongs}
#'   ...
#' }
#'
#' @examples
#' {
#'
#' print(valueType_list)
#'
#' }
"valueType_list"

#' @title
#' Built-in material allowing the user to test the package with demo data
#'
#' @description
#' Built-in tibbles and lists allowing the user to test the package with demo
#' material.
#'
#' @format ## `list`
#' A list with 20 elements used for testing the package 
#' (data frames and lists):
#'
#' \describe{
#'   \item{dd_MELBOURNE_1_format_maelstrom}{Data dictionary (1) of Melbourne 
#'   dataset}
#'   \item{dd_MELBOURNE_2_format_maelstrom}{Data dictionary (2) of Melbourne 
#'   dataset}
#'   \item{dd_PARIS_format_flatten - ERROR}{Data dictionary of Paris dataset 
#'   containing errors for testing purpose}
#'   \item{dd_PARIS_format_flatten}{Data dictionary of Paris in preprocessed 
#'   format}
#'   \item{dd_PARIS_format_maelstrom}{Data dictionary of Paris dataset}
#'   \item{dd_TOKYO_format_maelstrom}{Data dictionary of Tokyo dataset}
#'   \item{dd_TOKYO_format_maelstrom_tagged - ERROR WITH DATA}{Tagged 
#'   data dictionary of Tokyo dataset containing errors for testing purpose}
#'   \item{dd_TOKYO_format_maelstrom_tagged - ERROR}{Tagged data dictionary of 
#'   Tokyo dataset containing errors for testing purpose}
#'   \item{dd_TOKYO_format_maelstrom_tagged}{Tagged data dictionary of Tokyo 
#'   dataset}
#'   \item{dd_TOKYO_format_opal_tagged - ERROR WITH TAXO}{Tagged 
#'   data dictionary of Tokyo dataset containing errors for testing purpose}
#'   \item{dd_TOKYO_format_opal_tagged}{Tagged data dictionary of Tokyo dataset 
#'   opal format}
#'   \item{dataset_MELBOURNE_1}{Dataset of Melbourne (1)}
#'   \item{dataset_MELBOURNE_2}{Dataset of Melbourne (2)}
#'   \item{dataset_PARIS}{Dataset of Paris}
#'   \item{dataset_TOKYO - ERROR WITH DATA}{Dataset of Tokyo containing errors 
#'   for testing purpose}
#'   \item{dataset_TOKYO}{Dataset of Tokyo}
#'   \item{taxonomy_PARIS}{Taxonomy specific to Paris dataset}
#'   \item{taxonomy_opal}{Opal Taxonomy}
#'   \item{taxonomy_opal_mlstr}{Maelstrom Taxonomy}
#'   \item{summary_var}{Variables summary for testing purpose}
#'   ...
#' }
#'
#' @examples
#' {
#'
#'  print(DEMO_files$dataset_TOKYO)
#'
#' }
"DEMO_files"


#' @title
#' Add an index column at the first place of a tibble
#'
#' @description
#' Add an index, possibly by group, at the first place of a data frame or a
#' tibble The name by default is 'index' but can be named. If 'index' already
#' exists, or the given name, the column can be forced to be created, and
#' replace the other one.
#'
#' @param tbl tibble or data frame
#' @param name_index A character string of the name of the column.
#' @param start integer indicating first index number. 1 by default.
#' @param .force TRUE or FALSE, that parameter indicates whether or not the
#' column is created if already exists. FALSE by default.
#'
#' @return
#' A tibble or a data frame containing one extra first column 'index' or
#' any given name.
#'
#' @examples
#' {
#'
#' ##### Example 1 -------------------------------------------------------------
#' # add an index for the tibble
#' add_index(iris, "my_index")
#'
#' ##### Example 2 -------------------------------------------------------------
#' # add an index for the grouped tibble
#' library(tidyr)
#' library(dplyr)
#'
#' my_tbl <- tibble(iris) %>% group_by(Species) %>% slice(1:3)
#' add_index(my_tbl, "my_index")
#'
#' }
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @export
add_index <- function(tbl, name_index = "index", start = 1, .force = FALSE){
  
  class_tbl <- toString(class(tbl))
  group_name <- group_vars(tbl)
  `madshapR::start` <- start
  
  tbl_index <-
    data.frame(index = NA_integer_) %>%
    rename_with(.cols = 'index', ~ name_index)
  
  if(.force == FALSE){
    
    if(name_index %in% (tbl %>% names)){
      stop(paste0("\n\nThe column ",name_index," already exists.\n",
                  "Please specifie another name or use .force = TRUE\n"))}
    
    tbl <- suppressMessages(bind_cols(tbl_index,tbl))
  }else{
    tbl <- suppressMessages(bind_cols(tbl_index,tbl %>%
                                        select(-any_of(name_index))))}
  
  
  if(length(group_name)) tbl <- group_by_at(tbl, group_name)
  
  tbl <- tbl %>% mutate(across(all_of(name_index),
                               ~ as.integer(row_number() + 
                                              `madshapR::start` - 1)))
  
  if(str_detect(class_tbl,"tbl")) tbl <- tibble(tbl)
  
  return(tbl)
}
