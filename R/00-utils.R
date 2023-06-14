#' @title
#' Call the help center for full documentation
#'
#' @description
#' This function is a direct call to the documentation in the repository hosting
#' the package. The user can access the description of the latest version of the
#' package, the vignettes, and the list of functions.
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
#' A data frame with 12 rows and 6 columns:
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
#' valueType_list
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
#' A list with 22 elements used for testing the package (data frames and lists):
#'
#' \describe{
#'   \item{DEMO_data_processing_elements - final}{Data processing element
#'   without error}
#'   \item{DEMO_data_processing_elements - with error}{Data processing element
#'   containing errors}
#'   \item{DEMO_data_processing_elements - work in progress}{Data processing
#'   element in construction}
#'   \item{DEMO_dataschema}{DataSchema used, combined with data processing
#'   elements and study-specific material}
#'   \item{dd_MELBOURNE_1_format_maelstrom}{Data dictionary (1) of Melbourne
#'   dataset}
#'   \item{dd_MELBOURNE_2_format_maelstrom}{Data dictionary (2) of Melbourne
#'   dataset}
#'   \item{dd_PARIS_format_maelstrom}{Data dictionary of Paris dataset}
#'   \item{dd_PARIS_format_preprocessed - ERROR}{Data dictionary of Paris
#'   dataset containing errors}
#'   \item{dd_PARIS_format_preprocessed}{Data dictionary of Paris in
#'   preprocessed format}
#'   \item{dd_TOKYO_format_maelstrom_tagged - ERROR WITH DATA}{Data dictionary
#'   of Tokyo dataset containing errors}
#'   \item{dd_TOKYO_format_maelstrom_tagged - ERROR}{Data dictionary of Tokyo
#'   dataset containing errors}
#'   \item{dd_TOKYO_format_maelstrom_tagged}{Data dictionary of Tokyo dataset}
#'   \item{dd_TOKYO_format_opal_tagged - ERROR WITH TAXO}{Data dictionary of
#'   Tokyo dataset containing errors}
#'   \item{dd_TOKYO_format_opal_tagged}{Data dictionary of Tokyo dataset opal
#'   format}
#'   \item{dataset_MELBOURNE_1}{Dataset of Melbourne (1)}
#'   \item{dataset_MELBOURNE_2}{Dataset of Melbourne (2)}
#'   \item{dataset_PARIS}{Dataset of Paris}
#'   \item{dataset_TOKYO - ERROR WITH DATA}{Dataset of Tokyo containing errors}
#'   \item{dataset_TOKYO}{Dataset of Tokyo}
#'   \item{PARIS_taxonomy}{Taxonomy specific to Paris dataset}
#'   \item{taxonomy_mlstr}{Maelstrom Taxonomy}
#'   \item{taxonomy_opal}{Opal Taxonomy}
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






