#' @title
#' Call to online documentation
#'
#' @description
#' Direct call to the online documentation for the package, which includes a 
#' description of the latest version of the package, vignettes, user guides, 
#' and a reference list of functions and help pages.
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
#' Built-in data frame of allowed valueType values
#'
#' @description
#' Provides a built-in data frame showing the list of allowed Opal valueType 
#' values and their corresponding R data types. This data frame is mainly used 
#' for internal processes and programming.
#'
#' @details
#' The valueType is a declared property of a variable that is required in 
#' certain functions to determine handling of the variables. Specifically, 
#' valueType refers to the 
#' [OBiBa data type of a variable](https://opaldoc.obiba.org/en/variables-data.html#value-types). 
#' The valueType is specified in a data dictionary in a column 'valueType' and 
#' can be associated with variables as attributes. Acceptable valueTypes 
#' include 'text', 'integer', 'decimal', 'boolean', datetime', 'date'. The full 
#' list of OBiBa valueType possibilities and their correspondence with R data 
#' types are available using [valueType_list].
#'
#' @seealso
#' [Opal documentation](https://opaldoc.obiba.org/en/dev/magma-user-guide/value/type.html)
#'
#' @format ## `data.frame`
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
#' Demo datasets and data dictionaries, and taxonomy, to provide illustrative 
#' examples of objects used by madshapR.
#'
#' @format ## `list`
#' A list with 12 elements (data frames and lists) providing example objects 
#' for testing the package:
#'
#' \describe{
#'   \item{data_dict_MELBOURNE}{Example Data dictionary for Melbourne dataset}
#'   \item{data_dict_PARIS}{Example Data dictionary for Paris dataset}
#'   \item{data_dict_PARIS - collapsed}{Example Data dictionary for Paris 
#'   with collapsed categories}
#'   \item{data_dict_TOKYO}{Example Data dictionary for Tokyo dataset}
#'   \item{data_dict_TOKYO - errors}{Data dictionary for Tokyo dataset 
#'   with errors}
#'   \item{data_dict_TOKYO - errors with data}{Example Data Dictionary for Tokyo 
#'   dataset with errors with Tokyo dataset}
#'   \item{dataset_MELBOURNE}{Example Dataset for MELBOURNE dataset}
#'   \item{dataset_PARIS}{Example Dataset for PARIS dataset}
#'   \item{dataset_TOKYO}{Example Dataset for TOKYO dataset}
#'   \item{dataset_TOKYO - errors with data}{Example dataset of Tokyo 
#'   with errors with Tokyo data dictionary}
#'   \item{taxonomy_PARIS}{Example Taxonomy for Paris dataset}
#'   \item{dataset_summary}{Example of dataset summary}
#'   ...
#' }
#'
#' @examples
#' {
#'
#'  print(madshapR_DEMO$dataset_TOKYO)
#'
#' }
"madshapR_DEMO"
