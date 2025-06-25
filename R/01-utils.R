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
#' madshapR_website()
#' 
#' }
#'
#' @importFrom utils browseURL
#'
#' @export
madshapR_website <- function(){

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
#' [OBiBa data type of a variable](https://opaldoc.obiba.org/en/dev/variables-data.html#value-types). 
#' The valueType is specified in a data dictionary in a column 'valueType' and 
#' can be associated with variables as attributes. Acceptable valueTypes 
#' include 'text', 'integer', 'decimal', 'boolean', datetime', 'date'. The full 
#' list of OBiBa valueType possibilities and their correspondence with R data 
#' types are available using [valueType_list]. The valueType can be used to 
#' coerce the variable to the corresponding data type.
#'
#' @seealso
#' [Opal documentation](https://opaldoc.obiba.org/en/dev/magma-user-guide/value/type.html)
#'
#' @format ## `data.frame`
#' A data frame with 12 rows and 7 columns:
#' \describe{
#'   \item{valueType}{data type as described in Opal}
#'   \item{typeof}{data type provided by base::typeof(x)}
#'   \item{class}{data class provided by attributes(x)}
#'   \item{class}{data class provided by base::class(x) explicit class}
#'   \item{call}{function to transpose object according base::do.call function}
#'   \item{toValueType}{ensemble data type as described in Opal}
#'   \item{genericType}{ensemble data type which valueType belongs}
#'   ...
#' }
#'
#' @examples
#' {
#'
#' head(valueType_list)
#'
#' }
"valueType_list"

#' @title
#' Built-in material allowing the user to test the package with example data
#'
#' @description
#' Example datasets and data dictionaries, and taxonomy, to provide illustrative 
#' examples of objects used by madshapR.
#'
#' @format ## `list`
#' A list with 9 elements (data frames and lists) providing example objects 
#' for testing the package:
#'
#' \describe{
#'   \item{dataset_example}{Dataset for example dataset}
#'   \item{data_dictionary_example - as_data_dict}{Data dictionary for example 
#'   dataset where the structure is a data dictionary}
#'   \item{data_dictionary_example - as_data_dict_mlstr}{Data dictionary for 
#'   example dataset where the structure is a data dictionary compliant with 
#'   Maelstrom}
#'   \item{dataset_example - errors with data}{Dataset of example 
#'   with errors with example data dictionary}
#'   \item{data_dictionary_example - errors with data}{Data Dictionary for example 
#'   dataset with errors with example dataset}
#'   \item{data_dictionary_example - errors}{Data dictionary for example dataset 
#'   with errors}
#'   \item{data_dictionary_example - collapsed}{Data dictionary for example 
#'   with collapsed categories}
#'   \item{taxonomy_example}{Taxonomy for example dataset}
#'   \item{summary - dataset_example}{Dataset example summary}
#'   ...
#' }
#'
#' @examples
#' {
#'  
#' library(dplyr)
#'  
#' head(madshapR_examples$`dataset_example`)
#' glimpse(madshapR_examples$`data_dictionary_example`)
#'
#' }
"madshapR_examples"


#' @title
#' Built-in data frame of colors used in the graphs and charts.
#'
#' @description
#' Provides a built-in data frame of the colors used in the graphs and charts.
#'
#' @format ## `data.frame`
#' A data frame with 51 rows and 2 columns:
#' \describe{
#'   \item{values}{possible class value in a dataset.}
#'   \item{color_palette}{associated color}
#'   ...
#' }
#'
#' @examples
#' {
#'
#' head(color_palette_maelstrom)
#'
#' }
"color_palette_maelstrom"
