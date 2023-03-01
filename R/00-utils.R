#' @title
#' Give list of authorized valueType values
#'
#' @description
#' Built-in tibbles showing the list of authorized valueType values, as described
#' in Opal, and their data type (R wise) correspondancy. This tibble is designed
#' for programmatic use.
#'
#' @details
#' valueType is one of the property of a variable entity. It refers to
#' the (Obiba internal) type of any variable. The valueType can be 'text',
#' 'integer', 'decimal', 'boolean', 'locale', 'datetime', 'date', 'binary',
#' 'point', 'linestring', 'polygon'.
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
"valueType_list"


#' @title
#' Give list of authorized valueType values
#'
#' @description
#' Built-in tibbles showing the list of authorized valueType values, as described
#' in Opal, and their data type (R wise) correspondancy. This tibble is designed
#' for programmatic use.
#'
#' @details
#' valueType is one of the property of a variable entity. It refers to
#' the (Obiba internal) type of any variable. The valueType can be 'text',
#' 'integer', 'decimal', 'boolean', 'locale', 'datetime', 'date', 'binary',
#' 'point', 'linestring', 'polygon'.
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
"DEMO_files"


#' @title
#' Built-in tibbles and lists allowing the user to test the package with demo material.
#'
#' @description
#' Built-in tibbles and lists allowing the user to test the package with demo material.
#'
#' @format ## `tibble`
#' A list with 20 elements used for testing purpose (data frames and lists):
#'
#' \describe{
#'   \item{DEMO_data_processing_elements - final}{Data processing element without error}
#'   \item{DEMO_data_processing_elements - with error}{Data processing element containing errors}
#'   \item{DEMO_data_processing_elements - work in progress}{Data processing element in construction}
#'   \item{DEMO_dataschema}{DataSchema used, combined with data processing elements and study specific material}
#'   \item{dd_MELBOURNE_1_format_maelstrom}{Data dictionary (1) of Melbourne dataset}
#'   \item{dd_MELBOURNE_2_format_maelstrom}{Data dictionary (2) of Melbourne dataset}
#'   \item{dd_PARIS_format_maelstrom}{Data dictionary of Paris dataset}
#'   \item{dd_PARIS_format_preprocessed - ERROR}{Data dictionary of Paris dataset containing errors}
#'   \item{dd_PARIS_format_preprocessed}{Data dictionary of Paris in preprocessed format}
#'   \item{dd_TOKYO_format_maelstrom_tagged - ERROR WITH DATA}{Data dictionary of Tokyo dataset containing errors}
#'   \item{dd_TOKYO_format_maelstrom_tagged - ERROR}{Data dictionary of Tokyo dataset containing errors}
#'   \item{dd_TOKYO_format_maelstrom_tagged}{Data dictionary of Tokyo dataset}
#'   \item{dd_TOKYO_format_opal_tagged - ERROR WITH TAXO}{Data dictionary of Tokyo dataset containing errors}
#'   \item{dd_TOKYO_format_opal_tagged}{Data dictionary of Tokyo dataset opal format}
#'   \item{dataset_MELBOURNE_1}{Dataset of Melbourne (1)}
#'   \item{dataset_MELBOURNE_2}{Dataset of Melbourne (2)}
#'   \item{dataset_PARIS}{Dataset of Paris}
#'   \item{dataset_TOKYO - ERROR WITH DATA}{Dataset of Tokyo containing errors}
#'   \item{dataset_TOKYO}{Dataset of Tokyo}
#'   \item{PARIS_taxonomy}{Taxonomy specific to Paris dataset}
#'   \item{Mlstr_taxonomy}{Maelstrom Taxonomy}
#'   \item{Opal_taxonomy}{Opal Taxonomy}
#'   ...
#' }






