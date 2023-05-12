#' @title
#' Create an empty dataset from a data dictionary
#'
#' @description
#' Creates an empty dataset using information contained in a data dictionary.
#' The column names are taken from 'name' in the 'Variables' element of the
#' data dictionary. If a 'valueType' or alternatively 'typeof' column is
#' provided, the class of each column is set accordingly (default is text).
#'
#' @details
#' A data dictionary contains metadata about variables and can be associated 
#' with a dataset. It must be a list of data frame-like objects with elements 
#' named 'Variables' (required) and 'Categories' (if any). To be usable in any 
#' function, the 'Variables' element must contain at least the 'name' column, 
#' and the 'Categories' element must contain at least the 'variable' and 'name' 
#' columns. To be considered as a minimum workable data dictionary, in 
#' 'Variables' the 'name' column must also have unique and non-null entries, 
#' and in 'Categories' the combination of 'variable' and 'name' columns must 
#' also be unique'.
#'
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset (to be generated).
#' @param data_dict_apply whether to apply the data dictionary to its dataset.
#' The resulting tibble will have for each column its associated meta data as
#' attributes. The factors will be preserved. FALSE by default.
#'
#' @return
#' A tibble identifying the dataset created from the variable names list in
#' 'Variables' element of the data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$dd_MELBOURNE_1_format_maelstrom
#' data_extract(data_dict)
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
data_extract <- function(data_dict, data_dict_apply = FALSE){

  # tests
  if(toString(attributes(data_dict)$`madshapR::class`) == "data_dict_mlstr"){
    data_dict <- as_data_dict_mlstr(data_dict, as_data_dict = TRUE)}else{
      data_dict <- as_data_dict(data_dict)}

  if(nrow(data_dict[['Variables']]) == 0){
    dataset <- tibble(.rows = 0)
    return(dataset)}

  if(!is.logical(data_dict_apply))
    stop(call. = FALSE,
'`data_dict_apply` must be TRUE of FALSE (FALSE by default)')


  # valueType/typeof() is needed to generate dataset, which will be all text if
  # not present
  vT_list <- madshapR::valueType_list

  data_dict_temp <- data_dict

  if(is.null(data_dict_temp[["Variables"]][['valueType']])){
    data_dict_temp[["Variables"]] <-
      data_dict_temp[["Variables"]] %>%
      left_join(
        vT_list %>%
          select(
            typeof = .data$`toTypeof`,
            valueType = .data$`toValueType`) %>%
          distinct, by = "typeof") %>%
      mutate(valueType = replace_na(.data$`valueType`, "character"))}

  dataset <-
    data_dict_temp[["Variables"]]  %>%
    select(.data$`name`) %>%
    pivot_wider(names_from = .data$`name`, values_from = .data$`name`) %>%
    slice(0)

  for(i in seq_len(ncol(dataset))){
    # stop()}
    dataset[i] <-
      as_valueType(dataset[i], data_dict_temp[["Variables"]]$`valueType`[i])
  }

  if(data_dict_apply == TRUE){
    dataset <- data_dict_apply(dataset, data_dict)
    return(dataset)
  }

  dataset <- as_dataset(dataset,attributes(dataset)$`madshapR::col_id`)
  return(dataset)
}

#' @title
#' Remove labels (attributes) from a data frame, leaving its unlabelled columns
#'
#' @description
#' Removes any attributes attached to a tibble. Any value in columns will be
#' preserved. Any 'Date' (typeof) column will be recast as character to
#' preserve information.
#'
#' @details
#' A dataset must be a data frame-like object and can be associated with a 
#' data dictionary. If no data dictionary is provided, a minimum workable 
#' data dictionary will be generated as needed by relevant functions. 
#' An identifier `id` column for sorting can be specified by the user. If 
#' specified, the `id` values must be non-missing and will be used in functions 
#' that require it. If no identifier column is specified, indexing is handled 
#' automatically by the function.
#'
#' @seealso
#' [haven::zap_labels()].
#'
#' @param dataset A tibble identifying the input dataset observations associated 
#' to its data dictionary.
#'
#' @return
#' A tibble identifying a dataset.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' dataset <- DEMO_files$dataset_TOKYO
#' data_dict <- as_data_dict_mlstr(DEMO_files$dd_TOKYO_format_maelstrom_tagged)
#' dataset <- data_dict_apply(dataset,data_dict)
#' dataset_zap_data_dict(dataset)
#'
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#' @importFrom lubridate is.Date
#'
#' @export
dataset_zap_data_dict <- function(dataset){

  # test input
  as_dataset(dataset, attributes(dataset)$`madshapR::col_id`)
  preserve_attributes <- attributes(dataset)$`madshapR::col_id`

  for(i in seq_len(length(dataset))){
  # stop()}
    if(is.Date(dataset[[i]])) dataset[[i]] <- as.character(dataset[[i]])
  }

  dataset <- 
    dataset %>% lapply(as.vector) %>% as_tibble() %>%
    as_dataset(col_id = preserve_attributes)

  return(dataset)
}

#' @title
#' Apply data dictionary category labels to the associated dataset variables
#' 
#' @description
#' Applies category labels declared in a data dictionary to the associated 
#' columns (variables) in the dataset.
#'
#' @details
#' A dataset must be a data frame-like object and can be associated with a 
#' data dictionary. If no data dictionary is provided, a minimum workable 
#' data dictionary will be generated as needed by relevant functions. 
#' An identifier `id` column for sorting can be specified by the user. If 
#' specified, the `id` values must be non-missing and will be used in functions 
#' that require it. If no identifier column is specified, indexing is handled 
#' automatically by the function.
#'
#' A data dictionary contains metadata about variables and can be associated 
#' with a dataset. It must be a list of data frame-like objects with elements 
#' named 'Variables' (required) and 'Categories' (if any). To be usable in any 
#' function, the 'Variables' element must contain at least the 'name' column, 
#' and the 'Categories' element must contain at least the 'variable' and 'name' 
#' columns. To be considered as a minimum workable data dictionary, in 
#' 'Variables' the 'name' column must also have unique and non-null entries, 
#' and in 'Categories' the combination of 'variable' and 'name' columns must 
#' also be unique'.
#'
#' @param dataset A tibble identifying the input dataset observations associated 
#' to its data dictionary.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset (to be generated).
#' @param col_names A character string specifying the name(s) of the column(s)
#' which refer to existing column(s) in the dataset. The column(s) can be named
#' or indicated by position.
#'
#' @return
#' A tibble identifying a dataset.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' dataset <- DEMO_files$dataset_TOKYO
#' data_dict <- as_data_dict_mlstr(DEMO_files$dd_TOKYO_format_maelstrom_tagged)
#' dataset <- data_dict_apply(dataset,data_dict)
#' dataset_cat_as_labels(dataset)
#'
#' }
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @export
dataset_cat_as_labels <- function(
    dataset, 
    data_dict = NULL,
    col_names = names(dataset)){
  
  # tests
  dataset <- as_dataset(dataset) # no col_id
  dataset[col_names]
  preserve_attributes <- attributes(dataset)$`madshapR::col_id`
  
  # if data_dict empty
  if(is.null(data_dict)){
    # preserve_data_dict <- TRUE
    data_dict <- suppressMessages(data_dict_extract(dataset[col_names]))
  } else {
    # preserve_data_dict <- FALSE  
    data_dict <- 
      data_dict_match_dataset(dataset[col_names],data_dict)$data_dict}

  if(sum(nrow(data_dict[['Categories']])) == 0) return(dataset)
  
  for(i in col_names){
    # stop()}
    
    message(paste0('Processing of : ',i))
    col <- dataset_zap_data_dict(as_dataset(dataset[i]))
    data_dict_temp <- data_dict_match_dataset(col,data_dict)$data_dict
    
    if(sum(nrow(data_dict_temp[['Categories']])) > 0){
      names(col) <- '___values___'
      label_name <- 
        names(data_dict_temp[['Categories']] %>% 
                select(
                  matches(c("^label$","^label:[[:alnum:]]","^labels$"))[1]))
      
      cat_col <- 
        data_dict_temp$Categories %>% 
        select('___values___' = "name", '___label___' = all_of(label_name))
      
      col <- 
        col %>% 
        mutate(across(everything(), ~ str_squish(as.character(.)))) %>%
        left_join(
          cat_col %>% mutate(across(everything(),~str_squish(as.character(.)))),
          by = intersect(names(col),names(cat_col))) %>%
        mutate(
          `___label___` = 
            ifelse(is.na(.data$`___label___`),
                   .data$`___values___`,
                   .data$`___label___`)) %>%
        select("___label___")
    
      # variable_names <- data_dict_temp[['Categories']]['name']
      
      data_dict_temp[['Categories']] <- 
        data_dict_temp[['Categories']] %>%
        mutate(
          `___mlstr_name___` = .data$`name`,
          name = !!as.symbol(label_name)) %>%
        mutate(across(
          any_of(label_name), 
          ~ .data$`___mlstr_name___`)) %>% 
        select(-'___mlstr_name___')
      
      names(col) <- i 
      data_dict_temp <- 
        valueType_adjust(from = as_dataset(col), to = data_dict_temp)
      col <- valueType_adjust(from = data_dict_temp, to = as_dataset(col))
      dataset[i] <- data_dict_apply(col, data_dict_temp)
    }
  }
  
  dataset <- 
    tibble(dataset) %>%
    as_dataset(col_id = preserve_attributes)
  
  # if(preserve_data_dict == TRUE){
  #   data_dict <- valueType_adjust(from = dataset,to = data_dict)
  #   dataset <- data_dict_apply(dataset,data_dict)}
  # 
  return(dataset)
}

#' @title
#' Create a dossier object from a list of dataset(s)
#'
#' @description
#' Assembles a dossier object from the listed datasets. A dossier is a list 
#' containing at least one valid dataset and is the input used by key functions 
#' of the package.
#'
#' @details
#' A dataset must be a data frame-like object and can be associated with a 
#' data dictionary. If no data dictionary is provided, a minimum workable 
#' data dictionary will be generated as needed by relevant functions. 
#' An identifier `id` column for sorting can be specified by the user. If 
#' specified, the `id` values must be non-missing and will be used in functions 
#' that require it. If no identifier column is specified, indexing is handled 
#' automatically by the function.
#'
#' @param dataset_list A list of tibble(s), identifying the input data
#' observations.
#' @param data_dict_apply whether to apply the data dictionary to its dataset.
#' The resulting tibble will have for each column its associated meta data as
#' attributes. The factors will be preserved. FALSE by default.
#'
#' @return
#' A list of tibble(s), each of them identifying datasets in a dossier.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' ###### Example 1: datasets can be gathered into a dossier which is a list.
#' dossier <- dossier_create(
#'  dataset_list = list(
#'    dataset_MELBOURNE_1 = DEMO_files$dataset_MELBOURNE_1,
#'    dataset_MELBOURNE_2 = DEMO_files$dataset_MELBOURNE_2))
#'    
#' ###### Example 2: any data frame (or tibble) can be gathered into a dossier
#' dossier_create(list(iris, mtcars))
#'    
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
dossier_create <- function(dataset_list, data_dict_apply = FALSE){

  # tests input
  if(is.data.frame(dataset_list)) dataset_list <- list(dataset_list)
  if(!is.logical(data_dict_apply))
    stop(call. = FALSE,
         '`data_dict_apply` must be TRUE of FALSE (FALSE by default)')

  dossier <- dataset_list %>% lapply(FUN = function(x) {
    if(data_dict_apply == TRUE){
      x <- tibble(data_dict_apply(x)) ; return(x)}else{return(x)}
  })

  fargs <- as.list(match.call(expand.dots = TRUE))
  if(is.null(names(dataset_list))){
    names(dossier) <-
      fabR::make_name_list(as.character(fargs['dataset_list']), dossier)}

  dossier <- as_dossier(dossier)

  return(dossier)
}

#' @title
#' Validate and coerce an object to dataset format
#'
#' @description
#' Confirms that the input object is a valid dataset and returns it as a dataset
#' with the appropriate madshapR::class attribute. This function mainly helps 
#' validate inputs within other functions of the package but could be used to 
#' check if a dataset is valid.
#'
#' @details
#' A dataset must be a data frame-like object and can be associated with a 
#' data dictionary. If no data dictionary is provided, a minimum workable 
#' data dictionary will be generated as needed by relevant functions. 
#' An identifier `id` column for sorting can be specified by the user. If 
#' specified, the `id` values must be non-missing and will be used in functions 
#' that require it. If no identifier column is specified, indexing is handled 
#' automatically by the function.
#'
#' @param object A potential dataset to be coerced.
#' @param col_id A character string specifying the name(s) of the column(s)
#' which refer to key identifier of the dataset. The column(s) can be named
#' or indicated by position.
#'
#' @return
#' A tibble identifying a dataset.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' ###### Example 1: a dataset can have an id column(s) which is specified as
#' # an attribute. 
#' dataset <- as_dataset(DEMO_files$dataset_MELBOURNE_1, col_id = "id")
#' 
#' ###### Example 2: any data frame (or tibble) can be a dataset by definition.
#' as_dataset(iris, col_id = "Species")
#'}
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
as_dataset <- function(object, col_id = NULL){

  # if only the tibble is given in parameter
  if(is.data.frame(object)) {

    # first column must be completly filled
    if(ncol(object) == 0){
      attributes(object)$`madshapR::class` <- "dataset"
      attributes(object)$`madshapR::col_id` <- NULL
      return(object)}

    # if(is.null(col_id)) col_id <- names(object)[[1]]
    # if !is.null(col_id) column must be present and completely filled
    if(length(intersect(names(object), col_id)) != length(unique(col_id)))
      stop(call. = FALSE,
           "All of your id column(s) must be present in your dataset.")

    if(sum(is.na(object[col_id])) > 0)
      stop(call. = FALSE,
           "Your id column(s) must not contain any NA values.")

    attributes(object)$`madshapR::class` <- "dataset"
    attributes(object)$`madshapR::col_id` <- col_id

    object <-
      object %>% select(all_of(col_id), everything())

    return(object)
  }

  # else
  stop(call. = FALSE,
"\n\nThis object is not a dataset as defined by Maelstrom standards, which must 
be a data frame (or tibble). 
Please refer to documentation.")

}

#' @title
#' Validate and coerce an object to dossier format
#'
#' @description
#' Confirms that the input object is a valid dossier and returns it as a dossier
#' with the appropriate madshapR::class attribute. This function mainly helps 
#' validate input within other functions of the package but could be used to
#' check if a dossier is valid.
#'
#' @details
#' A dossier must be a named list containing at least one data frame or
#' data frame extension (e.g. a tibble), each of them being datasets.
#' The name of each tibble will be use as the reference name of the dataset.
#'
#' @seealso
#' For a better assessment, please use [dataset_evaluate()].
#'
#' @param object A potential dossier to be coerced.
#'
#' @return
#' A list of tibble(s), each of them identifying datasets in a dossier.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' library(stringr)
#'
#' ###### Example 1: a dataset list is a dossier by definition.
#' dossier <- 
#'   as_dossier(DEMO_files[str_detect(names(DEMO_files),"dataset")])
#'    
#' ###### Example 2: any list of data frame (or tibble) can be a dossier by 
#' # definition.
#' as_dossier(list(dataset_1 = iris, dataset_2 = mtcars))
#' 
#'}
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
as_dossier <- function(object){

  # check if names in object exist
  name_objs <-
    vapply(X = as.list(names(object)),
           FUN = function(x) nchar(x),
           FUN.VALUE = integer(1))

  if(is.null(names(object)) | all(name_objs) == FALSE){
    stop(call. = FALSE,
"One or more datasets are not named. Please provide named list of datasets.")}

  # check if names in object are unique
  if(!setequal(length(names(object)),length(unique(names(object))))){
    stop(call. = FALSE,
"The name of your datasets are not unique. Please provide different names.")}
  
  tryCatch(
    object <- object %>% lapply(
      FUN = function(x) as_dataset(x, attributes(x)$`madshapR::col_id`)),
    error = function(x) stop(call. = FALSE,
"\n
This object is not a dossier as defined by Maelstrom standards, which must be 
exclusively a list of (at least one) dataset(s). Each dataset may have column(s)
which refer to key identifier of the dataset. If attributed, this(ese) columns 
must be present in the dataset.

Please refer to documentation."))

  attributes(object)$`madshapR::class` <- "dossier"
  return(object)

}

#' @title
#' Test if an object is a valid dataset
#'
#' @description
#' Tests if the input object is a valid dataset. This function mainly helps
#'  validate input within other functions of the package but could be used
#' to check if a dataset is valid.
#'
#' @details
#' A dataset must be a data frame-like object and can be associated with a 
#' data dictionary. If no data dictionary is provided, a minimum workable 
#' data dictionary will be generated as needed by relevant functions. 
#' An identifier `id` column for sorting can be specified by the user. If 
#' specified, the `id` values must be non-missing and will be used in functions 
#' that require it. If no identifier column is specified, indexing is handled 
#' automatically by the function.
#'
#' @seealso
#' For a better assessment, please use [dataset_evaluate()].
#'
#' @param object A potential dataset to be evaluated.
#'
#' @return
#' A logical.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' # any data frame (or tibble) can be a dataset by definition.
#' 
#' is_dataset(DEMO_files$dataset_MELBOURNE_1)
#' is_dataset(iris)
#' is_dataset(AirPassengers)
#' 
#'}
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
is_dataset <- function(object){

  object <- object
  # if only the tibble is given in parameter
  test <- fabR::silently_run(
    try(
      as_dataset(
        object,
        col_id = attributes(object)$`madshapR::col_id`),
      silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)

}

#' @title
#' Test if an object is a valid dossier
#'
#' @description
#' Tests if the input object is a valid dossier. This function mainly helps 
#' validate input within other functions of the package but could be used to 
#' check if a dossier is valid.
#'
#' @details
#' A dossier must be a named list containing at least one data frame or
#' data frame extension (e.g. a tibble), each of them being datasets.
#' The name of each tibble will be use as the reference name of the dataset.
#'
#' @param object A potential dossier to be evaluated.
#'
#' @return
#' A logical.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' # Any list of data frame (or tibble) can be a dossier by definition.
#' library(stringr)
#' 
#' is_dossier(DEMO_files[str_detect(names(DEMO_files),"dataset")])
#' is_dossier(list(dataset_1 = iris, dataset_2 = mtcars))
#' is_dossier(iris)
#' 
#'}
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
is_dossier <- function(object){

  object <- object
  # if only the dossier is given in parameter
  test <- fabR::silently_run(try(as_dossier(object),silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)

}
