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
#' A dataset is a data table containing variables. A dataset object is a 
#' data frame and can be associated with a data dictionary. If no 
#' data dictionary is provided with a dataset, a minimum workable 
#' data dictionary will be generated as needed within relevant functions.
#' Identifier variable(s) for indexing can be specified by the user. 
#' The id values must be non-missing and will be used in functions that 
#' require it. If no identifier variable is specified, indexing is 
#' handled automatically by the function.
#' 
#' A data dictionary contains the list of variables in a dataset and metadata 
#' about the variables and can be associated with a dataset. A data dictionary 
#' object is a list of data frame(s) named 'Variables' (required) and 
#' 'Categories' (if any). To be usable in any function, the data frame 
#' 'Variables' must contain at least the `name` column, with all unique and 
#' non-missing entries, and the data frame 'Categories' must contain at least 
#' the `variable` and `name` columns, with unique combination of 
#' `variable` and `name`.
#'
#' @param data_dict A list of data frame(s) representing metadata.
#' @param data_dict_apply Whether data dictionary(ies) should be applied to 
#' associated dataset(s), creating labelled dataset(s) with variable attributes. 
#' Any previous attributes will be preserved. FALSE by default.
#'
#' @returns
#' A data frame identifying the dataset created from the variable names list in
#' 'Variables' element of the data dictionary.
#'
#' @examples
#' {
#' 
#' # use madshapR_examples provided by the package
#' # from a data dictionary, you can use the function to extract and generate an 
#' # empty dataset
#' 
#' data_dict <- as_data_dict_mlstr(madshapR_examples$`data_dictionary_example`)
#' dataset   <- data_extract(data_dict)
#' 
#' head(dataset)
#' 
#' }
#'
#' @import dplyr tidyr haven
#' @importFrom rlang .data
#'
#' @export
data_extract <- function(data_dict, data_dict_apply = FALSE){
  
  # tests
  if(suppressWarnings(check_data_dict_valueType(data_dict)) %>% 
     dplyr::filter(str_detect(.data$`condition`,"\\[ERROR\\]")) %>% nrow > 0){
    
    message(
      '[INFO] - valueType is not compatible with variable categories. The dataset 
will begenerated with compatible valueType.')
    
    data_dict <- valueType_self_adjust(data_dict)
    
  }
  
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
  
  dataset <-
    data_dict_temp[["Variables"]]  %>%
    select("name") %>%
    pivot_wider(names_from = "name", values_from = "name") %>%
    slice(0)
  
  for(i in seq_len(ncol(dataset))){
    # stop()}
    dataset[i] <-
      as_valueType(dataset[i],data_dict_temp[["Variables"]]$`valueType`[i])
  }
  
  if(data_dict_apply == TRUE){
    dataset <- data_dict_apply(dataset, data_dict)
    return(dataset)
  }
  
  dataset <- as_dataset(dataset)
  return(dataset)
}


#' @title
#' Remove labels (attributes) from a data frame, leaving its unlabelled columns
#'
#' @description
#' Removes any attributes attached to a data frame. Any value in columns will be
#' preserved. Any 'Date' (typeof) column will be recast as character to
#' preserve information.
#'
#' @details
#' A dataset is a data table containing variables. A dataset object is a 
#' data frame and can be associated with a data dictionary. If no 
#' data dictionary is provided with a dataset, a minimum workable 
#' data dictionary will be generated as needed within relevant functions.
#' Identifier variable(s) for indexing can be specified by the user. 
#' The id values must be non-missing and will be used in functions that 
#' require it. If no identifier variable is specified, indexing is 
#' handled automatically by the function.
#' 
#' @seealso
#' [haven::zap_labels()].
#'
#' @param dataset A dataset object.
#' @param zap_factor Whether the factor column should be coerced with its 
#' corresponding valueType. FALSE by default.
#'
#' @returns
#' A data frame identifying a dataset.
#'
#' @examples
#' {
#' 
#' # use madshapR_examples provided by the package
#'
#' dataset <- madshapR_examples$`dataset_example`
#' data_dict <- as_data_dict_mlstr(madshapR_examples$`data_dictionary_example`)
#' dataset <- data_dict_apply(dataset,data_dict)
#' unlabbeled_dataset <- dataset_zap_data_dict(dataset)
#' 
#' head(unlabbeled_dataset)
#'
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#' @importFrom lubridate is.Date
#'
#' @export
dataset_zap_data_dict <- function(dataset, zap_factor = FALSE){

  # test input
  if(!is.logical(zap_factor))
    stop(call. = FALSE,
         '`zap_factor` must be TRUE or FALSE (FALSE by default)')

  # preserve dataset
  as_dataset(dataset, col_id(dataset))
  preserve_attributes <- col_id(dataset)
  preserve_group <- group_vars(dataset)
  dataset <- as_dataset(ungroup(dataset))
  
  categorical_variables <-
    dataset %>%
    ungroup() %>%
    reframe(across(everything(), ~ is_category(.))) %>%
    pivot_longer(everything()) %>%
    dplyr::filter(.data$`value`) %>% pull("name")

  dataset_init <- dataset # = dataset_init

  vec <- tibble(index = as.integer(), valueType = as.character())
  for(i in seq_len(length(dataset))){
    # stop()}
    vT_init <- valueType_of(dataset_init[[i]])
    if(vT_init %in% c('date','datetime')){
      dataset[[i]] <- as.character(dataset[[i]])}
    vec <- vec %>% add_row(index = i, valueType = vT_init)}
  
  dataset <- 
    as_tibble(dataset %>% lapply(as.vector))
  
  for(i in seq_len(nrow(vec))){
    # stop()}
    tryCatch(
      expr = {dataset[[vec$`index`[[i]]]] <- 
      as_valueType(dataset[[vec$`index`[[i]]]],vec$`valueType`[[i]])},
      error = function(cond){
        warning('Column : ',names(dataset[vec$`index`[[i]]]),' as character.')
        dataset[[vec$`index`[[i]]]] <- as_valueType(dataset[[vec$`index`[[i]]]],'text')
      })
  }
  
  # if categorical, replace by factor
  if(zap_factor == FALSE){
  dataset <- 
    dataset %>% 
    mutate(across(any_of(categorical_variables),as.factor))}
  
  dataset <- 
    dataset %>%
    group_by(pick(any_of(preserve_group))) 
  
  # [GF] NOTE : attribute such as col_id may not be preserved here.
  # %>% as_dataset(col_id = preserve_attributes) 
  
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
#' A dataset is a data table containing variables. A dataset object is a 
#' data frame and can be associated with a data dictionary. If no 
#' data dictionary is provided with a dataset, a minimum workable 
#' data dictionary will be generated as needed within relevant functions.
#' Identifier variable(s) for indexing can be specified by the user. 
#' The id values must be non-missing and will be used in functions that 
#' require it. If no identifier variable is specified, indexing is 
#' handled automatically by the function.
#'
#' A data dictionary contains the list of variables in a dataset and metadata 
#' about the variables and can be associated with a dataset. A data dictionary 
#' object is a list of data frame(s) named 'Variables' (required) and 
#' 'Categories' (if any). To be usable in any function, the data frame 
#' 'Variables' must contain at least the `name` column, with all unique and 
#' non-missing entries, and the data frame 'Categories' must contain at least 
#' the `variable` and `name` columns, with unique combination of 
#' `variable` and `name`.
#'
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param col_names A character string specifying the name(s) of the column(s)
#' which refer to existing column(s) in the dataset. The column(s) can be named
#' or indicated by position.
#'
#' @returns
#' A data frame identifying a dataset.
#'
#' @examples
#' {
#'
#' dataset = madshapR_examples$`dataset_example`
#' data_dict = as_data_dict_mlstr(madshapR_examples$`data_dictionary_example`)
#' dataset_cat_as_labels(dataset, data_dict, col_names = 'gndr')
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
  
  # preserve dataset
  as_dataset(dataset, col_id(dataset))
  preserve_attributes <- col_id(dataset)
  preserve_group <- group_vars(dataset)
  dataset <- as_dataset(ungroup(dataset))
  
  # tests
  dataset[col_names]
  
  # if data_dict empty
  if(is.null(data_dict)){
    # preserve_data_dict <- TRUE
    data_dict <- suppressMessages(data_dict_extract(dataset[col_names]))
  } else {
    # preserve_data_dict <- FALSE  
    data_dict <- 
      suppressWarnings({
        data_dict_match_dataset(dataset[col_names],data_dict)$data_dict
      })
    }

  if(has_categories(data_dict)){
  
  
    for(i in col_names){
      # stop()}
      
      message(paste0('Processing of : ',i))
      col <- dataset_zap_data_dict(as_dataset(dataset[i]))
      data_dict_temp <- suppressWarnings({
        data_dict_match_dataset(col,data_dict)$data_dict})
      
      if(has_categories(data_dict_temp)){
        names(col) <- '___values___'
        
        first_lab_var <- first_label_get(data_dict_temp)[['Categories']]
        
        cat_col <- 
          data_dict_temp$`Categories` %>% 
          select('___values___' = "name", '___label___' = all_of(first_lab_var))
        
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
            name = !!as.symbol(first_lab_var)) %>%
          mutate(across(
            any_of(first_lab_var), 
            ~ .data$`___mlstr_name___`)) %>% 
          select(-'___mlstr_name___')
        
        names(col) <- i 
        
        vT_final <- valueType_guess(unique(c(col[[1]],data_dict_temp$`Categories`$`name`)))
        
        col[[1]] <- as_valueType(col[[1]], vT_final)
        data_dict_temp$`Variables`$valueType <- vT_final
        dataset[i] <- data_dict_apply(col, data_dict_temp)
      }
    }
  }
    
  dataset <- 
    dataset %>%
    group_by(pick(any_of(preserve_group))) %>% 
    as_dataset(col_id = preserve_attributes)
  
  return(dataset)
}

#' @title
#' Generate a dossier from a list of one or more datasets
#'
#' @description
#' Generates a dossier object (list of one or more datasets).
#'
#' @details
#' A dataset is a data table containing variables. A dataset object is a 
#' data frame and can be associated with a data dictionary. If no 
#' data dictionary is provided with a dataset, a minimum workable 
#' data dictionary will be generated as needed within relevant functions.
#' Identifier variable(s) for indexing can be specified by the user. 
#' The id values must be non-missing and will be used in functions that 
#' require it. If no identifier variable is specified, indexing is 
#' handled automatically by the function.
#'
#' @param dataset_list A list of data frame, each of them being dataset object.
#' @param data_dict_apply Whether data dictionary(ies) should be applied to 
#' associated dataset(s), creating labelled dataset(s) with variable attributes. 
#' Any previous attributes will be preserved. FALSE by default.
#'
#' @returns
#' A list of data frame(s), containing input dataset(s).
#'
#' @examples
#' {
#' 
#' # use madshapR_examples provided by the package
#' library(dplyr)
#'
#' ###### Example 1: datasets can be gathered into a dossier which is a list.
#' dataset_example1 <- madshapR_examples$`dataset_example`
#' dataset_example2 <- madshapR_examples$`dataset_example - errors with data`
#' dossier <- dossier_create(list(dataset_example1,dataset_example2))
#' 
#' glimpse(dossier)
#'     
#' ###### Example 2: Any data frame can be gathered into a dossier
#' dossier <- dossier_create(list(iris,mtcars))
#' glimpse(dossier)
#'    
#' }
#'
#' @import dplyr tidyr fabR
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
      make_name_list(as.character(fargs['dataset_list']), dossier)}

  if(sum(is.na(names(dossier))) >= 1)
    names(dossier) <- paste0('dataset.',seq_along(dossier))
  
  dossier <- as_dossier(dossier)

  return(dossier)
}

#' @title
#' Validate and coerce any object as a dataset
#'
#' @description
#' Checks if an object is a valid dataset and returns it with the appropriate 
#' `madshapR::class` attribute. This function mainly helps validate inputs 
#' within other functions of the package but could be used separately to check 
#' if a dataset is valid.
#'
#' @details
#' A dataset is a data table containing variables. A dataset object is a 
#' data frame and can be associated with a data dictionary. If no 
#' data dictionary is provided with a dataset, a minimum workable 
#' data dictionary will be generated as needed within relevant functions.
#' Identifier variable(s) for indexing can be specified by the user. 
#' The id values must be non-missing and will be used in functions that 
#' require it. If no identifier variable is specified, indexing is 
#' handled automatically by the function.
#'
#' @param object A potential dataset object to be coerced.
#' @param col_id An optional character string specifying the name(s) or 
#' position(s) of the column(s) used as identifiers.
#'
#' @returns
#' A data frame with `madshapR::class` 'dataset'.
#'
#' @examples
#' {
#' 
#' # use madshapR_examples provided by the package
#' library(dplyr)
#' library(fabR)
#'
#' ###### Example 1: A dataset can have an id column specified as an attribute. 
#' dataset <- as_dataset(madshapR_examples$`dataset_example`, col_id = "part_id")
#' print(attributes(dataset)$`madshapR::col_id`)
#' glimpse(dataset)
#' 
#' ###### Example 2: Any data frame can be a dataset by definition.
#' dataset <- tibble(iris %>% add_index("my_index"))
#' dataset <- as_dataset(dataset, "my_index")
#' print(attributes(dataset)$`madshapR::col_id`)
#' 
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
as_dataset <- function(object, col_id = NULL){

  
  # [GF] NOTE : col_id is lost when combines group_by and slice in that 
  # specific order
  # col_id(as_dataset(madshapR_examples$`dataset_example`,col_id = "part_id") %>%
  #        group_by(pick('gndr')) %>% slice(1))
  
  # if only the data frame is given in parameter
  if(is.data.frame(object)) {

    # first column must be completly filled
    if(ncol(object) == 0){
      attributes(object)$`madshapR::class` <- "dataset"
      attributes(object)$`madshapR::col_id` <- NULL
      return(object)}

    # if(is.null(col_id)) col_id <- names(object)[[1]]
    # if !is.null(col_id) column must be present and completely filled
    if(class(silently_run(
      ungroup(object) %>% select(!! all_of(col_id))))[1] == 'try-error')
      stop(call. = FALSE,
           paste0("`",col_id,"` identifier column must be present in your dataset."))

    if(sum(is.na(ungroup(object) %>% select(!! all_of(col_id)))) > 0)
      stop(call. = FALSE,
           paste0("`",col_id,"` identifier column must not contain any empty values."))

    names_id <- names(ungroup(object) %>% select(!! all_of(col_id)))
    if(length(names_id) == 0) names_id <- NULL

    object <-
      object %>% select(all_of(names_id), everything())
    
    attributes(object)$`madshapR::class` <- "dataset"
    attributes(object)$`madshapR::col_id` <- names_id
    # attributes(object)$`madshapR::Data dictionary` <- data_dict_extract(object)

    return(object)
  }

  # else
  stop(call. = FALSE,
"\n\nThis object is not a dataset as defined by the package, which must be a 
data frame. 
Please refer to documentation.")

}

#' @title
#' Validate and coerce any object as a dossier (list of dataset(s))
#'
#' @description
#' Checks if an object is a valid dossier (list of datasets) and returns it 
#' with the appropriate `madshapR::class` attribute. This function mainly helps 
#' validate inputs within other functions of the package but could be used to 
#' check if a dossier is valid.
#'
#' @details
#' A dossier is a named list containing at least one data frame or more, 
#' each of them being datasets. The name of each tibble will be use as the 
#' reference name of the dataset.
#' 
#' A dataset is a data table containing variables. A dataset object is a 
#' data frame and can be associated with a data dictionary. If no 
#' data dictionary is provided with a dataset, a minimum workable 
#' data dictionary will be generated as needed within relevant functions.
#' Identifier variable(s) for indexing can be specified by the user. 
#' The id values must be non-missing and will be used in functions that 
#' require it. If no identifier variable is specified, indexing is 
#' handled automatically by the function.
#'
#' @seealso
#' For a better assessment, please use [dataset_evaluate()].
#'
#' @param object A potential dossier object to be coerced.
#'
#' @returns
#' A list of data frame(s) with `madshapR::class` 'dossier'.
#'
#' @examples
#' {
#' 
#' # use madshapR_examples provided by the package
#' library(dplyr)
#' library(stringr)
#'
#' ###### Example 1: a dataset list is a dossier by definition.
#' dossier <- 
#'   as_dossier(madshapR_examples[str_detect(names(madshapR_examples),"^dataset_example")])
#' glimpse(dossier)
#'    
#' ###### Example 2: any list of data frame can be a dossier by 
#' # definition.
#' dossier <- as_dossier(list(dataset_1 = iris, dataset_2 = mtcars))
#' glimpse(dossier)
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
"The name of your datasets are not unique or have been wrongly parsed. 
Please provide different names.")}
  
  tryCatch(
    object <- object %>% lapply(
      FUN = function(x) as_dataset(x, attributes(x)$`madshapR::col_id`)),
    error = function(x) stop(call. = FALSE,
"\n
This object is not a dossier as defined in madshapR, which must be exclusively a 
list of (at least one) dataset(s). Each dataset may have column(s) which refer 
to key identifier of the dataset. If attributed, this(ese) columns must be 
present in the dataset.

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
#' A dataset is a data table containing variables. A dataset object is a 
#' data frame and can be associated with a data dictionary. If no 
#' data dictionary is provided with a dataset, a minimum workable 
#' data dictionary will be generated as needed within relevant functions.
#' Identifier variable(s) for indexing can be specified by the user. 
#' The id values must be non-missing and will be used in functions that 
#' require it. If no identifier variable is specified, indexing is 
#' handled automatically by the function.
#'
#' @seealso
#' For a better assessment, please use [dataset_evaluate()].
#'
#' @param object A potential dataset to be evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#' 
#' # use madshapR_examples provided by the package
#' # any data frame can be a dataset by definition.
#' 
#' is_dataset(madshapR_examples$`dataset_example`)
#' is_dataset(iris)
#' is_dataset(AirPassengers)
#' 
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
is_dataset <- function(object){

  object <- object
  # if only the data frame is given in parameter
  test <- silently_run(
    try(
      as_dataset(
        object,
        col_id = attributes(object)$`madshapR::col_id`),
      silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)

}

#' @title
#' Test if an object is a valid dossier (list of dataset(s))
#'
#' @description
#' Tests if the input object is a valid dossier. This function mainly helps 
#' validate input within other functions of the package but could be used to 
#' check if a dossier is valid.
#'
#' @details
#' A dossier is a named list containing at least one data frame or more, 
#' each of them being datasets. The name of each tibble will be use as the 
#' reference name of the dataset.
#'
#' @param object A potential dossier to be evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#' 
#' # use madshapR_examples provided by the package
#' # Any list of data frame can be a dossier by definition.
#' library(stringr)
#' 
#' dossier <- madshapR_examples[str_detect(names(madshapR_examples),"^dataset_example")]
#' is_dossier(dossier)
#' is_dossier(list(dataset_1 = iris, dataset_2 = mtcars))
#' is_dossier(iris)
#' 
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
is_dossier <- function(object){

  object <- object
  # if only the dossier is given in parameter
  test <- silently_run(try(as_dossier(object),silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)

}

#' @title
#' Return the id column names(s) of a dataset
#'
#' @description
#' Return the id column names(s) of a dataset if any. If not, the function 
#' returns a NULL object.
#'
#' @details
#' A dataset is a data table containing variables. A dataset object is a 
#' data frame and can be associated with a data dictionary. If no 
#' data dictionary is provided with a dataset, a minimum workable 
#' data dictionary will be generated as needed within relevant functions.
#' Identifier variable(s) for indexing can be specified by the user. 
#' The id values must be non-missing and will be used in functions that 
#' require it. If no identifier variable is specified, indexing is 
#' handled automatically by the function.
#' 
#' @param dataset A data frame object.
#'
#' @returns
#' Name(s) of identifier column(s). NULL if not.
#'
#' @examples
#' {
#' 
#' # use madshapR_examples provided by the package
#' library(dplyr)
#' library(fabR)
#' 
#' ###### Example 1: A dataset can have an id column specified as an attribute. 
#' dataset <- as_dataset(madshapR_examples$`dataset_example`)
#' col_id(dataset)
#' 
#' dataset <- as_dataset(dataset, col_id = "part_id")
#' col_id(dataset)
#' 
#' ###### Example 2: Any data frame can be a dataset by definition.
#' dataset <- tibble(iris %>% add_index("my_index"))
#' dataset <- as_dataset(dataset, "my_index")
#' col_id(dataset)
#' 
#'}
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
col_id <- function(dataset){
  
  # test if enough dataset
  as_dataset(dataset)

  return(attributes(dataset)$`madshapR::col_id`)
  
}
