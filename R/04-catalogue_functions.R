#' @title
#' Return the valueType of an object
#'
#' @description
#' Determines the valueType of an object based on [typeof()] and 
#' [class()]. The possible values returned are 'date', 'boolean', 
#' 'integer', 'decimal', and 'text'.
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
#' [typeof()], [class()]
#' [Opal documentation](https://opaldoc.obiba.org/en/dev/magma-user-guide/value/type.html)
#'
#' @param x Object. Can be a vector.
#'
#' @returns
#' A character string which is the valueType of the input object.
#'
#' @examples
#' {
#' 
#' # use madshapR_DEMO provided by the package
#'
#' dataset <- madshapR_DEMO$dataset_MELBOURNE
#' valueType_of(dataset$Gender)
#' valueType_of(iris$Sepal.Length)
#'
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
valueType_of <- function(x){

  # check if the col is empty
  if(is.list(x) & sum(nrow(x)) <= 1)
    return(as_valueType(x = x[[1]], valueType))

  # check if the col is a vector
  if(is.list(x))
    stop(call. = FALSE, "'list' object cannot be coerced to valueType")

  type  <- x %>% typeof()
  class <- class(x)[[max(length(class(x)))]]

  vT_list <- madshapR::valueType_list

  valueType <-
    unique(vT_list[
      which(vT_list[['typeof']] == type &
            vT_list[['class']]  == class),]$`toValueType`)

  if(type %in% c("character","double") & class == "Date")    valueType <- "date"
  if(type %in% c("character","double") & class == "POSIXt")  valueType <- "datetime"

  silently_run({
    if(class == "factor"){
      lvls <- attributes(x)$`levels` %>% as.character()
      valueType <-
        try({as_valueType(lvls,"integer");valueType <- "integer"},silent = TRUE)

      if(class(valueType)[1] == "try-error") valueType <-
        try({as_valueType(lvls,"decimal");valueType <- "decimal"},silent = TRUE)

      if(class(valueType)[1] == "try-error") valueType <-
        try({as_valueType(lvls,"date")   ;valueType <- "date"   },silent = TRUE)

      if(class(valueType)[1] == "try-error") valueType <-
        try({as_valueType(lvls,"boolean");valueType <- "boolean"},silent = TRUE)

      if(class(valueType)[1] == "try-error") valueType <-
        try({                             valueType <- "text"   },silent = TRUE)
    }
  })
  if(length(valueType) == 0) valueType <- "text"

  return(valueType)
}

#' @title
#' Guess and attribute the valueType of a data dictionary or dataset variable
#'
#' @description
#' Determines the valueType of an object based on [base::typeof()] and 
#' [base::class()].
#' The possible values returned are 'date', 'boolean', 'integer', 'decimal', and
#' 'text'.
#'
#' @details
#' A data dictionary contains the list of variables in a dataset and metadata 
#' about the variables and can be associated with a dataset. A data dictionary 
#' object is a list of data frame(s) named 'Variables' (required) and 
#' 'Categories' (if any). To be usable in any function, the data frame 
#' 'Variables' must contain at least the `name` column, with all unique and 
#' non-missing entries, and the data frame 'Categories' must contain at least 
#' the `variable` and `name` columns, with unique combination of 
#' `variable` and `name`.
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
#' [valueType_adjust()]
#'
#' @param ... Object that can be either a dataset or a data dictionary.
#'
#' @returns
#' Either a data frame, identifying the dataset, or a list of data frame(s)
#' identifying a data dictionary, depending which the input refers to.
#'
#' @examples
#' {
#' 
#' ###### Example : The valueType of a dataset can be adjusted. each column is
#' # evaluated as whole, and the best valueType match found is applied. If 
#' # there is no better match found, the column is left as it is.
#' 
#' head(valueType_self_adjust(mtcars['cyl']))
#'
#' }
#'
#' @import dplyr tidyr stringr fabR
#' @importFrom rlang .data
#'
#' @export
valueType_self_adjust <- function(...){

  # is dataset
  if(is_dataset(...) & !is_data_dict(...)){
    
    dataset <- as_dataset(...,col_id = col_id(...))
    
    if(ncol(dataset) == 0) return(dataset)
    if(nrow(dataset) == 0) return(dataset)
    
    preserve_attributes <- col_id(dataset)

    is_factor <-
      dataset %>%
      summarise(across(everything(), ~ toString(class(.)))) %>%
      pivot_longer(everything()) %>%
      dplyr::filter(.data$`value` %in% c("factor"))

    data_dict <- data_dict_extract(dataset)
    data_dict[['Categories']] <-
      bind_rows(
        Categories = tibble(name = as.character(),variable = as.character()),
        data_dict[['Categories']])

    for(i in names(dataset)) {
      dataset[[i]] <-
        as_valueType(x = dataset[[i]],
                     valueType = valueType_guess(x = dataset[[i]]))
      }

    data_dict_final <- data_dict_extract(dataset)
    data_dict[['Variables']]['valueType'] <- NULL
    data_dict_final[['Variables']] <-
      data_dict_final[['Variables']][c('name','valueType')] %>%
      left_join(data_dict[['Variables']], by = c("name"))
    data_dict_final <- c(data_dict_final['Variables'], data_dict['Categories'])

    dataset <-
      data_dict_apply(dataset, data_dict_final) %>%
      mutate(across(c(is_factor$`name`), ~ as.factor(.))) %>%
      as_dataset(col_id = preserve_attributes)

    return(dataset)
  }

  # is data_dict
  if(!is_dataset(...) & is_data_dict(...)){
    
    data_dict <- as_data_dict_shape(...)
    attributes(data_dict)$`madshapR::class` <- attributes(...)$`madshapR::class`
    
    if(nrow(data_dict[['Variables']]) == 0) return(data_dict)

    if(sum(nrow(data_dict[['Categories']])) == 0){
      warning("Your data dictionary contains no categorical variables.")
      return(data_dict)

    }else{

      category_outcomes <-
        data_dict[['Categories']] %>%
        select("name") %>% distinct %>%
        rowwise() %>%
        mutate(valueType = valueType_guess(.data$`name`))

      category_outcomes <-
        data_dict[['Categories']] %>%
        select(.data$`variable`,.data$`name`) %>%
        left_join(category_outcomes, by = "name") %>%
        select(.data$`variable`,.data$`valueType`) %>%
        distinct %>%
        group_by(.data$`variable`) %>%
        summarise(valueType = paste0(.data$`valueType`,collapse = "|"))

      category_outcomes <-
        data_dict[['Categories']] %>%
        select(.data$`variable`,.data$`name`) %>%
        left_join(category_outcomes, by = "variable") %>%
        group_by(.data$`variable`) %>% group_split() %>%
        lapply(function(x){
          test_vT <- str_detect(x$valueType[1], "\\|")
          if(test_vT) x <-
              x %>% mutate(valueType = valueType_guess(unique(x$name)))
          return(x)
        }) %>%
        bind_rows() %>%
        left_join(madshapR::valueType_list, by = "valueType") %>%
        select(
          name = .data$`variable`,
          proposed_tO = .data$`typeof`,
          proposed_vT = .data$`valueType`) %>%
        distinct

      if(length(data_dict[['Variables']][['typeof']]) > 0){

        data_dict_tO <-
          data_dict[['Variables']] %>% select(.data$`name`,.data$`typeof`) %>%
          left_join(category_outcomes, by = "name") %>%
          mutate(
            proposed_tO =
              ifelse(is.na(.data$`proposed_tO`),
                     .data$`typeof`,.data$`proposed_tO`)) %>%
          mutate(
            `proposed_tO` =
              replace_na(.data$`proposed_tO`,'character')) %>%
          select(typeof = .data$`proposed_tO`)

        data_dict[['Variables']]['typeof'] <- data_dict_tO

      }

      if(length(data_dict[['Variables']][['valueType']]) > 0){

        data_dict_vT <-
          data_dict[['Variables']] %>%
          select(.data$`name`,.data$`valueType`) %>%
          left_join(category_outcomes, by = "name") %>%
          mutate(
            proposed_vT =
              ifelse(is.na(.data$`proposed_vT`),
                     .data$`valueType`,.data$`proposed_vT`)) %>%
          mutate(`proposed_vT` = replace_na(.data$`proposed_vT`,'text')) %>%
          select(valueType = .data$`proposed_vT`)

        data_dict[['Variables']]['valueType'] <- data_dict_vT

      }

      if(length(data_dict[['Variables']][['valueType']]) == 0 &
         length(data_dict[['Variables']][['typeof']])    == 0   ) {

        data_dict_vT <-
          data_dict[['Variables']] %>%
          left_join(category_outcomes, by = "name") %>%
          rename(typeof = .data$`proposed_tO`, valueType = .data$`proposed_vT`)}

    }

    return(data_dict)
  }

  message("The argument is neither a dataset or a data dictionary.")
  message("\nTesting dataset :")
  try(as_dataset(...))
  
  message("\nTesting data dictionary :")
  try(as_data_dict(...))
  silently_run(stop(call. = FALSE))
}

#' @title
#' Attribute the valueType from a data dictionary to a dataset, or vice versa
#'
#' @description
#' Takes the valueType of the input (from) and attributes it to the output (to).
#' The parameters 'from' and 'to' can be either a dataset or a data dictionary.
#' Depending on the input provided, the valueType replaced is either in the
#' 'valueType' column of a data dictionary or cast to a column in a dataset.
#' If 'to' is not provided, the function calls [valueType_self_adjust()] 
#' instead. The possible values returned are 'date', 'boolean', 'integer', 
#' 'decimal', and text'.
#'
#' @details
#' A data dictionary contains the list of variables in a dataset and metadata 
#' about the variables and can be associated with a dataset. A data dictionary 
#' object is a list of data frame(s) named 'Variables' (required) and 
#' 'Categories' (if any). To be usable in any function, the data frame 
#' 'Variables' must contain at least the `name` column, with all unique and 
#' non-missing entries, and the data frame 'Categories' must contain at least 
#' the `variable` and `name` columns, with unique combination of 
#' `variable` and `name`.
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
#' [valueType_self_adjust()]
#'
#' @param from Object to be adjusted. Can be either a dataset or a data 
#' dictionary.
#' @param to Object to be adjusted. Can be either a dataset or a data 
#' dictionary. NULL by default.
#'
#' @returns
#' Either a data frame, identifying the dataset, or a list of data frame(s)
#' identifying a data dictionary, depending which is 'to'.
#'
#' @examples
#' {
#' 
#' # use madshapR_DEMO provided by the package
#' library(dplyr)
#' 
#' dataset <- madshapR_DEMO$dataset_TOKYO[c(1:4),'prg_ever']
#' data_dict <-
#'   madshapR_DEMO$data_dict_TOKYO %>%
#'   data_dict_filter(filter_var = 'name == "prg_ever"') %>%
#'   as_data_dict_mlstr()
#' 
#' head(valueType_adjust(from = data_dict,to = dataset))
#' 
#' }
#'
#' @import dplyr tidyr
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
valueType_adjust <- function(from, to = NULL){

  # test dataset
  if(is.null(to)) return(valueType_self_adjust(from))

  # apply the data dictionary of the dataset to the data dictionary
  if(is_dataset(from) & is_data_dict(to)){
    as_dataset(from) # no col_id
    as_data_dict_shape(to)

    dataset <- from
    data_dict <- to
    
    # dataset must match
    if(suppressWarnings(check_dataset_variables(dataset, data_dict)) %>% 
       dplyr::filter(str_detect(.data$`condition`,"\\[ERR\\]")) %>% nrow > 0){
      stop(call. = FALSE,
"Names across your data dictionary differ from names across the dataset.",
bold("\n\nUseful tip:"),
" Use dataset_evaluate(dataset, data_dict) for a full assessment of the dataset"
)}

    if(ncol(dataset) == 0) return(data_dict)
    
    
    max_len <- length(data_dict$Categories[['name']])
    all_vT_type <- 
      as_tibble(data.frame(matrix(nrow = nrow(dataset)+max_len,ncol=ncol(dataset))))
    names(all_vT_type) <- names(dataset)
  
    for(i in names(dataset)){
      cat_i <- data_dict$Categories[data_dict$Categorie[['variable']] == i,'name']$name
      all_vT_type[[i]] <- c(as.character(dataset[[i]]),as.character(cat_i))}
    
    vT_list<- madshapR::valueType_list
    vT_tables <-
      all_vT_type %>%
      summarise(across(everything(), ~ valueType_guess(.))) %>%
      pivot_longer(cols = everything()) %>%
      rename(valueType = "value") %>%
      left_join(vT_list, by = "valueType") %>%
      select("name", "valueType","typeof")

    data_dict[['Variables']]['typeof'] <-
      data_dict[['Variables']]['name'] %>%
      left_join(vT_tables %>%
                  select("name", "typeof"), by = "name") %>%
      select("typeof")
    # }

    # if(length(data_dict[['Variables']][['valueType']]) > 0){
    data_dict[['Variables']]['valueType'] <-
      data_dict[['Variables']]['name'] %>%
      left_join(vT_tables %>%
                  select("name", "valueType"), by = "name") %>%
      select("valueType")
    # }

    data_dict <- as_data_dict_mlstr(data_dict)

    return(data_dict)
    # }
  }

  if(is_data_dict(from) & is_dataset(to)){

  
    # test data_dict
    tryCatch({data_dict <-
      as_data_dict_mlstr(from, name_standard = FALSE)},
      warning = function(cond){
        stop(call. = FALSE,cond)})

    # test dataset
    dataset <- as_dataset(to,col_id = attributes(to)$`madshapR::col_id`)
    preserve_attributes <- attributes(dataset)$`madshapR::col_id`

    # dataset must match
    if(suppressWarnings(check_dataset_variables(dataset, data_dict)) %>% 
       dplyr::filter(str_detect(.data$`condition`,"\\[ERR\\]")) %>% nrow > 0){
      stop(call. = FALSE,
"Names across your data dictionary differ from names across the dataset.",
bold("\n\nUseful tip:"),
" Use dataset_evaluate(dataset, data_dict) for a full assessment of the dataset"
)}

    if(ncol(dataset) == 0) return(dataset)
    
    data_dict_data <-
      data_dict_extract(dataset) %>%
      as_data_dict_mlstr(name_standard = FALSE)

    is_factor <-
      dataset %>%
      summarise(across(everything(), ~ class(.))) %>%
      pivot_longer(everything()) %>%
      dplyr::filter(.data$`value` == "factor")

    data_dict_data[['Variables']] <-
      data_dict_data[['Variables']] %>%
      select(-.data$`valueType`) %>%
      left_join(data_dict[['Variables']] %>%
                  select(.data$`name`, .data$`valueType`),by = "name")

    for(i in names(dataset)){
      dataset[[i]] <-
        as_valueType(
          x = dataset[[i]],
          valueType = data_dict[['Variables']][[
            which(data_dict[['Variables']]$`name` == i),
            'valueType']])}

    dataset <-
      data_dict_apply(dataset, data_dict_data) %>%
      mutate(across(c(is_factor$`name`), ~ as.factor(.))) %>%
      as_dataset(col_id = preserve_attributes)

    return(dataset)
  }

  if(is_dataset(from) & is_dataset(to))
    stop(call. = FALSE, "The argument are both datasets.")
  
  if(is_data_dict(from) & is_data_dict(to))
    stop(call. = FALSE, "The argument are both data dictionaries.")
  
  if(is_dataset(to))   { 
    message("The argument is not a data dictionary.")
    as_data_dict(from) }
  
  if(is_dataset(from)) { 
    message("The argument is not a data dictionary.")
    as_data_dict(to) }
  
  if(is_data_dict(to))   { 
    message("The argument is not a dataset.")
    as_data_dict(from) }
  
  if(is_data_dict(from)) { 
    message("The argument is not a dataset.")
    as_dataset(to) }
  
  message(
"The arguments are neither a dataset nor a data dictionary.")
  
  silently_run(stop(call. = FALSE))
  
}

#' @title
#' Guess the first possible valueType of an object (Can be a vector)
#'
#' @description
#' Provides the first possible valueType of a variable. The function tries to 
#' assign the valueType of the object first to 'boolean', then 'integer', then 
#' 'decimal', then 'date'. If all others fail, the default valueType is 'text'.
#'
#' @details
#' A data dictionary contains the list of variables in a dataset and metadata 
#' about the variables and can be associated with a dataset. A data dictionary 
#' object is a list of data frame(s) named 'Variables' (required) and 
#' 'Categories' (if any). To be usable in any function, the data frame 
#' 'Variables' must contain at least the `name` column, with all unique and 
#' non-missing entries, and the data frame 'Categories' must contain at least 
#' the `variable` and `name` columns, with unique combination of 
#' `variable` and `name`.
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
#' @param x Object. Can be a vector.
#'
#' @returns
#' A character string which is the first possible valueType of the input object.
#'
#' @examples
#' {
#' 
#' # use madshapR_DEMO provided by the package
#'
#' dataset <- madshapR_DEMO$dataset_TOKYO
#' valueType_guess(dataset$dob)
#' 
#' valueType_guess(mtcars$cyl)
#' 
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
valueType_guess <- function(x){

  # check if the col is empty
  if(is.list(x) & sum(nrow(x)) <= 1)
    return(valueType_guess(x = x[[1]]))
  
  # check if the col is a vector
  if(is.list(x))
    stop(call. = FALSE,"'list' object cannot be coerced to valueType")
  
  # check if all is na
  if(all(is.na(x))) return(valueType_of(x))

  # else :
  x <-   unique(x)
  
  vT_list <- madshapR::valueType_list

  test_vT_boolean  <- 
    silently_run(as_valueType(as.character.default(x),"boolean"))
  test_vT_integer  <- 
    silently_run(as_valueType(as.character.default(x),"integer"))
  test_vT_decimal  <- 
    silently_run(as_valueType(as.character.default(x),"decimal"))
  test_vT_date     <-
    silently_run(as_valueType(                     x ,"date"))
  test_vT_datetime <-
    silently_run(as_valueType(                     x ,"datetime"))
  test_vT_text     <-                    
    as_valueType(                                  x , "text")

  test_vT <-
    tribble(
      ~`valueType` ,~`class`                  ,
      "boolean"    ,
      class(test_vT_boolean)[[max(length(class(test_vT_boolean)))]][1],
      
      "integer"    ,
      class(test_vT_integer)[[max(length(class(test_vT_integer)))]][1],
      
      "decimal"    ,
      class(test_vT_decimal)[[max(length(class(test_vT_decimal)))]][1],
      
      "date"       ,
      class(test_vT_date)[[max(length(class(test_vT_date)))]][1],

      "datetime"   ,
      class(test_vT_datetime)[[max(length(class(test_vT_datetime)))]][1]
      
      ) %>%
    dplyr::filter(.data$`class` != "try-error") %>%
    summarise(
      valueType = paste0(.data$`valueType`,collapse = "|"),
      class = paste0(.data$`class`,collapse = "|")) %>%
    mutate(
      valueType =
        case_when(
          .data$`valueType` == "boolean|integer|decimal"      ~ "integer"      ,
          .data$`valueType` == "integer|decimal"              ~ "integer"      ,
          .data$`valueType` == "integer|decimal|date"         ~ "date"         ,
          .data$`valueType` == "integer|decimal|datetime"     ~ "datetime"     ,
          .data$`valueType` == "decimal|date"                 ~ "date"         ,
          .data$`valueType` == "date|datetime"                ~ "date"         ,
          .data$`valueType` == "boolean|integer|decimal|date" ~ valueType_of(x),
          TRUE                                              ~  .data$`valueType`
        )) %>% pull(.data$`valueType`)
  
  if(test_vT == "") test_vT <- 'text'
  
  return(test_vT)
}

#' @title
#' Validate and coerce any object according to a given valueType
#'
#' @description
#' Attributes a valueType to an object, that can be a vector, or in a data frame 
#' using [dplyr::mutate].
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
#' @param x Object to be coerced. Can be a vector.
#' @param valueType A character string of the valueType used to coerce x.
#'
#' @returns
#' The object coerced accordingly to the input valueType.
#'
#' @examples
#' {
#' 
#' # use madshapR_DEMO provided by the package
#'
#' dataset <- madshapR_DEMO$dataset_TOKYO
#' as_valueType(head(dataset$dob),'date')
#' 
#' # as_valueType is compatible with tidyverse philosophy
#' library(dplyr)
#' mtcars %>% mutate(cyl = as_valueType(cyl,'integer')) %>% head()
#' 
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
as_valueType <- function(x, valueType = 'text'){

  # check if the col is empty
  if(is.list(x) & sum(nrow(x)) <= 1) return(as_valueType(x = x[[1]], valueType))

  # check if the col is a vector
  if(is.list(x))
    stop(call. = FALSE,"'list' object cannot be coerced to valueType")

  # if x is already the output format, no need to go further
  if(class(x)[[max(length(class(x)))]] == "Date"    & 
     valueType == "date")     return(x)
  if(class(x)[[max(length(class(x)))]] == "POSIXt" & 
     valueType == "datetime") return(x)
  if(is.integer(x)            & 
     valueType == "integer")  return(x)
  if(class(x)[[max(length(class(x)))]] == "numeric" & 
     valueType == "decimal")  return(x)
  if(is.logical(x)            & 
     valueType == "boolean")  return(x)
  if(is.na(valueType)         | 
     valueType == "text")     return(as.character.default(x))

  vT_list <- madshapR::valueType_list
  # check if valueType exists
  if(!valueType %in% vT_list$`valueType`) {
    stop(call. = FALSE,
"\nThe valueType provided does not exists. Please refer to documentation.",
bold("\n\nUseful tip:"),
" Use data_dict_evaluate(data_dict) to get a full assessment of your
data dictionary")}

  dataType <- vT_list[[which(vT_list['valueType'] == valueType),'call']]

  if(dataType     == "as_any_date")     x <- 
    as.character.default(x)
  if(dataType     == "as_any_boolean")  x <- 
    as_any_boolean(as.character.default(x))
  if(class(x)[1]  == "factor")          x <- 
    as.character.default(x)

  if(dataType     == "as_any_date"){
    date_format <-
      guess_date_format(
        tibble(as.character.default(
          sample(x[!is.na(x)], size = min(length(x[!is.na(x)]),20)))))

    if(date_format$`% values formated` == 100){
      x_temp <- as_any_date(as.character.default(x), date_format$`Date format`)
      }else{x_temp <- NA}

  }else{
    x_temp <- do.call(dataType, list(x)) %>% unlist
    }

  condition <- tibble(to_test = x_temp, original = x)

  if(length(x_temp) == 0){
    return(x_temp)}

  if(valueType %in% c("text","locale","point","linestring","polygon","binary")){
    return(x_temp)}

  if(!all(is.na(condition$`to_test`) == is.na(condition$`original`))){
    test_condition <- FALSE
  }else{

    test_condition <-
      distinct(condition[which(!is.na(condition['original'])),])

    if(valueType %in% c("integer","decimal")){
      test_condition <- 
        test_condition %>%
        mutate(across(everything(), ~ as.numeric(as.character.default(.)))) %>%
        mutate(test = .data$`to_test` == .data$`original`) %>%
        pull(.data$`test`) %>% all}

    if(valueType %in% c("boolean")){
      test_condition <- 
        test_condition %>%
        mutate(
          across(everything(), ~ as_any_boolean(as.character.default(.)))) %>%
        mutate(test = .data$`to_test` == .data$`original`) %>%
        pull(.data$`test`) %>% all}

    if(valueType %in% c("date")){
      test_condition <-
        test_condition %>%
        mutate(across(
          "original",
          ~ as_any_date(as.character.default(.),date_format$`Date format`))) %>%
        mutate(
          test = toString(.data$`to_test`) == toString(.data$`original`)) %>%
        pull(.data$`test`) %>% all}
    
    if(valueType %in% c("datetime")){
      test_condition <- 
        test_condition %>%
        mutate(
          across(everything(), ~ as.POSIXct.default(.))) %>%
        mutate(test = .data$`to_test` == .data$`original`) %>%
        pull(.data$`test`) %>% all}
    }

  # test if dataset and data_dict content match

  if(test_condition == FALSE){
    stop(call. = FALSE,
"\n
The valueType conflicts with the data type. Object cannot be coerced to
valueType",
bold("\n\nUseful tip:"),
" Use valueType_guess(x) to evaluate the first potential valueType.
For further investigation, you can use dataset_evaluate(dataset, data_dict).")
  }

  return(x_temp)
}

#' @title
#' Validate and coerce any object as a taxonomy
#'
#' @description
#' Confirms that the input object is a valid taxonomy and returns it as a
#' taxonomy with the appropriate `madshapR::class` attribute. This function 
#' mainly helps validate input within other functions of the package but could 
#' be used to check if a taxonomy is valid.
#'
#' @details
#' A taxonomy is a classification schema that can be defined for variable 
#' attributes. A taxonomy is usually extracted from an 
#' [Opal environment](https://www.obiba.org/pages/products/opal/), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#' 
#' @seealso
#' [Opal documentation](https://opaldoc.obiba.org/en/dev/magma-user-guide/value/type.html)
#'
#' @param object A potential taxonomy to be coerced.
#'
#' @returns
#' A list of data frame(s) with `madshapR::class` 'taxonomy'.
#'
#' @examples
#' {
#' 
#' # use madshapR_DEMO provided by the package
#'
#' ###### Example
#' as_taxonomy(madshapR_DEMO$taxonomy_PARIS)
#' 
#'}
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
as_taxonomy <- function(object){

  # check if names in object exist
  if(sum(names(object) %in% c("taxonomy","vocabulary" ,"term")) != 3){
    stop(call. = FALSE,
"\n
This object is not a taxonomy as defined by Maelstrom standards, which must 
be a data frame containing at least 'taxonomy', 'vocabulary' and 'term' columns. 
Please refer to documentation.",

#       bold("\n\nUseful tip:"),
# " Use taxonomy_opal_get(opal) or taxonomy_opal_mlstr_get(opal) to get 
# the taxonomy present in your Opal environment."
)}

  # check if names in taxonomy exist
  if(sum(names(object) %in%
     c("vocabulary_short","taxonomy_scale",
       "vocabulary_scale","term_scale")) == 4){
    
    ## create index if not exists
    
    attributes(object)$`madshapR::class` <- "taxonomy_mlstr"
  }else{
    attributes(object)$`madshapR::class` <- "taxonomy_opal"}

  return(object)

}


#' @title
#' Test if a character object is one of the valid valueType values
#'
#' @description
#' Confirms whether the input object is a valid valueType. This function mainly
#' helps validate input within other functions of the package but could be used 
#' to check if a valueType is valid.
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
#' @param object A potential valueType name to be evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#'
#' is_valueType('integer')
#' is_valueType('integre')
#'
#'}
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
is_valueType <- function(object){

  object <- object
  vT_list <- madshapR::valueType_list
  # check if valueType exists
  if(!all(object %in% vT_list$`valueType`)) return(FALSE)

  # else
  return(TRUE)

}

#' @title
#' Test if an object is a valid taxonomy
#'
#' @description
#' Confirms whether the input object is a valid taxonomy. This function mainly
#' helps validate input within other functions of the  package but could be 
#' used to check if a taxonomy is valid.
#'
#' @details
#' A taxonomy is a classification schema that can be defined for variable 
#' attributes. A taxonomy is usually extracted from an 
#' [Opal environment](https://www.obiba.org/pages/products/opal/), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#'
#' @param object A potential taxonomy to be evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#' 
#' # use madshapR_DEMO provided by the package
#'
#' is_taxonomy(madshapR_DEMO$taxonomy_PARIS)
#'
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
is_taxonomy <- function(object){

  object <- object
  # if only the data frame is given in parameter
  test <- silently_run(try(as_taxonomy(object),silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)

}
