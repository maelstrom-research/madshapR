#' @title
#' Return the valueType of an object
#'
#' @description
#' Determines the valueType of an object based on [typeof()] and 
#' [class()]. The possible values returned are 'date', 'boolean', 
#' 'integer', 'decimal', and 'text'.
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
#' [typeof()], [class()]
#' [madshapR::valueType_list] for insights about possible valueType and
#' translation into type and class in R.
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
#' # use DEMO_files provided by the package
#'
#' dataset <- DEMO_files$dataset_MELBOURNE_1
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

  if(type == "double" & class == "Date") valueType <- "date"

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
#' A dataset must be a data frame-like object and can be associated with a 
#' data dictionary. If no data dictionary is provided, a minimum workable 
#' data dictionary will be generated as needed by relevant functions. 
#' An identifier `id` column for sorting can be specified by the user. If 
#' specified, the `id` values must be non-missing and will be used in functions 
#' that require it. If no identifier column is specified, indexing is handled 
#' automatically by the function.
#' 
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
#' [valueType_adjust()]
#'
#' @param ... Object that can be either a dataset or a data dictionary.
#'
#' @returns
#' Either a tibble, identifying the dataset, or a list of tibble(s)
#' identifying a data dictionary, depending which the input refers to.
#'
#' @examples
#' {
#' 
#' ###### Example : The valueType of a dataset can be adjusted. each column is
#' # evaluated as whole, and the best valueType match found is applied. If 
#' # there is no better match found, the column is left as it is.
#' 
#' valueType_self_adjust(mtcars['cyl'])
#'
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom rlang .data
#'
#' @export
valueType_self_adjust <- function(...){

  # test dataset

  if(is_dataset(...) & !is_data_dict(...)){
    
    dataset <- as_dataset(...,col_id = attributes(...)$`madshapR::col_id`)
    
    if(ncol(dataset) == 0) return(dataset)
    
    preserve_attributes <- attributes(dataset)$`madshapR::col_id`

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

  if(!is_dataset(...) & is_data_dict(...)){
    
    data_dict <- as_data_dict_shape(...)
    attributes(data_dict)$`madshapR::class` <- attributes(...)$`madshapR::class`
    
    if(nrow(data_dict[['Variables']]) == 0) return(data_dict)

    if(sum(nrow(data_dict[['Categories']])) == 0){
      warning(
"Your data dictionary contains no categorical variables.
The valueType will remain as it is.")
      return(data_dict)

    }else{

      category_outcomes <-
        data_dict[['Categories']] %>%
        select(.data$`name`) %>% distinct %>%
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

  stop(call. = FALSE, "The argument is neither a dataset or a data dictionary.")
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
#' A dataset must be a data frame-like object and can be associated with a 
#' data dictionary. If no data dictionary is provided, a minimum workable 
#' data dictionary will be generated as needed by relevant functions. 
#' An identifier `id` column for sorting can be specified by the user. If 
#' specified, the `id` values must be non-missing and will be used in functions 
#' that require it. If no identifier column is specified, indexing is handled 
#' automatically by the function.
#' 
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
#' [valueType_self_adjust()]
#'
#' @param from Object to be adjusted. Can be either a dataset or a data 
#' dictionary.
#' @param to Object to be adjusted. Can be either a dataset or a data 
#' dictionary. NULL by default.
#'
#' @returns
#' Either a tibble, identifying the dataset, or a list of tibble(s)
#' identifying a data dictionary, depending which is 'to'.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' library(dplyr)
#' 
#' dataset <- DEMO_files$dataset_TOKYO[c(1:4),'prg_ever']
#' data_dict <-
#'   DEMO_files$dd_TOKYO_format_maelstrom %>%
#'   data_dict_filter(filter_var = 'name == "prg_ever"') %>%
#'   as_data_dict_mlstr()
#' valueType_adjust(from = dataset,to = data_dict)
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
    
    vT_list<- madshapR::valueType_list
    vT_tables <-
      dataset %>%
      summarise(across(everything(), valueType_of)) %>%
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

  stop(call. = FALSE,
       "The arguments are neither or (both) a dataset or a data dictionary.")
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
#' A dataset must be a data frame-like object and can be associated with a 
#' data dictionary. If no data dictionary is provided, a minimum workable 
#' data dictionary will be generated as needed by relevant functions. 
#' An identifier `id` column for sorting can be specified by the user. If 
#' specified, the `id` values must be non-missing and will be used in functions 
#' that require it. If no identifier column is specified, indexing is handled 
#' automatically by the function.
#' 
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
#' @param x Object. Can be a vector.
#'
#' @returns
#' A character string which is the first possible valueType of the input object.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' dataset <- DEMO_files$dataset_TOKYO
#' valueType_of(dataset$dob)
#' valueType_guess(dataset$dob)
#' 
#' valueType_of(mtcars$cyl)
#' valueType_guess(mtcars$cyl)
#' 
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
valueType_guess <- function(x){

  # test
  x <- unique(x)

  # check if the col is empty
  if(is.list(x) & sum(nrow(x)) <= 1)
    return(as_valueType(x = x[[1]]))

  # check if the col is a vector
  if(is.list(x))
    stop(call. = FALSE,"'list' object cannot be coerced to valueType")

  vT_list <- madshapR::valueType_list

  test_vT_boolean <- 
    silently_run(as_valueType(as.character.default(x),"boolean"))
  test_vT_integer <- 
    silently_run(as_valueType(as.character.default(x),"integer"))
  test_vT_decimal <- 
    silently_run(as_valueType(as.character.default(x),"decimal"))
  test_vT_date    <- 
    silently_run(as_valueType(             x ,"date"   ))
  test_vT_text    <-                    
    as_valueType(x, "text"   )

  test_vT <-
    tribble(
      ~`valueType` ,~`class`                  ,
      "boolean"    ,  class(test_vT_boolean )[1],
      "integer"    ,  class(test_vT_integer )[1],
      "decimal"    ,  class(test_vT_decimal )[1],
      "date"       ,  class(test_vT_date    )[1]) %>%
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
          .data$`valueType` == "decimal|date"                 ~ "date"         ,
          .data$`valueType` == "boolean|integer|decimal|date" ~ valueType_of(x),
          TRUE                                              ~  .data$`valueType`
        )) %>% pull(.data$`valueType`)

  if(test_vT == "") test_vT <- 'text'

  return(test_vT)
}

#' @title
#' Validate and coerce an object according to a given valueType
#'
#' @description
#' Attributes a valueType to an object, that can be a vector, or in a tibble 
#' using [dplyr::mutate].
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
#' @param x Object to be coerced. Can be a vector.
#' @param valueType A character string of the valueType used to coerce x.
#'
#' @returns
#' The object coerced accordingly to the input valueType.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' dataset <- DEMO_files$dataset_TOKYO
#' valueType_of(dataset$dob)
#' valueType_guess(dataset$dob)
#' as_valueType(dataset$dob,'date') 
#' 
#' # as_valueType is compatible with tidyverse philosophy
#' library(dplyr)
#' mtcars %>% mutate(cyl = as_valueType(cyl,'integer'))
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
  if(class(x)[1] == "Date"    & valueType == "date")    return(x)
  if(is.integer(x)            & valueType == "integer") return(x)
  if(class(x)[1] == "numeric" & valueType == "decimal") return(x)
  if(is.logical(x)            & valueType == "boolean") return(x)
  if(is.na(valueType)         | valueType == "text")    return(
    as.character.default(x))

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
      test_condition <- test_condition %>%
        mutate(across(everything(), ~ as.numeric(as.character.default(.)))) %>%
        mutate(test = .data$`to_test` == .data$`original`) %>%
        pull(.data$`test`) %>% all}

    if(valueType %in% c("boolean")){
      test_condition <- test_condition %>%
        mutate(
          across(everything(), ~ as_any_boolean(as.character.default(.)))) %>%
        mutate(test = .data$`to_test` == .data$`original`) %>%
        pull(.data$`test`) %>% all}

    if(valueType %in% c("date","datetime")){
      test_condition <-
        test_condition %>%
        mutate(across(
          "original",
          ~ as_any_date(as.character.default(.),date_format$`Date format`))) %>%
        mutate(
          test = toString(.data$`to_test`) == toString(.data$`original`)) %>%
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
#' Validate and coerce an object to taxonomy format
#'
#' @description
#' Confirms that the input object is a valid taxonomy and returns it as a
#' taxonomy with the appropriate 'madshapR::class' attribute. This function 
#' mainly helps validate input within other functions of the package but could 
#' be used to check if a taxonomy is valid.
#'
#' @details
#' A taxonomy is classification scheme that can be defined for variable 
#' attributes. If defined, a taxonomy must be a data frame-like object. It must 
#' be compatible with (and is generally extracted from) an Opal environment. To 
#' work with certain functions, a valid taxonomy must contain at least the 
#' columns 'taxonomy', 'vocabulary', and 'terms'. In addition, the taxonomy
#' may follow Maelstrom research taxonomy, and its content can be evaluated
#' accordingly, such as naming convention restriction, tagging elements,
#' or scales, which are specific to Maelstrom Research. In this particular
#' case, the tibble must also contain 'vocabulary_short', 'taxonomy_scale',
#' 'vocabulary_scale' and 'term_scale' to work with some specific functions.
#' 
#' @seealso
#' [Opal documentation](https://opaldoc.obiba.org/en/dev/magma-user-guide/value/type.html)
#'
#' @param object A potential taxonomy to be coerced.
#'
#' @returns
#' A tibble identifying a taxonomy (generally generated from Opal taxonomy).
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' ###### Example
#' as_taxonomy(DEMO_files$taxonomy_PARIS)
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
be a data frame (or tibble) containing at least 'taxonomy', 'vocabulary' and 
'term' columns. 
Please refer to documentation.",

#       bold("\n\nUseful tip:"),
# " Use taxonomy_opal_get(opal) or taxonomy_opal_mlstr_get(opal) to get 
# the taxonomy present in your Opal environment."
)}

  # check if names in taxonomy exist
  if(sum(names(object) %in%
     c("vocabulary_short","taxonomy_scale",
       "vocabulary_scale","term_scale")) == 4){

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
#' A taxonomy is classification scheme that can be defined for variable 
#' attributes. If defined, a taxonomy must be a data frame-like object. It must 
#' be compatible with (and is generally extracted from) an Opal environment. To 
#' work with certain functions, a valid taxonomy must contain at least the 
#' columns 'taxonomy', 'vocabulary', and 'terms'. In addition, the taxonomy
#' may follow Maelstrom research taxonomy, and its content can be evaluated
#' accordingly, such as naming convention restriction, tagging elements,
#' or scales, which are specific to Maelstrom Research. In this particular
#' case, the tibble must also contain 'vocabulary_short', 'taxonomy_scale',
#' 'vocabulary_scale' and 'term_scale' to work with some specific functions.
#'
#' @param object A potential taxonomy to be evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' is_taxonomy(DEMO_files$taxonomy_PARIS)
#' is_taxonomy(DEMO_files$taxonomy_opal_mlstr)
#' is_taxonomy(iris)
#'
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
is_taxonomy <- function(object){

  object <- object
  # if only the tibble is given in parameter
  test <- silently_run(try(as_taxonomy(object),silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)

}
