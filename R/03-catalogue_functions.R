#' @title
#' Give the valueType of any object
#'
#' @description
#' Determines the valueType based on [base::typeof()] and [base::class()] of
#' an object.
#' The possible values returned are 'date', 'boolean', 'integer',
#' 'decimal', and 'text'.
#'
#' @details
#' valueType is one of the property of a variable entity. It refers to
#' the (Obiba internal) type of any variable. The valueType can be 'text',
#' 'integer', 'decimal', 'boolean', 'locale', 'datetime', 'date', 'binary',
#' 'point', 'linestring', 'polygon'.
#' The valueType list is available using [madshapR::valueType_list], and their
#' corresponding with typeof which is the (R internal) type of any object.
#'
#' @seealso
#' [base::typeof()], [base::class()]
#' [madshapR::valueType_list] for insights about possible valueType and
#' translation into type and class in R.
#' [Opal documentation](https://opaldoc.obiba.org/en/dev/magma-user-guide/value/type.html)
#'
#' @param x R object. Can be a vector.
#'
#' @return
#' A character string which is the valueType of the given object.
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
#' @import dplyr tidyr
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

  fabR::silently_run({
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
#' Search and replace the valueType of any data dictionary or dataset
#'
#' @description
#' Using the function [madshapR::valueType_guess()], this function provides the
#' first possible valueType, by trying to assign the object to 'boolean', then
#' 'integer', then 'decimal', then 'date'. (If all fails, 'text', by default),
#' then either replace the valueType column of a data dictionary of casts a
#' column in the dataset. This depending on the input provided.
#'
#' @details
#' A data dictionary-like structure must be a list of at least one or two
#' data frame or data frame extension (e.g. a tibble) named 'Variables'
#' and 'Categories' (if any), representing meta data of an associated dataset.
#' The 'Variables' component must contain at least 'name' column and the
#' 'Categories' component must at least contain 'variable' and 'name'
#' columns to be usable in any function of the package.
#' To be considered as a minimum (workable) data dictionary, it must also
#' have unique and non-null entries in 'name' column and the combination
#' 'name'/'variable' must also be unique in 'Categories'.
#' In addition, the data dictionary may follow Maelstrom research standards,
#' and its content can be evaluated accordingly, such as naming convention
#' restriction, columns like 'valueType', 'missing' and 'label(:xx)',
#' and/or any taxonomy provided.
#'
#' A dataset must be a data frame or data frame extension (e.g. a tibble) and
#' can be associated to a data dictionary. If not, a minimum workable data
#' dictionary can always be generated, when any column will be reported, and
#' any factor column will be analysed as categorical variable (the column
#' 'levels' will be created for that. In addition, the dataset may follow
#' Maelstrom research standards, and its content can be evaluated accordingly,
#' such as naming convention restriction, or id columns declaration (which
#' full completeness is mandatory.
#'
#' @seealso
#' [madshapR::valueType_adjust()]
#'
#' @param ... R object that can be either a dataset or a data dictionary.
#'
#' @return
#' Either a tibble, identifying the dataset, or a list of tibble(s)
#' identifying a data dictionary, depending which the input refers to.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$dd_TOKYO_format_maelstrom_tagged
#' data_dict$Variables$valueType <- "text"
#' dataset <- DEMO_files$dataset_TOKYO
#' 
#' ###### Example 1: The valueType of a dataset can be adjusted. each column is
#' # evaluated as whole, and the best valueType match found is applied. If 
#' # there is no better match found, the column is left as it is.
#' valueType_self_adjust(dataset)
#' 
#' #' ###### Example 2: The valueType present in a data dictionary can be 
#' # adjusted (only for categorical variables). Each categorical variable is
#' # evaluated as whole (with the values present in the 'name' column in the 
#' # 'Categories' component), and the best valueType match found is applied. If 
#' # there is no better match found, the valueType is left as it is.
#' valueType_self_adjust(data_dict)
#'
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom rlang .data
#'
#' @export
valueType_self_adjust <- function(...){

  # test data

  if(is_dataset(...) & !is_data_dict(...)){
    data <- as_dataset(...,col_id = attributes(...)$`Mlstr::col_id`)
    preserve_attributes <- attributes(data)$`Mlstr::col_id`

    is_factor <-
      data %>%
      summarise(across(everything(), ~ toString(class(.)))) %>%
      pivot_longer(everything()) %>%
      filter(.data$`value` %in% c("factor"))

    data_dict <- data_dict_extract(data)
    data_dict[['Categories']] <-
      bind_rows(
        Categories = tibble(name = as.character(),variable = as.character()),
        data_dict[['Categories']])

    for(i in names(data)) {
      data[[i]] <-
        as_valueType(x = data[[i]], valueType = valueType_guess(x = data[[i]]))
      }

    data_dict_final <- data_dict_extract(data)
    data_dict[['Variables']]['valueType'] <- NULL
    data_dict_final[['Variables']] <-
      data_dict_final[['Variables']][c('name','valueType')] %>%
      left_join(data_dict[['Variables']], by = c("name"))
    data_dict_final <- c(data_dict_final['Variables'], data_dict['Categories'])

    data <-
      data_dict_apply(data, data_dict_final) %>%
      mutate(across(c(is_factor$`name`), ~ as.factor(.))) %>%
      as_dataset(col_id = preserve_attributes)

    return(data)
  }

  if(!is_dataset(...) & is_data_dict(...)){
    data_dict <- as_data_dict_shape(...)

    if(sum(nrow(data_dict[['Categories']])) == 0){
      warning(
        "Your data dictionary contains no categorical variables.
valueType will remain as it is.")
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
#' Replace the valueType from (or to) any data dictionary to (or from) dataset
#'
#' @description
#' Depending on the input provided (from and to can be either a dataset or a
#' data dictionary, this function takes the valueType of the first input (from),
#' and assigns it to the second attribute (to). The valueType replaced is
#' either in the 'valueType' column of a data dictionary or cast to a column
#' in the dataset. If 'to' is not provided, the function calls the function
#' [valueType_self_adjust()] instead.
#'
#' @details
#' A data dictionary-like structure must be a list of at least one or two
#' data frame or data frame extension (e.g. a tibble) named 'Variables'
#' and 'Categories' (if any), representing meta data of an associated dataset.
#' The 'Variables' component must contain at least 'name' column and the
#' 'Categories' component must at least contain 'variable' and 'name'
#' columns to be usable in any function of the package.
#' To be considered as a minimum (workable) data dictionary, it must also
#' have unique and non-null entries in 'name' column and the combination
#' 'name'/'variable' must also be unique in 'Categories'.
#' In addition, the data dictionary may follow Maelstrom research standards,
#' and its content can be evaluated accordingly, such as naming convention
#' restriction, columns like 'valueType', 'missing' and 'label(:xx)',
#' and/or any taxonomy provided.
#'
#' A dataset must be a data frame or data frame extension (e.g. a tibble) and
#' can be associated to a data dictionary. If not, a minimum workable data
#' dictionary can always be generated, when any column will be reported, and
#' any factor column will be analysed as categorical variable (the column
#' 'levels' will be created for that. In addition, the dataset may follow
#' Maelstrom research standards, and its content can be evaluated accordingly,
#' such as naming convention restriction, or id columns declaration (which
#' full completeness is mandatory.
#'
#' @seealso
#' [madshapR::valueType_self_adjust()]
#'
#' @param from R object to be adjusted. Can be either a dataset or a data
#' dictionary.
#' @param to R object to be adjusted. Can be either a dataset or a data
#' dictionary.
#' NULL by default, which is equivalent to valueType_self_adjust(... = from)
#'
#' @return
#' Either a tibble, identifying the dataset, or a list of tibble(s)
#' identifying a data dictionary, depending which is 'to'.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$dd_TOKYO_format_maelstrom_tagged
#' dataset <- DEMO_files$dataset_TOKYO
#' 
#' ###### Example 1: The valueType of a dataset can be adjusted using 
#' # the valueType present in the data dictionary.
#' valueType_adjust(from = data_dict,to = dataset)
#' 
#' ###### Example 2: The valueType of a dataset can be adjusted using 
#' # the valueType present in the data dictionary. Here the valueType is 'text'
#' data_dict[['Variables']]$valueType <- 'text'
#' valueType_adjust(from = data_dict,to = dataset)
#' 
#' ###### Example 1: The valueType of a data dictionary can be adjusted using 
#' # the valueType of each column of the dataset.
#' data_dict[['Variables']]$valueType <- 'text'
#' dataset <- valueType_self_adjust(dataset)
#' valueType_adjust(from = dataset,to = data_dict)
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
valueType_adjust <- function(from, to = NULL){

  # test data
  if(is.null(to)) return(valueType_self_adjust(from))

  # apply the data dictionary of the dataset to the data dictionary
  if(is_dataset(from) & is_data_dict(to)){
    as_dataset(from)
    as_data_dict_shape(to)

    data <- from
    data_dict <- to

    # data must match
    if(suppressWarnings(check_dataset_variables(data, data_dict)) %>% nrow > 0){
      stop(call. = FALSE,
"Names across your data dictionary differ from names across the dataset.",
crayon::bold("\n\nUseful tip:"),
" Use dataset_evaluate(dataset, data_dict) to get a full assessment of your dataset")}

    vT_list<- madshapR::valueType_list
    vT_tables <-
      data %>%
      summarise(across(everything(), valueType_of)) %>%
      pivot_longer(cols = everything()) %>%
      rename(valueType = .data$`value`) %>%
      left_join(vT_list, by = "valueType") %>%
      select(.data$`name`, .data$`valueType`,.data$`typeof`)

    data_dict[['Variables']]['typeof'] <-
      data_dict[['Variables']]['name'] %>%
      left_join(vT_tables %>%
                  select(.data$`name`, .data$`typeof`), by = "name") %>%
      select( .data$`typeof`)
    # }

    # if(length(data_dict[['Variables']][['valueType']]) > 0){
    data_dict[['Variables']]['valueType'] <-
      data_dict[['Variables']]['name'] %>%
      left_join(vT_tables %>%
                  select(.data$`name`, .data$`valueType`), by = "name") %>%
      select( .data$`valueType`)
    # }

    return(data_dict)
    # }
  }

  if(is_data_dict(from) & is_dataset(to)){

    # test data dict
    tryCatch({data_dict <-
      as_mlstr_data_dict(from)},
      warning = function(cond){
        stop(call. = FALSE,cond)})

    # test dataset
    data <- as_dataset(to,col_id = attributes(to)$`Mlstr::col_id`)
    preserve_attributes <- attributes(data)$`Mlstr::col_id`

    # data must match
    if(suppressWarnings(check_dataset_variables(data, data_dict)) %>% nrow > 0){
      stop(call. = FALSE,
"Names across your data dictionary differ from names across the dataset.",
crayon::bold("\n\nUseful tip:"),
" Use dataset_evaluate(dataset, data_dict) to get a full assessment of your dataset")}

    data_dict_data <-
      data_dict_extract(data) %>%
      as_mlstr_data_dict()

    is_factor <-
      data %>%
      summarise(across(everything(), ~ class(.))) %>%
      pivot_longer(everything()) %>%
      filter(.data$`value` == "factor")

    data_dict_data[['Variables']] <-
      data_dict_data[['Variables']] %>%
      select(-.data$`valueType`) %>%
      left_join(data_dict[['Variables']] %>%
                  select(.data$`name`, .data$`valueType`),by = "name")

    for(i in names(data)){
      data[[i]] <-
        as_valueType(
          x = data[[i]],
          valueType = data_dict[['Variables']][[
            which(data_dict[['Variables']]$`name` == i),
            'valueType']])}

    data <-
      data_dict_apply(data, data_dict_data) %>%
      mutate(across(c(is_factor$`name`), ~ as.factor(.))) %>%
      as_dataset(col_id = preserve_attributes)

    return(data)
  }

  stop(call. = FALSE,
       "The arguments are neither or (both) a dataset or a data dictionary.")
}

#' @title
#' Guess the first possible valueType of any object
#'
#' @description
#' Provides the first possible valueType, by trying to assign the object to
#' 'boolean', then 'integer', then 'decimal', then 'date'.
#' If all fails, 'text', by default.
#'
#' @details
#' valueType is one of the property of a variable entity. It refers to
#' the (Obiba internal) type of any variable. The valueType can be 'text',
#' 'integer', 'decimal', 'boolean', 'locale', 'datetime', 'date', 'binary',
#' 'point', 'linestring', 'polygon'
#' The valueType list is available using [madshapR::valueType_list], and their
#' corresponding with typeof which is the (R internal) type of any object.
#'
#' @seealso
#' [Opal documentation](https://opaldoc.obiba.org/en/dev/magma-user-guide/value/type.html)
#'
#' @param x R object. Can be a vector.
#'
#' @return
#' A character string which is the first possible valueType of the given
#' object.
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
#' @import dplyr tidyr
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

  test_vT_boolean <- fabR::silently_run(as_valueType(as.character(x),"boolean"))
  test_vT_integer <- fabR::silently_run(as_valueType(as.character(x),"integer"))
  test_vT_decimal <- fabR::silently_run(as_valueType(as.character(x),"decimal"))
  test_vT_date    <- fabR::silently_run(as_valueType(             x ,"date"   ))
  test_vT_text    <-                    as_valueType(x, "text"   )

  test_vT <-
    tribble(
      ~`valueType` ,~`class`                  ,
      "boolean"    ,  class(test_vT_boolean )[1],
      "integer"    ,  class(test_vT_integer )[1],
      "decimal"    ,  class(test_vT_decimal )[1],
      "date"       ,  class(test_vT_date    )[1]) %>%
    filter(.data$`class` != "try-error") %>%
    summarise(
      valueType = paste0(.data$`valueType`,collapse = "|"),
      class = paste0(.data$`class`,collapse = "|")) %>%
    mutate(
      valueType =
        case_when(
          .data$valueType == "boolean|integer|decimal"      ~ "integer"       ,
          .data$valueType == "integer|decimal"              ~ "integer"       ,
          .data$valueType == "integer|decimal|date"         ~ "date"          ,
          .data$valueType == "decimal|date"                 ~ "date"          ,
          .data$valueType == "boolean|integer|decimal|date" ~ valueType_of(x) ,
          TRUE                                              ~  .data$valueType
        )) %>% pull(.data$`valueType`)

  if(test_vT == "") test_vT <- 'text'

  return(test_vT)
}

#' @title
#' Validate and coerce any object according to a given valueType
#'
#' @description
#' Uses a data dictionary in the Maelstrom Research formats (with 'Variables'
#' and 'Categories' in separate tibbles and standard columns in each) to apply
#' their valueType to a dataset in tibble format. If no data dictionary is
#' provided, the function will automatically evaluate the most restrictive
#' valueType for each variable in the dataset and apply it.
#'
#' @details
#' valueType is one of the property of a variable entity. It refers to
#' the (Obiba internal) type of any variable. The valueType can be 'text',
#' 'integer', 'decimal', 'boolean', 'locale', 'datetime', 'date', 'binary',
#' 'point', 'linestring', 'polygon'
#' The valueType list is available using [madshapR::valueType_list], and their
#' corresponding with typeof which is the (R internal) type of any object.
#'
#' @seealso
#' [Opal documentation](https://opaldoc.obiba.org/en/dev/magma-user-guide/value/type.html)
#'
#' @param x R object to be coerced. Can be a vector.
#' @param valueType A character string of the valueType used to coerce x.
#'
#' @return
#' The object coerced accordingly to the given valueType.
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
  if(is.na(valueType)         | valueType == "text")    return(as.character(x))

  vT_list <- madshapR::valueType_list
  # check if valueType exists
  if(!valueType %in% vT_list$`valueType`) {
    stop(call. = FALSE,
"\nThe valueType provided does not exists. Please refer to documentation.",
crayon::bold("\n\nUseful tip:"),
" Use data_dict_evaluate(data_dict) to get a full assessment of your
data dictionary")}

  dataType <- vT_list[[which(vT_list['valueType'] == valueType),'call']]

  if(dataType     == "as_any_date")     x <- as.character(x)
  if(dataType     == "as_any_boolean")  x <- as_any_boolean(x)
  if(class(x)[1]  == "factor")          x <- as.character(x)

  if(dataType     == "as_any_date"){
    date_format <-
      fabR::guess_date_format(
        tibble(sample(x[!is.na(x)], size = min(length(x[!is.na(x)]),20))))

    if(date_format$`% values formated` == 100){
      x_temp <- fabR::as_any_date(x, date_format$`Date format`)
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
        mutate(across(everything(), ~ as.numeric(.))) %>%
        mutate(test = .data$`to_test` == .data$`original`) %>%
        pull(.data$`test`) %>% all}

    if(valueType %in% c("boolean")){
      test_condition <- test_condition %>%
        mutate(across(everything(), ~ fabR::as_any_boolean(.))) %>%
        mutate(test = .data$`to_test` == .data$`original`) %>%
        pull(.data$`test`) %>% all}

    if(valueType %in% c("date","datetime")){
      test_condition <-
        test_condition %>%
        mutate(across(
          .data$`original`,
          ~ fabR::as_any_date(.,date_format$`Date format`))) %>%
        mutate(
          test = toString(.data$`to_test`) == toString(.data$`original`)) %>%
        pull(.data$`test`) %>% all}
    }

  # test if data and data_dict content match

  if(test_condition == FALSE){
    stop(call. = FALSE,
"\n
The valueType conflicts with the data type. Object cannot be coerced to
valueType",
crayon::bold("\n\nUseful tip:"),
" Use valueType_guess(x) to evaluate the first potential valueType.
For further investigation, you can use dataset_evaluate(data, data_dict).")
  }

  return(x_temp)
}

#' @title
#' Validate and coerce any object as taxonomy
#'
#' @description
#' Confirms that the input object is a valid taxonomy, and return it as a
#' taxonomy with the appropriate mlstr_class attribute. This function mainly
#' helps validate input within other functions of the package but could be used
#' to check if a taxonomy is valid.
#'
#' @details
#' A taxonomy must be a data frame or data frame extension (e.g. a tibble).
#' The taxonomy must be compatible with (and generally extracted from) an
#' Opal environment, and must contain at least 'taxonomy', 'vocabulary' and
#' 'terms' to work with some specific functions. In addition, the taxonomy
#' may follow Maelstrom research standards, and its content can be evaluated
#' accordingly, such as naming convention restriction, tagging elements,
#' or scales, which are specific to Maelstrom Research. In this particular
#' case, the tibble must also contain 'vocabulary_short', 'taxonomy_scale',
#' 'vocabulary_scale' and 'term_scale' to work with some specific functions.
#' 
#' @seealso
#' [Opal documentation](https://opaldoc.obiba.org/en/dev/magma-user-guide/value/type.html)
#' [opal_taxonomy_get()]
#'
#'
#' @param object A potential taxonomy to be coerced.
#'
#' @return
#' A tibble identifying a taxonomy (generally generated from Opal taxonomy).
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' ###### Example 1: this function is compatible with opal_taxonomy_get()
#' library(opalr)
#' opal <- 
#'   opal.login('administrator','password',url = 'https://opal-demo.obiba.org/')
#' opal_taxo <- opal_mlstr_taxonomy_get(opal)
#' as_taxonomy(opal_taxo)
#' 
#' ###### Example 2: you can create your own taxonomy
#' print(DEMO_files$taxonomy_PARIS)
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
"\n\nThis object is not a taxonomy as defined by Maelstrom standards, which must 
be a data-frame (or tibble) containing at least 'taxonomy', 'vocabulary' and 
'term' columns. 
Please refer to documentation.",

      crayon::bold("\n\nUseful tip:"),
" Use opal_taxonomy_get(opal) or mlstr_taxonomy_get(opal) to get the taxonomy
present in your Opal environment.")}

  # check if names in taxonomy exist
  if(sum(names(object) %in%
     c("vocabulary_short","taxonomy_scale",
       "vocabulary_scale","term_scale")) == 4){

    attributes(object)$`Mlstr::class` <- "mlstr_taxonomy"
  }else{
    attributes(object)$`Mlstr::class` <- "opal_taxonomy"}

  return(object)

}


#' @title
#' Evaluate if any object is a valid valueType name or not
#'
#' @description
#' Uses a data dictionary in the Maelstrom Research formats (with 'Variables'
#' and 'Categories' in separate tibbles and standard columns in each) to apply
#' their valueType to a dataset in tibble format. If no data dictionary is
#' provided, the function will automatically evaluate the most restrictive
#' valueType for each variable in the dataset and apply it.
#'
#' @details
#' valueType is one of the property of a variable entity. It refers to
#' the (Obiba internal) type of any variable. The valueType can be 'text',
#' 'integer', 'decimal', 'boolean', 'locale', 'datetime', 'date', 'binary',
#' 'point', 'linestring', 'polygon'.
#' The valueType list is available using [madshapR::valueType_list], and their
#' corresponding with typeof which is the (R internal) type of any object.
#'
#' @seealso
#' [Opal documentation](https://opaldoc.obiba.org/en/dev/magma-user-guide/value/type.html)
#'
#' @param object A potential valueType name to be evaluated.
#'
#' @return
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
#' Evaluate if any object is a taxonomy scheme or not
#'
#' @description
#' Confirms whether the input object is a valid taxonomy.
#' This function mainly helps validate input within other functions of the
#' package but could be used to check if a taxonomy is valid.
#'
#' @details
#' A taxonomy must be a data frame or data frame extension (e.g. a tibble).
#' The taxonomy must be compatible with (and generally extracted from) an
#' Opal environment, and must contain at least 'taxonomy', 'vocabulary' and
#' 'terms' to work with some specific functions. In addition, the taxonomy
#' may follow Maelstrom research standards, and its content can be evaluated
#' accordingly, such as naming convention restriction, tagging elements,
#' or scales, which are specific to Maelstrom Research. In this particular
#' case, the tibble must also contain 'vocabulary_short', 'taxonomy_scale',
#' 'vocabulary_scale' and 'term_scale' to work with some specific functions.
#'
#' @param object A potential taxonomy to be evaluated.
#'
#' @return
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
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
is_taxonomy <- function(object){

  object <- object
  # if only the tibble is given in parameter
  test <- fabR::silently_run(try(as_taxonomy(object),silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)

}
