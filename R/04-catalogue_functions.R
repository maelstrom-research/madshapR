#' @title
#' Return the valueType of an object
#'
#' @description
#' Determines the valueType of an object based on [typeof()] and 
#' [class()]. The possible values returned are 'date', 'datetime', 'boolean', 
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
#' # use madshapR_example provided by the package
#' dataset <- madshapR_example$`dataset_example`
#' valueType_of(dataset[['part_id']])
#' 
#' # any data frame can be dataset by definition
#' valueType_of(iris[['Sepal.Length']])
#'
#' }
#'
#' @import dplyr tidyr fabR haven stringr
#' @importFrom rlang .data
#'
#' @export
valueType_of <- function(x){

  # check if x is a column
  if(is.list(x) & sum(nrow(x)) <= 1)
    return(as_valueType(x = x[[1]], valueType))

  # check if the col is a vector
  if(is.list(x))
    stop(call. = FALSE, "'list' object cannot be coerced to valueType")

  type  <- typeof(x)
  class <- class(zap_labels(x))
  # class <- class(x)[[max(length(class(x)))]]
  
  vT_list <- madshapR::valueType_list

  valueType <-
    unique(vT_list[
      which(vT_list[['typeof']] == type),]$`toValueType`)

  if(str_detect(toString(class), c("^numeric$"))) valueType <- "decimal"
  if(str_detect(toString(class), c("^Date$")))    valueType <- "date"
  if(str_detect(toString(class), c("POSIX")))     valueType <- "datetime"
  if(type == "logical")                           valueType <- "boolean"

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
      
      test_vT_bool  <- all(str_detect(toupper(lvls),"T|F"))
      if(valueType == "integer" & test_vT_bool) valueType <- "boolean" 
      
    }
  })
  
  if(length(valueType) == 0) valueType <- "text"

  return(valueType)
}

#' @title
#' Self-adjust the valueType from a data dictionary or a dataset.
#'
#' @description
#' It is sometimes useful to take variable valueType’s from a dataset and 
#' attribute them to the associated data dictionary, or vice versa. 
#' [valueType_self_adjust()] takes the valueType guessed of the input and 
#' attributes it to itself. The parameter can be either a dataset or a 
#' data dictionary. Depending on the input provided, the valueType replaced is 
#' either in the 'valueType' column of the data dictionary or cast to a column 
#' in the dataset. The possible values of the valueTypes returned are 
#' 'date','datetime', 'boolean', 'integer', 'decimal', and text'.
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
#' # use madshapR_example provided by the package
#' 
#' ###### Example 1: The valueType of a dataset can be adjusted. each column is
#' # evaluated as whole, and the best valueType match found is applied. If 
#' # there is no better match found, the column is left as it is.
#' 
#' dataset <- madshapR_example$`dataset_example`
#' dataset <- valueType_self_adjust(dataset["gndr"])
#' head(dataset)
#' 
#' ###### Example 2: Aany data frame can be dataset by definition
#' dataset <- valueType_self_adjust(mtcars)
#' head(dataset)
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
    
    # preserve dataset
    preserve_attributes <- col_id(dataset)
    preserve_group <- group_vars(dataset)
    dataset <- as_dataset(ungroup(dataset))
    
    if(ncol(dataset) > 0 & nrow(dataset) > 0){
    
      # is_factor <-
      #   dataset %>%
      #   reframe(across(everything(), ~ toString(class(.)))) %>%
      #   pivot_longer(everything()) %>%
      #   rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
      #   dplyr::filter(.data$`value` %in% c("factor")) %>% ungroup
      
      data_dict <- data_dict_extract(dataset,as_data_dict_mlstr = TRUE)
      
      data_dict[['Categories']] <-
        bind_rows(
          Categories = tibble(name = as.character(),variable = as.character()),
          data_dict[['Categories']])
      
      vT <- 
        dataset %>%
        reframe(across(everything(),~ valueType_guess(.))) %>%
        pivot_longer(everything())
      
      # for(i in names(dataset)) {
      #   dataset[[i]] <-
      #     as_valueType(
      #       x = dataset[[i]],
      #       valueType = vT$value[vT$`name` == i])}
      # data_dict_final <- data_dict_extract(dataset,as_data_dict_mlstr = TRUE)
      
      data_dict[['Variables']][['valueType']] <- vT$value
      dataset <- data_dict_apply(dataset, data_dict)
      
    }
    
    dataset <- 
      dataset %>%
      group_by(pick(any_of(preserve_group))) %>% 
      as_dataset(col_id = preserve_attributes)
    
    return(dataset)
  }

  # is data_dict
  if(!is_dataset(...) & is_data_dict(...)){
    
    data_dict <- as_data_dict_shape(...)
    attributes(data_dict)$`madshapR::class` <- attributes(...)$`madshapR::class`
    
    if(nrow(data_dict[['Variables']]) == 0) return(data_dict)

    if(!has_categories(data_dict)){
      # warning("Your data dictionary contains no categorical variables.")
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
        reframe(valueType = paste0(.data$`valueType`,collapse = "|"))

      category_outcomes <-
        data_dict[['Categories']] %>%
        select(.data$`variable`,.data$`name`) %>%
        left_join(category_outcomes, by = "variable") %>%
        group_by(.data$`variable`) %>% group_split() %>%
        lapply(function(x){
          test_vT <- str_detect(x$valueType[1], "\\|")
          if(test_vT) x <-
              x %>% mutate(valueType = valueType_guess(unique(x$`name`)))
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

  message("The argument is neither a dataset nor a data dictionary.")
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
#' It is sometimes useful to take variable valueType’s from a dataset and 
#' attribute them to the associated data dictionary, or vice versa. 
#' [valueType_adjust()] takes the valueType of the input (from) and 
#' attributes it to the output (to). The parameters 'from' and 'to' can be 
#' either a dataset or a data dictionary. Depending on the input provided, 
#' the valueType replaced is either in the 'valueType' column of a 
#' data dictionary or cast to a column in a dataset. If 'to' is not provided, 
#' the function calls [valueType_self_adjust()] instead. The possible values 
#' of the valueTypes returned are date','datetime', 'boolean', 'integer', 
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
#' @param from Object to take attributes from. Can be either a dataset or a data 
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
#' library(dplyr)
#'  
#' # use madshapR_example provided by the package
#' dataset <- madshapR_example$`dataset_example`
#' data_dict <- as_data_dict_mlstr(madshapR_example$`data_dict_example`)
#' 
#' dataset <- valueType_adjust(from = data_dict,to = dataset)
#' head(dataset)
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
    
    is_data_dict <- 
      toString(attributes(data_dict)$`madshapR::class`) == 'data_dict'

    tryCatch({data_dict <-
      as_data_dict_mlstr(data_dict)},
      warning = function(cond){
        stop(call. = FALSE,cond)})
    
    # dataset must match
    if(suppressWarnings(check_dataset_variables(dataset, data_dict)) %>% 
       dplyr::filter(str_detect(.data$`condition`,"\\[ERROR\\]")) %>% nrow > 0){
      stop(call. = FALSE,
"Names across your data dictionary differ from names across the dataset.",
bold("\n\nUseful tip:"),
" Use dataset_evaluate(dataset, data_dict) for a full assessment of the dataset"
)}

    if(ncol(dataset) == 0) return(data_dict)
  
    # clean valueType, typeof, class
    vT_data_dict <-
      tibble(name = rep(names(dataset)),
             valueType = rep(NA_character_,ncol(dataset)))
      
    for(i in names(dataset)){
      # stop()}

      # category values in the data dictionary : check the data dictionary vT, 
      # the dataset vT, and the combination of the two of them (categories in 
      # the data dict but not in the dataset). 
      data_dict_cat_i <- data_dict$`Categories`[data_dict$`Categories`[['variable']] == i,'name']$`name`
      dataset_value_i <- unique(dataset[[i]])
      vT_dataset_i <- valueType_of(dataset_value_i)
      vec_i <- unique(c(as.character(data_dict_cat_i),as.character(dataset_value_i)))
      
      vec_i_rect <- silently_run(as_valueType(vec_i,vT_dataset_i))
      
      # if no problem, vT of the data dict <- vT of the dataset
      if(class(vec_i_rect)[[1]] != 'try-error') {
        vT_data_dict[vT_data_dict[["name"]] == i,][['valueType']] <- vT_dataset_i
      }else{
        
        # else, test if vT combined is integer, if yes, vT data dict <- vT integer
        vec_i_rect <- silently_run(as_valueType(vec_i,"integer"))
        if(class(vec_i_rect)[[1]] != 'try-error') {
          vT_data_dict[vT_data_dict[["name"]] == i,][['valueType']] <- "integer"
        }else{
          
          # else, vT data dict <- guess the vT 
          vT_data_dict[vT_data_dict[["name"]] == i,][['valueType']] <- valueType_guess(vec_i)
        }
        
        # once the vT of the data dict is set, vT dataset <- vT data dict. (go backward.)
        dataset[[i]] <- as_valueType(dataset[[i]],vT_data_dict[vT_data_dict[["name"]] == i,][['valueType']])
      }
    }
    
    # vT_list <- madshapR::valueType_list
    # vT_data_dict <-
    #   left_join(vT_data_dict,vT_list, by = "valueType") %>%
    #   select("name", valueType_data_dict = "valueType")

    # vT_dataset <-
    #   dataset %>%
    #   reframe(across(everything(), ~ valueType_of(.))) %>%
    #   pivot_longer(cols = everything()) %>%
    #   rename(valueType = "value") %>%
    #   left_join(vT_list, by = "valueType") %>%
    #   select("name", valueType_dataset = "valueType")

    # before correction
    # vT_final <-
    #   vT_data_dict %>%
    #   full_join(vT_dataset,by = join_by('name')) %>%
    #   mutate(valueType = ifelse(
    #     .data$`valueType_data_dict` == "integer",
    #     .data$`valueType_dataset`,
    #     .data$`valueType_data_dict`)) %>%
    #   mutate(typeof = ifelse(
    #     .data$`typeof_data_dict` == "integer",
    #     .data$`typeof_dataset`,
    #     .data$`typeof_data_dict`)) %>%
    #   select('name','valueType','typeof')
    #
    # data_dict[['Variables']]['typeof'] <-
    #   data_dict[['Variables']]['name'] %>%
    #   left_join(vT_final %>%
    #               select("name", "typeof"), by = "name") %>%
    #   select("typeof")
    
    # vT_final <-
    #   vT_data_dict %>%
    #   full_join(vT_dataset,by = join_by('name')) %>%
    #   mutate(valueType = .data$`valueType_data_dict`) %>%
    #   select('name','valueType')

    # data_dict[['Variables']]['typeof'] <-
    #   data_dict[['Variables']]['name'] %>%
    #   left_join(vT_final %>%
    #               select("name", "typeof"), by = "name") %>%
    #   select("typeof")
    # }
    # if(length(data_dict[['Variables']][['valueType']]) > 0){
    
    # vT_final <-
    #   vT_data_dict %>%
    #   full_join(vT_dataset,by = join_by('name')) %>%
    #   mutate(valueType = .data$`valueType_dataset`) %>%
    #   select('name','valueType')
    
    data_dict[['Variables']]['valueType'] <-
      data_dict[['Variables']]['name'] %>%
      left_join(vT_data_dict %>%
                  select("name", "valueType"), by = "name") %>%
      select("valueType")
    # }
    
    if(is_data_dict)
      data_dict <- as_data_dict(data_dict)

    return(data_dict)
    # }
  }

  if(is_data_dict(from) & is_dataset(to)){

    # test data_dict
    tryCatch({data_dict <-
      as_data_dict_mlstr(from)},
      warning = function(cond){
        stop(call. = FALSE,cond)})

    # preserve dataset
    as_dataset(dataset, col_id(dataset))
    preserve_attributes <- col_id(dataset)
    preserve_group <- group_vars(dataset)
    dataset <- as_dataset(ungroup(dataset))

    # dataset must match
    if(suppressWarnings(check_dataset_variables(dataset, data_dict)) %>% 
       dplyr::filter(str_detect(.data$`condition`,"\\[ERROR\\]")) %>% nrow > 0){
      stop(call. = FALSE,
"Names across your data dictionary differ from names across the dataset.",
bold("\n\nUseful tip:"),
" Use dataset_evaluate(dataset, data_dict) for a full assessment of the dataset"
)}

    if(ncol(dataset) > 0){
    
    # data_dict_data <-
    #   data_dict_extract(dataset) %>%
    #   as_data_dict_mlstr()

    # is_factor <-
    #   dataset %>%
    #   reframe(across(everything(), ~ toString(class(.)))) %>%
    #   pivot_longer(everything()) %>%
    #   rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
    #   dplyr::filter(.data$`value` %in% c("factor")) %>% ungroup 

    # data_dict_data[['Variables']] <-
    #   data_dict_data[['Variables']] %>%
    #   select(-"valueType") %>%
    #   left_join(data_dict[['Variables']] %>%
    #               select("name", "valueType"),by = "name")

      for(i in names(dataset)){
        dataset[[i]] <-
          as_valueType(
            x = dataset[[i]],
            valueType = data_dict[['Variables']][[
              which(data_dict[['Variables']]$`name` == i),
              'valueType']])}
      
      dataset <- data_dict_apply(dataset, data_dict)

    } 
    
    dataset <- 
      dataset %>%
      group_by(pick(any_of(preserve_group))) %>% 
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
#' # use madshapR_example provided by the package
#' dataset <- madshapR_example$`dataset_example`
#' valueType_of(dataset$dob)
#' valueType_guess(dataset$dob)
#' 
#' # any data frame can be a dataset by definition
#' valueType_guess(mtcars$cyl)
#' valueType_guess(mtcars$cyl)
#' 
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
valueType_guess <- function(x){

  # check if x is a column
  if(is.list(x) & sum(nrow(x)) <= 1)
    return(valueType_guess(x = x[[1]]))
  
  # check if the col is a vector
  if(is.list(x))
    stop(call. = FALSE,"'list' object cannot be coerced to valueType")
  
  # check if all is na
  if(all(is.na(x))) return(valueType_of(x))

  # else :
  x <- unique(x)
  x <- x[!is.na(x)]
  
  vT_list <- madshapR::valueType_list

  test_vT_integer  <- 
    silently_run(as_valueType(as.character(x),"integer"))
  
  test_vT_bool  <- all(str_detect(toupper(x),"T|F"))

  if(class(test_vT_integer)[[max(length(class(test_vT_integer)))]][1] == 'integer'){
    if(test_vT_bool) return('boolean') else return('integer')
  }
    
  test_vT_decimal  <- 
    silently_run(as_valueType(as.character.default(x),"decimal"))
    
  if(class(test_vT_decimal)[[1]] != 'try-error'){
    
    test_vT_date <- silently_run(as_valueType(x ,"date"))
    if(class(test_vT_date)[[1]] != 'try-error'){
      return('date')}
    
    test_vT_datetime <- silently_run(as_valueType(x ,"datetime"))
    if(class(test_vT_datetime)[[1]] != 'try-error'){
      return('datetime')}
    
    return('decimal')
  }
    
  test_vT_date <- silently_run(as_valueType(x ,"date"))
  if(class(test_vT_date)[[1]] != 'try-error'){
    return('date')}
  
  test_vT_datetime <- silently_run(as_valueType(x ,"datetime"))
  if(class(test_vT_datetime)[[1]] != 'try-error'){
    return('datetime')}
  
  return(valueType_of(x))

    
  # t1 = Sys.time()
  # # test_vT_boolean  <- 
  # #   silently_run(as_valueType(as.character.default(x),"boolean"))
  # # 
  # # test_vT_integer  <- 
  # #   silently_run(as_valueType(as.character.default(x),"integer"))
  #   
  # test_vT_decimal  <- 
  #   silently_run(as_valueType(as.character.default(x),"decimal"))
  #   
  # test_vT_date     <-
  #   silently_run(as_valueType(                     x ,"date"))
  #   
  # test_vT_datetime <-
  #   silently_run(as_valueType(                     x ,"datetime"))
  #   
  # test_vT_text     <-                    
  #   as_valueType(                                  x , "text")
  # 
  # t2 = Sys.time()

  # test_vT <-
  #   tribble(
  #     ~`valueType` ,~`class`                  ,
  #     # "boolean"    ,
  #     # class(test_vT_boolean)[[max(length(class(test_vT_boolean)))]][1],
  #     # 
  #     # "integer"    ,
  #     # class(test_vT_integer)[[max(length(class(test_vT_integer)))]][1],
  #     # 
  #     "decimal"    ,
  #     class(test_vT_decimal)[[max(length(class(test_vT_decimal)))]][1],
  # 
  #     "date"       ,
  #     class(test_vT_date)[[max(length(class(test_vT_date)))]][1],
  # 
  #     "datetime"   ,
  #     class(test_vT_datetime)[[max(length(class(test_vT_datetime)))]][1]
  # 
  #     ) %>%
  #   dplyr::filter(.data$`class` != "try-error") %>%
  #   reframe(
  #     valueType = paste0(.data$`valueType`,collapse = "|"),
  #     class = paste0(.data$`class`,collapse = "|")) %>%
  #   mutate(
  #     valueType =
  #       case_when(
  #         # .data$`valueType` == "boolean|integer"              ~ "boolean"      ,
  #         .data$`valueType` == "boolean|integer|decimal"      ~ "integer"      ,
  #         # .data$`valueType` == "integer|decimal"              ~ "integer"      ,
  #         .data$`valueType` == "integer|decimal|date"         ~ "date"         ,
  #         .data$`valueType` == "integer|decimal|datetime"     ~ "datetime"     ,
  #         .data$`valueType` == "decimal|date"                 ~ "date"         ,
  #         .data$`valueType` == "date|datetime"                ~ "date"         ,
  #         .data$`valueType` == "boolean|integer|decimal|date" ~ valueType_of(x),
  #         TRUE                                              ~  .data$`valueType`
  #       )) %>% pull(.data$`valueType`)
  # 
  # if(test_vT == "") test_vT <- 'text'
  # 
  # message(paste0(test_vT," ",t2-t1))
  # return(test_vT)
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
#' # use madshapR_example provided by the package
#' dataset <- madshapR_example$`dataset_example`
#' as_valueType(head(dataset$dob),'date')
#' 
#' # as_valueType is compatible with tidyverse syntax
#' library(dplyr)
#' dataset <- 
#'   tibble(mtcars) %>%
#'   mutate(cyl = as_valueType(cyl,'integer'))
#' 
#' head(dataset)
#' 
#'}
#'
#' @import dplyr tidyr fabR haven stringr
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
as_valueType <- function(x, valueType = 'text'){

  # check if x is a column
  if(is.list(x) & sum(nrow(x)) <= 1) 
    return(as_valueType(x = x[[1]], valueType))

  # check if the col is a vector
  if(is.list(x))
    stop(call. = FALSE,"'list' object cannot be coerced to valueType")

  class_x <- class(zap_labels(x))
  # class_x <- class(x)[[max(length(class(x)))]]
  x_init <- x
  
  # if x is already the output format, no need to go further
  if(str_detect(toString(class_x), c("^Date$"))    & valueType == "date")     return(x)
  if(str_detect(toString(class_x), c("POSIX"))     & valueType == "datetime") return(x)
  if(is.integer(x)                               & valueType == "integer")  return(x)
  if(str_detect(toString(class_x), c("^numeric$")) & valueType == "decimal")  return(x)
  if(is.logical(x)                               & valueType == "boolean")  return(x)
  if(is.na(valueType)                            | valueType == "text")     return(as.character.default(x))

  vT_list <- madshapR::valueType_list
  # check if valueType exists
  if(!valueType %in% vT_list$`valueType`) {
    stop(call. = FALSE,
"\nThe valueType provided does not exists. Please refer to documentation.",
bold("\n\nUseful tip:"),
" Use data_dict_evaluate(data_dict) to get a full assessment of your
data dictionary")}

  dataType <- vT_list[[which(vT_list['valueType'] == valueType),'call']]

  # if integer
  if(dataType     == "as_any_integer")  x <- 
    return(as_any_integer(as.character.default(x)))
  
  # if boolean
  if(dataType     == "as_any_boolean")  x <- 
    return(as_any_boolean(as.character.default(x)))

  # if date
  if(dataType     == "as_any_date"){

    # x <- as.character(x)
    # if(length(x) == 0) return(as.Date(x))
    
    date_format <-
      guess_date_format(
        tibble(x = as.character.default(
          sample(x[!is.na(x)], size = min(length(x[!is.na(x)]),20)))))

    if(date_format$`% values formated` == 100){
      
      return(as_any_date(as.character.default(x), date_format$`Date format`))
      
    }else{x <- NA}}
  
  # if none of the above
  
  # if datetime  
  if(dataType == "as.POSIXct") 
    x <- as.POSIXct(as.character(x))
  # if factor
  if(class(x)[1]  == "factor") 
    x <- as.character.default(x)
  
  x_to_test <- do.call(dataType, list(x)) %>% unlist

  condition <- tibble(to_test = x_to_test, original = x_init)

  if(length(x_to_test) == 0){
    return(x_to_test)}

  if(valueType %in% c("text","locale","point","linestring","polygon","binary")){
    return(x_to_test)}

  if(!all(is.na(condition$`to_test`) == is.na(condition$`original`))){
    test_condition <- FALSE
  }else{

    test_condition <-
      distinct(condition[which(!is.na(condition['original'])),])

    if(valueType %in% c("decimal")){
      test_condition <- 
        test_condition %>%
        mutate(across(everything(), ~ as.numeric(as.character.default(.)))) %>%
        mutate(test = .data$`to_test` == .data$`original`) %>%
        pull(.data$`test`) %>% all}

    # [GF] The test seems obsolete
    # if(valueType %in% c("date")){
    #   test_condition <-
    #     test_condition %>%
    #     mutate(across(
    #       "original",
    #       ~ as_any_date(as.character.default(.),date_format$`Date format`))) %>%
    #     mutate(
    #       test = toString(.data$`to_test`) == toString(.data$`original`)) %>%
    #     pull(.data$`test`) %>% all}
    
    # [GF] The test seems obsolete
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

  return(x_to_test)
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
#' # use madshapR_example provided by the package
#' taxonomy <- as_taxonomy(madshapR_example$`taxonomy_example`)
#' head(taxonomy)
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
This object is not a taxonomy as defined by the package, which must 
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
#' is_valueType('intereg')
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
#' # use madshapR_example provided by the package
#' is_taxonomy(madshapR_example$`taxonomy_example`)
#' is_taxonomy(madshapR_example$`dataset_example`)
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

#' @title
#' Convert typeof (and class if any) into its corresponding valueType
#'
#' @description
#' The function converts a given typeof string into its corresponding valueType 
#' representation. This function is particularly useful for mapping different 
#' data types to their equivalent value types in contexts such as data modeling 
#' and data dictionary creation. An optional class parameter allows for more 
#' specific conversions when necessary.
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
#' @param typeof A string representing the type to be converted. 
#' Supported values include "character", "integer", "double", "logical".
#' 
#' @param class An optional parameter that specifies a class context. 
#' If provided, the function may return a more refined value type based on the 
#' class type; if not, the function will return a general equivalent. 
#' Supported values include "character", "integer","numeric","logical","Date" 
#' and "POSIXct". NULL is the default.
#'
#' @returns
#' A character vector, named 'valueType'.
#'
#' @examples
#' {
#'
#' typeof_convert_to_valueType(typeof = "character")
#' typeof_convert_to_valueType(typeof = "double")
#' typeof_convert_to_valueType(typeof = "double", class = "Date")
#'
#'}
#'
#' @import dplyr stringr
#' @importFrom rlang .data
#'
#' @export
typeof_convert_to_valueType <- function(typeof, class = NA_character_){
  
  # check if params are a column
  if((is.list(typeof) & sum(nrow(typeof)) <= 1) & 
     (is.list(class) & sum(nrow(class)) <= 1))
    return(typeof_convert_to_valueType(typeof = typeof[[1]], class[[1]]))
  
  # check if the col is a vector
  if(is.list(typeof) | is.list(class))
    stop(call. = FALSE,"'list' object cannot be coerced to valueType")
  
  if(!is.character(typeof))
    stop(call. = FALSE,'`typeof` must be a character string.')
  
  if(!is.character(class))
      stop(call. = FALSE,'`class` must be a character string.')
  
  vT_list <- 
    madshapR::valueType_list %>%
    rowwise %>%
    mutate(class = paste0(.data$`class`, collapse = " _; ")) %>%
    mutate(class = na_if(.data$`class`,"NA")) 
  
  typeof_test <- 
    vT_list %>%
    dplyr::filter(.data$typeof == !! toString(typeof)) %>%
    select("typeof","class","toValueType") %>% distinct
  
  if(!is.na(class)){
    typeof_test <-
      typeof_test %>%
      dplyr::filter(.data$class == !! toString(class)) %>%
      select("typeof","class","toValueType") %>% distinct
    }
  
  if(nrow(typeof_test) == 1) return(c(valueType = typeof_test$toValueType))
  if(nrow(typeof_test) == 0) stop(call. = FALSE,
"
The provided typeof/class combination has no valueType equivalent. Please see
documentation or print(madshapR::valueType_list)")

  if(typeof == "double") return(c(valueType = "decimal"))
  
}

#' @title
#' Convert valueType into its corresponding typeof and class in R representation
#'
#' @description
#' The function converts a given valueType string into its corresponding typeof 
#' representation. This function is particularly useful for mapping different 
#' data types to their equivalent value types in contexts such as data modeling 
#' and data dictionary creation. The class is provided and allows for more 
#' specific conversions when necessary.
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
#' @param valueType  A character string of the valueType to convert.
#'
#' @returns
#' A character vector, named 'typeof' and 'class'.
#'
#' @examples
#' {
#'
#' valueType_convert_to_typeof(valueType = NA)
#' valueType_convert_to_typeof(valueType = "text")
#' valueType_convert_to_typeof(valueType = "date")
#' valueType_convert_to_typeof(valueType = "decimal")
#'
#'}
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
valueType_convert_to_typeof <- function(valueType){
  
  # check if valueType is a column
  if(is.list(valueType) & sum(nrow(valueType)) <= 1)
    return(valueType_convert_to_typeof(valueType = valueType[[1]]))
  
  # check if the col is a vector
  if(is.list(valueType))
    stop(call. = FALSE,"'list' object cannot be coerced to typeof/class")
  
  # check if all is na
  if(all(is.na(valueType))) return(c(typeof = "character",class = NA))
  
  # check if character
  if(!is.character(valueType))
    stop(call. = FALSE,'`typeof` must be a character string.')
  
  vT_list <- madshapR::valueType_list
  
  vT_test <- 
    vT_list %>%
    rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
    dplyr::filter(.data$valueType %in% !! toString(valueType)) %>% ungroup %>%
    select("typeof","class") %>% distinct
  
  if(nrow(vT_test) == 0) stop(call. = FALSE,
"
The provided valueType combination has no typeof/class equivalent. Please see
documentation or print(madshapR::valueType_list)")
  
  return(c(typeof = vT_test$typeof,class = vT_test$class))
}
