#' @title
#' Generate an assessment report for a dataset
#'
#' @description
#' Assesses the content and structure of a dataset object and generates reports 
#' of the results. This function can be used to evaluate data structure, 
#' presence of specific fields, coherence across elements, and data dictionary 
#' formats.
#'
#' @details
#' A data dictionary contains the list of variables in a dataset and metadata 
#' about the variables and can be associated with a dataset. A data dictionary 
#' object is a list of data frame(s) named 'Variables' (required) and 
#' 'Categories' (if any). To be usable in any function, the data frame 
#' 'Variables' must contain at least the `name` column, with all unique and 
#' non-missing entries, and the data frame 'Categories' must contain at least 
#' the `variable` and `name` columns, with unique combination of 
#' `variable` and `name`. The function truncates each cell to a maximum of 
#' 10000 characters, to be readable and compatible with Excel.
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
#' A taxonomy is a classification schema that can be defined for variable 
#' attributes. A taxonomy is usually extracted from an 
#' [Opal environment](https://www.obiba.org/pages/products/opal/), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#' 
#' The object may be specifically formatted to be compatible with additional 
#' [Maelstrom Research software](https://maelstrom-research.org/page/software), 
#' in particular [Opal environments](https://www.obiba.org/pages/products/opal/).
#' 
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided.
#' @param is_data_dict_mlstr Whether the input data dictionary should be coerced 
#' with specific format restrictions for compatibility with other 
#' Maelstrom Research software. TRUE by default.
#' @param valueType_guess Whether the output should include a more accurate 
#' valueType that could be applied to the dataset. TRUE by default.
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#' @param dataset_name A character string specifying the name of the dataset 
#' (used internally in the function [dossier_evaluate()]).
#'
#' @seealso
#' [dossier_evaluate()]
#'
#' @returns
#' A list of data frames containing assessment reports.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' ###### Example 1: use madshapR_example provided by the package
#' dataset <- madshapR_example$`dataset_example - errors with data`
#' data_dict <- madshapR_example$`data_dict_example - errors with data`
#' 
#' eval_dataset <- dataset_evaluate(dataset, data_dict)
#' glimpse(eval_dataset)
#' 
#' ###### Example 2: Any data frame can be a dataset by definition
#' eval_iris <- dataset_evaluate(iris)
#' 
#' glimpse(eval_iris)
#' 
#' }
#'
#' @import dplyr tidyr stringr fabR
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
dataset_evaluate <- function(
    dataset,
    data_dict = NULL,
    is_data_dict_mlstr = TRUE,
    valueType_guess = TRUE,
    taxonomy = NULL,
    dataset_name = NULL){
  
  # future dev
  # add emptiness of the dataset in the Dataset assessment
  
  # fargs <- list()
  fargs <- as.list(match.call(expand.dots = TRUE))
  
  if(!is.logical(is_data_dict_mlstr))
    stop(call. = FALSE,
         '`is_data_dict_mlstr` must be TRUE or FALSE (TRUE by default)')
  if(!is.logical(valueType_guess))
    stop(call. = FALSE,
         '`valueType_guess` must be TRUE or FALSE (TRUE by default)')
  
  # check on arguments : dataset
  as_dataset(dataset) # no col_id
  dataset <- ungroup(dataset)
  
  # check on arguments : data_dict
  if(is.null(data_dict)){
    data_dict <-
      silently_run({data_dict_extract(
        dataset = dataset,
        as_data_dict_mlstr = TRUE)})
    
    if(class(data_dict)[1] == "try-error"){
      data_dict <- 
        silently_run({data_dict_extract(
          dataset = dataset,
          as_data_dict_mlstr = FALSE)})}
  }

  # check on arguments : data_dict. 
  as_data_dict_shape(data_dict)
  
  # every column as character. Suppression of Categories if not exists
  data_dict <- data_dict[c('Variables','Categories')]
  data_dict <-
    data_dict[!is.na(names(data_dict))] %>%
    lapply(function(x) x %>% mutate(across(everything(),as.character)))
  
  # Addition of an index for ordering assessment output
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    add_index(.force = TRUE)
  
  # add valueType and missing if don't exist
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    bind_rows(tibble(valueType = as.character()))
  
  if(has_categories(data_dict)){
    
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      bind_rows(tibble(missing = as.character()))}
  
  # extract col_id, or create it if not exists.
  col_id <- col_id(dataset)
  if(is.null(col_id) | ncol(dataset) == 1){
    dataset <- dataset %>% add_index("madshapR::index", .force = TRUE)
    dataset <- as_dataset(dataset, names(dataset)[1])}
  
  # extract again col_id
  col_id <- col_id(dataset)
  
  zap_dataset <- 
    dataset_zap_data_dict(dataset,zap_factor = TRUE) %>% 
    select(-all_of(col_id)) %>%
    mutate(across(where(is.character),tolower))
  
  dataset_name <- 
    ifelse(!is.null(dataset_name),dataset_name,
           make_name_list(
             as.character(fargs[['dataset']]),list_elem = list(NULL)))
  
  # check on argument : taxonomy
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  
  # creation of the structure of the report
  report <- 
    data_dict_evaluate(
      data_dict,is_data_dict_mlstr = is_data_dict_mlstr)
  
  message(
    "- DATASET ASSESSMENT: ",
    bold(dataset_name), if(nrow(dataset) == 0) " (empty dataset)",
    " --------------------------")
  
  test_name_standards <-
    test_matching_variable <-
    test_duplicated_columns <-
    test_duplicated_rows <-
    test_duplicated_identifier <-
    test_empty_row <-
    test_empty_col <-
    test_unique_value <-
    test_existing_variable_category <-
    test_valueType <-
    tibble(
      # "Variable name" = as.character(),
      "name_var" = as.character(),
      "Value" = as.character())
  
  if(is_data_dict_mlstr == TRUE){
    message(
      "    Assess the standard adequacy of naming")
    test_name_standards  <- 
      check_name_standards(names(zap_dataset)) %>%
      rename(
        # "Variable name" = "name_var",
        "name_var" = "name_var",
        "Dataset assessment" = "condition") %>%
      mutate(across(everything(),as.character))
  }
  
  message(
    "    Assess the presence of variable names both in dataset and data dictionary")
  test_matching_variable <-
    check_dataset_variables(dataset, data_dict) %>%
    dplyr::filter(.data$`name_var` != 'madshapR::index') %>%
    rename(
      # "Variable name" = "name_var",
      "name_var" = "name_var",
      "Dataset assessment" = "condition") %>%
    mutate(across(everything(),as.character))
  
  if(nrow(dataset) > 0 & ncol(dataset %>% select(-matches('madshapR::index'))) > 1) {
    
    message(
      "    Assess the presence of possible duplicated variable in the dataset")
    
    test_duplicated_columns <-
      get_duplicated_cols(
        dataset %>% select(-matches('madshapR::index'))) %>%
      rename(name_var = "col_name") %>%
      mutate(
        value = str_squish(
          str_remove(.data$`condition`,'Possible duplicated columns:')),
        condition = "[INFO] - Possible duplicated variable.") %>%
      select('condition','name_var','value') %>%
      rename(
        # "Variable name" = "name_var",
        "name_var" = "name_var",
        "Dataset assessment" = "condition",
        "Value" = "value") %>%
      mutate(across(everything(),as.character))
  }
  
  if(nrow(dataset) > 0){                                                        
    
    message(
      "    Assess the presence of possible duplicated participants")
    
    test_duplicated_rows <-
      get_duplicated_rows(zap_dataset) %>%
      rename(value = "row_number") %>%
      add_index('madshapR::index') %>%
      separate_rows("value",sep = " ; ") %>%
      group_by(.data$`madshapR::index`) %>%
      add_index('madshapR::index2') 
    
    if(col_id != "madshapR::index"){
      
      test_duplicated_rows <- 
        test_duplicated_rows %>%
        rename('madshapR::value' = 'value') %>%
        left_join(by = 'madshapR::value',
                  dataset %>% select(all_of(col_id)) %>%
                    add_index('madshapR::value') %>%
                    mutate(across(everything(), as.character))) %>%
        rename('value' = any_of(!!as.symbol('col_id'))) %>%
        select(-all_of('madshapR::value'))}
    
    test_duplicated_rows <-               
      test_duplicated_rows %>%
      group_by(.data$`madshapR::index`) %>%
      slice(1:6) %>%
      mutate(
        value = 
          ifelse(.data$`madshapR::index2` == 6 , "[...]",.data$`value`)) %>%
      reframe(`value` = paste0(.data$`value`, collapse = " ; ")) %>%
      mutate(condition = "[INFO] - Duplicated rows (excluding identifier).") %>%
      mutate(
        `name_var` = 
          ifelse(col_id == "madshapR::index",'(all)', !! col_id)) %>%
      select(-"madshapR::index") %>%
      rename(
        # "Variable name" = "name_var",
        "name_var" = "name_var",
        "Dataset assessment" = "condition",
        "Value" = "value") %>%
      mutate(across(everything(),as.character))
    
    if(col_id != "madshapR::index"){
      
      test_duplicated_identifier <- 
        get_duplicated_rows(dataset %>% select(all_of(col_id))) %>%
        rename(value = "row_number") %>%
        add_index('madshapR::index') %>%
        separate_rows("value",sep = " ; ") %>%
        group_by(.data$`madshapR::index`) %>%
        add_index('madshapR::index2') 
      
      test_duplicated_identifier <- 
        test_duplicated_identifier %>%
        rename('madshapR::value' = 'value') %>%
        left_join(by = 'madshapR::value',
                  dataset %>% select(all_of(col_id)) %>%
                    add_index('madshapR::value') %>%
                    mutate(across(everything(), as.character))) %>%
        rename('value' = any_of(!!as.symbol('col_id'))) %>%
        select(-all_of('madshapR::value'))
      
      test_duplicated_identifier <-               
        test_duplicated_identifier %>%
        group_by(.data$`madshapR::index`) %>%
        slice(1:6) %>%
        mutate(
          value = 
            ifelse(.data$`madshapR::index2` == 6 , "[...]",.data$`value`)) %>%
        reframe(`value` = paste0(.data$`value`, collapse = " ; ")) %>%
        mutate(condition = "[INFO] - Duplicated identifier values.") %>%
        mutate(
          `name_var` = 
            ifelse(col_id == "madshapR::index",'(all)', !! col_id)) %>%
        select(-"madshapR::index") %>%
        rename(
          # "Variable name" = "name_var",
          "name_var" = "name_var",
          "Dataset assessment" = "condition",
          "Value" = "value") %>%
        mutate(across(everything(),as.character))
      
    }
    
    distinct_information <- 
      bind_rows(distinct(test_duplicated_rows['Value']),
                distinct(test_duplicated_identifier['Value'])) %>%
      count(.data$`Value`) %>% pull("n") %>% unique
    
    if(length(distinct_information) == 1 & sum(distinct_information) == 2){
      test_duplicated_rows <- 
        test_duplicated_rows %>% 
        mutate(`Dataset assessment` = "[INFO] - Duplicated rows.")    
      
      test_duplicated_identifier <-   
        tibble(
          # "Variable name" = as.character(),
          "name_var" = as.character(),
          "Value" = as.character())
    }
    
    if(col_id == "madshapR::index"){
      test_duplicated_rows <- 
        test_duplicated_rows %>% 
        mutate(`Dataset assessment` = "[INFO] - Duplicated rows.")  
    }
  }
  
  if(nrow(dataset) > 0){
    message(
      "    Assess the presence of unique value columns in dataset")
    
    test_unique_value <-
      get_unique_value_cols(zap_dataset) %>%
      mutate(condition = "[INFO] - Variable has a constant value.") %>%
      rename(`name_var` = "col_name")  %>%
      rename(
        # "Variable name" = "name_var",
        "name_var" = "name_var",
        "Dataset assessment" = "condition",
        "Value" = "value") %>%
      mutate(across(everything(),as.character))
  }
  
  
  if(nrow(dataset) > 0){
    message(
      "    Assess the presence of empty rows in the dataset")
    test_empty_row <-
      get_all_na_rows(zap_dataset) %>%
      rename('value' = "row_number") %>%
      mutate(
        "name_var" = "(all)",
        "condition" =
          "[INFO] - Empty row (except for participant identifier variable).")
    
    if(col_id != "madshapR::index"){
      
      test_empty_row <- 
        test_empty_row %>%
        rename('madshapR::value' = 'value') %>%
        left_join(by = 'madshapR::value',
                  dataset %>% select(all_of(col_id)) %>% 
                    add_index('madshapR::value') %>%
                    mutate(across(everything(), as.character))) %>%
        rename('value' = !!as.symbol(col_id)) %>%
        select(-'madshapR::value')
    }
    
    test_empty_row <- 
      test_empty_row %>%
      rename(
        # "Variable name" = "name_var",
        "name_var" = "name_var",
        "Dataset assessment" = "condition",
        "Value" = "value") %>%
      mutate(across(everything(),as.character))
  }
  
  if(nrow(dataset) > 0){
    message(
      "    Assess the presence all empty variable in the dataset")
    test_empty_col <-
      get_all_na_cols(dataset) %>%
      mutate(
        value = "(empty)",
        condition = "[INFO] - Empty variable.") %>%
      rename(`name_var` = "col_name") %>%
      rename(
        # "Variable name" = "name_var",
        "name_var" = "name_var",
        "Dataset assessment" = "condition",
        "Value" = "value") %>%
      mutate(across(everything(),as.character))
  }
  
  
  if(nrow(dataset) > 0){
    message(
      "    Assess the Categories comparison in dataset and data dictionary")
    
    test_existing_variable_category <-
      silently_run({
        check_dataset_categories(dataset,data_dict) %>%
          dplyr::filter(!is.na(.data$`value`)) %>%
          distinct()}) %>%
      dplyr::filter(!is.na(.data$`name_var`)) %>%
      rename(
        # "Variable name" = "name_var",
        "name_var" = "name_var",
        "Dataset assessment" = "condition",
        "Value" = "value") %>%
      mutate(across(everything(),as.character))
  }
  
  if(is_data_dict_mlstr == TRUE){
    
    if(nrow(dataset) > 0){
      message(
        "    Assess the `valueType` comparison in dataset and data dictionary")
      test_valueType <-
        check_dataset_valueType(
          dataset = zap_dataset, 
          data_dict = data_dict['Variables'],
          valueType_guess = valueType_guess) %>%
        rename(
          # "Variable name" = "name_var",
          "name_var" = "name_var",
          "Dataset assessment" = "condition",
          "Value" = "value",
          "Suggested valueType" = "suggestion") %>%
        mutate(across(everything(),as.character))}
    
    # replace elements in the data dictionary assessment concerning the valueType
    
    if(!is.null(report[['Data dictionary assessment']])){
      
      report$`Data dictionary assessment` <- 
        report$`Data dictionary assessment` %>% 
        bind_rows(tibble("Column name" = as.character())) %>%
        rowwise() %>%                # [GF] NOTE : rowwise
        dplyr::filter(!.data$`Column name` %in% "valueType") %>% ungroup
      
      if(sum(nrow(report[['Data dictionary assessment']])) == 0){
        
        report[['Data dictionary assessment']] <- NULL
      }  
    }
  }
  
  # test_name_standards
  # test_matching_variable
  # test_duplicated_columns
  # test_duplicated_rows
  # test_duplicated_identifier
  # test_empty_row
  # test_empty_col
  # test_unique_value
  # test_existing_variable_category
  # test_valueType
  
  data_dict_labels <- data_dict_trim_labels(data_dict,.keep_columns = TRUE)
  
  report$`Dataset assessment` <-
    test_name_standards %>%
    bind_rows(test_matching_variable) %>%
    bind_rows(test_duplicated_columns) %>%
    bind_rows(test_duplicated_rows) %>%
    bind_rows(test_duplicated_identifier) %>%
    bind_rows(test_empty_row) %>%
    bind_rows(test_empty_col) %>%
    bind_rows(test_unique_value) %>%
    bind_rows(test_existing_variable_category) %>%
    bind_rows(test_valueType) %>% 
    left_join(
      data_dict_labels$`Variables` %>% select("name_var" = "name","index","Variable name"),
      relationship = "many-to-many",
      by = "name_var") %>%
    mutate(index = as.integer(.data$`index`)) %>%
    rowwise %>%
    mutate("Variable name" = ifelse(is.na(.data$`Variable name`),.data$`name_var`,.data$`Variable name`)) %>%
    ungroup %>%
    mutate(index = as.integer(.data$`index`)) %>%
    select(
      'Index' = "index",
      'Variable name',
      "Dataset assessment" , 
      matches("Value"), 
      matches("Suggested valueType")) %>%
    distinct() %>% tibble %>%
    arrange(.data$`Index`) %>%
    mutate(across(everything(), ~ as.character(.)))

  if(nrow(dataset) == 0){
    message("\n    The dataset has 0 rows.")
    report$`Dataset assessment` <-
      tibble(
        'Variable name' = '(all)',
        'Dataset assessment' = "[INFO] - The dataset has 0 rows.")
  }
  
  if(all(is.na(report[['Dataset assessment']][['Suggested valueType']]))){
    report[['Dataset assessment']][['Suggested valueType']] <- NULL}
  
  message("    Generate report")
  
  if(nrow(report$`Dataset assessment`) == 0){
    message("\n    The dataset contains no errors/warnings.")
    report$`Dataset assessment` <-
      tibble(
        'Variable name' = '(all)',
        'Dataset assessment' = "[INFO] - The dataset contains no errors/warnings.")
  }
  
  message(bold(
    "
  - WARNING MESSAGES (if any): -------------------------------------------------
    "
  ))
  
  report <-   
    report %>%
    lapply(function(y){
      y %>%
        lapply(function(x) str_trunc(x, 10000)) %>%
        as_tibble()
    })
  
  return(report)
}

#' @title
#' Generate an assessment report of a dossier
#'
#' @description
#' Assesses the content and structure of a dossier object (list of datasets) 
#' and generates reports of the results. This function can be used to evaluate 
#' data structure, presence of specific fields, coherence across elements, and 
#' data dictionary formats.
#'
#' @details
#' A dossier is a named list containing at least one data frame or more, 
#' each of them being datasets. The name of each data frame will be use as the 
#' reference name of the dataset.
#' 
#' A taxonomy is a classification schema that can be defined for variable 
#' attributes. A taxonomy is usually extracted from an 
#' [Opal environment](https://www.obiba.org/pages/products/opal/), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#'
#' The object may be specifically formatted to be compatible with additional 
#' [Maelstrom Research software](https://maelstrom-research.org/page/software), 
#' in particular [Opal environments](https://www.obiba.org/pages/products/opal/).
#'
#' @param dossier List of data frame, each of them being datasets.
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#' @param is_data_dict_mlstr Whether the input data dictionary should be coerced 
#' with specific format restrictions for compatibility with other 
#' Maelstrom Research software. TRUE by default.
#'
#' @returns
#' A list of data frames containing assessment reports.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#'
#' # use madshapR_example provided by the package
#' dataset1 <- as_dataset(madshapR_example$`dataset_example`)
#' dataset2 <- as_dataset(madshapR_example$`dataset_example - error`,col_id = "part_id")
#' dossier <- dossier_create(list(dataset1,dataset2))
#' 
#' eval_dossier <- dossier_evaluate(dossier,is_data_dict_mlstr = TRUE)
#' glimpse(eval_dossier)
#'
#' }
#'
#' @import dplyr stringr tidyr
#' @importFrom crayon bold
#' @importFrom rlang .data
#' @export
dossier_evaluate <- function(
    dossier, taxonomy = NULL, is_data_dict_mlstr = TRUE){
  
  # amelioration :rajouter taxonomy
  
  # check on arguments
  as_dossier(dossier)
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  if(!is.logical(is_data_dict_mlstr))
    stop(call. = FALSE,
         '`is_data_dict_mlstr` must be TRUE or FALSE (TRUE by default)')
  
  report_list <-
    vector(mode = "list", length = length(names(dossier)))
  names(report_list) <- names(dossier)
  
  message(bold(
    "- DOSSIER ASSESSMENT: ----------------------------------------------------"
  ))
  
  for(i in seq_len(length(dossier))){
    # stop()}
    report_list[[i]] <-
      dataset_evaluate(
        dataset = dossier[[i]],
        taxonomy = taxonomy,
        dataset_name = names(dossier[i]),
        is_data_dict_mlstr = is_data_dict_mlstr)
  }
  
  return(report_list)
}

#' @title
#' Generate an assessment report for a data dictionary
#'
#' @description
#' Assesses the content and structure of a data dictionary and generates reports 
#' of the results. The report can be used to help assess data dictionary 
#' structure, presence of fields, coherence across elements, and taxonomy 
#' or data dictionary formats.
#'
#' @details
#' A data dictionary contains the list of variables in a dataset and metadata 
#' about the variables and can be associated with a dataset. A data dictionary 
#' object is a list of data frame(s) named 'Variables' (required) and 
#' 'Categories' (if any). To be usable in any function, the data frame 
#' 'Variables' must contain at least the `name` column, with all unique and 
#' non-missing entries, and the data frame 'Categories' must contain at least 
#' the `variable` and `name` columns, with unique combination of 
#' `variable` and `name`. The function truncates each cell to a maximum of 
#' 10000 characters, to be readable and compatible with Excel.
#' 
#' A taxonomy is a classification schema that can be defined for variable 
#' attributes. A taxonomy is usually extracted from an 
#' [Opal environment](https://www.obiba.org/pages/products/opal/), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#'
#' The object may be specifically formatted to be compatible with additional 
#' [Maelstrom Research software](https://maelstrom-research.org/page/software), 
#' in particular [Opal environments](https://www.obiba.org/pages/products/opal/).
#'
#' @param data_dict A list of data frame(s) representing metadata to be evaluated.
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#' @param is_data_dict_mlstr Whether the input data dictionary should be coerced 
#' with specific format restrictions for compatibility with other 
#' Maelstrom Research software. TRUE by default.
#'
#' @returns
#' A list of data frames containing assessment reports.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' # use madshapR_example provided by the package
#' data_dict <- madshapR_example$`data_dict_example - errors`
#' eval_data_dict <- data_dict_evaluate(data_dict,is_data_dict_mlstr = TRUE)
#' 
#' glimpse(eval_data_dict)
#'
#' }
#'
#' @import dplyr tidyr stringr fabR janitor
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
data_dict_evaluate <- function(
    data_dict,
    taxonomy = NULL,
    is_data_dict_mlstr = TRUE){
  
  fargs <- as.list(match.call(expand.dots = TRUE))
  
  data_dict_name <-
    silently_run(make_name_list(args_list = fargs['data_dict'], list(NULL)))
  
  # check args
  if(!is.logical(is_data_dict_mlstr))
    stop(call. = FALSE,
         '`is_data_dict_mlstr` must be TRUE or FALSE (TRUE by default)')
  
  # check on arguments : data_dict. 
  as_data_dict_shape(data_dict)
  
  # every column as character. Suppression of Categories if not exists
  data_dict <- data_dict[c('Variables','Categories')]
  data_dict <-
    data_dict[!is.na(names(data_dict))] %>%
    lapply(function(x) x %>% mutate(across(everything(),as.character)))
  
  # Addition of an index for ordering assessment output
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    add_index(.force = TRUE)
  
  # add valueType if don't exist
  if(is_data_dict_mlstr == TRUE){

    data_dict[['Variables']] <-
      data_dict[['Variables']] %>%
      bind_rows(tibble(valueType = as.character()))

  }
  
  first_lab_var <- first_label_get(data_dict)[['Variables']]
  data_dict_labels <- data_dict_trim_labels(data_dict)
  
  if(nchar(first_lab_var) == 0){
    
    data_dict[['Variables']] <-
      data_dict[['Variables']] %>%
      bind_rows(tibble(label = as.character()))
      first_lab_var <- first_label_get(data_dict)[['Variables']]
  }
  
  if(has_categories(data_dict)){
    
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      bind_rows(tibble(missing = as.character()))
    
    if(!first_lab_var %in% names(data_dict[['Categories']])){
      data_dict[['Categories']] <-
        data_dict[['Categories']] %>%
        bind_rows(tibble(label_temp = as.character())) %>%
        rename_with(.cols = "label_temp", .fn =  ~ first_lab_var)
    }
  }
  
  # check on arguments : taxonomy
  if(!is.null(taxonomy)) taxonomy <- as_taxonomy(taxonomy)
  
  message(
    "- DATA DICTIONARY ASSESSMENT: ",bold(data_dict_name)," --------------")
  
  # creation of the structure of the report
  report <- list()

  test_name_standards <-
    test_unique_variable <-
    test_duplicated_columns <-
    test_duplicated_rows <-
    test_empty_row <-
    test_empty_col <-
    test_existing_variable_category <-
    test_var_label <-
    test_valueType <-
    test_cat_label <-
    test_missing_category <-
    tibble("name_var" = as.character())
  
  # [GF] - to correct

  # suppress labels
  data_dict <- 
    data_dict %>% lapply(function(x) x %>% select(
      -any_of(c('Variable name','Variable label')),
      -starts_with(c('Categories in data dictionary','Non-valid categories'))))
  
  if(is_data_dict_mlstr == TRUE){
    
    message("    Assess the standard adequacy of naming")
    test_name_standards  <-
      check_name_standards(data_dict[['Variables']][['name']]) %>%
      mutate(Sheet = "Variables", 'Column name' = "name") %>%
      bind_rows(
        if(has_categories(data_dict) ){
          check_name_standards(data_dict[['Categories']][['variable']]) %>%
            mutate(Sheet = "Categories" , 'Column name' = "variable") 
        }else{tibble("name_var" = as.character())}) %>%
      rename(
        # "Variable name" = "name_var",
        "name_var" = "name_var",
        "Data dictionary assessment" = "condition") %>%
      mutate(across(everything(),as.character)) %>%
      group_by(pick("name_var")) %>% slice(1) %>% ungroup
  }
  
  # 
  message("    Assess the uniqueness and presence of variable names")
  test_unique_variable <-
    check_data_dict_variables(data_dict) %>%
    mutate(Sheet    = "Variables", 'Column name' = "name") %>%
    rename(
      # "Variable name" = "name_var",
      "name_var" = "name_var",
      "Data dictionary assessment" = "condition")%>%
    mutate(across(everything(),as.character))
  
  
  # 
  message("    Assess the presence of possible duplicated columns")
  test_duplicated_columns <-
    suppressWarnings(get_duplicated_cols(
      data_dict[['Variables']] %>% select(-"index",-"name"))) %>%
    dplyr::filter(!is.na(.data$`condition`)) %>%
    mutate(Sheet     = "Variables") %>%
    mutate(across(everything(),as.character)) %>%
    
    bind_rows(
      if(has_categories(data_dict) ){
        suppressWarnings(get_duplicated_cols(
          data_dict[['Categories']] %>% select(-"name"))) %>%
          dplyr::filter(!is.na(.data$`condition`)) %>%
          mutate(Sheet    = "Categories")
      }else{tibble()}) %>%
    mutate(
      "Value"     = str_squish(
      str_remove(.data$`condition`,"Possible duplicated columns:")),
      # `Variable name` = "(all)",
      "name_var" = '(all)',
      "condition" = "[INFO] - Possible duplicated columns.") %>%
    rename(
      "Column name" = "col_name",
      "Data dictionary assessment" = "condition") %>%
    mutate(across(everything(),as.character))
  
  
  #  
  message("    Assess the presence of duplicated rows")
  test_duplicated_rows <-
    data_dict[['Variables']] %>%
    select(-"name",-"index") %>%
    get_duplicated_rows() %>%
    mutate("Value" = paste0("Row numbers: ",.data$`row_number`)) %>%
    separate_rows("row_number",sep = " ; ") %>%
    mutate("index" = as.integer(.data$`row_number`)) %>%
    left_join(data_dict[['Variables']], by = "index") %>%
    select("name", "row_number", "Value") %>%
    group_by(.data$`Value`) %>%
    fill("name",.direction = "down") %>%
    select("name","Value") %>%
    mutate(
      "Column name" = "(all)",
      "condition" = "[INFO] - Possible duplicated rows (except for variable name).",
      "Sheet"    = "Variables") %>%
    distinct() %>%
    rename(
      # "Variable name" = "name",
      "name_var" = "name",
      "Data dictionary assessment" = "condition") %>%
    mutate(across(everything(),as.character))
  
  
  # 
  message("    Assess the presence of empty rows in the data dictionary")
  test_empty_row <-
    data_dict[['Variables']] %>% 
    select(-"index",-"name") %>% 
    get_all_na_rows() %>%
    mutate("index" = as.integer(.data$`row_number`)) %>%
    left_join(data_dict[['Variables']], by = "index") %>%
    select('condition','index','name') %>%
    mutate(
      name = paste0("Row number: ", .data$`index`), 
      Sheet    = "Variables",
      condition = "[INFO] - Empty row (except for variable name).") %>% 
    select(-"index") %>%
    rename(
      # "Variable name" = "name",
      "name_var" = "name",
      "Data dictionary assessment" = "condition") %>%
    mutate(across(everything(),as.character))
  
  # 
  message("    Assess the presence of empty column in the data dictionary")
  test_empty_col <-
    data_dict[['Variables']] %>% 
    get_all_na_cols() %>%
    mutate(Sheet    = "Variables",
           condition = "[INFO] - Empty column.") %>%
    bind_rows(
      if(has_categories(data_dict) ){
        data_dict[['Categories']] %>%
          get_all_na_cols() %>%
          mutate("Sheet"    = "Categories",
                 "condition" = "[INFO] - Empty column.")
      }else{tibble()}) %>%
    mutate(
      # "Variable name" = '(all)',
      "name_var" = '(all)',
      ) %>%
    rename(
      "Column name" = "col_name",
      "Data dictionary assessment" = "condition") %>%
    mutate(across(everything(),as.character))
  
  if(has_categories(data_dict)){
    
    # 
    message("    Assess the presence of categories not in the data dictionary")
    test_existing_variable_category <-
      suppressWarnings(check_data_dict_categories(data_dict)) %>%
      mutate(
        Sheet    = "Categories") %>%
      rename(
        # "Variable name" = "name_var",
        "name_var" = "name_var",
        "Column name" = "col_name",
        "Value" = "value",
        "Data dictionary assessment" = "condition") %>%
      mutate(across(everything(),as.character))
  }
  
  if(is_data_dict_mlstr == TRUE){
    
    #   
    message("    Assess the `valueType` column in 'Variables'")
    test_valueType <-
      check_data_dict_valueType(data_dict) %>%
      mutate(
        "Column name" = "valueType",
        "Sheet"    = "Variables") %>%
      bind_rows(tibble("suggestion" = as.character())) %>%
      rename(
        # "Variable name" = "name_var",
        "name_var" = "name_var",
        "Value" = "value",
        "Data dictionary assessment" = "condition",
        "Suggested valueType" = "suggestion") %>%
      mutate(across(everything(),as.character))
    
    
    #  
    
    message(
      "    Assess the completion of `",first_lab_var,"` column in 'Variables'")
    test_var_label <-
      data_dict[['Variables']] %>%
      select(
        "name", "label" = !! first_lab_var) %>%
      pivot_longer(
        cols = !"name",
        names_to = "col_name",
        values_to = "value") %>%
      dplyr::filter(is.na(.data$`value`)) %>%
      select(-"value") %>%
      mutate(
        condition =
          paste0(
            "[ERROR] - Column `",!! first_lab_var,"` contains empty values.")) %>%
      mutate("Sheet"       = "Variables",
             "col_name" = !! first_lab_var,) %>%
      rename(
        # "Variable name"  = "name",
        "name_var" = "name",
        "Column name"    = "col_name",
        "Data dictionary assessment" = "condition") %>%
      mutate(across(everything(),as.character))
    
    if(all(is.na(data_dict[['Variables']][[first_lab_var]]))){
      test_var_label <- 
        test_var_label %>% mutate(
          # "Variable name" = '(all)',
          "name_var" = '(all)',
          ) %>% distinct}
    
    if(has_categories(data_dict)){
      
      #   
      message(
        "    Assess presence and completion of `",first_lab_var,"` column in 'Categories'")
      
      test_cat_label <-
        data_dict[['Categories']] %>% 
        select(
          "variable", "label" = !! first_lab_var) %>%
        pivot_longer(
          cols = !"variable",
          names_to = "col_name",
          values_to = "value") %>%
        dplyr::filter(is.na(.data$`value`)) %>%
        select(-"value") %>%
        mutate(
          condition =
            paste0(
              "[ERROR] - Column `",!! first_lab_var,"` contains empty values.")) %>%
        mutate("Sheet"       = "Categories",
               "col_name" = !! first_lab_var,) %>%
        rename(
          # "Variable name"  = "variable",
          "name_var" = "variable",
          "Column name"    = "col_name",
          "Data dictionary assessment" = "condition") %>%
        mutate(across(everything(),as.character))
      
      if(all(is.na(data_dict[['Variables']][[first_lab_var]]))){
        test_var_label <- 
          test_var_label %>% mutate(
            "Variable name" = '(all)',
            "name_var" = '(all)',
            ) %>% distinct}
      
      
      message("    Assess the logical values of missing column in Categories")
      test_missing_category <- 
        suppressWarnings(check_data_dict_missing_categories(data_dict)) %>%
        mutate(Sheet    = "Categories", "Column name" = "missing") %>%
        rename(
          # "Variable name" = "name_var",
          "name_var" = "name_var",
          "Value" = "value",
          "Data dictionary assessment" = "condition") %>%
        mutate(across(everything(),as.character))
    }
  }
  
  
  # catch first label (after addtion of categorical grouping variable)
  # data_dict_labels <- data_dict_trim_labels(data_dict)
  
  report$`Data dictionary summary` <- 
    data_dict_labels[['Variables']] %>%
    select(
      'Index' = 'index',
      'Variable name', 
      'Variable label',
      'Data dictionary valueType' = 'valueType',
      'Categories in data dictionary' =     'Categories in data dictionary long',
      'Non-valid categories' =     'Non-valid categories long') %>%
    mutate(Index = as.integer(replace_na(.data$`Index`,0)))
  
  # test_var_label
  # test_valueType
  # test_name_standards
  # test_unique_variable
  # test_existing_variable_category
  # test_cat_label
  # test_missing_category
  # test_duplicated_columns
  # # test_duplicated_rows
  # test_empty_row
  # test_empty_col

  report$`Data dictionary assessment` <-
    test_name_standards %>%
    bind_rows(test_unique_variable) %>%
    bind_rows(test_duplicated_columns) %>%
    bind_rows(test_duplicated_rows) %>%
    bind_rows(test_empty_row) %>%
    bind_rows(test_empty_col) %>%
    bind_rows(test_existing_variable_category) %>%
    bind_rows(test_var_label) %>%
    bind_rows(test_valueType) %>%
    bind_rows(test_cat_label) %>%
    bind_rows(test_missing_category) %>%
    #anchor
    left_join(
      data_dict_labels$`Variables` %>% select("name_var" = "name","index","Variable name"),
              relationship = "many-to-many",
              by = "name_var") %>%
    rowwise %>%
    mutate("Variable name" = ifelse(is.na(.data$`Variable name`),.data$`name_var`,.data$`Variable name`)) %>%
    ungroup %>%
    mutate(index = as.integer(.data$`index`)) %>%
    # arrange elements
    mutate(
      index2 = case_when( 
        str_detect(.data$`Variable name`,"Row number:") ~ -3,
        .data$`Variable name` == "(all)"                ~ -2,
        .data$`Variable name` == "(empty)"              ~ -1,
        TRUE                                            ~ 0)) %>%
    arrange(.data$`index`,.data$`index2`,.data$`Variable name`,desc(.data$`Sheet`)) %>%
    select(
      "Index" = "index",
      "Variable name",
      matches("Column name"),
      matches("Data dictionary assessment"),
      matches("Sheet"),
      matches("Value"), 
      matches("Suggested valueType")) %>%
    mutate(across(everything(), ~ as.character(.))) %>%
    distinct() %>% tibble %>%
    mutate('Index' = as.integer(.data$`Index`)) %>%
    arrange(.data$`Index`)
  
  message("    Generate report")
  
  if(nrow(report$`Data dictionary assessment`) == 0){
    message("\n    The data dictionary contains no errors/warnings.")
    report$`Data dictionary assessment` <- NULL
  }
  
  message(bold(
    "
  - WARNING MESSAGES (if any): --------------------------------------------\n"))
  
  report <-   
    report %>%
    lapply(function(y){
      y %>%
        lapply(function(x) str_trunc(x, 10000)) %>%
        as_tibble()      
    })
  
  return(report)
}


