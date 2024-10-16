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
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#' @param dataset_name A character string specifying the name of the dataset 
#' (used internally in the function [dossier_evaluate()]).
#' @param .dataset_name `r lifecycle::badge("deprecated")`
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
#' eval_dataset <- dataset_evaluate(dataset, data_dict, is_data_dict_mlstr = TRUE)
#' glimpse(eval_dataset)
#' 
#' ###### Example 2: Any data frame can be a dataset by definition
#' eval_iris <- dataset_evaluate(iris)
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
    taxonomy = NULL,
    dataset_name = .dataset_name,
    .dataset_name = NULL){
  
  # future dev
  # add emptiness of the dataset in the Dataset assessment
  
  # fargs <- list()
  fargs <- as.list(match.call(expand.dots = TRUE))
  
  # check on arguments : dataset
  as_dataset(dataset) # no col_id
  
  if(!is.logical(is_data_dict_mlstr))
    stop(call. = FALSE,
         '`is_data_dict_mlstr` must be TRUE or FALSE (TRUE by default)')
  
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
  
  if(sum(nrow(data_dict[['Categories']])) > 0){
    
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      bind_rows(tibble(missing = as.character()))}

  preserve_attributes <- 
    col_id <- attributes(dataset)$`madshapR::col_id`
  
  if(is.null(col_id) | ncol(dataset) == 1){
    dataset <- dataset %>% add_index("madshapR::index")
    dataset <- as_dataset(dataset, names(dataset)[1])}
  
  col_id <- attributes(dataset)$`madshapR::col_id`
  # if(!is.null(preserve_attributes)) col_id <- preserve_attributes
  
  zap_dataset <- 
    dataset_zap_data_dict(dataset) %>% 
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
    test_empty_row <-
    test_empty_col <-
    test_unique_value <-
    test_existing_variable_category <-
    test_valueType <-
    tibble("Variable name" = as.character())
  
  if(is_data_dict_mlstr == TRUE){
    message(
      "    Assess the standard adequacy of naming")
    test_name_standards  <- 
      check_name_standards(names(zap_dataset))%>%
      rename("Variable name" = "name_var",
             "Dataset assessment" = "condition") %>%
      mutate(across(everything(),as.character))
  }
  
  message(
    "    Assess the presence of variable names both in dataset and data dictionary")
  test_matching_variable <-
    check_dataset_variables(dataset, data_dict) %>%
    dplyr::filter(.data$`name_var` != 'madshapR::index') %>%
    rename("Variable name" = "name_var",
           "Dataset assessment" = "condition") %>%
    mutate(across(everything(),as.character))
  
  
  message(
    "    Assess the presence of possible duplicated variable in the dataset")
  if(nrow(dataset) > 0 & ncol(dataset %>% select(-matches('madshapR::index'))) > 1) {
    test_duplicated_columns <-
      get_duplicated_cols(
        dataset %>% select(-matches('madshapR::index'))) %>%
      rename(name_var = "col_name") %>%
      mutate(
        value = str_squish(
          str_remove(.data$`condition`,'Possible duplicated columns:')),
        condition = "[INFO] - Possible duplicated variable.") %>%
      select('condition','name_var','value') %>%
      rename("Variable name" = "name_var",
             "Dataset assessment" = "condition",
             "Value" = "value") %>%
      mutate(across(everything(),as.character))
  }
  
  message(
    "    Assess the presence of duplicated participants in the dataset")
  if(nrow(dataset) > 0){                                                        #icitte
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
        select(-all_of('madshapR::value'))
    }
    
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
      rename("Variable name" = "name_var",
             "Dataset assessment" = "condition",
             "Value" = "value") %>%
      mutate(across(everything(),as.character))
  }
  
  if(nrow(dataset) > 0){
    message("    Assess the presence of unique value columns in dataset")
    test_unique_value <-
      get_unique_value_cols(zap_dataset) %>%
      mutate(condition = "[INFO] - Variable has a constant value.") %>%
      rename(`name_var` = "col_name")  %>%
      rename("Variable name" = "name_var",
             "Dataset assessment" = "condition",
             "Value" = "value") %>%
      mutate(across(everything(),as.character))
  }
  
  message(
    "    Assess the presence of empty rows in the dataset")
  test_empty_row <-
    get_all_na_rows(zap_dataset) %>%
    rename('value' = "row_number") %>%
    mutate(
      name_var = "(all)",
      condition =
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
    rename("Variable name" = "name_var",
           "Dataset assessment" = "condition",
           "Value" = "value") %>%
    mutate(across(everything(),as.character))
  
  message(
    "    Assess the presence all empty variable in the dataset")
  test_empty_col <-
    get_all_na_cols(dataset) %>%
    mutate(
      value = "(empty)",
      condition = "[INFO] - Empty variable.") %>%
    rename(`name_var` = "col_name") %>%
    rename("Variable name" = "name_var",
           "Dataset assessment" = "condition",
           "Value" = "value") %>%
    mutate(across(everything(),as.character))
  
  message(
    "    Assess the presence of categories not in the data dictionary")
  
  test_existing_variable_category <-
    silently_run({
      check_dataset_categories(dataset,data_dict) %>%
        dplyr::filter(!is.na(.data$`value`)) %>%
        distinct() %>% group_by(.data$`name_var`,.data$`condition`) %>%
        reframe(
          `value` = paste0(.data$`value`, collapse = " ; "))}) %>%
    dplyr::filter(!is.na(.data$`name_var`))
  
  test_existing_variable_category <-  
    test_existing_variable_category %>%
    add_index('madshapR::index') %>%
    separate_rows("value",sep = " ; ") %>%
    group_by(.data$`madshapR::index`) %>%
    add_index('madshapR::index2') %>%
    group_by(.data$`madshapR::index`,.data$`name_var`,.data$`condition`) %>%
    slice(1:6) %>%
    mutate(
      value =
        ifelse(.data$`madshapR::index2` == 6 , "[...]",.data$`value`)) %>%
    reframe(`value` = paste0(.data$`value`, collapse = " ; ")) %>%
    select(-"madshapR::index") %>%
    rename("Variable name" = "name_var",
           "Dataset assessment" = "condition",
           "Value" = "value") %>%
    mutate(across(everything(),as.character))
  
  if(is_data_dict_mlstr == TRUE){
    message(
      "    Assess the `valueType` comparison in dataset and data dictionary")
    test_valueType <-
      check_dataset_valueType(
        dataset = zap_dataset, 
        data_dict = data_dict['Variables'],valueType_guess = TRUE) %>%
      rename("Variable name" = "name_var",
             "Dataset assessment" = "condition",
             "Value" = "value",
             "Suggested valueType" = "suggestion") %>%
      mutate(across(everything(),as.character))
    
    
    # replace elements in the data dictionary assessment concerning the valueType
    
    if(!is.null(report[['Data dictionary assessment']])){
      
      report$`Data dictionary assessment` <- 
        report$`Data dictionary assessment` %>% 
        bind_rows(tibble("Column name" = as.character())) %>%
        dplyr::filter(!`Column name` %in% "valueType")
      
      if(sum(nrow(report[['Data dictionary assessment']])) == 0){

        report[['Data dictionary assessment']] <- NULL
      }  
    }
  }
  
  # test_name_standards
  # test_matching_variable
  # test_duplicated_columns
  # test_duplicated_rows
  # test_empty_row
  # test_empty_col
  # test_unique_value
  # test_existing_variable_category
  # test_valueType
  
  report$`Dataset assessment` <-
    test_name_standards %>%
    bind_rows(test_matching_variable) %>%
    bind_rows(test_duplicated_columns) %>%
    bind_rows(test_duplicated_rows) %>%
    bind_rows(test_empty_row) %>%
    bind_rows(test_empty_col) %>%
    bind_rows(test_unique_value) %>%
    bind_rows(test_existing_variable_category) %>%
    bind_rows(test_valueType) %>%
    select(
      'Variable name',
      "Dataset assessment" , 
      matches("Value"), 
      matches("Suggested valueType")) %>%
    mutate(across(everything(), ~ as.character(.))) %>%
    distinct() %>% tibble %>%
    left_join(
      report$`Data dictionary summary` %>%
        select("Index", "Variable name"),
      by = 'Variable name') %>% 
    select("Index", everything()) %>%
    arrange(.data$`Index`) %>%
    select(-"Index")
  
  
  if(all(is.na(report[['Dataset assessment']][['Suggested valueType']]))){
    report[['Dataset assessment']][['Suggested valueType']] <- NULL}
  
  message("    Generate report")
  
  if(nrow(report$`Dataset assessment`) == 0){
    message("\n    The dataset contains no error/warning.")
    report$`Dataset assessment` <- NULL
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
#' # use madshapR_example provided by the package
#' library(dplyr)
#'
#' ###### Example : a dataset list is a dossier by definition.
#'    
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
#' # use madshapR_example provided by the package
#' library(dplyr)
#' 
#' data_dict <- madshapR_example$`data_dict_example - errors`
#' eval_data_dict <- data_dict_evaluate(data_dict,is_data_dict_mlstr = TRUE)
#' 
#' glimpse(eval_data_dict)
#'
#' }
#'
#' @import dplyr tidyr stringr fabR
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
  
  # add label, valueType and missing if don't exist
  # if(is_data_dict_mlstr == TRUE){
    
    data_dict[['Variables']] <-
      data_dict[['Variables']] %>%
      bind_rows(tibble(valueType = as.character()))
  
    if(length(names(
      data_dict[['Variables']] %>%
      select(matches(c("^label$","^label:[[:alnum:]]"))))) == 0){
      
      data_dict[['Variables']] <-
        data_dict[['Variables']] %>%
        bind_rows(tibble(label = as.character()))
    }
    
    if(sum(nrow(data_dict[['Categories']])) > 0){
    
      data_dict[['Categories']] <-
        data_dict[['Categories']] %>%
        bind_rows(tibble(missing = as.character()))
      
      first_lab_var <- 
        data_dict[['Variables']] %>%
        select(matches(c("^label$","^label:[[:alnum:]]"))[1]) %>% names
      
      if(!first_lab_var %in% names(data_dict[['Categories']])){
        data_dict[['Categories']] <-
          data_dict[['Categories']] %>%
          bind_rows(tibble(label_temp = as.character())) %>%
          rename_with(.cols = "label_temp", .fn =  ~ first_lab_var)
      }
    }
  # }
  
  # check on arguments : taxonomy
  if(!is.null(taxonomy)) taxonomy <- as_taxonomy(taxonomy)
  
  message(
"- DATA DICTIONARY ASSESSMENT: ",bold(data_dict_name)," --------------")
  
  # creation of the structure of the report
  report <- list()
  
  first_lab_var <- 
    data_dict[['Variables']] %>%
    select(matches(c("^label$","^label:[[:alnum:]]"))[1]) %>% names
  

  missing_labels <- tibble(
    "Index" = as.integer(),
    "Variable name"  = as.character(), 
    "Category missing codes" = as.character())

  if(length(data_dict[["Categories"]] > 0)){

    missing_labels <-
      suppressWarnings(data_dict_collapse(
        data_dict %>% data_dict_filter(filter_cat = "missing %in% 'TRUE'")))
    
    missing_labels <- 
      missing_labels[['Variables']] %>%
      bind_rows(tibble("Index" = as.integer())) %>%
      bind_rows(tibble("Categories::missing" = as.character())) %>%
      select("index","name","Categories::missing") %>%
      mutate("Categories::missing" = 
               str_remove_all(.data$`Categories::missing`,"(\\ \\= TRUE)|(\\ _\\= TRUE)")) %>%
      select("Index" = "index",
             "Variable name" = "name",
             "Category missing codes" = "Categories::missing")
    }
  
  report$`Data dictionary summary` <-
    suppressWarnings(data_dict_collapse(data_dict))
  
  report$`Data dictionary summary` <-
    tibble(report$`Data dictionary summary`[['Variables']]) %>%
    bind_rows(tibble("Categories::label" = as.character()) %>% 
                rename_with(.cols = "Categories::label", .fn = ~ paste0("Categories::",first_lab_var))) %>%
    select(
      'Index' = 'index',
      'Variable name' = 'name',
      'Variable label' = !! first_lab_var,
      'Variable valueType' = 'valueType',
      "Category codes and labels" = paste0("Categories::", !!first_lab_var)) %>%
    left_join(missing_labels, by = c('Index', 'Variable name')) %>%
    mutate(Index = as.integer(replace_na(.data$`Index`,0)))
   
  # clean labels and missing
  # if(all(is.na(report$`Data dictionary summary`[["Category missing codes"]]))){
  # 
  #   report$`Data dictionary summary`[["Category missing codes"]] <- NULL
  #   
  #   }

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
    tibble("Variable name" = as.character())
  
  if(is_data_dict_mlstr == TRUE){
    
    # [GF - tested and validated]
    message("    Assess the standard adequacy of naming")
    test_name_standards  <-
      check_name_standards(data_dict[['Variables']][['name']]) %>%
      mutate(Sheet = "Variables", 'Column name' = "name") %>%
      bind_rows(
        if(sum(nrow(data_dict[['Categories']])) > 0 ){
          check_name_standards(data_dict[['Categories']][['variable']]) %>%
            mutate(Sheet = "Categories" , 'Column name' = "variable") 
        }else{tibble("name_var" = as.character())}) %>%
      rename("Variable name" = "name_var",
             "Data dictionary assessment" = "condition") %>%
      mutate(across(everything(),as.character))
  }
  
  # [GF - tested and validated]
  message("    Assess the uniqueness and presence of variable names")
  test_unique_variable <-
    check_data_dict_variables(data_dict) %>%
    mutate(Sheet    = "Variables", 'Column name' = "name") %>%
    rename("Variable name" = "name_var",
           "Data dictionary assessment" = "condition")%>%
    mutate(across(everything(),as.character))
    
  
  # [GF - tested and validated]
  message("    Assess the presence of possible duplicated columns")
  test_duplicated_columns <-
    suppressWarnings(get_duplicated_cols(
      data_dict[['Variables']] %>% select(-"index",-"name"))) %>%
    dplyr::filter(!is.na(.data$`condition`)) %>%
    mutate(Sheet     = "Variables") %>%
    mutate(across(everything(),as.character)) %>%
    
    bind_rows(
      if(sum(nrow(data_dict[['Categories']])) > 0 ){
        suppressWarnings(get_duplicated_cols(
          data_dict[['Categories']] %>% select(-"name"))) %>%
          dplyr::filter(!is.na(.data$`condition`)) %>%
          mutate(Sheet    = "Categories")
      }else{tibble()}) %>%
    mutate(Value     = str_squish(
             str_remove(.data$`condition`,"Possible duplicated columns:")),
           `Variable name` = "(all)",
           condition = "[INFO] - Possible duplicated columns.") %>%
    rename("Column name" = "col_name",
           "Data dictionary assessment" = "condition") %>%
    mutate(across(everything(),as.character))
  
  
  # [GF - tested and validated] (no need for Categories)
  message("    Assess the presence of duplicated rows")
  test_duplicated_rows <-
    data_dict[['Variables']] %>%
    select(-"name",-"index") %>%
    get_duplicated_rows() %>%
    mutate("Value" = paste0("Row numbers: ",.data$`row_number`)) %>%
    separate_rows("row_number",sep = " ; ") %>%
    mutate(index = as.integer(.data$`row_number`)) %>%
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
    rename("Variable name" = "name",
           "Data dictionary assessment" = "condition") %>%
    mutate(across(everything(),as.character))
  
  
  # [GF - tested and validated]
  message("    Assess the presence of empty rows in the data dictionary")
  test_empty_row <-
    data_dict[['Variables']] %>% 
    select(-"index",-"name") %>% 
    get_all_na_rows() %>%
    mutate(index = as.integer(.data$`row_number`)) %>%
    left_join(data_dict[['Variables']], by = "index") %>%
    select('condition','index','name') %>%
    mutate(
      name = paste0("Row number: ", .data$`index`), 
      Sheet    = "Variables",
      condition = "[INFO] - Empty row (except for variable name).") %>% 
    select(-"index") %>%
    rename("Variable name" = "name",
           "Data dictionary assessment" = "condition") %>%
    mutate(across(everything(),as.character))
  
  # [GF - tested and validated]
  message("    Assess the presence of empty column in the data dictionary")
  test_empty_col <-
    data_dict[['Variables']] %>% 
    get_all_na_cols() %>%
    mutate(Sheet    = "Variables",
           condition = "[INFO] - Empty column.") %>%
    bind_rows(
      if(sum(nrow(data_dict[['Categories']])) > 0 ){
        data_dict[['Categories']] %>%
          get_all_na_cols() %>%
          mutate(Sheet    = "Categories",
                 condition = "[INFO] - Empty column.")
      }else{tibble()}) %>%
    mutate("Variable name" = '(all)') %>%
    rename("Column name" = "col_name",
           "Data dictionary assessment" = "condition") %>%
    mutate(across(everything(),as.character))
  
  if(sum(nrow(data_dict[['Categories']])) > 0){
    
    # [GF - tested and validated]
    message("    Assess the presence of categories not in the data dictionary")
    test_existing_variable_category <-
      suppressWarnings(check_data_dict_categories(data_dict)) %>%
      mutate(
        Sheet    = "Categories") %>%
      rename("Variable name" = "name_var",
             "Column name" = "col_name",
             "Value" = "value",
             "Data dictionary assessment" = "condition") %>%
      mutate(across(everything(),as.character))
  }
  
  if(is_data_dict_mlstr == TRUE){
  
    # [GF - tested and validated]  
    message("    Assess the `valueType` column in 'Variables'")
    test_valueType <-
      check_data_dict_valueType(data_dict) %>%
      mutate(
        "Column name" = "valueType",
        Sheet    = "Variables") %>%
      bind_rows(tibble("suggestion" = as.character())) %>%
      rename("Variable name" = "name_var",
             "Value" = "value",
             "Data dictionary assessment" = "condition",
             "Suggested valueType" = "suggestion") %>%
      mutate(across(everything(),as.character))
    
    
    # [GF - tested and validated] 
    
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
        "Variable name"  = "name",
        "Column name"    = "col_name",
        "Data dictionary assessment" = "condition") %>%
      mutate(across(everything(),as.character))
    
    if(all(is.na(data_dict[['Variables']][[first_lab_var]]))){
      test_var_label <- 
        test_var_label %>% mutate("Variable name" = '(all)') %>% distinct}

    if(sum(nrow(data_dict[['Categories']])) > 0){
    
      # [GF - tested and validated]  
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
          "Variable name"  = "variable",
          "Column name"    = "col_name",
          "Data dictionary assessment" = "condition") %>%
        mutate(across(everything(),as.character))
      
      if(all(is.na(data_dict[['Variables']][[first_lab_var]]))){
        test_var_label <- 
          test_var_label %>% mutate("Variable name" = '(all)') %>% distinct}
      
      
      
      # [GF - tested and validated]  

      message("    Assess the logical values of missing column in Categories")
      test_missing_category <- 
        suppressWarnings(check_data_dict_missing_categories(data_dict)) %>%
        mutate(Sheet    = "Categories", "Column name" = "missing") %>%
        rename("Variable name" = "name_var","Value" = "value",
               "Data dictionary assessment" = "condition") %>%
        mutate(across(everything(),as.character))
      }
  }
  
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
    left_join(data_dict$Variables %>% select("Variable name" = "name","index"),
              relationship = "many-to-many",by = "Variable name") %>%
    mutate(index = as.integer(.data$`index`)) %>%
    arrange(.data$`index`,.data$`Variable name`) %>%
    select(
      "Variable name",
      matches("Column name"),
      matches("Data dictionary assessment"),
      matches("Sheet"),
      matches("Value"), 
      matches("Suggested valueType")) %>%
    mutate(across(everything(), ~ as.character(.))) %>%
    distinct() %>% tibble %>% 
    remove_empty("cols")
  
  message("    Generate report")
  
  if(nrow(report$`Data dictionary assessment`) == 0){
    message("\n    The data dictionary contains no error/warning.")
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


