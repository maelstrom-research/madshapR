#' @title
#' Assess a data dictionary for potential issues in variables
#'
#' @description
#' Generates a data frame report of any non-unique variable names in the 
#' 'Variables' element.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param data_dict A list of data frame(s) representing metadata to be evaluated.
#'
#' @returns
#' A data frame providing non unique variables across a data dictionary.
#'
#' @examples
#' {
#' 
#' # use madshapR_example provided by the package
#'
#' data_dict <- madshapR_example$`data_dict_example - errors`
#' check_data_dict_variables(data_dict)
#'
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
check_data_dict_variables <- function(data_dict){
  
  # test if enough data_dict
  as_data_dict_shape(data_dict)
  
  var_names <-
    data_dict[['Variables']] %>%
    add_count(.data$`name`) %>%
    dplyr::filter(.data$`n` > 1) %>%
    select("name")
  
  test <-
    var_names %>%
    mutate(
      condition = "[ERROR] - Duplicated variable name.") %>%
    select(name_var = "name", "condition") %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct
  
  var_NA <-
    data_dict[['Variables']] %>%
    add_index(.force = TRUE) %>%
    dplyr::filter(is.na(.data$`name`) | .data$`name` == "") %>%
    mutate(name = paste0("Row number: ",.data$`index`)) %>%
    select(.data$`name`)
  
  test <-
    test %>% bind_rows(
      var_NA %>%
        mutate(
          condition = "[ERROR] - Missing variable name.") %>%
        select(name_var = .data$`name`, .data$`condition`) %>%
        mutate(across(everything(), ~as.character(.)))) %>%
    dplyr::filter(!is.na(.data$`name_var`)) %>%
    distinct
  
  return(test)
}


#' @title
#' Assess a data dictionary for potential issues in categories
#'
#' @description
#' Generates a data frame report of any categorical variable name present in the
#' 'Categories' element but not present in 'Variables'. The data frame also 
#' reports any non-unique combinations of 'variable' and 'name' in the 
#' 'Categories' element.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param data_dict A list of data frame(s) representing metadata to be evaluated.
#'
#' @returns 
#' A data frame providing categorical variables that has issues within a 
#' data dictionary.
#'
#' @examples
#' {
#' 
#' # use madshapR_example provided by the package
#'
#' data_dict <- madshapR_example$`data_dict_example - errors`
#' check_data_dict_categories(data_dict)
#'
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
check_data_dict_categories <- function(data_dict){
  
  # test if enough data_dict
  as_data_dict_shape(data_dict)
  
  test <- test_cat_presence <- test_cat_unique <-
    tibble(
      name_var = as.character(),
      col_name = as.character(),
      value = as.character(),
      condition = as.character())
  
  if(sum(nrow(data_dict[['Categories']])) == 0){
    warning("You data dictionary contains no categorical variables")
    return(test)}
  
  var_names <-
    data_dict[['Variables']] %>%
    mutate(name_var = as.character(.data$`name`)) %>%
    select("name_var") %>% distinct %>%
    dplyr::filter(!is.na(.data$`name_var`))
  
  cat_names <-
    data_dict[['Categories']] %>%
    mutate(across(everything(), as.character)) %>%
    mutate(name_var = as.character(.data$`variable`)) %>%
    add_index(.force = TRUE) %>%
    mutate(index = paste0("Row number: ",.data$`index`)) %>%
    select(value = "name","name_var","index") %>%
    arrange(.data$`name_var`) %>%
    distinct
  
  test_cat_presence <-
    anti_join(cat_names,var_names, by = "name_var") %>%
    bind_rows(tibble(
      value = NA_character_,name_var = NA_character_,index = NA_character_,col_name = NA_character_)) %>%
    rowwise() %>%
    mutate(
      
      # if the variable name in categories has not been found in the variable list,
      # or is empty
      condition = ifelse(is.na(.data$`name_var`),
"[ERROR] - Category 'variable' name is empty.",
"[ERROR] - Category 'variable' name has no corresponding variable in 'Variables' sheet."
      ),
      col_name = "variable",
      name_var = ifelse(is.na(.data$`name_var`),"(empty)",.data$`name_var`),

      # if the value code (name) is empty 
      condition = ifelse(is.na(.data$`value`),
"[ERROR] - Category 'name' is empty.", .data$`condition`),
      col_name  = ifelse(is.na(.data$`value`), "name",.data$`col_name`),
      value = replace_na(.data$`value`,.data$`index`)) %>%

      # value = na_if(.data$`value`,"value"),
      # value = ifelse(is.na(.data$`name_var`),.data$`index`,.data$`value`)) %>%
    ungroup() %>%
    dplyr::filter(!is.na(.data$`index`)) %>%
    group_by(.data$`name_var`,.data$`col_name`) %>%
    add_index("group_index") %>%
    mutate(value = ifelse(.data$`group_index` == 1,.data$`index`,.data$`value`)) %>%
    dplyr::filter(.data$`value` == .data$`index`) %>%
    select("name_var", "condition", "value","col_name") %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct
  
  cat_names_count <-
    data_dict[['Categories']] %>%
    select(name_var = "variable","name") %>%
    mutate(across(everything(), as.character)) %>%
    group_by(.data$`name_var`,.data$`name`) %>% add_count() %>% ungroup %>%
    dplyr::filter(.data$`n` > 1) %>% distinct %>%
    mutate(col_name = 'name')
  
  test_cat_unique <-
    cat_names_count %>%
    mutate(
      condition = "[ERROR] - Duplicated category 'name'.")%>%
    select("name_var", value = "name", "condition","col_name") %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct
  
  test <- bind_rows(test, test_cat_presence, test_cat_unique)
  
  return(test)
  
}

#' @title
#' Assess categorical variables for non-Boolean values in 'missing' column
#'
#' @description
#' Generates a data frame report of any categorical variables with non-Boolean 
#' (or compatible with boolean) values in the 'missing' column of the 
#' 'Categories' element.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param data_dict A list of data frame(s) representing metadata to be evaluated.
#'
#' @returns 
#' A data frame providing categorical values which 'missing' column is not a 
#' boolean.
#'
#' @examples
#' {
#' 
#' # use madshapR_example provided by the package
#'
#' data_dict <- madshapR_example$`data_dict_example - errors`
#' check_data_dict_missing_categories(data_dict)
#'
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
check_data_dict_missing_categories <- function(data_dict){
  
  # test if enough data_dict
  as_data_dict_shape(data_dict)
  
  test <- tibble(
    name_var = as.character(), 
    value = as.character(), 
    condition = as.character())
  
  if(is.null(data_dict[['Categories']][['missing']])){
    warning(
      "You data dictionary contains no missing column in you categorical variables")
    return(test)}
  
  missing_val <-
    data_dict[['Categories']] %>%
    select(name_var = .data$`variable`,.data$`missing`)
  
  missing_val_test <-
    missing_val %>%
    select(-.data$`name_var`) %>%
    distinct() %>%
    rowwise() %>%
    mutate(
      value =
        class(silently_run(as_any_boolean(.data$`missing`)))[1]) %>%
    dplyr::filter(.data$`value` == "try-error") %>%
    inner_join(missing_val,by = "missing")
  
  test <-
    missing_val_test %>%
    select(.data$`name_var`,value = .data$`missing`) %>%
    mutate(
      condition = "[ERROR] - Value in 'missing' column is non-Boolean.")%>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct()
  
  return(test)
}

#' @title
#' Assess a data dictionary for non-valid taxonomy values
#'
#' @description
#' Generates a data frame report of any variable with a taxonomy value that is 
#' not in the list of allowed values for a given taxonomy.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' A taxonomy is a classification schema that can be defined for variable 
#' attributes. A taxonomy is usually extracted from an 
#' [Opal environment](https://www.obiba.org/pages/products/opal/), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#'
#' @param data_dict A list of data frame(s) representing metadata to be evaluated.
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#'
#' @returns
#' A data frame providing non-standard taxonomy declared in a data dictionary.
#'
#' @examples
#' {
#' 
#' # use madshapR_example provided by the package
#'
#' taxonomy <- madshapR_example$`taxonomy_example`
#' data_dict <- madshapR_example$`data_dict_example`
#' check_data_dict_taxonomy(data_dict, taxonomy)
#'
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @noRd
check_data_dict_taxonomy <- function(data_dict, taxonomy){
  
  # check_taxo_one_col function(col, taxonomy, na_allowed = TRUE){
  #
  #   if(na_allowed) col <- col[!is.na(col[2]),]
  #
  #   value <- names(col)[2]
  #   col <-
  #     col %>%
  #     rename_with(.cols = 2, ~ "value") %>%
  #     group_by(.data$`value`) %>%
  #     reframe(name_var = paste0(.data$`name_var`,collapse = " ; "))
  #
  #   taxo <-
  #     taxonomy %>% mutate(taxonomy = paste0(.data$`taxonomy`,"::",
  # .data$`vocabulary`)) %>%
  #     dplyr::filter(.data$`taxonomy` == !! value) %>%
  #     select(value = .data$`term`)
  #
  #   test <-
  #     anti_join(col,taxo,by = join_by(value)) %>%
  #     separate_rows(.data$`name_var`, sep = " ; ") %>%
  #     mutate(
  #       condition = paste0("[ERROR] - '",!! value,"' is not valid."))
  #
  #   return(test)
  # }
  
  # data_dict <- data_dict_list$`data_dict_example - errors`
  
  # only works for mlstr_taxo
  
  as_data_dict_shape(data_dict)
  as_taxonomy(taxonomy)
  
  test <-
    tibble(
      name_var = as.character(),
      value = as.character(),
      condition = as.character())
  
  # data_dict_elem <-
  #   data_dict[['Variables']] %>%
  #   select(.data$`name`,starts_with(unique(taxonomy$taxonomy))) %>%
  #   full_join(tibble(
  #    `Mlstr_additional::Source` = as.character(),
  #    `Mlstr_additional::Target` = as.character(),
  #    `Mlstr_area::1` = as.character(),`Mlstr_area::1.term` = as.character(),
  #    `Mlstr_area::1.scale` = as.character(),
  #    `Mlstr_area::2` = as.character(),`Mlstr_area::2.term` = as.character(),
  #    `Mlstr_area::3` = as.character(),`Mlstr_area::3.term` = as.character()),
  #             by = join_by(
  #               'Mlstr_additional::Source','Mlstr_additional::Target',
  #               'Mlstr_area::1', 'Mlstr_area::1.term','Mlstr_area::1.scale',
  #               'Mlstr_area::2', 'Mlstr_area::2.term',
  #               'Mlstr_area::3', 'Mlstr_area::3.term')) %>%
  #   add_index(.force = TRUE) %>%
  #   mutate(name = ifelse(is.na(.data$`name`),paste0("line:",.data$`index`),
  #   .data$`name`)) %>%
  #   select(-.data$`index`) %>%
  #   mutate(across(everything(), as.character))
  #
  # # Mlstr_additional::Source and Mlstr_additional::Target are mandatory
  #
  # if(is.null(data_dict_elem[['Mlstr_additional::Target']])){
  #   test <- test %>% add_row(
  #     name_var = 'Mlstr_additional::Target',
  #     condition = "[ERROR] - 'Mlstr_additional::Target' column is missing.")}
  #
  #   for(i in 2:ncol(data_dict_elem)){
  #     # stop()}
  #     test <-
  #       bind_rows(test, check_taxo_one_col(col = data_dict_elem[c(1,i)],
  #       taxonomy, na_allowed = FALSE))
  #
  #   }
  #
  #
  #
  #   test <- test %>% add_row(
  #     name_var = 'Mlstr_additional::Source',
  #     condition = "[ERROR] - 'Mlstr_additional::Source' column is missing.")
  #
  #   if(is.null(data_dict_elem[['Mlstr_additional::Target']])){
  #     test <- test %>% add_row(
  #       name_var = 'Mlstr_additional::Target',
  #       condition = "[ERROR] - 'Mlstr_additional::Target' column is missing.")}
  #
  #   # area::1 and area::1.term are mandatory
  #   if(is.null(data_dict_elem[['Mlstr_area::1']])){
  #     test <- test %>% add_row(
  #       name_var = 'Mlstr_area::1',
  #       condition = "[ERROR] - 'Mlstr_area::1' column is missing.")}
  #
  #   if(is.null(data_dict_elem[['Mlstr_area::1.term']])){
  #     test <- test %>% add_row(
  #       name_var = 'Mlstr_area::1.term',
  #       condition = "[ERROR] - 'Mlstr_area::1.term' column is missing.")}
  #
  #
  #
  #
  #   # if area::2 then area::2.term and !area::1
  #   if(!is.null(data_dict_elem[['Mlstr_area::1']]) &
  #   !is.null(data_dict_elem[['Mlstr_area::1.term']])){
  #     test <- test %>% add_row(
  #       name_var = 'Mlstr_area::1.term',
  #       condition = "[ERROR] - 'Mlstr_area::1.term' column is missing.")}
  #
  #
  #
  #   # if area::3 then area::3.term and !area::2 and ! area::1
  #
  #   # if Mlstr_area::1.scale
  #
  #
  # # }
  #
  #
  # # if taxonomy_mlstr
  #
  # # if area::1 and area::1.term are mandatory
  #
  # # if area::2 then area::2.term and !area::1
  #
  # # if area::3 then area::3.term and !area::2 and ! area::1
  #
  #
  #
  # if(is.null(data_dict[['Variables']][['valueType']])){
  #   warning("Your data dictionary contains no valueType column")
  #   return(test)}
  #
  # vT_list <- madshapR::valueType_list
  # test_valueType_names <-
  #   data_dict[['Variables']] %>%
  #   dplyr::filter(! .data$`valueType` %in% vT_list$`valueType`) %>%
  #   select(name_var = .data$`name`,value = .data$`valueType`) %>%
  #   mutate(
  # condition = "[ERROR] - valueType is not an accepted type (see ??valueType_list for complete list).") %>%
  #   mutate(across(everything(), ~as.character(.))) %>%
  #   distinct()
  #
  # if(length(data_dict[['Categories']]) > 0){
  #
  #   vT_text <-
  #     vT_list %>%
  #     dplyr::filter(.data$`toValueType` == 'text') %>% pull(.data$`valueType`)
  #
  #   data_dict_vt <-
  #     data_dict[['Variables']] %>%
  #     select(name_var = .data$`name`,.data$`valueType`) %>%
  #     dplyr::filter(! .data$`valueType` %in% vT_text)
  #
  #   vT_names <-
  #     data_dict[['Categories']] %>%
  #     select(name_var = .data$`variable`,.data$`name`) %>%
  #     inner_join(data_dict_vt,by = "name_var")
  #
  #   test_valueType_cat <-
  #     vT_names %>%
  #     select(-.data$`name_var`) %>%
  #     distinct() %>%
  #     rowwise() %>%
  #     mutate(
  #       test =
  #       class(try((as_valueType(.data$`name`,.data$`valueType`))))[1]) %>%
  #     dplyr::filter(.data$`test` == "try-error") %>%
  #     inner_join(vT_names,by = c("name", "valueType")) %>%
  #     select(.data$`name_var`, .data$`valueType`) %>%
  #     distinct
  #
  #
  #   test_valueType_cat <-
  #     test_valueType_cat %>%
  #     full_join(
  #       suppressWarnings(
  #         valueType_self_adjust(
  #           data_dict_filter(
  #             data_dict,
  #             paste0("name %in% c('",
  #             paste0(unique(
  #             test_valueType_cat$name_var),collapse = "','"),"')")
  #           ))[['Variables']]) %>%
  #         select(name_var = .data$`name`, suggestion = .data$`valueType`),
  #       by = "name_var")
  #
  #   test_valueType_cat <-
  #     test_valueType_cat %>%
  #     mutate(
  #       condition = "[ERROR] - valueType is not compatible with variable categories.") %>%
  #     select(.data$`name_var`, value = .data$`valueType`, .data$`condition`,
  #     .data$`suggestion`) %>%
  #     mutate(across(everything(), ~as.character(.))) %>%
  #     distinct
  # }
  #
  # test <- bind_rows(test_valueType_names, test_valueType_cat)
  
  return(test)
}


#' @title
#' Assess a data dictionary for non-valid valueType values
#'
#' @description
#' Generates a data frame report of any variable with a valueType that is not in 
#' the list of allowed valueType values. This function also assesses if the 
#' valueType is compatible with any associated categorical values declared.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param data_dict A list of data frame(s) representing metadata to be evaluated.
#'
#' @returns 
#' A data frame providing non-standard valueType declared in a data dictionary.
#'
#' @examples
#' {
#' 
#' # use madshapR_example provided by the package
#'
#' data_dict <- madshapR_example$`data_dict_example - errors with data`
#' check_data_dict_valueType(data_dict)
#'
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#' @importFrom stats na.omit
#'
#' @export
check_data_dict_valueType <- function(data_dict){
  
  # test if enough data_dict
  as_data_dict_shape(data_dict)
  
  test <- 
    test_valueType_suggested <- 
    tibble(
      index = as.character(),
      name_var = as.character(),
      suggestion = as.character(),
      condition2 = as.character(),
      value2 = as.character())
  
  if(is.null(data_dict[['Variables']][['valueType']]))
    data_dict[['Variables']][['valueType']] <- NA_character_    
  
  vT_list <- madshapR::valueType_list[!is.na(madshapR::valueType_list[['valueType']]),]
  var_index <- data_dict[['Variables']]['name'] %>%
    rename (name_var = "name") %>% add_index('index')
  
  test_valueType_names <-
    data_dict[['Variables']] %>%
    dplyr::filter(! .data$`valueType` %in% vT_list$`valueType`) %>%
    select(name_var = "name",value = "valueType") %>%
    mutate(
      condition = ifelse(is.na(.data$`value`),
"[INFO] - valueType is missing (see ??valueType_list for complete list).",
"[ERROR] - valueType is not an accepted type (see ??valueType_list for complete list).")) %>%
    inner_join(var_index, by = "name_var") %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct()
  
  if(length(data_dict[['Categories']]) > 0){
    
    vT_categorical <-
      data_dict[['Categories']] %>%
      select(name_var = "variable", "name") %>%
      left_join(test_valueType_names,by = "name_var") %>%
      select(-"index") %>%
      inner_join(var_index, by = "name_var") %>%
      dplyr::filter(!.data$`name_var` %in% test_valueType_names$`name_var` |
                    is.na(.data$`value`)) %>%
      inner_join(
        data_dict[['Variables']] %>%
          select(name_var = 'name','valueType'),by = "name_var")
    
    # for these, find the best match
    test_valueType_suggested <- 
      vT_categorical %>%
      group_by(across(any_of(c("name_var","index")))) %>%
      reframe(
        valueType = .data$`valueType`,
        name = paste0(.data$`name`,collapse = '|')) %>%
      distinct() %>%
      group_by(across(any_of(c("valueType", 'name',"index")))) %>%
      reframe(
        name_var = paste0(.data$`name_var`,collapse = '|')) %>%
      distinct() %>%
      separate_longer_delim(cols = 'name',delim = '|') %>%
      group_by(across(any_of(c("name_var","index")))) %>%
      reframe(
        valueType = .data$`valueType`,
        test = class(silently_run(as_valueType(.data$`name`,.data$`valueType`[[1]])))[1],
        suggestion = valueType_guess(.data$`name`)) %>%
      separate_longer_delim(cols = 'name_var',delim = '|') %>%
      distinct %>%
      dplyr::filter(!.data$`valueType` %in% .data$`suggestion`) %>%
      mutate(
        valueType = replace_na(.data$`valueType`,"(empty)"),
        condition = case_when(
          .data$`test` == 'try-error'  ~ "[ERROR] - valueType is not compatible with variable categories.",
          TRUE                         ~ "[INFO] - Suggested valueType.")) %>%
      select("index", 'name_var', 'value2' = 'valueType', "condition2" = 'condition','suggestion') %>%
      mutate(across(everything(), ~ as.character(.)))
    
  }
  
  test <- 
    full_join(
      test_valueType_names, 
      test_valueType_suggested, join_by("index","name_var")) %>%
    mutate(
      condition = ifelse(!is.na(.data$`condition`),.data$`condition`,.data$`condition2`),
      value = ifelse(!is.na(.data$`value2`),.data$`value2`,.data$`value`),
      suggestion = replace_na(.data$`suggestion`,"text")) %>% 
    select(-c("condition2","value2","index")) %>%
    mutate(across(everything(), ~ as.character(.)))
    
  return(test)
}

#' @title
#' Assess a data dictionary and associated dataset for undeclared variables
#'
#' @description
#' Generates a data frame report of any variable that is present in a dataset 
#' but not in the associated data dictionary or present in a data dictionary but 
#' not in the associated dataset.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata to be evaluated.
#'
#' @returns
#' A data frame providing undeclared variables across a data dictionary.
#'
#' @examples
#' {
#' 
#' # use madshapR_example provided by the package
#'
#' dataset <- madshapR_example$`dataset_example - errors`
#' data_dict <- madshapR_example$`data_dict_example - errors`
#' check_dataset_variables(dataset,data_dict)
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
check_dataset_variables <- function(dataset, data_dict = NULL){
  
  if(is.null(data_dict)) data_dict <- data_dict_extract(dataset)
  
  # test if enough data_dict or dataset
  as_data_dict_shape(data_dict)
  as_dataset(dataset) # no col_id
  
  test_cat <- tibble(name_var = as.character(), condition = as.character())
  
  var_names_in_data_dict <-
    data_dict[['Variables']] %>%
    select(name_var = "name") %>%
    mutate(data_dict = "data_dict") %>% 
    filter(!is.na(.data$`name_var`)) %>% 
    distinct
  
  var_names_in_dataset <-
    dataset %>% names %>% as_tibble() %>%
    rename(name_var = "value") %>%
    mutate(dataset = "dataset")
  
  test <-
    full_join(var_names_in_dataset,var_names_in_data_dict,by = "name_var") %>%
    mutate(condition = case_when(
      is.na(data_dict) ~ "[ERROR] - Variable present in dataset but not in data dictionary.",
      is.na(dataset)   ~ "[ERROR] - Variable present in data dictionary but not in dataset.",
      TRUE ~ NA_character_)) %>%
    mutate(across(everything(), ~as.character(.))) %>%
    dplyr::filter(!is.na(.data$`condition`)) %>%
    select("name_var", "condition") %>%
    distinct()
  
  return(test)
}

#' @title
#' Assess a data dictionary and associated dataset for category differences
#'
#' @description
#' Generates a data frame report of any categorical value options (the 
#' combination of 'variable' and 'name' in 'Categories') in a data dictionary 
#' that are not in the associated dataset and any categorical variable values 
#' in a dataset that are not declared in the associated data dictionary.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata to be evaluated.
#'
#' @returns
#' A data frame providing categorical values which differ between dataset and
#' their data dictionary.
#'
#' @examples
#' {
#' 
#' # use madshapR_example provided by the package
#' library(tidyr)
#' 
#' data_dict <- madshapR_example$`data_dict_example - errors with data`
#' dataset <- madshapR_example$`dataset_example - errors with data`
#' 
#' check_dataset_categories(dataset['gndr'], data_dict)
#' 
#' }
#'
#' @import dplyr tidyr fabR 
#' @importFrom rlang .data
#'
#' @export
check_dataset_categories <- function(
    dataset, 
    data_dict = NULL){
  
  
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
  
  # test if enough data_dict or dataset
  as_dataset(dataset) # no col_id
  as_data_dict_shape(data_dict)
  
  ##  test_categories ##
  # category in dd but not in dataset WARNING
  # category in dataset but not in dd ERROR
  
  # apply as_category declared in the data_dict to the variables in dataset
  categorical_var_dataset <- dataset %>% select(where(is_category)) %>% names
  
  categorical_var_data_dict <- c()
  if(sum(nrow(data_dict[['Categories']])) > 0){
    categorical_var_data_dict <- unique(data_dict$`Categorie`$`variable`)
  }else{
    data_dict[['Categories']] <- 
      tibble("variable" = as.character(), "name" = as.character())
  }
  
  categorical_variables <- unique(c(categorical_var_data_dict,categorical_var_dataset))
  
  # exclude already adressed all_na
  categorical_variables <-
    categorical_variables[
      categorical_variables %in% names(dataset %>% remove_empty('cols'))]
  
  test <- tibble("name_var" = as.character(),
                 "value"     = as.character(),
                 "condition" = as.character())
  if(length(categorical_variables) == 0){
    return(test)
  }else{
    
    for(i in categorical_variables){
      # stop()}
      
      dd_cat <- data_dict$`Categories`[data_dict$`Categories`$`variable` == i,]$`name`
      if(is.factor(dataset[[i]])) 
        ds_cat <- as.character(unique(dataset[!is.na(dataset[[i]]),i])) else
          ds_cat <- as.character(unique(dataset[!is.na(dataset[[i]]),i])[[1]])
        
        cat_in_dd_only <- as.character(dd_cat[!dd_cat %in% ds_cat])
        cat_in_ds_only <- as.character(ds_cat[!ds_cat %in% dd_cat])
        
        if(length(cat_in_dd_only) > 0){
          test <-
            test %>%
            bind_rows(
              tibble(
                name_var  = i,
                value     = cat_in_dd_only,
                condition =
                  "[INFO] - Variable is categorical in data dictionary but not in dataset."))} # GF Question
        
        if(length(cat_in_ds_only) > 0){
          test <-
            test %>%
            bind_rows(
              tibble(
                name_var  = i,
                value     = cat_in_ds_only,
                condition =
                  "[INFO] - Variable is categorical in dataset but not in data dictionary."))} # GF Question
    }
  }
  
  return(test)
}

#' @title
#' Assess a data dictionary and associated dataset for valueType differences
#'
#' @description
#' Generates a data frame report of any incompatibility between variable values 
#' in a dataset and the declared valueType in the associated data dictionary.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata to be evaluated.
#' @param valueType_guess Whether the output should include a more accurate 
#' valueType that could be applied to the dataset. FALSE by default.
#'
#' @returns
#' A data frame providing values which valueType differs between dataset and
#' their data dictionary.
#'
#' @examples
#' {
#'
#' data_dict <- madshapR_example$`data_dict_example - errors with data`
#' dataset <- madshapR_example$`dataset_example - errors with data`
#' 
#' check_dataset_valueType(dataset, data_dict, valueType_guess = TRUE)
#' check_dataset_valueType(dataset, data_dict, valueType_guess = FALSE)
#'
#' }
#'
#' @import dplyr tidyr stringr fabR
#' @importFrom rlang .data
#'
#' @export
check_dataset_valueType <- function(
    dataset, 
    data_dict = NULL,
    valueType_guess = FALSE){
  
  # test if enough data_dict or dataset
  as_dataset(dataset) # no col_id
  as_data_dict_shape(data_dict)

  if(!is.logical(valueType_guess))
    stop(call. = FALSE,
         '`valueType_guess` must be TRUE of FALSE (FALSE by default)')

  data_dict_init <- data_dict
  data_dict_unique_name <-
    make.unique(replace_na(data_dict[['Variables']]$`name`,"NA"))
  
  data_dict[['Variables']]$`name` <- data_dict_unique_name
  
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    mutate(across(everything(),as.character))

  dataset <- suppressWarnings(
    data_dict_match_dataset(dataset, data_dict, output = "dataset"))
  
  data_dict <- suppressWarnings(
    data_dict_match_dataset(dataset, data_dict, output = "data_dict"))

  
  # check if `valueType` column exists
  if(is.null(data_dict[['Variables']][['valueType']]))
    data_dict[['Variables']][['valueType']] <- NA_character_    
  
  vT_list <- madshapR::valueType_list[!is.na(madshapR::valueType_list[['valueType']]),]
  var_index <- data_dict[['Variables']]['name'] %>%
    rename (name_var = "name") %>% add_index('index')
  
  test_vT_data_dict <- 
    check_data_dict_valueType(data_dict) %>%
    left_join(var_index,by = "name_var")
  
  test_vT_dataset <- tibble(
    name_var = as.character(),
    value2 = as.character(),
    condition2 = as.character(),
    suggestion2 = as.character())
  
  for(i in names(dataset)){
    # stop()}

    data_dict_vT <-
      data_dict[['Variables']][
        which(data_dict[['Variables']]$`name` == i),]$`valueType`

    vec <- as.character(c(dataset[[i]]))
    vec2 <- data_dict[['Categories']][['name']][
            data_dict[['Categories']][['variable']] == i]
    if(length(vec2) > 0) vec <- c(vec,vec2)
    
    condition2 <- class(silently_run(as_valueType(vec,data_dict_vT)))[1]
    actual    <- as.character(valueType_of(dataset[[i]]))
    guess     <- ifelse(valueType_guess == FALSE,NA_character_,as.character(valueType_guess(vec)))
    
    # preserve original valueType when is NA
    if(all(is.na(vec))){
      guess     <- ifelse(
        valueType_guess == FALSE,actual,as.character(valueType_guess(dataset[[i]])))}
  
    test_vT_dataset <-
      rbind(
        test_vT_dataset,
        tibble(
          name_var  = i,
          value2     = data_dict_vT,
          condition2 = ifelse(
            condition2 == 'try-error',
"[ERROR] - valueType in data dictionary is not compatible with dataset values.",NA_character_),
          suggestion2 = guess) %>% 
          mutate(
          condition2 = ifelse(
            data_dict_vT == 'date' & guess == 'datetime',
"[ERROR] - Inconsistent or ambiguous date format.",.data$`condition2`),
          suggestion2 = ifelse(
            data_dict_vT == 'date' & guess == 'datetime',
            'text',.data$`suggestion2`)))
  }
  
  test_vT_dataset <- 
    test_vT_dataset %>%
    left_join(var_index,by = "name_var") %>%
    rowwise() %>%
    filter(!.data$`value2` %in% .data$`suggestion2` | !is.na(.data$`condition2`))

  if(valueType_guess == FALSE) 
    test_vT_dataset <- 
    test_vT_dataset %>% dplyr::filter(!is.na(.data$`condition2`))

  test <- 
    full_join(
      test_vT_data_dict, 
      test_vT_dataset, join_by("index","name_var")) %>%
    mutate(suggestion = .data$`suggestion2`,value = .data$`value2`) %>%
    rowwise() %>%
    mutate(
      replace_suggestion = .data$`value` %in% vT_list$valueType | is.na(.data$`value`),
      replace_suggestion = .data$`value` %in% vT_list$valueType | is.na(.data$`value`),
      condition = ifelse(isTRUE(.data$`replace_suggestion`) & is.na(.data$`condition`), 
                         .data$`condition2`,.data$`condition`),
      value = replace_na(.data$`value`,"(empty)")) %>%
    mutate(
      condition = ifelse(
        is.na(.data$`condition`),
        "[INFO] - Suggested valueType.",.data$`condition`),
      condition = as.character(.data$`condition`)) %>%
    mutate(index = as.integer(.data$`index`)) %>%
    arrange(.data$`index`) %>%
    select(-"index",-"suggestion2",-"condition2",-"value2",-"replace_suggestion") %>%
    distinct()
  
  return(test)
}


#' @title
#' Assess variable names in a data dictionary for non-standard formats
#'
#' @description
#' Generates a data frame report of any variable names that are not compatible 
#' in Maelstrom Research ecosystem, including Opal.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
#'
#' @details
#' The object may be specifically formatted to be compatible with additional 
#' [Maelstrom Research software](https://maelstrom-research.org/page/software), 
#' in particular [Opal environments](https://www.obiba.org/pages/products/opal/).
#'
#' @param var_names A character vector of names.
#'
#' @returns
#' A data frame providing non-standard names across a vector.
#'
#' @examples
#' {
#' 
#' # use madshapR_example provided by the package
#'
#' names_in_data_dict <- 
#'   madshapR_example$`data_dict_example - errors`$Variables$name
#' check_name_standards(names_in_data_dict)
#' check_name_standards(c("coucou", "cou cou", "$coucou",NA))
#'
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom rlang .data
#'
#' @export
check_name_standards <- function(var_names){

  # [GF - tested and validated]
  
  var_names_valid <- make.names(
    paste0("X",var_names) %>% str_replace_all("-","_"))

  test <-
    var_names[paste0("X",var_names)%>%
                str_replace_all("-","_") != var_names_valid] %>%
    as_tibble() %>% rename(name_var = .data$`value`) %>%
    mutate(
      condition =
"[INFO] - Variable names contain special characters, contain spaces, or begin with a number.") %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct()

  return(test)
}
