#' @title
#' Assess a data dictionary for potential issues in variables
#'
#' @description
#' Generates a tibble report of any non-unique variable names in the 
#' 'Variables' element.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#'
#' @returns
#' A tibble providing non unique variables across a data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$`dd_TOKYO_format_maelstrom_tagged - ERROR`
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
      condition = "[ERR] - duplicated variable name") %>%
    select(name_var = "name", "condition") %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct

  var_NA <-
    data_dict[['Variables']] %>%
    add_index(.force = TRUE) %>%
    dplyr::filter(is.na(.data$`name`) | .data$`name` == "") %>%
    mutate(name = paste0("row number: ",.data$`index`)) %>%
    select(.data$`name`)

  test <-
    test %>% bind_rows(
      var_NA %>%
        mutate(
          condition = "[ERR] - missing variable name") %>%
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
#' Generates a tibble report of any categorical variable name present in the
#' 'Categories' element but not present in 'Variables'. The tibble also reports 
#' any non-unique combinations of 'variable' and 'name' in the 'Categories'
#' element.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#'
#' @returns 
#' A tibble providing categorical variables that has issues within a 
#' data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$`dd_TOKYO_format_maelstrom_tagged - ERROR`
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
      value = as.character(),
      condition = as.character())

  if(sum(nrow(data_dict[['Categories']])) == 0){
    warning("You data dictionary contains no categorical variables")
    return(test)}

  var_names <-
    data_dict[['Variables']] %>%
    mutate(name_var = as.character(.data$`name`)) %>%
    select(.data$`name_var`) %>% distinct %>%
    dplyr::filter(!is.na(.data$`name_var`))

  cat_names <-
    data_dict[['Categories']] %>%
    mutate(across(everything(), as.character)) %>%
    mutate(name_var = as.character(.data$`variable`)) %>%
    add_index(.force = TRUE) %>%
    mutate(index = paste0("row number: ",.data$`index`)) %>%
    select(value = .data$`name`,.data$`name_var`,.data$`index`) %>%
    arrange(.data$`name_var`) %>%
    distinct

  test_cat_presence <-
    anti_join(cat_names,var_names, by = "name_var") %>%
    bind_rows(tibble(
      value = NA_character_,name_var = NA_character_,index = NA_character_)) %>%
    rowwise() %>%
    mutate(
      condition = ifelse(is.na(.data$`name_var`),
        "[ERR] - In 'variable', the value cannot be NA",
        "[ERR] - Categories not present in the variable names (in 'Variables')"
        ),
      condition = ifelse(is.na(.data$`value`),
        "[ERR] - In 'name', the value cannot be NA", .data$`condition`),
      value = ifelse(!is.na(.data$`value`),"value",.data$`value`),
      value = replace_na(.data$`value`,.data$`index`),
      value = na_if(.data$`value`,"value"),
      value = ifelse(is.na(.data$`name_var`),.data$`index`,.data$`value`)) %>%
    ungroup() %>%
    dplyr::filter(!is.na(.data$`index`)) %>%
    select(.data$`name_var`, .data$`condition`, .data$`value`) %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct

  cat_names_count <-
    data_dict[['Categories']] %>%
    select(name_var = .data$`variable`,.data$`name`) %>%
    mutate(across(everything(), as.character)) %>%
    group_by(.data$`name_var`,.data$`name`) %>% add_count() %>% ungroup %>%
    dplyr::filter(.data$`n` > 1)

  test_cat_unique <-
    cat_names_count %>%
    mutate(
      condition = "[ERR] - Category names not unique in the data dictionary")%>%
    select(.data$`name_var`, value = .data$`name`, .data$`condition`) %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct

  test <- bind_rows(test, test_cat_presence, test_cat_unique)

  return(test)

}

#' @title
#' Assess categorical variables for non-Boolean values in 'missing' column
#'
#' @description
#' Generates a tibble report of any categorical variables with non-Boolean (or
#' compatible with boolean) values in the 'missing' column of the 'Categories'
#' element.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#'
#' @returns 
#' A tibble providing categorical values which 'missing' column is not a 
#' boolean.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$`dd_TOKYO_format_maelstrom_tagged - ERROR`
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
      condition =
"[ERR] - incompatible value in the missing columns with Maelstrom standards")%>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct()

  return(test)
}

#' @title
#' Assess a data dictionary for non-valid taxonomy values
#'
#' @description
#' Generates a tibble report of any variable with a taxonomy value that is not
#' in the list of allowed values for a given taxonomy.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#' @param taxonomy A tibble identifying the scheme used for variables 
#' classification.
#'
#' @returns
#' A tibble providing non-standard taxonomy declared in a data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' taxonomy <- DEMO_files$taxonomy_opal
#' data_dict <- DEMO_files$`dd_TOKYO_format_maelstrom_tagged - ERROR`
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
  #       condition = paste0("[ERR] - '",!! value,"' is not valid."))
  #
  #   return(test)
  # }

  # data_dict <- data_dict_list$`dd_TOKYO_format_maelstrom_tagged - ERROR`

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
  #     condition = "[ERR] - 'Mlstr_additional::Target' column is missing")}
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
  #     condition = "[ERR] - 'Mlstr_additional::Source' column is missing")
  #
  #   if(is.null(data_dict_elem[['Mlstr_additional::Target']])){
  #     test <- test %>% add_row(
  #       name_var = 'Mlstr_additional::Target',
  #       condition = "[ERR] - 'Mlstr_additional::Target' column is missing")}
  #
  #   # area::1 and area::1.term are mandatory
  #   if(is.null(data_dict_elem[['Mlstr_area::1']])){
  #     test <- test %>% add_row(
  #       name_var = 'Mlstr_area::1',
  #       condition = "[ERR] - 'Mlstr_area::1' column is missing")}
  #
  #   if(is.null(data_dict_elem[['Mlstr_area::1.term']])){
  #     test <- test %>% add_row(
  #       name_var = 'Mlstr_area::1.term',
  #       condition = "[ERR] - 'Mlstr_area::1.term' column is missing")}
  #
  #
  #
  #
  #   # if area::2 then area::2.term and !area::1
  #   if(!is.null(data_dict_elem[['Mlstr_area::1']]) &
  #   !is.null(data_dict_elem[['Mlstr_area::1.term']])){
  #     test <- test %>% add_row(
  #       name_var = 'Mlstr_area::1.term',
  #       condition = "[ERR] - 'Mlstr_area::1.term' column is missing")}
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
  # condition = "[ERR] - Incompatible valueType names with Opal standards") %>%
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
  #       condition = "[ERR] - valueType conflict in 'Categories'") %>%
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
#' Generates a tibble report of any variable with a valueType that is not in the
#' list of allowed valueType values. This function also assesses if the 
#' valueType is compatible with any associated categorical values declared.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' The valueType is a property of a variable and is required in certain 
#' functions to determine the handling of the variables. The valueType refers 
#' to the OBiBa-internal type of a variable. It is specified in a data 
#' dictionary in a column `valueType` and can be associated with variables as 
#' attributes. Acceptable valueTypes include 'text', 'integer', 'decimal', 
#' 'boolean', datetime', 'date'). The full list of OBiBa valueType 
#' possibilities and their correspondence with R data types are available using
#' [madshapR::valueType_list].
#'
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#'
#' @returns 
#' A tibble providing non-standard valueType declared in a data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$`dd_TOKYO_format_maelstrom_tagged - ERROR`
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
    test_valueType_names <- 
    test_valueType_cat <- 
    test_valueType_refined <- 
    tibble(
      name_var = as.character(),
      value = as.character(),
      condition = as.character())

  if(is.null(data_dict[['Variables']][['valueType']])){
    warning("Your data dictionary contains no valueType column")
    return(test)}

  vT_list <- madshapR::valueType_list
  test_valueType_names <-
    data_dict[['Variables']] %>%
    dplyr::filter(! .data$`valueType` %in% vT_list$`valueType`) %>%
    select(name_var = "name",value = "valueType") %>%
    mutate(
      condition = "[ERR] - Incompatible valueType names with Opal standards")%>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct()

  if(length(data_dict[['Categories']]) > 0){

    vT_text <-
      vT_list %>%
      dplyr::filter(.data$`toValueType` == 'text') %>% pull(.data$`valueType`)

    data_dict_vt <-
      data_dict[['Variables']] %>%
      select(name_var = .data$`name`,.data$`valueType`) %>%
      dplyr::filter(! .data$`valueType` %in% vT_text)

    vT_names <-
      data_dict[['Categories']] %>%
      select(name_var = "variable", "name") %>%
      inner_join(data_dict_vt,by = "name_var")

    test_valueType_cat <-
      vT_names %>%
      select(-"name_var") %>%
      distinct() %>%
      rowwise() %>%
      mutate(
        test = class(
          silently_run(as_valueType(.data$`name`,.data$`valueType`)))[1]
        ) %>%
      dplyr::filter(.data$`test` == "try-error") %>%
      inner_join(vT_names,by = c("name", "valueType")) %>%
      select("name_var", "valueType") %>%
      distinct

    test_valueType_cat <-
      test_valueType_cat %>%
      full_join(
        silently_run(
        valueType_self_adjust(
          data_dict_filter(
            data_dict,paste0("name %in% c('",
                           paste0(unique(test_valueType_cat$name_var),
                                  collapse = "','"),"')")))[['Variables']]) %>%
          select(name_var = "name", suggestion = "valueType"),
        by = "name_var")

    test_valueType_cat <-
      test_valueType_cat %>%
      mutate(
        condition = "[ERR] - valueType conflict in 'Categories'") %>%
      select(.data$`name_var`, value = .data$`valueType`, .data$`condition`,
             .data$`suggestion`) %>%
      mutate(across(everything(), ~as.character(.))) %>%
      distinct
    
    vT_cat <- 
      na.omit(intersect(
      unique(data_dict[['Variables']][['name']]),
      unique(data_dict[['Categories']][['variable']])))
      
    vT_cat <- vT_cat[!vT_cat %in% (test_valueType_cat %>% pull(.data$`name_var`))]
    
    for(i in vT_cat){
      # stop()}
      
      data_dict_i <- 
        data_dict %>%
        data_dict_filter(paste0('name == "',i,'"'))
      
      vT_data_dict <- data_dict_i['Variables']
      vT_dataset <- 
        data_dict_i[['Categories']]['name'] %>%
        rename_with(.cols = "name", .fn = ~ i)
      
      test_valueType_refined <- 
        test_valueType_refined %>%
        bind_rows(
          check_dataset_valueType(
            vT_dataset,vT_data_dict,valueType_guess = TRUE)) %>%
        mutate(
          condition = str_replace(.data$`condition`,"dataset","'Categories'"))
    }
  }

  test <- bind_rows(
    test_valueType_names, 
    test_valueType_cat,
    test_valueType_refined)

  return(test)
}

#' @title
#' Assess a data dictionary and associated dataset for undeclared variables
#'
#' @description
#' Generates a tibble report of any variable that is present in a dataset but
#' not in the associated data dictionary or present in a data dictionary but 
#' not in the associated dataset.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param dataset A tibble identifying the input dataset observations.
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#'
#' @returns
#' A tibble providing undeclared variables across a data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' dataset <- DEMO_files$`dataset_TOKYO - ERROR WITH DATA`
#' data_dict <- DEMO_files$`dd_TOKYO_format_maelstrom_tagged - ERROR`
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
    select(name_var = .data$`name`) %>%
    mutate(data_dict = "data_dict")

  var_names_in_dataset <-
    dataset %>% names %>% as_tibble() %>%
    rename(name_var = .data$`value`) %>%
    mutate(dataset = "dataset")

  test <-
    full_join(var_names_in_dataset,var_names_in_data_dict,by = "name_var") %>%
    mutate(condition = case_when(
      is.na(data_dict) ~ "[ERR] - Variable only present in the dataset",
      is.na(dataset)   ~ "[ERR] - Variable only present in the data dictionary",
      TRUE ~ NA_character_)) %>%
    mutate(across(everything(), ~as.character(.))) %>%
    dplyr::filter(!is.na(.data$`condition`)) %>%
    select(.data$`name_var`, .data$`condition`) %>%
    distinct()

  return(test)
}

#' @title
#' Assess a data dictionary and associated dataset for category differences
#'
#' @description
#' Generates a tibble report of any categorical value options (the combination
#' of 'variable' and 'name' in 'Categories') in a data dictionary that are not 
#' in the associated dataset and any categorical variable values in a dataset 
#' that are not declared in the associated data dictionary.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param dataset A tibble identifying the input dataset observations.
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#'
#' @returns
#' A tibble providing categorical values which differ between dataset and
#' their data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$`dd_TOKYO_format_maelstrom_tagged - ERROR WITH DATA`
#' dataset      <- DEMO_files$`dataset_TOKYO - ERROR WITH DATA`
#' check_dataset_categories(dataset, data_dict)
#' 
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
check_dataset_categories <- function(dataset, data_dict = NULL){

  if(is.null(data_dict)) data_dict <-
      data_dict_extract(dataset,as_data_dict_mlstr = FALSE)

  # test if enough data_dict or dataset
  as_data_dict_shape(data_dict)
  as_dataset(dataset) # no col_id

  test <-
    test_cat_in_data_dict_only    <- test_cat_in_dataset_only <-
    test_values_in_data_dict_only <- test_values_in_dataset_only <-
    tibble(
      name_var = as.character(),
      value = as.character(),
      condition = as.character())

  # if(sum(nrow(data_dict[['Categories']])) == 0){
  #   warning("You data dictionary contains no categorical variables")
  #   return(test)}

  # rajouter possible_dataset_category <-
  #   dataset %>%
  #   summarise(across(everything(),
  #              ~{ class(.)[1] %in% c('factor','haven_labelled')})
  #   ) %>%
  #   pivot_longer(everything()) %>%
  #   mutate(value = ifelse(.data$`value`,"yes","no")) %>%
  #   select(.data$`name`, `Categorical in dataset` = .data$`value`)

  # categorical content extracted from dataset
  data_dict_cat_from_data <- 
    silently_run(data_dict_extract(dataset,as_data_dict_mlstr = FALSE))
  data_dict_cat_from_data[['Variables']] <-
    data_dict_cat_from_data[['Variables']] %>%
    dplyr::filter(.data$`name` %in% 
                    data_dict_cat_from_data[['Categories']]$`variable`)

  # categorical content extracted from data_dict
  data_dict_cat_from_data_dict <- data_dict
  data_dict_cat_from_data_dict[['Variables']] <-
    data_dict_cat_from_data_dict[['Variables']] %>%
    dplyr::filter(
      .data$`name` %in% data_dict_cat_from_data_dict[['Categories']]$`variable`)

  # categorical variable in the data_dict, but not in the dataset
  test_cat_in_data_dict_only <-
    data_dict_cat_from_data_dict[['Variables']]['name'] %>%
    anti_join(
      data_dict_cat_from_data[['Variables']]['name'], by = "name") %>%
    rename(name_var = .data$`name`) %>%
    mutate(condition =
"[INFO] - Categorical variable in the data dictionary but not in the dataset")

  # categorical variable in the dataset, but not in the data_dict
  test_cat_in_dataset_only <-
    data_dict_cat_from_data[['Variables']]['name'] %>%
    anti_join(
      data_dict_cat_from_data_dict[['Variables']]['name'], by = "name") %>%
    rename(name_var = .data$`name`) %>%
    mutate(condition =
"[INFO] - Categorical variable in the dataset but not in the data dictionary")

  # remove already assessed
  data_dict_cat_from_data_dict[['Variables']] <-
    data_dict_cat_from_data_dict[['Variables']] %>%
    dplyr::filter(!.data$`name` %in% test_cat_in_data_dict_only$`name_var`)

  if(sum(nrow(data_dict_cat_from_data_dict[['Categories']])) > 0){
    data_dict_cat_from_data_dict[['Categories']] <-
      data_dict_cat_from_data_dict[['Categories']] %>%
      dplyr::filter(
        !.data$`variable` %in% test_cat_in_data_dict_only$`name_var`)}

  data_dict_cat_from_data[['Variables']] <-
    data_dict_cat_from_data[['Variables']] %>%
    dplyr::filter(!.data$`name` %in% test_cat_in_dataset_only$`name_var`)

  if(sum(nrow(data_dict_cat_from_data[['Categories']])) > 0){
    data_dict_cat_from_data[['Categories']] <-
      data_dict_cat_from_data[['Categories']] %>%
      dplyr::filter(!.data$`variable` %in% test_cat_in_dataset_only$`name_var`)}

  if(sum(nrow(data_dict_cat_from_data[['Categories']])) > 0){
    # categorical values in the data_dict, but not in the dataset
    test_values_in_data_dict_only <-
      anti_join(
        data_dict_cat_from_data_dict[['Categories']] %>%
          select(.data$`variable`, .data$`name`),
        data_dict_cat_from_data[['Categories']] %>%
          select(.data$`variable`, .data$`name`),
        by = c("variable", "name")) %>%
      rename(name_var = .data$`variable`) %>%
      rename(value = .data$`name`) %>%
      mutate(condition =
"[INFO] - More categories declared in the data dictionary than unique values in the dataset")

    # categorical values in the dataset, but not in the data_dict
    test_values_in_dataset_only      <-
      anti_join(
        data_dict_cat_from_data[['Categories']] %>%
          select(.data$`variable`, .data$`name`),
        data_dict_cat_from_data_dict[['Categories']] %>%
          select(.data$`variable`, .data$`name`),
        by = c("variable", "name")) %>%
      rename(name_var = .data$`variable`) %>%
      rename(value = .data$`name`) %>%
      mutate(condition =
"[INFO] - More unique values in the dataset than categories declared in the data dictionary")
  }

  test <-
    bind_rows(test, test_cat_in_data_dict_only, test_cat_in_dataset_only,
              test_values_in_data_dict_only, test_values_in_dataset_only) %>%
    dplyr::filter(!is.na(.data$`name_var`)) %>%
    dplyr::filter(!is.na(.data$`value`)) %>%
    distinct()

  return(test)
}

#' @title
#' Assess a data dictionary and associated dataset for valueType differences
#'
#' @description
#' Generates a tibble report of any incompatibility between variable values in a
#' dataset and the declared valueType in the associated data dictionary.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
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
#' @param dataset A tibble identifying the input dataset observations.
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#' @param valueType_guess Whether the output should include a more accurate
#' valueType that could be applied to the dataset. TRUE by default.
#'
#' @returns
#' A tibble providing values which valueType differs between dataset and
#' their data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' dataset <- DEMO_files$`dataset_TOKYO - ERROR WITH DATA`
#' data_dict <- DEMO_files$`dd_TOKYO_format_maelstrom_tagged - ERROR WITH DATA`
#' dataset <- data_dict_apply(dataset, data_dict)
#' check_dataset_valueType(dataset, data_dict,valueType_guess = TRUE)
#'
#' }
#'
#' @import dplyr tidyr stringr fabR
#' @importFrom rlang .data
#'
#' @export
check_dataset_valueType <- function(
    dataset, data_dict = NULL,
    valueType_guess = FALSE){
  
  if(is.null(data_dict)) data_dict <-
      data_dict_extract(dataset,as_data_dict_mlstr = TRUE)

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
  
  test <- test_vT_dataset <-
    # test_vT_compatible <-
    tibble(
      name_var = as.character(),
      value = as.character(),
      condition = as.character(),
      suggestion = as.character())

  # check if `valueType` column exists
  if(is.null(data_dict[['Variables']][['valueType']])){
    warning("Unknown or uninitialised column: `valueType`")
    return(test)}

  # dataset <-
  #   dataset[vapply(
  #     X = dataset, FUN = function(x) !all(is.na(x)), FUN.VALUE = logical(1))]

  dataset <- suppressWarnings(
    data_dict_match_dataset(dataset, data_dict, output = "dataset"))
  
  data_dict <- suppressWarnings(
    data_dict_match_dataset(dataset, data_dict, output = "data_dict"))

  for(i in names(dataset)){
    # stop()}

    data_dict_vT <-
      data_dict[['Variables']][
        which(data_dict[['Variables']]$`name` == i),]$`valueType`

    # test valueType
    # test_vT   <-
    #   class(silently_run(as_valueType(dataset[[i]],data_dict_vT)))[1]
    condition <-
      class(silently_run(as_valueType(dataset[[i]],data_dict_vT)))[1]
    guess   <- as.character(valueType_guess(dataset[[i]]))
    actual  <- as.character(valueType_of(dataset[[i]]))

    # test_vT_compatible <-
    #   tibble(
    #     name_var     = i,
    #     data_dict_vT = data_dict_vT,
    #     test_vT      = condition,
    #     guess        = actual
    #   )

    test_vT_dataset <-
      rbind(
        test_vT_dataset,
        tibble(
          name_var  = i,
          value     = data_dict_vT,
          condition = ifelse(
            condition == 'try-error',
            "[ERR] - valueType conflict in dataset",NA_character_),
          suggestion = guess))
  }

  test <-
    bind_rows(test, test_vT_dataset) %>%
    mutate(
      condition = ifelse(
        is.na(.data$`condition`) &
          .data$`value` %in% c("text","decimal", "integer","date"),
        "[INFO] - refined valueType proposed",.data$`condition`),
      condition = as.character(.data$`condition`)) %>%
    dplyr::filter(.data$`value` != .data$`suggestion`) %>%
    dplyr::filter(!is.na(.data$`condition`)) %>%
    distinct()

  if(valueType_guess == FALSE){
    test <- test %>% 
      dplyr::filter(!str_detect(.data$`condition`,"^\\[INFO\\]"))}

  return(test)
}


#' @title
#' Assess variable names in a data dictionary for non-standard formats
#'
#' @description
#' Generates a tibble report of any variable names that are not compatible in 
#' Maelstrom Research ecosystem, including Opal.
#' This report can be used to help assess data structure, presence of fields, 
#' coherence across elements, and taxonomy or data dictionary formats.
#'
#' @details
#' The user must provide element which respect a certain structure to work with
#' the functions of the package or its environment (Maelstrom and/or
#' Obiba suite). In addition, any element may be compatible with Maelstrom 
#' Research ecosystem, including Opal, and its content can be evaluated 
#' accordingly, such as naming convention restriction, columns like 'valueType'
#' and 'label(:xx)' and/or any taxonomy provided.
#'
#' @param var_names A character vector of names.
#'
#' @returns
#' A tibble providing non-standard names across a vector.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' check_name_standards(c("coucou", "cou cou", "$coucou",NA))
#' check_name_standards(
#'  DEMO_files$`dd_TOKYO_format_maelstrom_tagged - ERROR`$Variables$name)
#'
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom rlang .data
#'
#' @export
check_name_standards <- function(var_names){

  var_names_valid <- make.names(
    paste0("X",var_names) %>% str_replace_all("-","_"))

  test <-
    var_names[paste0("X",var_names)%>%
                str_replace_all("-","_") != var_names_valid] %>%
    as_tibble() %>% rename(name_var = .data$`value`) %>%
    mutate(
      condition =
"[INFO] - Incompatible variable names with usual standards, including Maelstrom.") %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct()

  return(test)
}
