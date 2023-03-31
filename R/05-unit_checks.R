#' @title
#' Return non unique variables across a data dictionary
#'
#' @description
#' Generates a tibble that reports any variable whose name is not
#' unique across the list of variable names in the 'Variables' sheet.
#' This tibble can be used to assist the user in the assessment of the data
#' structure, fields investigation (mandatory or not), coherence across elements
#' and taxonomy or standard evaluation.
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
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#'
#' @return
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
    filter(.data$`n` > 1) %>%
    select(.data$`name`)

  test <-
    var_names %>%
    mutate(
      condition = "[ERR] - duplicated variable name") %>%
    select(name_var = .data$`name`, .data$`condition`) %>%
    mutate(across(everything(), ~as.character(.)))

  var_NA <-
    data_dict[['Variables']] %>%
    fabR::add_index(.force = TRUE) %>%
    filter(is.na(.data$`name`) | .data$`name` == "") %>%
    mutate(name = paste0("row number: ",.data$`index`)) %>%
    select(.data$`name`)

  test <-
    test %>% bind_rows(
      var_NA %>%
        mutate(
          condition = "[ERR] - missing variable name") %>%
        select(name_var = .data$`name`, .data$`condition`) %>%
        mutate(across(everything(), ~as.character(.)))) %>%
    filter(!is.na(.data$`name_var`)) %>%
    distinct

  return(test)
}


#' @title
#' Return categorical variables which has issues in a data dictionary
#'
#' @description
#' Generates a tibble that reports any categorical variable which
#' name is in the 'Categories' but not present in the main list of variable
#' names in 'Variables' component. It also reports any categorical variable
#' which possible observation (the combination of 'variable' and 'name' in
#' 'Categories') is not unique.
#' This tibble can be used to assist the user in the assessment of the data
#' structure, fields investigation (mandatory or not), coherence across elements
#' and taxonomy or standard evaluation.
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
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#'
#' @return
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
    filter(!is.na(.data$`name_var`))

  cat_names <-
    data_dict[['Categories']] %>%
    mutate(across(everything(), as.character)) %>%
    mutate(name_var = as.character(.data$`variable`)) %>%
    fabR::add_index(.force = TRUE) %>%
    mutate(index = paste0("At line: ",.data$`index`)) %>%
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
    filter(!is.na(.data$`index`)) %>%
    select(.data$`name_var`, .data$`condition`, .data$`value`) %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct

  cat_names_count <-
    data_dict[['Categories']] %>%
    select(name_var = .data$`variable`,.data$`name`) %>%
    mutate(across(everything(), as.character)) %>%
    group_by(.data$`name_var`,.data$`name`) %>% add_count() %>% ungroup %>%
    filter(.data$`n` > 1)

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
#' Return categorical values which 'missing' column is not a boolean
#'
#' @description
#' Generates a tibble that reports any categorical variable where
#' 'missing' column is not a boolean (or compatible value that can be coerced
#' into a boolean). This tibble can be used to assist the user in the assessment
#' of the data structure, fields investigation (mandatory or not), coherence
#' across elements and taxonomy or standard evaluation.
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
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#'
#' @return
#' A tibble providing categorical values which 'missing' column is
#' not a boolean.
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

  test <- tibble(name_var = as.character(), value = as.character())

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
        class(fabR::silently_run(fabR::as_any_boolean(.data$`missing`)))[1]) %>%
    filter(.data$`value` == "try-error") %>%
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
#' Return evaluation of the valueType declared in a data dictionary
#'
#' @description
#' Generates a tibble that reports any variable whose taxonomy is
#' not present in the list of authorized taxonomy list. This tibble can be used
#' to assist the user in the assessment of the data structure, fields
#' investigation (mandatory or not), coherence across elements and taxonomy or
#' standard evaluation.
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
#'
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
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#' @param taxonomy A data frame or data frame extension (e.g. a tibble),
#' identifying the scheme used for variables classification as a tibble.
#'
#' @return
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
  #     filter(.data$`taxonomy` == !! value) %>%
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
  #   fabR::add_index(.force = TRUE) %>%
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
  # # if mlstr_taxonomy
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
  #   filter(! .data$`valueType` %in% vT_list$`valueType`) %>%
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
  #     filter(.data$`toValueType` == 'text') %>% pull(.data$`valueType`)
  #
  #   data_dict_vt <-
  #     data_dict[['Variables']] %>%
  #     select(name_var = .data$`name`,.data$`valueType`) %>%
  #     filter(! .data$`valueType` %in% vT_text)
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
  #     filter(.data$`test` == "try-error") %>%
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
#' Return evaluation of the valueType declared in a data dictionary
#'
#' @description
#' Generates a tibble that reports any variable whose valueType is
#' not present in the list of authorized valueType list. Additionally, it
#' evaluates if the valueType is compatible with categorical values declared.
#' This tibble can be used to assist the user in the assessment of the data
#' structure, fields investigation (mandatory or not), coherence across elements
#' and taxonomy or standard evaluation.
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
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#'
#' @return
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
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
check_data_dict_valueType <- function(data_dict){

  # test if enough data_dict
  as_data_dict_shape(data_dict)

  test <- test_valueType_names <- test_valueType_cat <-
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
    filter(! .data$`valueType` %in% vT_list$`valueType`) %>%
    select(name_var = .data$`name`,value = .data$`valueType`) %>%
    mutate(
      condition = "[ERR] - Incompatible valueType names with Opal standards")%>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct()

  if(length(data_dict[['Categories']]) > 0){

    vT_text <-
      vT_list %>%
      filter(.data$`toValueType` == 'text') %>% pull(.data$`valueType`)

    data_dict_vt <-
      data_dict[['Variables']] %>%
      select(name_var = .data$`name`,.data$`valueType`) %>%
      filter(! .data$`valueType` %in% vT_text)

    vT_names <-
      data_dict[['Categories']] %>%
      select(name_var = .data$`variable`,.data$`name`) %>%
      inner_join(data_dict_vt,by = "name_var")

    test_valueType_cat <-
      vT_names %>%
      select(-.data$`name_var`) %>%
      distinct() %>%
      rowwise() %>%
      mutate(
        test = class(
          fabR::silently_run(as_valueType(.data$`name`,.data$`valueType`)))[1]
        ) %>%
      filter(.data$`test` == "try-error") %>%
      inner_join(vT_names,by = c("name", "valueType")) %>%
      select(.data$`name_var`, .data$`valueType`) %>%
      distinct


    test_valueType_cat <-
      test_valueType_cat %>%
      full_join(
        fabR::silently_run(
        valueType_self_adjust(data_dict_filter(
            data_dict,paste0("name %in% c('",
                           paste0(unique(test_valueType_cat$name_var),
                                  collapse = "','"),"')")))[['Variables']]) %>%
          select(name_var = .data$`name`, suggestion = .data$`valueType`),
        by = "name_var")

    test_valueType_cat <-
      test_valueType_cat %>%
      mutate(
        condition = "[ERR] - valueType conflict in 'Categories'") %>%
      select(.data$`name_var`, value = .data$`valueType`, .data$`condition`,
             .data$`suggestion`) %>%
      mutate(across(everything(), ~as.character(.))) %>%
      distinct
  }

  test <- bind_rows(test_valueType_names, test_valueType_cat)

  return(test)
}

#' @title
#' Return undeclared variables across a data dictionary
#'
#' @description
#' Generates a tibble that reports any variable which is either
#' present in the dataset but not in the data dictionary associated, or present
#' in the data dictionary but absent in the associated data dictionary.
#' This tibble can be used to assist the user in the assessment of the data
#' structure, fields investigation (mandatory or not), coherence across elements
#' and taxonomy or standard evaluation.
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
#' @param data A tibble identifying the input data observations.
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#'
#' @return
#' A tibble providing undeclared variables across a data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data <- DEMO_files$`dataset_TOKYO - ERROR WITH DATA`
#' data_dict <- DEMO_files$`dd_TOKYO_format_maelstrom_tagged - ERROR`
#' check_dataset_variables(data,data_dict)
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
check_dataset_variables <- function(data, data_dict = NULL){

  if(is.null(data_dict)) data_dict <- data_dict_extract(data)

  # test if enough data_dict or dataset
  as_data_dict_shape(data_dict)
  as_dataset(data) # no col_id

  test_cat <- tibble(name_var = as.character(), condition = as.character())

  var_names_in_data_dict <-
    data_dict[['Variables']] %>%
    select(name_var = .data$`name`) %>%
    mutate(data_dict = "data_dict")

  var_names_in_dataset <-
    data %>% names %>% as_tibble() %>%
    rename(name_var = .data$`value`) %>%
    mutate(data = "data")

  test <-
    full_join(var_names_in_dataset,var_names_in_data_dict,by = "name_var") %>%
    mutate(condition = case_when(
      is.na(data_dict) ~ "[ERR] - Variable only present in the dataset",
      is.na(data)      ~ "[ERR] - Variable only present in the data dictionary",
      TRUE ~ NA_character_)) %>%
    mutate(across(everything(), ~as.character(.))) %>%
    filter(!is.na(.data$`condition`)) %>%
    select(.data$`name_var`, .data$`condition`) %>%
    distinct()

  return(test)
}

#' @title
#' Return categorical values which differ between data and their data dictionary
#'
#' @description
#' Generates a tibble that reports any categorical variable which
#' categorical possible observation (the combination of 'variable' and 'name' in
#' 'Categories' sheet) is actually observed in the data observations. Likewise,
#' the actual observations are evaluated to report any categorical variable
#' which occurrence is not in the data dictionary categorical variables.
#' This tibble can be used to assist the user in the assessment of the data
#' structure, fields investigation (mandatory or not), coherence across elements
#' and taxonomy or standard evaluation.
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
#' @param data A tibble identifying the input data observations.
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#'
#' @return
#' A tibble providing categorical values which differ between data and
#' their data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$`dd_TOKYO_format_maelstrom_tagged - ERROR WITH DATA`
#' data      <- DEMO_files$`dataset_TOKYO - ERROR WITH DATA`
#' check_dataset_categories(data, data_dict)
#' 
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
check_dataset_categories <- function(data, data_dict = NULL){

  if(is.null(data_dict)) data_dict <-
      data_dict_extract(data,as_mlstr_data_dict = FALSE)

  # test if enough data_dict or dataset
  as_data_dict_shape(data_dict)
  as_dataset(data) # no col_id

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
  data_dict_cat_from_data <- data_dict_extract(data,as_mlstr_data_dict = FALSE)
  data_dict_cat_from_data[['Variables']] <-
    data_dict_cat_from_data[['Variables']] %>%
    filter(.data$`name` %in% data_dict_cat_from_data[['Categories']]$`variable`)

  # categorical content extracted from data_dict
  data_dict_cat_from_data_dict <- data_dict
  data_dict_cat_from_data_dict[['Variables']] <-
    data_dict_cat_from_data_dict[['Variables']] %>%
    filter(
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
    filter(!.data$`name` %in% test_cat_in_data_dict_only$`name_var`)

  if(sum(nrow(data_dict_cat_from_data_dict[['Categories']])) > 0){
    data_dict_cat_from_data_dict[['Categories']] <-
      data_dict_cat_from_data_dict[['Categories']] %>%
      filter(!.data$`variable` %in% test_cat_in_data_dict_only$`name_var`)}

  data_dict_cat_from_data[['Variables']] <-
    data_dict_cat_from_data[['Variables']] %>%
    filter(!.data$`name` %in% test_cat_in_dataset_only$`name_var`)

  if(sum(nrow(data_dict_cat_from_data[['Categories']])) > 0){
    data_dict_cat_from_data[['Categories']] <-
      data_dict_cat_from_data[['Categories']] %>%
      filter(!.data$`variable` %in% test_cat_in_dataset_only$`name_var`)}

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
    filter(!is.na(.data$`name_var`)) %>%
    distinct()

  return(test)
}

#' @title
#' Return values which valueType differs between data and their data dictionary
#'
#' @description
#' Generates a tibble that reports any valueType differing
#' between data observation and their data dictionary declaration.
#' This tibble can be used to assist the user in the assessment of the data
#' structure, fields investigation (mandatory or not), coherence across elements
#' and taxonomy or standard evaluation.
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
#' @param data A tibble identifying the input data observations.
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#' @param valueType_guess Whether the output should include a more accurate
#' valueType that could be applied to the dataset. TRUE by default.
#'
#' @return
#' A tibble providing values which valueType differs between data and
#' their data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data <- DEMO_files$`dataset_TOKYO - ERROR WITH DATA`
#' data_dict <- DEMO_files$`dd_TOKYO_format_maelstrom_tagged - ERROR WITH DATA`
#' dataset <- data_dict_apply(data, data_dict)
#' check_dataset_valueType(dataset, data_dict,valueType_guess = TRUE)
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
check_dataset_valueType <- function(
    data, data_dict = NULL,
    valueType_guess = FALSE){
  
  if(is.null(data_dict)) data_dict <-
      data_dict_extract(data,as_mlstr_data_dict = TRUE)

  # test if enough data_dict or data
  as_dataset(data) # no col_id
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
      condition = as.character())

  # check if `valueType` column exists
  if(is.null(data_dict[['Variables']][['valueType']])){
    warning("Unknown or uninitialised column: `valueType`")
    return(test)}

  data <-
    data[vapply(
      X = data, FUN = function(x) !all(is.na(x)), FUN.VALUE = logical(1))]

  data <-      data_dict_match_dataset(data, data_dict, output = "data")
  data_dict <- data_dict_match_dataset(data, data_dict, output = "data_dict")

  for(i in names(data)){
    # stop()}

    data_dict_vT <-
      data_dict[['Variables']][
        which(data_dict[['Variables']]$`name` == i),]$`valueType`

    # test valueType
    # test_vT   <-
    #   class(fabR::silently_run(as_valueType(data[[i]],data_dict_vT)))[1]
    condition <-
      class(fabR::silently_run(as_valueType(data[[i]],data_dict_vT)))[1]
    guess   <- valueType_guess(data[[i]])
    actual  <- valueType_of(data[[i]])

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
        "[INFO] - refined valueType proposed",.data$`condition`)) %>%
    filter(.data$`value` != .data$`suggestion`) %>%
    filter(!is.na(.data$`condition`)) %>%
    distinct()

  if(valueType_guess == FALSE){
    test <- test %>% filter(!str_detect(.data$`condition`,"^\\[INFO\\]"))}

  return(test)
}


#' @title
#' Return non-standard names across a vector
#'
#' @description
#' Generates a tibble that reports any variable whose name is not
#' standard, as defined by Maelstrom naming standards.
#' This tibble can be used to assist the user in the assessment of the data
#' structure, fields investigation (mandatory or not), coherence across elements
#' and taxonomy or standard evaluation.
#'
#' @details
#' The user must provide element which respect a certain structure to work with
#' the functions of the package or its environment (Maelstrom and/or
#' Obiba suite). In addition, any element may follow Maelstrom research
#' standards, and its content can be evaluated accordingly, such as naming
#' convention restriction, columns like 'valueType', , and 'label(:xx)',
#' and/or any taxonomy provided.
#'
#' @param var_names A character vector of names.
#'
#' @return
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
