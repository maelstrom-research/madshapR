#' @title
#' Generate a report as an Excel spreadsheet of a study-specific dataset
#'
#' @description
#' Generates an Excel spreadsheet report for a dataset
#' for each variable to facilitate the assessment of input data.
#' This report can be used to assist the user in the assessment of the data
#' structure, fields investigation (mandatory or not), coherence across elements
#' and taxonomy, or standard evaluation. The summary associated provides dataset
#' composition, with observation repartition and descriptive statistics.
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
#' In addition, the data dictionary may follow Maelstrom research stardards,
#' and its content can be evaluated accordingly, such as naming convention
#' restriction, columns like 'valueType', 'missing' and 'label(:xx)',
#' and/or any taxonomy provided.
#'
#' A taxonomy must be a data frame or data frame extension (e.g. a tibble).
#' The taxonomy must be compatible with (and generally extracted from) an
#' Opal environment, and must contain at least 'taxonomy', 'vocabulary' and
#' 'terms' to work with some specific functions. In addition, the taxonomy
#' may follow Maelstrom research stardards, and its content can be evaluated
#' accordingly, such as naming convention restriction, tagging elements,
#' or scales, which are specific to Maelstrom Research. In this particular
#' case, the tibble must also contain 'vocabulary_short', 'taxonomy_scale',
#' 'vocabulary_scale' and 'term_scale' to work with some specific functions.
#'
#' @param dataset A tibble identifying the input data observations associated to
#' its data dictionary.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset. Automatically generated if not provided.
#' @param taxonomy A data frame or data frame extension (e.g. a tibble),
#' identifying the scheme used for variables classification as a tibble.
#' @param .dataset_name A character string specifying the name of the dataset
#' (internally used in the function `datashapR::study_evaluate()`).
#' @param as_mlstr_data_dict Whether the output data dictionary has a simple
#' data dictionary structure or not (meaning has a Maelstrom data dictionary
#' structure, compatible with Maelstrom ecosystem such as Opal environment).
#' FALSE by default.
#'
#' @seealso
#' [datashapR::study_evaluate()]
#'
#' @return
#' A list of tibbles of report for one study-specific data dictionary.
#'
#' @examples
#' \donttest{
#' # Example 1: yyy yyy yyy.
#' dataset <- study_TOKYO %>% mutate(dob = fabR::as_any_date(dob, format = ""mdy""))
#' data_dict <- dd_TOKYO_format_maelstrom_tagged
#' study_assessement_report(dataset, data_dict)
#' }
#'
#' @import dplyr tidyr stringr fabR
#' @importFrom magrittr %>%
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
dataset_evaluate <- function(dataset, data_dict = NULL, taxonomy = NULL, .dataset_name = NULL, as_mlstr_data_dict = TRUE){

  fargs <- as.list(match.call(expand.dots = TRUE))

  # check on arguments : data dict
  if(is.null(data_dict)) {
    data_dict <- try({data_dict_extract(data = dataset,as_mlstr_data_dict = as_mlstr_data_dict)},silent = TRUE)
    if(class(data_dict)[1] == "try-error") data_dict <-
        data_dict_extract(data = dataset,as_mlstr_data_dict = FALSE)}

  if(!is.logical(as_mlstr_data_dict)) stop('`as_mlstr_data_dict` must be TRUE of FALSE (TRUE by default)')
  as_data_dict_shape(data_dict)

  data_dict[['Variables']] <-
    data_dict[['Variables']] %>% fabR::add_index(.force = TRUE) %>%
    mutate(across(everything(),as.character))

  data_dict <- data_dict[c('Variables','Categories')]
  data_dict <-
    data_dict[!is.na(names(data_dict))] %>%
    lapply(function(x) x %>% mutate(across(everything(),as.character)))

  # add label, valueType and missing if don't exist
  if(as_mlstr_data_dict == TRUE){

    data_dict[['Variables']] <-
      data_dict[['Variables']] %>%
      bind_rows(tibble(valueType = as.character()))

    if(sum(nrow(data_dict[['Categories']])) > 0){
      data_dict[['Categories']] <-
        data_dict[['Categories']] %>%
        bind_rows(tibble(missing = as.character()))}}

  # check on arguments : dataset
  as_dataset(dataset)
  zap_dataset <- dataset_zap_data_dict(dataset)
  col_id <- attributes(dataset)$`Mlstr::col_id`
  if(is.null(col_id)) col_id <- names(dataset)[1]
  dataset_name <-
    ifelse(!is.null(.dataset_name),.dataset_name,
           fabR::make_name_list(as.character(fargs[['dataset']]),list_elem = list(NULL)))

  # check on argument : taxonomy
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)

  # creation of the structure of the report
  report <- list(
    `Data dictionary summary` = tibble(),
    `Data dictionary assessement` = tibble(),
    `Dataset assessement` = tibble())

  dictionary_report <- data_dict_evaluate(data_dict, as_mlstr_data_dict = as_mlstr_data_dict)
  report$`Data dictionary summary` <- dictionary_report$`Data dictionary summary`
  report$`Data dictionary assessement` <- dictionary_report$`Data dictionary assessement`

  message(
    "- DATASET ASSESSMENT: ",
    crayon::bold(dataset_name), if(dataset %>% nrow == 0) " (empty dataset)",
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
    tibble(name_var = as.character())

  message("    Assess the standard adequacy of naming")
  test_name_standards  <- check_name_standards(names(zap_dataset))

  message("    Assess the presence of variable names both in dataset and data dictionary")
  test_matching_variable <-
    check_dataset_variables(zap_dataset, data_dict) %>%
    filter(str_detect(.data$`condition`,"Variable only present")& !is.na(.data$`name_var`))

  message("    Assess the presence of possible duplicated variable in the dataset")
  if(zap_dataset %>% nrow > 0){
    test_duplicated_columns <-
      fabR::get_duplicated_cols(zap_dataset) %>%
      rename(`name_var` = .data$`name_col`)}

  message("    Assess the presence of duplicated participants in the dataset")
  if(dataset %>% nrow > 0){
    test_duplicated_rows <-
      fabR::get_duplicated_rows(zap_dataset[,names(zap_dataset) != col_id]) %>%
      mutate(
        condition = str_remove(.data$`condition`,"\\[INFO\\] - Possible duplicated observations: ")) %>%
      separate_rows(.data$`condition`,sep = " ; ") %>%
      mutate(index = as.integer(.data$`condition`)) %>%
      full_join(zap_dataset[,col_id] %>% fabR::add_index(.force = TRUE), by = "index") %>%
      filter(!is.na(.data$`condition`)) %>%
      select(`value` = !! col_id) %>%
      summarise(`value` = paste0(.data$`value`, collapse = " ; ")) %>%
      mutate(condition = "[INFO] - possible duplicated participant") %>%
      filter(.data$`value` != "")}

  if(dataset %>% nrow > 0){
    message("    Assess the presence of unique value columns in dataset")  # + table
    test_unique_value <-
      fabR::get_unique_value_cols(dataset) %>%
      rename(`name_var` = .data$`name_col`) %>%
      distinct()}

  message("    Assess the presence of empty rows in the data dictionary")
  test_empty_row <-
    fabR::get_all_na_rows(zap_dataset) %>%
    distinct %>%
    mutate(participant = str_replace_all(.data$participant,", "," ; ")) %>%
    rename(`value` = .data$`participant`) %>%
    mutate(
      condition = "[INFO] - Empty participant(s) (Except participant identifier column")

  message("    Assess the presence all NA(s) of columns in the data dictionary")
  test_empty_col <-
    fabR::get_all_na_cols(zap_dataset) %>%
    rename(`name_var` = .data$`name_col`)

  message("    Assess the presence of categories not in the data dictionary")

  test_existing_variable_category <-
    check_dataset_categories(dataset,data_dict) %>%
    distinct() %>% group_by(.data$`condition`,.data$`name_var`) %>%
    suppressMessages(summarise(`value` = paste0(.data$`value`, collapse = " ; "))) %>%
    filter(!is.na(.data$`name_var`)) %>%
    ungroup()


  if(as_mlstr_data_dict == TRUE){
    message("    Assess the `valueType` comparison in dataset and data dictionary")
    test_valueType <-
      check_dataset_valueType(zap_dataset, data_dict['Variables'],valueType_guess = TRUE)}

  # test_name_standards
  # test_matching_variable
  # test_duplicated_columns
  # test_duplicated_rows
  # test_empty_row
  # test_empty_col
  # test_unique_value
  # test_existing_variable_category
  # test_valueType

  report$`Dataset assessement` <-
    test_name_standards %>%
    bind_rows(test_matching_variable) %>%
    bind_rows(test_duplicated_columns) %>%
    bind_rows(test_duplicated_rows) %>%
    bind_rows(test_empty_row) %>%
    bind_rows(test_empty_col) %>%
    bind_rows(test_unique_value) %>%
    bind_rows(test_existing_variable_category) %>%
    bind_rows(test_valueType) %>%

    select(.data$`name_var`,
           `Quality assessment comment` = .data$`condition`, everything()) %>%
    mutate(across(everything(), ~ as.character(.))) %>%
    arrange(.data$`name_var`) %>%
    distinct()

  message("    Generate report")
  message(bold(
    "
  - WARNING MESSAGES (if any): ---------------------------------------------------\n"))

  return(report)
}

#' @title
#' Generate an Excel spreadsheet report of a study-specific datasets list
#'
#' @description
#' Generates an Excel spreadsheet report for a study-specific dataset
#' list (or study) showing descriptive statistics for each variable to
#' facilitate the assessment of input data. Statistics are generated according
#' to their valueTypes.
#' This report can be used to assist the user in the assessment of the data
#' structure, fields investigation (mandatory or not), coherence across elements
#' and taxonomy, or standard evaluation. The summary associated provides
#' dataset composition, with observation repartition and descriptive statistics.
#'
#' @details
#' A study must be a named list containing at least one data frame or
#' data frame extension (e.g. a tibble), each of them being datasets.
#' The name of each tibble will be use as the reference name of the dataset.
#'
#' A data dictionary-like structure must be a list of at least one or two
#' data frame or data frame extension (e.g. a tibble) named 'Variables'
#' and 'Categories' (if any), representing meta data of an associated dataset.
#' The 'Variables' component must contain at least 'name' column and the
#' 'Categories' component must at least contain 'variable' and 'name'
#' columns to be usable in any function of the package.
#' To be considered as a minimum (workable) data dictionary, it must also
#' have unique and non-null entries in 'name' column and the combination
#' 'name'/'variable' must also be unique in 'Categories'.
#' In addition, the data dictionary may follow Maelstrom research stardards,
#' and its content can be evaluated accordingly, such as naming convention
#' restriction, columns like 'valueType', 'missing' and 'label(:xx)',
#' and/or any taxonomy provided.
#'
#' A dataset must be a data frame or data frame extension (e.g. a tibble) and
#' can be associated to a data dictionary. If not, a minimum workable data
#' dictionary can always be generated, when any column will be reported, and
#' any factor column will be analysed as categorical variable (the column
#' 'levels' will be created for that. In addition, the dataset may follow
#' Maelstrom research stardards, and its content can be evaluated accordingly,
#' such as naming convention restriction, or id columns declaration (which
#' full completeness is mandatory.
#'
#' @param study List of tibble, each of them being study specific datasets.
#' @param taxonomy A data frame or data frame extension (e.g. a tibble),
#' identifying the scheme used for variables classification as a tibble.
#' @param as_mlstr_data_dict Whether the output data dictionary has a simple
#' data dictionary structure or not (meaning has a Maelstrom data dictionary
#' structure, compatible with Maelstrom ecosystem such as Opal environment).
#' TRUE by default.
#'
#' @return
#' A list of tibbles of report for each study-specific dataset.
#'
#' @examples
#' \donttest{
#' # Example 1: yyy yyy yyy.
#' dataset <- study_TOKYO %>% mutate(dob = fabR::as_any_date(dob, format = ""mdy""))
#' data_dict <- dd_TOKYO_format_maelstrom_tagged
#' study_assessement_report(dataset, data_dict)
#' }
#'
#' @import dplyr stringr tidyr
#' @importFrom magrittr %>%
#' @importFrom crayon bold
#' @importFrom rlang .data
#' @export
study_evaluate <- function(study, taxonomy = NULL, as_mlstr_data_dict = TRUE){

  # amelioration :rajouter taxonomy

  # check on arguments
  as_study(study)
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  if(!is.logical(as_mlstr_data_dict)) stop('`as_mlstr_data_dict` must be TRUE of FALSE (TRUE by default)')

  report_list <-
    vector(mode = "list", length = length(names(study)))
  names(report_list) <- names(study)

  message(crayon::bold(
    "- STUDY ASSESSMENT: -------------------------------------------------------------"))

  for(i in 1:length(study)){
    # stop()}
    report_list[[i]] <-
      dataset_evaluate(
        dataset = study[[i]],
        taxonomy = taxonomy,
        .dataset_name = names(study[i]),
        as_mlstr_data_dict = as_mlstr_data_dict)
  }

  return(report_list)
}

#' @title
#' Generate a report as an Excel spreadsheet of a study-specific data dictionary
#'
#' @description
#' Generates an Excel spreadsheet report for a data dictionary
#' for each variable to facilitate the assessment of input data.
#' This report can be used to assist the user in the assessment of the data
#' structure, fields investigation (mandatory or not), coherence across elements
#' and taxonomy, or standard evaluation. The summary associated provides data
#' dictionary composition, and evaluates coherence between elements.
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
#' In addition, the data dictionary may follow Maelstrom research stardards,
#' and its content can be evaluated accordingly, such as naming convention
#' restriction, columns like 'valueType', 'missing' and 'label(:xx)',
#' and/or any taxonomy provided.
#'
#' A taxonomy must be a data frame or data frame extension (e.g. a tibble).
#' The taxonomy must be compatible with (and generally extracted from) an
#' Opal environment, and must contain at least 'taxonomy', 'vocabulary' and
#' 'terms' to work with some specific functions. In addition, the taxonomy
#' may follow Maelstrom research stardards, and its content can be evaluated
#' accordingly, such as naming convention restriction, tagging elements,
#' or scales, which are specific to Maelstrom Research. In this particular
#' case, the tibble must also contain 'vocabulary_short', 'taxonomy_scale',
#' 'vocabulary_scale' and 'term_scale' to work with some specific functions.
#'
#' @param data_dict A list of tibble(s) representing meta data to be evaluated.
#' @param taxonomy A tibble identifying the scheme used for variables
#' classification as a tibble.
#' @param as_mlstr_data_dict Whether the output data dictionary has a simple
#' data dictionary structure or not (meaning has a Maelstrom data dictionary
#' structure, compatible with Maelstrom ecosystem such as Opal environment).
#' FALSE by default.
#'
#' @return
#' A list of tibbles of report for one study-specific data dictionary.
#'
#' @examples
#' \donttest{
#' # Example 1: yyy yyy yyy.
#' dataset <- study_TOKYO %>% mutate(dob = fabR::as_any_date(dob, format = ""mdy""))
#' data_dict <- dd_TOKYO_format_maelstrom_tagged
#' study_assessement_report(dataset, data_dict)
#' }
#'
#' @import dplyr tidyr stringr fabR
#' @importFrom magrittr %>%
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
data_dict_evaluate <- function(data_dict, taxonomy = NULL, as_mlstr_data_dict = TRUE){

  fargs <- as.list(match.call(expand.dots = TRUE))

  data_dict_name <-
    suppressWarnings(fabR::make_name_list(args_list = fargs['data_dict'], list(NULL)))

  # check args
  if(!is.logical(as_mlstr_data_dict)) stop('`as_mlstr_data_dict` must be TRUE of FALSE (TRUE by default)')

  # check on arguments : data dict
  as_data_dict_shape(data_dict)
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>% fabR::add_index(.force = TRUE) %>%
    mutate(across(everything(),as.character))

  data_dict <- data_dict[c('Variables','Categories')]
  data_dict <-
    data_dict[!is.na(names(data_dict))] %>%
    lapply(function(x) x %>% mutate(across(everything(),as.character)))

  # add label, valueType and missing if don't exist
  if(as_mlstr_data_dict == TRUE){

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

      if(length(names(
        data_dict[['Categories']] %>%
        select(matches(c("^label$","^label:[[:alnum:]]"))))) == 0){

        data_dict[['Categories']] <-
          data_dict[['Categories']] %>%
          bind_rows(tibble(label = as.character()))
      }
    }
  }

  # check on arguments : taxonomy
  if(!is.null(taxonomy)) taxonomy <- as_taxonomy(taxonomy)


  message(
    "- DATA DICTIONARY ASSESSMENT: ",crayon::bold(data_dict_name)," --------------------------")

  # creation of the structure of the report
  report <- list(
    `Data dictionary summary` = tibble(),
    `Data dictionary assessement` = tibble())

  report$`Data dictionary summary` <- suppressWarnings(data_dict_flatten(data_dict))
  report$`Data dictionary summary` <-
    report$`Data dictionary summary`[['Variables']] %>%
    select(.data$`index`,.data$`name`,matches(c("^label$","^label:[[:alnum:]]"))[1],
           matches('^valueType$'),starts_with("Categories::"),everything())

  test_name_standards <-
    test_unique_variable <-
    test_duplicated_columns <-
    # test_duplicated_rows <-
    test_empty_row <-
    test_empty_col <-
    test_existing_variable_category <-
    test_var_label <-
    test_valueType <-
    test_cat_label <-
    test_missing_category <-
    tibble(name_var = as.character())

  message("    Assess the standard adequacy of naming")
  test_name_standards  <-
    check_name_standards(data_dict[['Variables']][['name']]) %>%
    mutate(
      name_col = "name",
      sheet    = "Variables") %>%
    bind_rows(
      if(sum(nrow(data_dict[['Categories']])) > 0 ){
        check_name_standards(data_dict[['Categories']][['variable']]) %>%
          mutate(
            name_col = "name",
            sheet    = "Categories")
      }else{tibble(name_var = as.character())})

  message("    Assess the uniqueness of variable names")
  test_unique_variable <-
    check_data_dict_variables(data_dict) %>%
    mutate(
      name_col = "name",
      sheet    = "Variables")

  message("    Assess the presence of possible duplicated columns")
  test_duplicated_columns <-
    suppressWarnings(fabR::get_duplicated_cols(data_dict[['Variables']])) %>%
    mutate(sheet    = "Variables") %>%
    bind_rows(
      if(sum(nrow(data_dict[['Categories']])) > 0 ){
        fabR::get_duplicated_cols(data_dict[['Categories']]) %>%
          mutate(sheet    = "Categories")
      }else{tibble()})

  # message("    Assess the presence of duplicated variable in the dataset")
  # test_duplicated_rows <-
  #   fabR::get_duplicated_rows(data_dict[['Variables']] %>% select(-.data$`name`)) %>%
  #   mutate(
  #     condition = str_remove(.data$`condition`,"\\[INFO\\] - Possible duplicated observations: ")) %>%
  #   separate_rows(.data$`condition`,sep = " ; ") %>%
  #   mutate(index = as.integer(.data$`condition`)) %>%
  #   full_join(data_dict[['Variables']] %>% fabR::add_index(.force = TRUE), by = "index") %>%
  #   filter(!is.na(.data$`condition`)) %>%
  #   select(col_name = .data$`name`) %>%
  #   summarise(col_name = paste0(.data$`col_name`, collapse = " ; ")) %>%
  #   mutate(
  #     condition = "[INFO] - possible duplicated rows (variables)",
  #     sheet    = "Variables")

  message("    Assess the presence of empty rows in the data dictionary")
  test_empty_row <-
    data_dict[['Variables']] %>% select(.data$`name`, everything()) %>%
    fabR::get_all_na_rows() %>%
    rename(name_col = .data$`participant`) %>%
    mutate(
      condition = "[INFO] - Empty line(s)",
      sheet    = "Variables")

  message("    Assess the presence of columns in the data dictionary")
  test_empty_col <-
    fabR::get_all_na_cols(data_dict[['Variables']] %>% select(-.data$`name`)) %>%
    mutate(sheet    = "Variables") %>%
    bind_rows(
      if(sum(nrow(data_dict[['Categories']])) > 0 ){
        fabR::get_all_na_cols(data_dict[['Categories']] %>% select(-.data$`variable`)) %>%
          mutate(sheet    = "Categories")
      }else{tibble()})

  if(sum(nrow(data_dict[['Categories']])) > 0){

    message("    Assess the presence of categories not in the data dictionary")
    test_existing_variable_category <-
      suppressWarnings(check_data_dict_categories(data_dict)) %>%
      mutate(
        name_col = "variable",
        sheet    = "Categories")
  }


  if(as_mlstr_data_dict == TRUE){

    message("    Assess the non-missingness of `label(:xx)` column in 'Variables'")
    test_var_label <-
      data_dict[['Variables']] %>%
      select(.data$`name`, label = matches(c("^label$","^label:[[:alnum:]]"))[1]) %>%
      # filter(if_any(-.data$`label`, ~ is.na(.))) %>%
      pivot_longer(cols = !.data$`name`,names_to = "name_col", values_to = "value") %>%
      filter(is.na(.data$value)) %>%
      select(name_var = .data$`name`, .data$`name_col`, -.data$value) %>%
      mutate(
        name_col = names(data_dict[['Variables']] %>% select(matches(c("^label$","^label:[[:alnum:]]"))[1])),
        condition = paste0("[INFO] - The column `",.data$`name_col`,"` should exist and not contain 'NA' values")) %>%
      mutate(sheet    = "Variables")

    message("    Assess the `valueType` column in 'Variables'")
    test_valueType <-
      check_data_dict_valueType(data_dict) %>%
      mutate(
        name_col = "valueType",
        sheet    = "Variables")

    if(sum(nrow(data_dict[['Categories']])) > 0){

      message("    Assess presence and non-missingness of `label(:xx)` column in 'Categories'")
      test_cat_label <-
        data_dict[['Categories']] %>%
        select(.data$`name`, label = matches(c("^label$","^label:[[:alnum:]]"))[1]) %>%
        # filter(if_any(-.data$`label`, ~ is.na(.))) %>%
        pivot_longer(cols = !.data$`name`,names_to = "name_col", values_to = "value") %>%
        filter(is.na(.data$value)) %>%
        select(name_var = .data$`name`, .data$`name_col`, -.data$value) %>%
        mutate(
          name_col = names(data_dict[['Variables']] %>% select(matches(c("^label$","^label:[[:alnum:]]"))[1])),
          condition = paste0("[INFO] - The column `",.data$`name_col`,"` should exist not contain 'NA' values")) %>%
        mutate(sheet    = "Categories")

      test_missing_category <-
        message("    Assess the logical values of missing column in Categories")
      if(as_mlstr_data_dict){
        check_data_dict_missing_categories(data_dict) %>%
          mutate(
            name_col = "missing",
            sheet    = "Categories")}else{tibble()}
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

  report$`Data dictionary assessement` <-
    test_name_standards %>%
    bind_rows(test_unique_variable) %>%
    bind_rows(test_duplicated_columns) %>%
    # bind_rows(test_duplicated_rows) %>%
    bind_rows(test_empty_row) %>%
    bind_rows(test_empty_col) %>%
    bind_rows(test_existing_variable_category) %>%
    bind_rows(test_var_label) %>%
    bind_rows(test_valueType) %>%
    bind_rows(test_cat_label) %>%
    bind_rows(test_missing_category) %>%

    select(.data$`sheet`,.data$`name_col`,.data$`name_var`,
           `Quality assessment comment` = .data$`condition`) %>%
    arrange(desc(.data$`sheet`),.data$`name_col`,.data$`name_var`) %>%
    mutate(across(everything(), ~ as.character(.))) %>%
    distinct()

  message("    Generate report")
  message(bold(
    "
  - WARNING MESSAGES (if any): --------------------------------------------\n"))

  return(report)
}


