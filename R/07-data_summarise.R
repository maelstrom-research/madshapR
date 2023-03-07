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
#' @param valueType_guess Whether the output should include refind valueType
#' that can be applied to the dataset. FALSE by default.
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
#' @import dplyr stringr tidyr fabR
#' @importFrom magrittr %>%
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
dataset_summarize <- function(dataset, data_dict = NULL, taxonomy = NULL, .dataset_name = NULL, valueType_guess = FALSE){

  fargs <- as.list(match.call(expand.dots = TRUE))

  if(is.null(data_dict)) {data_dict <- data_dict_extract(data = dataset,as_mlstr_data_dict = TRUE)
  }else{data_dict <- as_mlstr_data_dict(data_dict)}

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
    `Overview` = c(),
    `Data dictionary summary` = tibble(),
    `Data dictionary assessement` = tibble(),
    `Dataset assessement` = tibble(),
    `Variables summary (all)` = tibble(),
    `Text variable summary` = tibble(),
    `Date variable summary` = tibble(),
    `Numerical variable summary` = tibble(),
    `Categorical variable summary` = tibble())

  dataset_report <- dataset_evaluate(dataset,data_dict,as_mlstr_data_dict = TRUE)
  report$`Data dictionary summary`     <- dataset_report$`Data dictionary summary`
  report$`Data dictionary assessement` <- dataset_report$`Data dictionary assessement`
  report$`Dataset assessement`         <- dataset_report$`Dataset assessement`

  message(
    "- DATASET SUMMARIZE: ",
    crayon::bold(dataset_name), if(dataset %>% nrow == 0) " (empty dataset)",
    " --------------------------")

  dataset_valueType <-
    dataset %>%
    summarise(across(everything(), ~ valueType_of(.))) %>% pivot_longer(cols = everything()) %>%
    rename(name_var = .data$`name`, `Actual dataset valueType` = .data$`value`)

  if(valueType_guess == TRUE){
    estimated_valueType <-
      dataset %>%
      summarise(across(everything(), ~ valueType_guess(.))) %>% pivot_longer(cols = everything()) %>%
      rename(name_var = .data$`name`, `Estimated dataset valueType` = .data$`value`)
  }else{
    estimated_valueType <-
      dataset_valueType %>%
      select(.data$`name_var`, `Estimated dataset valueType` = .data$`Actual dataset valueType`)}

  ## variables
  data_dict_var <-
    data_dict[['Variables']] %>%
    select(-matches("^name_var$")) %>%
    rename(name_var = .data$`name`) %>%
    fabR::add_index("index in data dict.", .force = TRUE) %>%
    mutate(across(everything(),as.character)) %>%
    select(.data$`index in data dict.`,
           .data$`name_var`,
           .data$`name_var`,
           matches(c("^label$","^label:[[:alnum:]]"))[1],
           `Data Dictionary valueType` = .data$`valueType`) %>%
    full_join(estimated_valueType, by = "name_var") %>%
    full_join(dataset_valueType, by = "name_var")

  ## categories
  if(sum(nrow(data_dict[['Categories']])) > 0){
    data_dict_cat <-
      data_dict[['Categories']] %>%
      select(-matches("^name_var$")) %>%
      rename(name_var = .data$`variable`) %>%
      mutate(missing = ifelse(.data$`missing` == TRUE, "Missing categorical values :", "Valid categorical values :")) %>%
      unite("Categories in data dictionary", .data$`name`,matches(c("^label$","^label:[[:alnum:]]"))[1], sep = " = ",remove = TRUE) %>%
      group_by_at(vars(c(-.data$`Categories in data dictionary`))) %>%
      summarise(across(c(.data$`Categories in data dictionary`),~ paste0(.,collapse = "\n")),.groups = "drop") %>%
      arrange(.data$`name_var`,desc(.data$`missing`)) %>%
      unite("Categories in data dictionary", .data$`missing`,.data$`Categories in data dictionary`, sep = "\n",remove = TRUE) %>%
      group_by_at(vars(c(-.data$`Categories in data dictionary`))) %>%
      summarise(across(c(.data$`Categories in data dictionary`),~ paste0(.,collapse = "\n\n")),.groups = "drop") %>%
      # mutate(`Categories in data dictionary` = ifelse(is.na(.data$`name_var`), NA, .data$`Categories in data dictionary`)) %>%
      ungroup
  }else{
    data_dict[['Categories']] <-
      tibble(
        name = as.character(),
        variable = as.character(),
        missing = as.logical())
    data_dict_cat <- tibble(name_var = as.character(),`Categories in data dictionary` = as.character())}

  report$`Variables summary (all)` <- data_dict_var %>% left_join(data_dict_cat, by = "name_var")

  message("    Summarize the data type of each variable across the dataset")

  ### SUMMARIZE VARIABLES VALUES ###

  vT <- datashapR::valueType_list
  vT_text <- vT[vT$`genericType` == 'character',][['valueType']]
  report$`Text variable summary` <-
    report$`Variables summary (all)`[
      report$`Variables summary (all)`$`Estimated dataset valueType` %in% vT_text,]

  vT_num  <- vT[vT$`genericType` == 'numeric',][['valueType']]
  report$`Numerical variable summary` <-
    report$`Variables summary (all)`[
      report$`Variables summary (all)`$`Estimated dataset valueType` %in% vT_num,]

  vT_date <- vT[vT$`genericType` == 'date',][['valueType']]
  report$`Date variable summary` <-
    report$`Variables summary (all)`[
      report$`Variables summary (all)`$`Estimated dataset valueType` %in% vT_date,]

  report$`Categorical variable summary` <-
    report$`Variables summary (all)`[
      !is.na(report$`Variables summary (all)`$`Categories in data dictionary`),]

  if(nrow(zap_dataset) > 0){

    message("    Summarise information for all variables")
    .resume_var <- resume_variables(dataset,data_dict)
    summary_var <-
      summary_variables(.resume_var = .resume_var) %>%
      rename(name_var = .data$`name`)

    message("    Summarise information for text variables")
    .resume_var_text <-
      .resume_var[.resume_var$`name` %in% report$`Text variable summary`$name_var,]
    summary_text <-
      summary_variables_text(.resume_var = .resume_var_text) %>%
      rename(name_var = .data$`name`)

    message("    Summarise information for date variables")
    .resume_var_date <-
      .resume_var[.resume_var$`name` %in% report$`Date variable summary`$name_var,]
    summary_date <- # only works for ymd format
      summary_variables_date(.resume_var = .resume_var_date) %>%
      rename(name_var = .data$`name`)
      # yyy yyy yyy

    message("    Summarise information for numerical variables")
    .resume_var_num <-
      .resume_var[.resume_var$`name` %in% report$`Numerical variable summary`$name_var,]
    summary_num <-
      summary_variables_numerical(.resume_var = .resume_var_num) %>%
      rename(name_var = .data$`name`)

    message("    Summarise information for categorical variables")
    .resume_var_cat <-
      .resume_var[.resume_var$`name` %in% report$`Categorical variable summary`$name_var,]
    summary_cat <-
      summary_variables_categorical(.resume_var = .resume_var_cat) %>%
      rename(name_var = .data$`name`)

    report$`Variables summary (all)` <-
      report$`Variables summary (all)` %>%
      left_join(summary_var, by = "name_var")

    report$`Text variable summary` <-
      report$`Text variable summary` %>%
      left_join(summary_var, by = "name_var") %>%
      left_join(summary_text, by = "name_var")

    report$`Date variable summary` <-
      report$`Date variable summary` %>%
      left_join(summary_var, by = "name_var") %>%
      left_join(summary_date, by = "name_var")

    report$`Numerical variable summary` <-
      report$`Numerical variable summary` %>%
      left_join(summary_var, by = "name_var") %>%
      left_join(summary_num, by = "name_var")

    report$`Categorical variable summary` <-
      report$`Categorical variable summary` %>%
      left_join(summary_var, by = "name_var") %>%
      left_join(summary_cat, by = "name_var")

    # report$`Date variable summary` <-
    #   report$`Data dictionary summary` %>%
    #   filter(.data$`name` %in% var_date) %>%
    #   select(.data$`index in data dict.`, .data$`name`, starts_with("label:"),
    #          .data$`valueType`,.data$`Dataset valueType`,matches("Mix cat. and other values"),
    #          .data$`Quality assessment comment`) %>%
    #   left_join(summary, by = "name") %>%
    #   inner_join(summary_date, by = "name") %>%
    #   mutate(across(matches("Mix cat. and other values"),
    #                 `% Missing categorical values (if applicable)` =
    #                   ifelse(.data$`Mix cat. and other values` == "YES",
    #                          .data$`% Missing categorical values (if applicable)`,NA)))}

    # var_categorical <-
    #   report$`Data dictionary summary` %>%
    #   filter(!is.na(.data$`Categories::missing`)) %>%
    #   pull(.data$`name`)
    #
    # .resume_var_cat <- .resume_var[.resume_var$`name` %in% report$`Categorical variable summary`$name_var,]
    #
    #
    #
    # if(length(var_categorical)){
    #
    #   message("    Summarise information for categorical variables")
    #   summary_categorical <- tibble(name = as.character())
    #   for(var_c in var_categorical){
    #     summary_categorical <-
    #       summary_categorical %>%
    #       bind_rows(
    #         summary_variables_categorical_V1(
    #           name_var = zap_dataset %>% select(all_of(var_c)),
    #           dict_var = data_dict$Categories %>% filter(.data$`variable` == var_c)) %>%
    #           rename(name = .data$`name_var`))}
    #
    #   report$`Categorical variable summary` <- summary_categorical
    #
    # }
    # report$`Data dictionary summary` %>%
    # filter(!is.na(.data$`Categories::missing`)) %>%
    # select(.data$`index in data dict.`, .data$`name`,
    #        starts_with("label:"), .data$`valueType`, .data$`Dataset valueType`,
    #        matches("Mix cat. and other values"), .data$`Quality assessment comment`,
    #        .data$`Categories in data dictionary`) %>%
    # left_join(summary, by = "name") %>%
    # inner_join(summary_categorical, by = "name") %>%
    # select(everything(),
    #        -matches("Categories in data dictionary"),-matches("% Valid categorical values"),-matches("Values present in dataset"),
    #        -matches("Data dictionnary categories not present in dataset"), -matches("Dataset value not present in data dictionnary"),
    #
    #        matches("% Valid categorical values"),matches("Categories in data dictionary"),matches("Values present in dataset"),
    #        matches("Data dictionnary categories not present in dataset"),matches("Dataset value not present in data dictionnary"))}
  }

  message("    Summarise global information (Overview)")
  report$Overview <-
    tibble(`---` = c(
      'Quality control of study specific dataset'                                ,
      'Date'                                                                     ,
      '1_Name of the dataset'                                            ,
      '    1_Participant identifier'                                             ,
      '    1_Variables'                                                          ,
      '        1_Total number of variables'                                      ,
      '    1_Data type in dictionnary (valueType)'                                                         ,
      '        1_Nb. text variables'                                             ,
      '        1_Nb. date variables'                                             ,
      '        1_Nb. numerical variables'                                        ,
      '        1_Nb. categorical variables'                                      ,
      '    2_Rows'                                                               ,
      '        2_Total number of observations'                                   ,
      '        2_Nb. unique participants'                                        ))

  report$Overview <-
    report$Overview %>%
    mutate(`-----` = case_when(

      .data$`---` == 'Quality control of study specific dataset'                      ~ " ",
      .data$`---` == 'Date'                                                           ~ Sys.Date() %>% as.character,
      .data$`---` == '1_Name of the dataset'                                          ~ dataset_name %>% str_remove_all("`"),
      .data$`---` == '    1_Participant identifier'                                   ~ data_dict$Variables$name[1],
      .data$`---` == '    1_Variables'                                                ~ " ",
      .data$`---` == '        1_Total number of variables'                            ~ report$`Data dictionary summary`    %>% nrow() %>% as.character,
      .data$`---` == '    1_Data type in dictionnary (valueType)'                     ~ " ",
      .data$`---` == '        1_Nb. text variables'                                   ~ report$`Text variable summary`            %>% nrow() %>% as.character,
      .data$`---` == '        1_Nb. date variables'                                   ~ report$`Date variable summary`            %>% nrow() %>% as.character,
      .data$`---` == '        1_Nb. numerical variables'                              ~ report$`Numerical variable summary` %>% nrow() %>% as.character,
      .data$`---` == '        1_Nb. categorical variables'                            ~ report$`Categorical variable summary`    %>% nrow() %>% as.character,
      .data$`---` == '    2_Rows'                                                     ~ " ",
      .data$`---` == '        2_Total number of observations'                         ~ dataset %>% select(1) %>% nrow %>% as.character,
      .data$`---` == '        2_Nb. unique participants'                              ~ dataset %>% select(1) %>% distinct() %>% nrow %>% as.character,
      TRUE                                                                                ~ "EMPTY",
    )) %>%
    mutate(
      `---` = str_remove_all(.data$`---`, "1_"),
      `---` = str_remove_all(.data$`---`, "2_")) %>%
    rename(
      `Quality control of study specific dataset` = .data$`---`,
      ` ` = .data$`-----`) %>% slice(-1)

  message("    Generate report\n")

  # create report structure

  if(nrow(report$`Text variable summary`       ) == 0) report$`Text variable summary`       <- NULL
  if(nrow(report$`Date variable summary`       ) == 0) report$`Date variable summary`       <- NULL
  if(nrow(report$`Numerical variable summary`  ) == 0) report$`Numerical variable summary`  <- NULL
  if(nrow(report$`Categorical variable summary`) == 0) report$`Categorical variable summary`<- NULL

  return(report)
}

#' @title
#' Generate an Excel spreadsheet report of a study-specific datasets list
#'
#' @description
#' Generates an Excel spreadsheet report for a study-specific dataset
#' list (or study) showing descriptive statistics for each variable to facilitate
#' the assessment of input data. Statistics are generated according to their
#' valueTypes.
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
#' @param valueType_guess Whether the output should include refind valueType
#' that can be applied to the dataset. TRUE by default.
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
study_summarize <- function(study, taxonomy = NULL, valueType_guess = TRUE){

  # amelioration :rajouter taxonomy

  # check on arguments
  as_study(study)
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  if(!is.logical(valueType_guess)) stop('`as_mlstr_data_dict` must be TRUE of FALSE (TRUE by default)')

  report_list <-
    vector(mode = "list", length = length(names(study)))
  names(report_list) <- names(study)

  message(crayon::bold(
    "- STUDY SUMMARY: -------------------------------------------------------"))

  for(i in seq_len(length(study))){
    # stop()}
    report_list[[i]] <-
      dataset_summarize(
        dataset = study[[i]],
        taxonomy = taxonomy,
        .dataset_name = names(study[i]),
        valueType_guess = valueType_guess)
  }

  return(report_list)
}


#' @title
#' Generate a tibble resuming all variables present in a dataset
#'
#' @description
#' Generates a tibble that aggregates all columns
#' in a dataset with (if any) its data dictionary. The data dictionary (if
#' present separates observations between open values, missing values,
#' categorical values , and categorical missing values (which corresponds to the
#' 'missing' column in the 'Categories' sheet).
#' This internal function is used inside summary functions.
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
#' A dataset must be a data frame or data frame extension (e.g. a tibble) and
#' can be associated to a data dictionary. If not, a minimum workable data
#' dictionary can always be generated, when any column will be reported, and
#' any factor column will be analysed as categorical variable (the column
#' 'levels' will be created for that. In addition, the dataset may follow
#' Maelstrom research stardards, and its content can be evaluated accordingly,
#' such as naming convention restriction, or id columns declaration (which
#' full completeness is mandatory.
#'
#' @seealso
#' [datashapR::summary_variables()]
#'
#' @param data A tibble identifying the input data observations associated to
#' its data dictionary.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset. Automatically generated if not provided.
#'
#' @return
#' A tibble providing summary elements of a dataset, including its values and
#' data dictionary elements.
#'
#' @examples
#' \donttest{
#' # Example 1: yyy yyy yyy.
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
resume_variables <- function(data, data_dict = NULL){

  # handle atomics
  summary_tbl <-
    tibble(
      `index` = as.integer(),
      `name` = as.character(),
      `categorical` = as.character(),
      `valid_class` = as.character(),
      `value_var_occur` = as.numeric(),
      `value_var` = as.character(),
      `index_value` = as.integer(),
      `cat_index` = as.integer())

  # handle atomics
  if(is.atomic(data) & length(data) == 0){return(summary_tbl)}
  if(is.atomic(data)){return(resume_variables(data = tibble(name = data), data_dict))}

  # tests
  dataset <- as_dataset(data,attributes(data)$`Mlstr::col_id`)

  # if no data dict
  if(is.null(data_dict)){
    data_dict <- data_dict_extract(data,as_mlstr_data_dict = TRUE)
  }else{
    data_dict <- as_mlstr_data_dict(data_dict)}

  data_dict_var  <-
    data_dict[['Variables']] %>%
    select(.data$`name`) %>%
    mutate(categorical = NA_character_) %>%
    fabR::add_index()

  if(sum(nrow(data_dict[['Categories']])) > 0){
    data_dict_cat <-
      data_dict[['Categories']] %>%
      select(name = .data$`variable`, value_var = .data$`name`,
             valid_class = .data$`missing`) %>%
      group_by(.data$`name`, .data$`valid_class`) %>%
      fabR::add_index('cat_index') %>%
      ungroup() %>%
      mutate(
        valid_class = ifelse(.data$`valid_class` == TRUE, "2_Missing values", "1_Valid values")) %>%
      mutate(value_var = as.character(.data$`value_var`))

  }else{
    data_dict_cat <-
      tibble(cat_index = as.character(),name = as.character(),
             value_var = as.character(),valid_class = as.character())}

  data_dict_var  <-
    data_dict_var %>%
    full_join(data_dict_cat,by = "name") %>%
    mutate(categorical = ifelse(is.na(.data$`valid_class`),"no","yes"))

  summary <- tibble(name = as.character())

  if(nrow(dataset) > 0){
    for(i in names(dataset)){
      # stop()}

      tbl_var <- tibble()
      category_var <- tibble()
      summary <- tibble()

      # count the different observations in the columm
      tbl_var <-
        dataset %>% select(value_var = any_of(i)) %>%
        mutate(value_var_occur = 1) %>%
        mutate(value_var = as.character(.data$`value_var`)) %>%
        mutate(name = i)

      # count observations in the dataset, including no-observation of categorical outcomes
      # classification of observations not in the data dict(3) and 'true' NA as na values(4)
      # arrange (1),(2),(3),(4), by original index
      summary   <-
        tbl_var %>%
        full_join(data_dict_var[data_dict_var$`name` == i,],by = c("value_var", "name")) %>%
        mutate(value_var_occur = replace_na(.data$`value_var_occur`, 0)) %>%
        fill(
          .data$`index`,.direction = "downup") %>%
        mutate(
          valid_class =
            case_when(
              is.na(.data$`value_var`)  ~ "4_NA values",
              TRUE ~ ifelse(is.na(.data$`valid_class`),"3_Valid other values",.data$`valid_class`))) %>%
        arrange(.data$`valid_class`) %>%
        group_by(.data$`valid_class`) %>%
        fabR::add_index('index_value', .force = TRUE) %>%
        ungroup

      # handle categories which are non-categorical, categorical and mixed
      summary$`categorical` <-
        ifelse(
          nrow(summary[summary$`valid_class` ==     '3_Valid other values',])               >  0 &
          nrow(summary[summary$`valid_class` %in% c('1_Valid values','2_Missing values'),]) >= 1,
          "mix", unique(summary[!is.na(summary$`categorical`),][['categorical']]))

      summary_tbl <- bind_rows(summary_tbl, summary)
    }
  }

  # data_dict_var <-
  #   data_dict_var %>%
  #   anti_join(summary_tbl['name'], by = "name")

  # summary_tbl <-
  #   summary_tbl %>%
  #   add_row(data_dict_var)

  final_resume <-
    summary_tbl %>%
    mutate(value_var_occur = replace_na(.data$`value_var_occur`, 0)) %>%
    mutate(valid_class = replace_na(.data$`valid_class`, '3_Valid other values')) %>%
    filter(!(.data$`value_var_occur` == 0 & .data$`valid_class` == "4_NA values")) %>%
    select(.data$index,
           .data$`name`, everything()) %>%
    arrange(.data$`index`,.data$`valid_class`)

  return(final_resume)

}

#' @title
#' Provide statistical description of variables present in a dataset
#'
#' @description
#' Generates a tibble that summarises all columns
#' in a dataset with (if any) its data dictionary. The data dictionary (if present
#' separates observations between open values, missing values, categorical values ,
#' and categorical missing values (which corresponds to the 'missing' column in the
#' 'Categories' sheet). Statistics are generated according to their valueTypes.
#' This summary can be used to assist the user in the assessment of the dataset
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
#' A dataset must be a data frame or data frame extension (e.g. a tibble) and
#' can be associated to a data dictionary. If not, a minimum workable data
#' dictionary can always be generated, when any column will be reported, and
#' any factor column will be analysed as categorical variable (the column
#' 'levels' will be created for that. In addition, the dataset may follow
#' Maelstrom research stardards, and its content can be evaluated accordingly,
#' such as naming convention restriction, or id columns declaration (which
#' full completeness is mandatory.
#'
#' @param data A tibble identifying the input data observations associated to
#' its data dictionary.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset. Automatically generated if not provided.
#' @param .resume_var A tibble which provides summary of the variables (for
#' development purpose only)
#'
#' @return
#' A tibble providing statistical description of variables present in
#' a dataset.
#'
#' @examples
#' \donttest{
#' # Example 1: yyy yyy yyy.
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
summary_variables <- function(data = NULL, data_dict = NULL, .resume_var = NULL){

  # for dev purpose
  if(is.null(.resume_var)) .resume_var <- resume_variables(data, data_dict)
  summary <- .resume_var

  # init
  summary_tbl <- tibble(name = as.character())
  if(nrow(summary) == 0) return(summary_tbl)

  for(i in unique(summary$name)){
    # stop()}

    summary_i <- summary %>% filter(.data$`name` == i)

    # summary the output
    summary_i <-
      tibble(
        `name` = i,

        `categorical` = unique(summary_i$`categorical`),

        `Total number of observations` = sum(summary_i$`value_var_occur`),

        `Nb. distinct values` =
          length(unique(summary_i[summary_i$`value_var_occur` == 1 & !is.na(summary_i$`value_var`),]$`value_var`)),

        `% total Valid values` =
          sum(summary_i[summary_i$`valid_class` %in% c("1_Valid values","3_Valid other values"),c('value_var_occur')])/
          sum(summary_i$`value_var_occur`),

        `% NA` =
          sum(summary_i[summary_i$`valid_class` %in% c("4_NA values"),c('value_var_occur')])/
          sum(summary_i$`value_var_occur`),

        `% Valid categorical values (if applicable)` =
          ifelse(all(summary_i$`categorical` != 'no'),
                 sum(summary_i[summary_i$`valid_class` %in% c("1_Valid values"),c('value_var_occur')])/
                 sum(summary_i$`value_var_occur`),
                 NA_real_),

        `% Missing categorical values (if applicable)` =
          ifelse(all(summary_i$`categorical` != 'no'),
                 sum(summary_i[summary_i$`valid_class` %in% c("2_Missing values"),c('value_var_occur')])/
                   sum(summary_i$`value_var_occur`),
                 NA_real_)
      )

    summary_tbl <- bind_rows(summary_tbl, summary_i)
  }

  summary_tbl <-
    summary_tbl %>%
    mutate(
      `Quality assessment comment` = case_when(

        .data$`Total number of observations` == .data$`Nb. distinct values` ~
          "[INFO] - All observations are unique" ,

        .data$`Nb. distinct values` == 0                                    ~
          "[INFO] - The column is empty" ,

        .data$`Nb. distinct values` == 1                                    ~
          "[INFO] - The column has a constant value",

        .data$`Nb. distinct values` > 0 & .data$`% total Valid values` == 0 ~
          "[INFO] - All the categorical values present in the dataset are 'missing'",

        .data$`% total Valid values` > 0 & .data$`% Valid categorical values (if applicable)` == 0 ~
          "[INFO] - All the categorical values present in the dataset are non categorical",

        TRUE ~ NA_character_
      )) %>% select(.data$`name`,.data$`Quality assessment comment`, everything())

  return(summary_tbl)

}

#' @title
#' Provide statistical description of 'text' variables of a dataset
#'
#' @description
#' Generates a tibble that summarises all 'text' columns
#' in a dataset with (if any) its data dictionary. The data dictionary (if
#' present separates observations between open values, missing values,
#' categorical values, and categorical missing values (which corresponds to the
#' 'missing' column in the Categories' sheet). Statistics are generated
#' according to their valueTypes which can be 'text', 'datetime', linestring',
#' 'point', 'locale', or 'polygon'. This summary can be used to assist the user
#' in the assessment of the dataset composition, with observation repartition
#' and descriptive statistics.
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
#' A dataset must be a data frame or data frame extension (e.g. a tibble) and
#' can be associated to a data dictionary. If not, a minimum workable data
#' dictionary can always be generated, when any column will be reported, and
#' any factor column will be analysed as categorical variable (the column
#' 'levels' will be created for that. In addition, the dataset may follow
#' Maelstrom research stardards, and its content can be evaluated accordingly,
#' such as naming convention restriction, or id columns declaration (which
#' full completeness is mandatory.
#'
#' @param data A tibble identifying the input data observations associated to
#' its data dictionary.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset. Automatically generated if not provided.
#' @param .resume_var A tibble which provides summary of the variables (for
#' development purpose only)
#'
#' @return
#' A tibble providing statistical description of 'text' variables present
#' in a dataset.
#'
#' @examples
#' \donttest{
#' # Example 1: yyy yyy yyy.
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
summary_variables_text <- function(data = NULL, data_dict = NULL, .resume_var = NULL){

  # init
  summary_tbl <- tibble(name = as.character())
  if(is.null(.resume_var)) return(summary_tbl)

  summary <-
    .resume_var %>%
    filter(.data$`value_var_occur` == 1 ) %>%
    filter(.data$`valid_class`  == "3_Valid other values")

  # init
  summary_tbl <- tibble(name = as.character())
  if(nrow(summary) == 0) return(summary_tbl)

  for(i in unique(summary$name)){
    # stop()}

    summary_i <-
      summary %>%
      filter(.data$`name` == i)

    # turn the output to be readable
    if(summary_i %>% nrow > 0){
      summary_i <-
        tibble(
          `name` = i,

          `Most seen value(s)` =
            summary_i %>%
            count(.data$`value_var`) %>%
            filter(if_any(.data$`n`, ~ . == max(.))) %>%
            pull(.data$`value_var`) %>% toString(),

          `Rarest value(s)` =
            summary_i %>%
            count(.data$`value_var`) %>%
            filter(if_any(.data$`n`, ~ . == min(.))) %>%
            pull(.data$`value_var`) %>% toString(),
        )

    }else{summary_i <- tibble(name = as.character())}

    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }

  # final_summary <-
  #   summary_variables(data, data_dict, .resume_var) %>%
  #   filter(.data$`categorical` != 'yes') %>%
  #   full_join(summary_tbl, by = 'name')

  return(summary_tbl)
}

#' @title
#' Provide statistical description of 'date' variables of a dataset
#'
#' @description
#' Generates a tibble that summarises all 'date' columns
#' in a dataset with (if any) its data dictionary. The data dictionary (if
#' present separates observations between open values, missing values,
#' categorical values, and categorical missing values (which corresponds to the
#' 'missing' column in the Categories' sheet). Statistics are generated
#' according to their valueTypes which can be 'date'.
#' This summary can be used to assist the user in the assessment of the dataset
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
#' A dataset must be a data frame or data frame extension (e.g. a tibble) and
#' can be associated to a data dictionary. If not, a minimum workable data
#' dictionary can always be generated, when any column will be reported, and
#' any factor column will be analysed as categorical variable (the column
#' 'levels' will be created for that. In addition, the dataset may follow
#' Maelstrom research stardards, and its content can be evaluated accordingly,
#' such as naming convention restriction, or id columns declaration (which
#' full completeness is mandatory.
#'
#' @param data A tibble identifying the input data observations associated to
#' its data dictionary.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset. Automatically generated if not provided.
#' @param .resume_var A tibble which provides summary of the variables (for
#' development purpose only)
#'
#' @return
#' A tibble providing statistical description of 'date' variables present
#' in a dataset.
#'
#' @examples
#' \donttest{
#' # Example 1: yyy yyy yyy.
#' }
#'
#' @import dplyr tidyr lubridate fabR
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
summary_variables_date <- function(data = NULL, data_dict = NULL, .resume_var = NULL){

  # init
  summary_tbl <- tibble(name = as.character())
  if(is.null(.resume_var)) return(summary_tbl)

  date_format <-
    fabR::guess_date_format(distinct(.resume_var['value_var']))

  if(date_format$`% values formated` < 100){
    warning("Problem while computing date type variables due to ambiguous format.\n",
            "They will be analysed as text variables\n",
            crayon::bold("Useful tip:"),"Use dataset_evaluate(data) to get an assessment of your data.")

    final_summary <- summary_variables_text(.resume_var = .resume_var)
    return(final_summary)
  }

  summary <-
    .resume_var %>%
    mutate(value_var = fabR::as_any_date(.data$`value_var`,date_format$`Date format`)) %>%
    filter(.data$`value_var_occur` == 1 ) %>%
    filter(.data$`valid_class`  == "3_Valid other values")

  # init
  summary_tbl <- tibble(name = as.character())
  if(nrow(summary) == 0) return(summary_tbl)

  for(i in unique(summary$name)){
    # stop()}

    summary_i <-
      summary %>%
      filter(.data$`name` == i)

    # turn the output to be readable
    if(summary_i %>% nrow > 0){
      summary_i <-
        tibble(
          `name`         =  i,
          `Lowest date`      = min((summary_i$`value_var`),na.rm = TRUE),
          `Highest date`     = max((summary_i$`value_var`),na.rm = TRUE),
          `MIN\n(year)`      = summary(as.integer(year(summary_i$`value_var`)))[[1]],
          `Q1\n(year)`       = summary(as.integer(year(summary_i$`value_var`)))[[2]] %>% round,
          `MEDIAN\n(year)`   = summary(as.integer(year(summary_i$`value_var`)))[[3]] %>% round,
          `Q3\n(year)`       = summary(as.integer(year(summary_i$`value_var`)))[[5]] %>% round,
          `MAX\n(year)`      = summary(as.integer(year(summary_i$`value_var`)))[[6]],
          `MEAN\n(year)`     = summary(as.integer(year(summary_i$`value_var`)))[[4]] %>% round,
          `Span\n(year)` = year(.data$`Highest date`) - year(.data$`Lowest date`))

    }else{summary_i <- tibble(name = as.character())}

    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }

  # final_summary <-
  #   summary_variables(data, data_dict, .resume_var) %>%
  #   filter(.data$`categorical` != 'yes') %>%
  #   full_join(summary_tbl, by = 'name')

  return(summary_tbl)
}

#' @title
#' Provide statistical description of 'numerical' variables of a dataset
#'
#' @description
#' Generates a tibble that summarises all 'numerical' columns
#' in a dataset with (if any) its data dictionary. The data dictionary (if
#' present separates observations between open values, missing values,
#' categorical values, and categorical missing values (which corresponds to the
#' 'missing' column in the Categories' sheet). Statistics are generated
#' according to their valueTypes which can be 'integer', 'decimal', 'boolean' or
#' 'binary'. This summary can be used to assist the user in the assessment of
#' the dataset composition, with observation repartition and descriptive
#' statistics.
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
#' A dataset must be a data frame or data frame extension (e.g. a tibble) and
#' can be associated to a data dictionary. If not, a minimum workable data
#' dictionary can always be generated, when any column will be reported, and
#' any factor column will be analysed as categorical variable (the column
#' 'levels' will be created for that. In addition, the dataset may follow
#' Maelstrom research stardards, and its content can be evaluated accordingly,
#' such as naming convention restriction, or id columns declaration (which
#' full completeness is mandatory.
#'
#' @param data A tibble identifying the input data observations associated to
#' its data dictionary.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset. Automatically generated if not provided.
#' @param .resume_var A tibble which provides summary of the variables (for
#' development purpose only)
#' @return
#' A tibble providing statistical description of 'numerical' variables
#' present in a dataset.
#'
#' @examples
#' \donttest{
#' # Example 1: yyy yyy yyy.
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
summary_variables_numerical <- function(data = NULL, data_dict = NULL, .resume_var = NULL){

  # init
  summary_tbl <- tibble(name = as.character())
  if(is.null(.resume_var)) return(summary_tbl)

  summary <-
    .resume_var %>%
    filter(.data$`value_var_occur` == 1 ) %>%
    filter(.data$`valid_class`  == "3_Valid other values")

  # init
  summary_tbl <- tibble(name = as.character())
  if(nrow(summary) == 0) return(summary_tbl)

  for(i in unique(summary$name)){
    # stop()}

    summary_i <-
      summary %>%
      filter(.data$`name` == i)

    summary_i$`value_var` <- as.numeric(summary_i$`value_var`)

    # turn the output to be readable
    if(summary_i %>% nrow > 0){
      summary_i <-
        tibble(
          `name`         =  i,
          `MIN`     = summary(summary_i$`value_var`)[[1]],
          `Q1`      = summary(summary_i$`value_var`)[[2]],
          `MEDIAN`  = summary(summary_i$`value_var`)[[3]],
          `Q3`      = summary(summary_i$`value_var`)[[5]],
          `MAX`     = summary(summary_i$`value_var`)[[6]],
          `MEAN`    = summary(summary_i$`value_var`)[[4]])

    }else{summary_i <- tibble(name = as.character())}

    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }

  # final_summary <-
  #   summary_variables(data, data_dict, .resume_var) %>%
  #   filter(.data$`categorical` != 'yes') %>%
  #   full_join(summary_tbl, by = 'name')

  return(summary_tbl)
}

#' @title
#' Provide statistical description of 'categorical' variables of a dataset
#'
#' @description
#' Generates a tibble that summarises all categorical columns
#' in a dataset with (if any) its data dictionary. The data dictionary (if
#' present separates observations between open values, missing values,
#' categorical values, and categorical missing values (which corresponds to the
#' 'missing' column in the Categories' sheet). Statistics are generated
#' according to their valueTypes. This summary can be used to assist the user in
#' the assessment of the dataset composition, with observation repartition and
#' descriptive statistics.
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
#' A dataset must be a data frame or data frame extension (e.g. a tibble) and
#' can be associated to a data dictionary. If not, a minimum workable data
#' dictionary can always be generated, when any column will be reported, and
#' any factor column will be analysed as categorical variable (the column
#' 'levels' will be created for that. In addition, the dataset may follow
#' Maelstrom research stardards, and its content can be evaluated accordingly,
#' such as naming convention restriction, or id columns declaration (which
#' full completeness is mandatory.
#'
#' @param data A tibble identifying the input data observations associated to
#' its data dictionary.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset. Automatically generated if not provided.
#' @param .resume_var A tibble which provides summary of the variables (for
#' development purpose only)
#'
#' @return
#' A tibble providing statistical description of 'categorical' variables
#' present in a dataset.
#'
#' @examples
#' \donttest{
#' # Example 1: yyy yyy yyy.
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
summary_variables_categorical <- function(data = NULL, data_dict = NULL, .resume_var = NULL){

  # init
  summary_tbl <- tibble(name = as.character())
  if(is.null(.resume_var)) return(summary_tbl)

  summary <-
    .resume_var %>%
    group_by(across(c(-.data$`value_var_occur`,-.data$`index_value`))) %>%
    summarise(
      n = sum(as.integer(.data$`value_var_occur`)),
      .groups = 'drop') %>%
    arrange(.data$`index`, .data$`valid_class`) %>%
    ungroup

  if(nrow(summary) == 0) return(summary_tbl)

  for(i in unique(summary$name)){
    # stop()}

    summary_i <-
      summary %>%
      filter(.data$`name` == i)

    summary_category <-
      summary_i %>%
      filter(.data$`name` == i) %>%
      group_by(.data$`valid_class`,.data$`cat_index`) %>%
      summarise(n = sum(.data$`n`), name_var = paste0(.data$`value_var`, collapse = " ; "), .groups = "drop") %>%
      mutate(
        cat_index = replace_na(.data$`cat_index`,1),
        name_var  = str_replace(.data$`name_var`, "^NA$","")) %>%
      ungroup %>%
      mutate(n_perc = paste0(round(100*(.data$`n` / sum(.data$`n`)),2),"%")) %>%      # handle the round
      rowwise() %>%
      mutate(
        name_var = ifelse(
          .data$`valid_class` == "3_Valid other values",
          unlist(.data$`name_var` %>% str_split(" ; "))[1:5] %>%
            paste0(collapse = " ; "),.data$`name_var`),
        name_var = ifelse(.data$`valid_class` == "3_Valid other values" & n > 5, paste0(.data$`name_var`," [...]"), .data$`name_var`)) %>%
      ungroup %>%
      mutate(
        cat_var_absence    = ifelse(.data$`n` == 0, .data$`name_var`, ""),
        other_val_presence = ifelse(.data$`valid_class` == "3_Valid other values", .data$`name_var`, ""),
        list_values        = ifelse(.data$`valid_class` == "3_Valid other values", "", .data$`name_var`),
        n_perc             = paste0(" : ", .data$`n_perc`)) %>%
      unite("list_values",.data$`list_values`,.data$`n_perc`, sep = "",remove = TRUE, na.rm = TRUE) %>%
      mutate(categorical_index = str_sub(.data$`valid_class`,1,1)) %>%
      group_by(.data$`valid_class`,.data$`categorical_index`) %>%
      mutate(
        valid_class = case_when(
          .data$`valid_class` == "1_Valid values"       ~ "Valid categorical values : \n",
          .data$`valid_class` == "2_Missing values"     ~ "\nMissing categorical values : \n",
          .data$`valid_class` == "3_Valid other values" ~ "\nOther values (non categorical)",
          .data$`valid_class` == "4_NA values"          ~ "\nNA values",
          TRUE                             ~ .data$`valid_class`)) %>%
      select(-.data$`name_var`) %>%
      mutate(across(c(.data$`list_values`,.data$`cat_var_absence`,.data$`other_val_presence`), ~ ifelse(.data$`categorical_index` == 4 ,.,paste0(.,"\n")))) %>%
      mutate(valid_class = ifelse(.data$`cat_index` == 1 ,.data$`valid_class`,"")) %>%
      mutate(category_space_prefix = ifelse(.data$`cat_index` == 1 & .data$`categorical_index` %in% c(2,3,4),"\n","")) %>%
      mutate(category_space_suffix = ifelse(.data$`cat_index` == 1 & .data$`categorical_index` %in% c(1,2),  "\n","")) %>%
      unite("list_values",.data$`valid_class`,.data$`list_values`, sep = "",remove = TRUE, na.rm = TRUE) %>%
      unite("cat_var_absence",.data$`category_space_prefix`,.data$`cat_var_absence`,.data$`category_space_suffix`, sep = "",remove = FALSE, na.rm = TRUE) %>%
      unite("other_val_presence",.data$`category_space_prefix`,.data$`other_val_presence`,.data$`category_space_suffix`, sep = "",remove = TRUE, na.rm = TRUE) %>%
      mutate(
        cat_var_absence    = ifelse(.data$`cat_var_absence` %>% str_squish() == "","",.data$`cat_var_absence`),
        other_val_presence    = ifelse(.data$`other_val_presence` %>% str_squish() == "","",.data$`other_val_presence`)) %>%
      ungroup() %>%
      select(-.data$`categorical_index`, -.data$`n`) %>%
      summarise(across(everything(), ~ paste0(.,collapse = "")))

    if(summary_i %>% filter(.data$`valid_class` %in% c("1_Valid values","2_Missing values")) %>% nrow > 0){

      summary_i <-
        tibble(

          `name`                   = unique(summary_i$name),

          `% Valid categorical values` = round(summary_i %>% filter(.data$`valid_class` == "1_Valid values") %>%
                                                 pull(.data$`n`) %>% sum / (summary_i %>% pull(.data$`n`) %>% sum),4),

          `Values present in dataset`                           = summary_category$list_values,

          `Data dictionnary categories not present in dataset`  = summary_category$cat_var_absence,

          `Dataset value not present in data dictionnary`       = summary_category$other_val_presence

        )

    }else{summary_i <- tibble(name = as.character())}

    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }

  # final_summary <-
  #   summary_variables(data, data_dict, .resume_var) %>%
  #   filter(.data$`categorical` == 'yes' | .data$`categorical` == 'mix') %>%
  #   full_join(summary_tbl, by = 'name')

  return(summary_tbl)
}
