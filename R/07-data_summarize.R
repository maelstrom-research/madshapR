#' @title
#' Generate an assessment report and summary of a dataset
#'
#' @description
#' Assesses and summarizes the content and structure of a dataset and generates 
#' reports of the results. This function can be used to evaluate data structure, 
#' presence of specific fields, coherence across elements, and data dictionary 
#' formats, and to summarize additional information about variable distributions 
#' and descriptive statistics.
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
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param group_by A character string identifying the column in the dataset
#' to use as a grouping variable. Elements will be grouped by this 
#' column.
#' @param valueType_guess Whether the output should include a more accurate 
#' valueType that could be applied to the dataset. TRUE by default.
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#' @param dataset_name A character string specifying the name of the dataset
#' (internally used in the function [dossier_evaluate()]).
#'
#' @seealso
#' [dossier_evaluate()]
#'
#' @returns
#' A list of data frames containing assessment reports and summaries.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' 
#' ###### Example 1: use madshapR_example provided by the package
#' dataset <- as_dataset(madshapR_example$`dataset_example`, col_id = 'part_id')
#' data_dict <- as_data_dict_mlstr(madshapR_example$`data_dict_example`)
#' 
#' summary_dataset <- dataset_summarize(dataset, data_dict,group_by = "gndr")
#' glimpse(summary_dataset)
#' 
#' ###### Example 2: Any data frame can be a dataset by definition
#' summary_iris <- dataset_summarize(iris, group_by = "Species")
#' glimpse(summary_iris)
#'  
#' }
#'
#' @import dplyr stringr tidyr fabR janitor
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
dataset_summarize <- function(
    dataset,
    data_dict = data_dict_extract(dataset),
    group_by = group_vars(dataset),
    taxonomy = NULL,
    valueType_guess = TRUE, # must be the same in variable_visualize
    dataset_name = NULL){ 

  # fargs <- list()
  fargs <- as.list(match.call(expand.dots = TRUE))
  
  if(!is.logical(valueType_guess))
    stop(call. = FALSE,
         '`valueType_guess` must be TRUE or FALSE (FALSE by default)')
  
  # check on arguments : dataset
  as_dataset(dataset,col_id(dataset)) %>% 
    group_by(pick(all_of(group_by)))
  
  # check on arguments : data_dict. 
  as_data_dict_shape(data_dict)
  
  # check on argument : taxonomy
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  
  # catch the col_id if exists  
  col_id <- col_id(dataset)
  dataset <- as_dataset(ungroup(dataset),col_id)
  
  # original_data_dict <- data_dict
  data_dict_labels <- data_dict_trim_labels(data_dict)
  
  # match dataset and data_dict
  match_input_objects <- 
    suppressWarnings({
      data_dict_match_dataset(
        dataset,
        data_dict_labels)})
  
  dataset <- as_dataset(match_input_objects$dataset,col_id)
  attributes(dataset)[["madshapR::Data dictionary"]] <- NULL
  
  if(toString(attributes(data_dict)[["madshapR::class"]]) != "data_dict_mlstr")
    data_dict <- as_data_dict_mlstr(match_input_objects$data_dict) else
      data_dict <- match_input_objects$data_dict
  
  # if the group is not in the data dictionary, create it. Adds the 
  # group values that are in the dataset but not present in the data dictionary
  # _pps -> preprocess
  
  # # match dataset and data_dict
  # match_input_objects <- 
  #   suppressWarnings({
  #     data_dict_match_dataset(
  #       dataset,
  #       data_dict)})
  
  
  # dataset <- as_dataset(match_input_objects$dataset,col_id)
  # attributes(dataset)[["madshapR::Data dictionary"]] <- NULL
  # 
  # if(toString(attributes(data_dict)[["madshapR::class"]]) != "data_dict_mlstr")
  #   data_dict <- as_data_dict_mlstr(match_input_objects$data_dict)
  
  # catch group_by as a variable name.
  group_var <- names(dataset %>% select(all_of(group_by)))
  
  # catch name
  dataset_name <- 
    ifelse(!is.null(dataset_name),dataset_name,
           make_name_list(
             as.character(fargs[['dataset']]),list_elem = list(NULL)))
  
  # evaluate the dataset
  report <- list()
  
  report <- 
    dataset_evaluate(
      dataset,
      data_dict,
      taxonomy = taxonomy,
      dataset_name = dataset_name,
      is_data_dict_mlstr = TRUE,
      valueType_guess = valueType_guess)
  
  
  if(as_tibble(report[['Dataset assessment']]) %>% 
     bind_rows(tibble(`Dataset assessment` = as.character())) %>%
     dplyr::filter(str_detect(.data$`Dataset assessment`,"\\[ERROR\\]")) %>% nrow > 0){
    stop(call. = FALSE,
"The dataset and/or the data dictionary contain errors.",

bold("\n\nUseful tip:"),
" Use dataset_evaluate(dataset, data_dict) to get a full assessment of
your dataset")}
  
  # return(report)
  
  message(
    "- DATASET SUMMARIZE: ",
    bold(dataset_name), if(dataset %>% nrow == 0) " (empty dataset)",
    " --------------------------")

  dataset_valueType <- tibble(
      # "Variable name" = as.character(),
      "name_var" = as.character(),
      "Dataset valueType" = as.character())
  
  suggested_valueType <- tibble(
    "Variable name" = as.character(),
    # "name_var" = as.character(),
    "Suggested valueType" = as.character())
  
  if(ncol(dataset) > 0){
    dataset_valueType <-
      dataset %>%
      reframe(across(
        everything(),
        ~ valueType_of(.))) %>%
      pivot_longer(cols = everything()) %>%
      rename(
        # "Variable name" = "name",
        "name_var" = "name",
        `Dataset valueType` = "value")

    if(sum(nrow(report[["Dataset assessment"]])) > 0){
      suggested_valueType <-
        report[["Dataset assessment"]] %>%
        bind_rows(suggested_valueType) %>%
        dplyr::filter(!is.na(.data$`Suggested valueType`)) %>%
        select(
          "Variable name" ,
          # "name_var",
          "Suggested valueType")

    }
  }

  report$`Variables summary (all)` <-
    report$`Data dictionary summary` %>%
    left_join(
      data_dict_labels$`Variables` %>% select(
        "name_var" = "name","Variable name",
        "Data dictionary valueType" = "valueType"),
      relationship = "many-to-many",
      by = "Variable name") %>%
    full_join(dataset_valueType, by = "name_var") %>%
    full_join(suggested_valueType, by = "Variable name") 

  message("    Summarize the data type of each variable across the dataset")
  
  ### SUMMARIZE VARIABLES VALUES ###
  
  dataset_preprocess <-
    dataset_preprocess(
      dataset = dataset,
      data_dict = data_dict,
      group_by = group_var)
  
  message("    Summarize information for all variables")
  summary_var <- summary_overview <- 
    summary_variables(dataset_preprocess)
  
  gT <- unique(bind_rows(dataset_preprocess)[["genericType"]])
  
  # anchor
  if("numeric" %in% gT)   message("    Summarize information for numerical variables")     
  summary_num      <- dataset_preprocess %>% lapply(summary_variables_numeric)
  
  if("character" %in% gT) message("    Summarize information for text variables")
  summary_text     <- dataset_preprocess %>% lapply(summary_variables_text)
  
  if("date" %in% gT)      message("    Summarize information for date variables")
  summary_date     <- dataset_preprocess %>% lapply(summary_variables_date)

  if("datetime" %in% gT)  message("    Summarize information for datetime variables")
  summary_datetime <- dataset_preprocess %>% lapply(summary_variables_datetime)
  
  # anchor
  
  if(has_categories(data_dict)) message("    Summarize information for categorical variables")
  summary_category <- dataset_preprocess %>% lapply(summary_variables_categorical)
  
  name_group_short <- 
    unique(dataset_preprocess[["madshapR::grouping_var"]][['Variable name']])
  
  Index_group_short <- 
    as.character(unique(dataset_preprocess[["madshapR::grouping_var"]][['Index']]))
  
  grouping_col <- paste0("Grouping variable: ", name_group_short)  
  
  if(length(group_var) > 0){
  
    summary_var[['(all)']] <- summary_var[['madshapR::grouping_var']]
    summary_var[['madshapR::grouping_var']] <- NULL
    summary_num[['(all)']]      <- NULL
    summary_text[['(all)']]     <- NULL
    summary_date[['(all)']]     <- NULL
    summary_datetime[['(all)']] <- NULL
    summary_category[['(all)']] <- summary_category[['madshapR::grouping_var']]
    summary_category[['madshapR::grouping_var']] <- NULL
     
    summary_var      <- bind_rows(summary_var,.id = grouping_col) 
    summary_num      <- bind_rows(summary_num,.id = grouping_col)
    summary_text     <- bind_rows(summary_text,.id = grouping_col)
    summary_date     <- bind_rows(summary_date,.id = grouping_col)
    summary_datetime <- bind_rows(summary_datetime,.id = grouping_col)
    summary_category <- bind_rows(summary_category,.id = grouping_col)
    
  }else{
    
    summary_var      <- bind_rows(summary_var) 
    summary_num      <- bind_rows(summary_num)
    summary_text     <- bind_rows(summary_text)
    summary_date     <- bind_rows(summary_date)
    summary_datetime <- bind_rows(summary_datetime)
    summary_category <- bind_rows(summary_category)
  }
  
  minimum_cols <- c(
    "Index" ,
    grouping_col,
    "Variable name",
    "Variable label",
    "Quality assessment comment",
    "Data dictionary valueType",
    "Dataset valueType",
    "Suggested valueType",
    "Categorical variable",
    "Categories in data dictionary",  
    "Non-valid categories",           # [GF] TO VALIDATE"
    "Number of rows",
    "Number of valid values",
    "Number of non-valid values",
    "Number of empty values",
    "% Valid values",
    "% Non-valid values",
    "% Empty values",
    "Number of distinct values")
    

  # not tested, but the summary report and the summary var must always have
  # the same number of variables, full and inner must be equal
  if(nrow(report$`Variables summary (all)` %>%
    inner_join(summary_var, by = "Variable name")) !=
    nrow(report$`Variables summary (all)` %>%
    full_join(summary_var, by = "Variable name"))) stop('ERROR 104')
  
  report$`Variables summary (all)` <-
    report$`Variables summary (all)` %>%
    inner_join(summary_var, by = "Variable name") %>%
    mutate(
      `Quality assessment comment` = ifelse(.data$`Variable class` %in% "col id",
      "[INFO] - Identifier variable.",.data$`Quality assessment comment`),      # [GF] QUESTION :wording to validate
      `Quality assessment comment` = ifelse(.data$`Variable class` %in% "group",
      "[INFO] - Grouping variable.",.data$`Quality assessment comment`)) %>%    
    select(any_of(minimum_cols))
    
  report$`Text variable summary` <-
    report$`Variables summary (all)` %>%
    select(any_of(minimum_cols)) %>%
    select(
      -any_of("Dataset valueType"),
      -any_of("Suggested valueType"),
      -any_of("Categories in data dictionary"),
      -any_of("Non-valid categories")
      ) %>%
    inner_join(
      summary_text, 
      by = intersect(minimum_cols, names(summary_text))) %>%
    select(any_of(minimum_cols), everything())
  
  report$`Numerical variable summary` <-
    report$`Variables summary (all)` %>%
    select(any_of(minimum_cols)) %>%
    select(
      -any_of("Dataset valueType"),
      -any_of("Suggested valueType"),
      -any_of("Categories in data dictionary"),
      -any_of("Non-valid categories")
    ) %>%
    inner_join(
      summary_num, 
      by = intersect(minimum_cols, names(summary_num))) %>%
    select(any_of(minimum_cols), everything())
  
  report$`Date variable summary` <-
    report$`Variables summary (all)` %>%
    select(any_of(minimum_cols)) %>%
    select(
      -any_of("Dataset valueType"),
      -any_of("Suggested valueType"),
      -any_of("Categories in data dictionary"),
      -any_of("Non-valid categories")
    ) %>%
    inner_join(
      summary_date, 
      by = intersect(minimum_cols, names(summary_date))) %>%
    select(any_of(minimum_cols), everything()) 
  
  report$`Datetime variable summary` <-
    report$`Variables summary (all)` %>%
    select(any_of(minimum_cols)) %>%
    select(
      -any_of("Dataset valueType"),
      -any_of("Suggested valueType"),
      -any_of("Categories in data dictionary"),
      -any_of("Non-valid categories")
    ) %>%
    inner_join(
      summary_datetime, 
      by = intersect(minimum_cols, names(summary_datetime))) %>%
    select(any_of(minimum_cols), everything()) 
  
  report$`Categorical variable summary` <-
    report$`Variables summary (all)` %>%
    select(any_of(minimum_cols)) %>%
    select(
      -any_of("Dataset valueType"),
      -any_of("Suggested valueType")) %>%
    inner_join(
      summary_category, 
      by = intersect(minimum_cols, names(summary_category))) %>%
    select(any_of(minimum_cols), everything())
    
  message("    Summarize global information (Overview)")
  
  Overview <-
    tibble(`---` = c(
      'Overview'                                                               ,
      'Date report generated'                                                  ,
      '1_Name of the dataset'                                                  ,
      '    1_Identifier variable'                                              ,
      '    1_Grouping variable'                                                ,
      '    1_Variables'                                                        ,
      '        1_Number of variables'                                          ,
      '    1_ Variable data types (valueType)'                                 ,
      '        1_Number of text variables'                                     ,
      '        1_Number of date variables'                                     ,
      '        1_Number of datetime variables'                                 ,
      '        1_Number of numerical variables'                                ,
      '        1_Number of categorical variables'                              ,
      '    2_Rows'                                                             , # [GF] QUESTION :on devrait l'appeler by group pour match avec le visual report?
      '        2_Number of rows'                                               ,
      '        2_Number of distinct identifier variable values'                ,
      '        2_Number of empty variables'                                    ))

  
  # created with summary_var
  Overview_group <-   
    summary_overview[
      c("(all)", names(summary_overview)[!(names(summary_overview) %in% c("(all)","madshapR::grouping_var"))])]
  
  for(i in names(Overview_group)){
    # stop()}
    
    Overview_group[[i]] <-
      Overview %>%
      mutate(`-----` = case_when(
        .data$`---` == 'Overview'                                              ~
          " ",
        .data$`---` == 'Date report generated'                                 ~
          as.character(Sys.Date()),
        .data$`---` == '1_Name of the dataset'                                 ~
          dataset_name %>% str_remove_all("`"),
        .data$`---` == '    1_Identifier variable'                             ~
          as.character(ifelse(toString(col_id) != '',
                              toString(col_id), " ")), 
        .data$`---` == '    1_Grouping variable'                               ~
          as.character(ifelse(length(group_var) > 0,
                              toString(name_group_short), "madshapR::remove")),
        .data$`---` == '    1_Variables'                                       ~
          " ",
        .data$`---` == '        1_Number of variables'                         ~
          as.character(length(unique(
            report$`Data dictionary summary`$`Variable name`))),
        .data$`---` == '    1_ Variable data types (valueType)'                ~
          " ",
        .data$`---` == '        1_Number of text variables'                          ~
      as.character(ifelse(length(unique(report$`Text variable summary`$`Variable name`)) > 0,
                          length(unique(report$`Text variable summary`$`Variable name`)), "madshapR::remove")),
        .data$`---` == '        1_Number of date variables'                          ~
      as.character(ifelse(length(unique(report$`Date variable summary`$`Variable name`)) > 0,
                          length(unique(report$`Date variable summary`$`Variable name`)), "madshapR::remove")),
        .data$`---` == '        1_Number of datetime variables'                      ~
      as.character(ifelse(length(unique(report$`Datetime variable summary`$`Variable name`)) > 0,
                          length(unique(report$`Datetime variable summary`$`Variable name`)), "madshapR::remove")),
        .data$`---` == '        1_Number of numerical variables'                     ~
      as.character(ifelse(length(unique(report$`Numerical variable summary`$`Variable name`)) > 0,
                          length(unique(report$`Numerical variable summary`$`Variable name`)), "madshapR::remove")),
        .data$`---` == '        1_Number of categorical variables'                   ~
      as.character(ifelse(length(unique(report$`Categorical variable summary`$`Variable name`)) > 0,
                          length(unique(report$`Categorical variable summary`$`Variable name`)), " ")),
        .data$`---` == '    2_Rows'                                                  ~
          i,
        .data$`---` == '        2_Number of rows'                                    ~
        ifelse(as.character(unique(Overview_group[[i]][['Number of rows']])) == "0" ,"(empty)",
               as.character(unique(Overview_group[[i]][['Number of rows']]))),
        .data$`---` == '        2_Number of distinct identifier variable values'     ~
        ifelse(as.character(unique(Overview_group[[i]][['Number of rows']])) == "0" ,"(empty)",
        ifelse(is.null(col_id),
               as.character(unique(Overview_group[[i]][['Number of rows']])),
               as.character(Overview_group[[i]] %>% 
                            dplyr::filter(.data$`Variable class` == "col id") %>% 
                            pull('Number of distinct values')))),
        .data$`---` == '        2_Number of empty variables'                   ~
        ifelse(as.character(unique(Overview_group[[i]][['Number of rows']])) == "0" ,"(empty)",
            as.character(
            nrow(Overview_group[[i]] %>% dplyr::filter(
            `Quality assessment comment` %in% "[INFO] - Empty variable.")))),
        TRUE                                                                         ~ "EMPTY")) %>%
      mutate(
        `---` = str_remove_all(.data$`---`, "1_"),
        `---` = str_remove_all(.data$`---`, "2_")) %>%
      rename(
        `Overview` = "---",
        !! as.symbol(i) := "-----") %>% slice(-1)}
  
  index <- 
    Overview_group[[1]] %>%
    add_index %>% 
    dplyr::filter(.data$`Overview` == "    Rows") %>% 
    pull("index") - 1
  
  for(i in names(Overview_group)[-1]){

    # names(group) = "" # [GF] suggestion : enlever le nom du group en haut
    Overview_group[[i]][c(1:index),2] <- " "
    Overview_group[[i]][,1] <- NULL
  }
  
  report$Overview <- bind_cols(Overview_group)
  report$Overview <-
    report$Overview %>%
    dplyr::filter(!.data$`(all)` %in% "madshapR::remove")
  
  report[["Dataset assessment"]] <-
    report[["Dataset assessment"]] %>% filter(
      .data$`Dataset assessment` != "[INFO] - The dataset contains no errors/warnings.")
  
  if("[Unlabelled group]" %in% names(Overview_group)){  

    qual_comment = "[INFO] - Grouping variable contains empty values (NA)."

    report[["Dataset assessment"]] <-
      bind_rows(
        report[["Dataset assessment"]],
        tibble(
          `Index` = Index_group_short,
          `Variable name` = name_group_short,
          `Dataset assessment` = qual_comment,
          Value = '[Unlabelled group]')) %>% arrange(pick("Index"))
  }

  test_group_categorical <-   
    report[['Variables summary (all)']] %>%
    filter(.data$`Quality assessment comment` == "[INFO] - Grouping variable.") %>%
    pull(.data$`Categorical variable`) %>% unique %>% toString
  
  
  # [GF] Comment : further dev : exclude mix variables
  if(test_group_categorical == "no"){  
    
    qual_comment = "[INFO] - Grouping variable is not defined as categorical in data dictionary."
    report[["Dataset assessment"]] <-
      bind_rows(
        report[["Dataset assessment"]],
        tibble(
          `Index` = Index_group_short,
          `Variable name` = name_group_short,
          `Dataset assessment` = qual_comment)) %>% arrange(pick("Index"))}
  
  report[['Dataset assessment']] <-
    report[['Dataset assessment']] %>%
    mutate(
      'Dataset assessment' =
        ifelse(.data$`Variable name` %in% !! name_group_short & str_detect(
          .data$`Dataset assessment` ,
"Variable is categorical and has values defined in data dictionary that are not present in dataset"),
"[INFO] - Grouping variable has empty group (group with no participants).",                 
          .data$`Dataset assessment`),

      'Dataset assessment' =
        ifelse(.data$`Variable name` %in% !! name_group_short & str_detect(
          .data$`Dataset assessment` ,
"Variable is defined as categorical in data dictionary but not in dataset"),
"[INFO] - Grouping variable is not defined as categorical in data dictionary.", 
        .data$`Dataset assessment`))
 
  
  if(nrow(report$`Dataset assessment`) == 0){
    message("\n    The dataset contains no errors/warnings.")
    report$`Dataset assessment` <-
      tibble(
        'Variable name' = '(all)',
        'Dataset assessment' = "[INFO] - The dataset contains no errors/warnings.")
  }

  report$`Dataset assessment` <- 
    report$`Dataset assessment` %>%
    select(any_of("Index"),everything())
  
  message("    Generate report\n")
  
  # create report structure

  report$`Data dictionary summary`       <- NULL
  report <- report[vapply(X = report,
                          FUN = function(x) sum(nrow(x)) > 0,
                          FUN.VALUE = logical(1))]
  report <- report[unique(c(
    'Overview', 
    'Variables summary (all)',
    names(report)[str_detect(names(report),"variable summary")],
    names(report)[str_detect(names(report),"assessment")]
    ))]


  report <-   
    report %>%
    lapply(function(y){
      y %>%
        lapply(function(x) str_trunc(x, 10000)) %>%
        as_tibble()      
    })
  
  if(length(group_var) > 0){
    attributes(report)[['madshapR_group::group_by']] <- group_var    
  }
  
  
  

  return(report)
}


#' @title
#' Generate an assessment report and summary of a dossier
#'
#' @description
#' Assesses and summarizes the content and structure of a dossier 
#' (list of datasets) and generates reports of the results. This function can 
#' be used to evaluate data structure, presence of specific fields, coherence 
#' across elements, and data dictionary formats, and to summarize additional 
#' information about variable distributions and descriptive statistics.
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
#' @param dossier List of data frame(s), each of them being datasets.
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#' @param group_by A character string identifying the column in the dataset
#' to use as a grouping variable. Elements will be grouped by this 
#' column.
#' @param valueType_guess Whether the output should include a more accurate 
#' valueType that could be applied to the dataset. TRUE by default.
#'
#' @returns
#' A list of data frames containing overall assessment reports and summaries grouped by dataset.
#'
#' @examples
#' {
#' 
#' # use madshapR_example provided by the package
#' library(dplyr)
#'
#' ###### Example : a dataset list is a dossier by definition.
#'     
#' dataset1 <- as_dataset(madshapR_example$`dataset_example` %>% group_by(pick('gndr')))
#' dataset2 <- as_dataset(madshapR_example$`dataset_example - error`, col_id = "part_id")
#' dossier <- dossier_create(list(dataset1,dataset2))
#' 
#' summary_dossier <- dossier_summarize(dossier)
#' glimpse(summary_dossier)
#' 
#' }
#'
#' @import dplyr stringr tidyr
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
dossier_summarize <- function(
    dossier, 
    group_by = NULL,
    taxonomy = NULL, 
    valueType_guess = TRUE){
  
  # amelioration :rajouter taxonomy
  
  # check on arguments
  as_dossier(dossier)
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  if(!is.logical(valueType_guess))
    stop(call. = FALSE,
         '`valueType_guess` must be TRUE or FALSE (TRUE by default)')
  
  if(!is.null(group_by)) 
    dossier <- dossier %>% lapply(function(x) group_by(x, !!as.name(group_by)))
  
  report_list <-
    vector(mode = "list", length = length(names(dossier)))
  names(report_list) <- names(dossier)
  
  message(bold(
    "- DOSSIER SUMMARY: -----------------------------------------------------"))
  
  for(i in seq_len(length(dossier))){
    # stop()}
    report_list[[i]] <-
      dataset_summarize(
        dataset = dossier[[i]],
        taxonomy = taxonomy,
        dataset_name = names(dossier[i]),
        valueType_guess = valueType_guess)
    
  }
  
  return(report_list)
}


#' @title
#' Generate an evaluation of all variable values in a dataset
#'
#' @description
#' Analyses the content of a dataset and its data dictionary (if any), 
#' identifies variable(s) data type and values accordingly and preprocess the
#' variables. The elements of the data frame generated are evaluation of 
#' valid/non valid/empty values (based on the data dictionary information if 
#' provided). This function can be used to personalize report parameters and is 
#' internally used in the function [dataset_summarize()].
#' 
#' Generates a data frame that evaluates and aggregates all columns
#' in a dataset with (if any) its data dictionary. The data dictionary (if
#' present) separates observations between open values, empty values,
#' categorical values , and categorical non-valid values (which corresponds to the
#' 'missing' column in the 'Categories' sheet).
#' This internal function is mainly used inside summary functions.
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
#' @seealso
#' [summary_variables()]
#'
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#'
#' @returns
#' A data frame providing summary elements of a dataset, including its values 
#' and data dictionary elements.
#'
#' @examples
#' {
#'  
#' # use madshapR_example provided by the package
#' dataset <- madshapR_example$`dataset_example`
#' head(dataset_preprocess(dataset))
#'
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
dataset_preprocess <- function(
    dataset, 
    data_dict = data_dict_extract(dataset), 
    group_by = group_vars(dataset)){
  
  {
  # handle atomics
  # if(is.atomic(dataset) & length(dataset) == 0) return(summary_tbl) 
  if(is.atomic(dataset))
    return(dataset_preprocess(
      dataset = tibble(name = dataset), 
      data_dict = data_dict,
      group_by = group_by))
  
  # tests
  as_dataset(dataset) %>% 
    group_by(pick(all_of(group_by))) # no col_id
  
  # preserve dataset
  col_id <- col_id(dataset)
  group_var <- names(dataset %>% select(all_of(group_by)))
  dataset <- as_dataset(ungroup(dataset))
  
  # check on arguments : data_dict. 
  as_data_dict_shape(data_dict)
  
  data_dict_labels <- data_dict_trim_labels(data_dict)
  
  # match dataset and data_dict
  match_input_objects <- 
    suppressWarnings({
      data_dict_match_dataset(
        dataset,
        data_dict_labels)})
  
  dataset <- as_dataset(match_input_objects$dataset,col_id)
  attributes(dataset)[["madshapR::Data dictionary"]] <- NULL
  
  if(toString(attributes(data_dict)[["madshapR::class"]]) != "data_dict_mlstr")
    data_dict <- as_data_dict_mlstr(match_input_objects$data_dict) else
      data_dict <- match_input_objects$data_dict
        
  
  # if the group is not in the data dictionary, create it. Adds the 
  # group values that are in the dataset but not present in the data dictionary
  # _pps -> preprocess
  
  data_dict_pps <- data_dict
    # data_dict %>%
    # data_dict_trim_labels()
  
  if(has_categories(data_dict_pps)){
    data_dict_pps[['Categories']] <- 
      data_dict_pps[['Categories']] %>%
      group_by(pick("variable","missing")) %>%
      add_index("cat_index", .force = TRUE) %>%
      mutate(name_palette = ifelse(missing == FALSE,paste0("cat_",cat_index),paste0("miss_cat_",cat_index))) %>%
      select("cat_index",
            "variable",
            "name",
            "label" = any_of(first_label_get(data_dict_pps)[["Variables"]]), # "madshapR::label_cat_long",
            starts_with("Categories in data dictionary"),
            starts_with("Non-valid categories"),
            "missing",
            name_palette)
  }else{
    
    data_dict_pps[['Categories']] <- 
      tibble( 
        "cat_index" = as.integer(),
        "variable" = as.character(),
        "name" = as.character(), # `madshapR::label_cat_long` = as.character(),
        "label" = as.character(),
        "missing" = as.logical(),
        "Categories in data dictionary long" = as.character(),
        "Categories in data dictionary short" = as.character(),
        "Non-valid categories long" = as.character(),
        "Non-valid categories short" = as.character(),
        "name_palette" = as.character())
  }
  
  dataset_pps <-
    dataset %>% 
    add_index("index_value",.force = TRUE) 
  
  if(length(group_var) == 0){
    
    dataset_pps <-
      dataset_pps %>% 
      mutate("madshapR::no_group" = as_category("[No group]"))
    
    group_var <- "madshapR::no_group"
    data_dict_group <- 
      data_dict_extract(dataset_pps[group_var]) %>% 
      data_dict_trim_labels()
    
    data_dict_group <- 
      data_dict_group[['Categories']] %>%
      mutate(
        cat_index = 1,
        missing = FALSE,
        "Categories in data dictionary short" = '[No group]',
        "Categories in data dictionary long" = '[No group]',
        name_palette = "cat_1")
    
    data_dict_pps[['Categories']] <-
      data_dict_pps[['Categories']] %>%
      bind_rows(data_dict_group)
    
  }
  
  data_dict_pps[['Variables']] <- 
    data_dict_pps[['Variables']] %>% 
    add_index("Index in data_dict", .force = TRUE) %>%
    select("Index in data_dict",
           "name" = "name",
           "Variable name",
           "Variable label",
           "label" = any_of(first_label_get(data_dict)[["Variables"]]),
           "valueType",
           starts_with("Categories in data dictionary"),
           starts_with("Non-valid categories"))
  
  group_in_dataset <- 
    dataset_pps[c(group_var)] %>% 
    distinct() %>%
    rename("madshapR::grouping_var" = all_of(group_var)) %>%
    mutate("madshapR::group_occurrence" = 1)
  
  group_tibble <- 
    data_dict_pps$Categories %>% 
    dplyr::filter(data_dict_pps$Categories$variable == group_var) %>%
    mutate("name" = as_valueType(.data$name,valueType_of(dataset[[c(group_var)]]))) %>%
    rename(
      "madshapR::grouping_var" = "name",
      "madshapR::group_index" = "cat_index") %>%
    full_join(group_in_dataset, by = "madshapR::grouping_var") %>%
    mutate("variable" = group_var) %>%
    mutate("madshapR::grouping_var"  = ifelse(str_squish(.data$`madshapR::grouping_var`) == "",paste0("[",(.data$`madshapR::grouping_var`),"]"),.data$`madshapR::grouping_var`)) %>%
    mutate(label = ifelse(is.na(.data$`label`),as.character(.data$`madshapR::grouping_var`),.data$`label`)) %>%
    mutate("label" = replace_na(.data$`label`,"[Unlabelled group]")) %>%
    # mutate(across(starts_with("Categories in data dictionary"), ~ replace_na(.,"[Unlabelled group]"))) %>%
    # unite("madshapR::group_label",c("madshapR::group_label",!! group_var),sep = " ", na.rm = TRUE,remove = FALSE) %>%
    # mutate(`Variable name`             = replace_na(`Variable name`,group_var)) %>%
    mutate("madshapR::grouping_var" = as_valueType(.data$`madshapR::grouping_var`,"text")) %>%
    mutate("madshapR::group_occurrence" = replace_na(.data$`madshapR::group_occurrence`,0)) %>%
    mutate("madshapR::grouping_var" = replace_na(.data$`madshapR::grouping_var`,"[Unlabelled group]")) %>%
    add_index("madshapR::group_index",.force = TRUE) %>%
    select(
      "name" = "madshapR::grouping_var", 
      "variable",
      "madshapR::group_index",
      "label", 
      "madshapR::group_occurrence",
      starts_with("Categories in data dictionary"),
      starts_with("Non-valid categories"))
  
  group_name_short <- 
    data_dict_pps$Variables %>%
    filter(name == group_var) %>% pull("Variable name")
  
  group_tibble_labels <- 
    list(
    Variables = tibble(name = group_var),
    Categories  = group_tibble %>% select(variable,name,label)) %>%
    as_data_dict_mlstr() %>%
    data_dict_trim_labels()
  
  group_tibble_labels <-
    group_tibble_labels[['Categories']] %>%
    bind_rows(tibble(
      "name" = as.character(),
      "Categories in data dictionary short" = as.character(),
      "Categories in data dictionary long" = as.character(),)) %>%
    mutate(across(
      c("Categories in data dictionary short","Categories in data dictionary long"),
      ~ ifelse(.data$`name` %in% c("[Unlabelled group]","[No group]"),.data$`name`,.))) %>%
    mutate(across(
      c("Categories in data dictionary short", "Categories in data dictionary long"),
      ~ gsub("\\[\\[(.*?)\\]\\]", "[\\1]", .))) %>%
    mutate(across(
      c("name"),
      ~ ifelse(.data$`name` %in% c("[Unlabelled group]"),NA,.))) %>%
    mutate(across(
      c("name"),
      ~ gsub("\\[(.*?)\\]", "\\1", .))) %>%
    mutate("name" = as_valueType(.data$name,valueType_of(dataset[[c(group_var)]]))) %>%
    rename("madshapR::grouping_var" = "name") %>%
    select("madshapR::grouping_var","Categories in data dictionary short","Categories in data dictionary long")
    
  
  group_tibble <-
    group_tibble %>%
    mutate(across(
      c("Categories in data dictionary short","Categories in data dictionary long"),
      ~ ifelse(.data$`name` %in% c("[Unlabelled group]","[No group]"),.data$`name`,.))) %>%
    mutate(across(
      c("name"),
      ~ ifelse(.data$`name` %in% c("[Unlabelled group]"),NA,.))) %>%
    mutate("name" = as_valueType(.data$name,valueType_of(dataset[[c(group_var)]])))%>%
    mutate(across(
      c("name"),
      ~ gsub("\\[(.*?)\\]", "\\1", .))) %>%
    mutate("name" = as_valueType(.data$name,valueType_of(dataset[[c(group_var)]]))) %>%
    select(
      "madshapR::group_index",
      "madshapR::grouping_var" = "name",
      "madshapR::group_label long"  = "Categories in data dictionary long",
      "madshapR::group_label short" = "Categories in data dictionary short",
      "madshapR::group_occurrence") %>% left_join(group_tibble_labels,by = "madshapR::grouping_var") %>%
    mutate(
      "madshapR::group_label long" = ifelse(is.na(.data$`madshapR::group_label long`),.data$`Categories in data dictionary long`,.data$`madshapR::group_label long`),
      "madshapR::group_label short" = ifelse(is.na(.data$`madshapR::group_label short`),.data$`Categories in data dictionary short`,.data$`madshapR::group_label short`)) %>%
    select(-c("Categories in data dictionary long","Categories in data dictionary short"))
  
  # # add color palette to the group
  # col_palette <- madshapR::color_palette_maelstrom
  # nb_group <- max(0,group_tibble[["madshapR::group_index"]])
  # 
  # col_palette_group <-
  #   col_palette %>%
  #   filter(str_detect(.data$`values`,"group") & !str_detect(.data$`values`,"empty|total|no_group")) %>%
  #   pull(color_palette)
  # # 
  # col_palette_group <-
  #   rep(col_palette_group,ceiling(nb_group/length(col_palette_group)))[seq_along(1:nb_group)]
  # names(col_palette_group) <- paste0("group_",seq_along(1:nb_group))
  # 
  # col_palette_group <-
  #   tibble(
  #     name_palette  = names(col_palette),
  #     color_palette = col_palette) %>%
  #   bind_rows(
  #     col_palette %>% 
  #       filter(str_detect(.data$`values`,"group_empty|group_total|no_group")) %>% 
  #       rename(
  #         "name_palette_group" = "values",
  #         "color_palette_group"  = "color_palette"))

  group_tibble <-
    group_tibble %>%
    mutate(across(where(is.logical), as.character)) %>%
    mutate("name_palette_group" = case_when(
      
      .data$`madshapR::group_occurrence` == 0 ~ "group_empty",
      .data$`madshapR::grouping_var` == "[No group]" ~ "no_group",
      TRUE ~ paste0("group_",.data$`madshapR::group_index`))) %>%
    mutate(across(where(is.logical), as.character))
  # %>%
  #   left_join(col_palette_group,by = "name_palette_group")
    
  data_dict_pps <- 
    data_dict_trim_labels(data_dict_pps)
  
  data_dict_pps[['Variables']] <- 
    data_dict_pps[['Variables']] %>%
    select('Index in data_dict',"name_var" = 'name','valueType',"Variable name")
  
  data_dict_pps[['Categories']] <- 
    data_dict_pps[['Categories']] %>%
    rename(
      'name_var' = 'variable',
      'madshapR::code' = 'name',
      'madshapR::missing' = 'missing') %>%
    select("cat_index", "name_var","madshapR::code",
           "madshapR::missing","Categories in data dictionary long",
           "Categories in data dictionary short","name_palette")
  
  # # add color palette to the categories (missing = FALSE)
  # nb_cat <- max(0,data_dict_pps[['Categories']] %>% 
  #                 dplyr::filter(.data$`madshapR::missing` == FALSE) %>%
  #                 group_by(pick("name_var")) %>% group_size())
  # 
  # 
  # nb_miss <- max(0,data_dict_pps[['Categories']] %>% 
  #                 dplyr::filter(.data$`madshapR::missing` == TRUE) %>%
  #                 group_by(pick("name_var")) %>% group_size())
  # 
  # 
  # col_palette_cat <- 
  #   col_palette %>% 
  #   filter(str_detect(.data$`values`,"cat_")) %>% 
  #   pull(color_palette)
  # 
  # col_palette_cat <-
  #   rep(col_palette_cat,ceiling(nb_cat/length(col_palette_cat)))[seq_len(nb_cat)]
  # 
  # if(length(col_palette_cat > 0))
  #   names(col_palette_cat) <- paste0("cat_",seq_len(nb_cat))
  # 
  # col_palette_cat <-
  #   tibble(
  #     name_palette  = names(col_palette_cat),
  #     color_palette_valid_class = col_palette_cat)
  # 
  # col_palette_miss <- 
  #   col_palette %>% 
  #   filter(str_detect(.data$`values`,"miss_cat_")) %>% 
  #   pull(color_palette)
  # 
  # col_palette_miss <-
  #   rep(col_palette_miss,ceiling(nb_miss/length(col_palette_miss)))[seq_len(nb_miss)]
  # 
  # if(length(col_palette_miss > 0))
  #   names(col_palette_miss) <- paste0("miss_cat_",seq_len(nb_miss))
  # 
  # col_palette_miss <-
  #   tibble(
  #     name_palette  = names(col_palette_miss),
  #     color_palette_valid_class = col_palette_miss)
  # 
  # col_palette_total <- bind_rows(col_palette_cat,col_palette_miss)
  # 
  # data_dict_pps[['Categories']] <-
  #   data_dict_pps[['Categories']] %>%
  #   left_join(col_palette_total,by = "name_palette")
  
  summary_tbl <- tibble()

  for(i in names(select(dataset_pps, -"index_value"))){
    # stop()}
    
    col_set <-
      dataset_pps %>% 
      select("index_value", "madshapR::var" = all_of(i),
             "madshapR::grouping_var" = all_of(group_var)) %>%
      mutate("madshapR::grouping_var" = as.character(.data$`madshapR::grouping_var`)) %>%
      rename(!!i := "madshapR::var") %>% 
      mutate('name_var' = i) %>%  
      # group_by(Species) %>% slice(1:2) %>% ungroup %>%
      full_join(
        data_dict_pps$Categories %>% 
          dplyr::filter(data_dict_pps$Categories$`name_var` == i) %>% 
          mutate(`madshapR::code` = as_valueType(`madshapR::code`,valueType_of(dataset_pps[[c(i)]]))) %>%
          rename(!!i := "madshapR::code"),
        by = c(i, "name_var")) %>% 
      mutate("name_var" = i) %>%
      mutate("index_value" = ifelse(is.na(.data$`index_value`),0,.data$`index_value`)) %>%
      mutate(across(c("index_value"),as.integer))
    
    
    # if(nrow(col_set == 0)){
    no_col_set <- 
      col_set %>% 
      filter(.data$`index_value` == 0) %>%
      # select(-"madshapR::grouping_var") %>%
      full_join(
        group_tibble %>% 
          select("madshapR::grouping_var") %>%
          mutate("madshapR::grouping_var" = as.character(.data$`madshapR::grouping_var`)),
        by = "madshapR::grouping_var") %>%
      mutate("madshapR::grouping_var" = as.character(.data$`madshapR::grouping_var`)) %>%
      mutate("index_value" = ifelse(is.na(.data$`index_value`),0,.data$`index_value`)) %>%
      filter(!is.na(.data$`name_var`)) %>%
      select(-"madshapR::grouping_var") %>%
      cross_join(
        group_tibble %>% 
          select("madshapR::grouping_var") %>%
          mutate("madshapR::grouping_var" = as.character(.data$`madshapR::grouping_var`))) %>%
      mutate("madshapR::grouping_var" = as.character(.data$`madshapR::grouping_var`)) %>%
      mutate("index_value" = ifelse(is.na(.data$`index_value`),0,.data$`index_value`)) %>%
      filter(!is.na(.data$`name_var`))
        
        
    # }else{
    #   no_col_set <- 
    #     col_set %>% 
    #     filter(.data$`index_value` == 0) %>%
    #     select(-"madshapR::grouping_var") %>%
    #     cross_join(
    #       group_tibble %>% 
    #         select("madshapR::grouping_var") %>%
    #         mutate("madshapR::grouping_var" = as.character(.data$`madshapR::grouping_var`))) %>%
    #       # by = "madshapR::grouping_var") %>%
    #     mutate("madshapR::grouping_var" = as.character(.data$`madshapR::grouping_var`)) %>%
    #     mutate("index_value" = ifelse(is.na(.data$`index_value`),0,.data$`index_value`)) %>%
    #     filter(!is.na(.data$`name_var`))      
    # }

    col_set <- 
      col_set %>%
      filter(.data$`index_value` != 0) %>%
      bind_rows(no_col_set) %>%
      mutate("value_var_occur" = ifelse(.data$`index_value` == 0,0,1)) %>% 
      full_join(
        group_tibble[c("madshapR::grouping_var",
                       "madshapR::group_label long",
                       "madshapR::group_label short",
                       "madshapR::group_occurrence")] %>%
          mutate("madshapR::grouping_var" = as.character(.data$`madshapR::grouping_var`)),
        by = "madshapR::grouping_var") %>% 
      select(-"madshapR::grouping_var") %>% 
      mutate('name_var' = i) %>%  
      full_join(data_dict_pps$Variables[data_dict_pps$Variables$`name_var` == i,], 
                by = c("name_var")) %>%  
      full_join(group_tibble %>% select(
        -c("madshapR::group_occurrence", 
           "name_palette_group"#,
           # "color_palette_group"
           )
        ), by = c("madshapR::group_label long","madshapR::group_label short")) %>%  
      mutate(`value_var_occur` = ifelse(is.na(`madshapR::group_label long`),0,`value_var_occur`)) %>%
      mutate(across(c("value_var_occur","index_value"), as.integer)) %>%
      # mutate(`value_var_occur` = ifelse(`index_value` == 0,1)) %>% 
      select(`value_var_occur`,everything()) %>%  
      mutate(`value_var long` = 
               ifelse(is.na(!!as.name(i)) & !is.na(`index_value`),
                      "[Empty value]",
                      ifelse(!is.na(.data$`Categories in data dictionary long`),
                             .data$`Categories in data dictionary long`,
                             as.character(!!as.name(i))))) %>%
      mutate(`value_var short` = 
               ifelse(is.na(!!as.name(i)) & !is.na(`index_value`),
                      "[Empty value]",
                      ifelse(!is.na(.data$`Categories in data dictionary short`),
                             .data$`Categories in data dictionary short`,
                             as.character(!!as.name(i))))) %>%
      mutate(across(
        c("value_var short","value_var long"),
        ~ ifelse(str_squish(.) == "",paste0('[',.,']'),.))) %>%
      mutate(across(c("value_var long","value_var short"), as.character)) %>%
      mutate("index_value" = ifelse(is.na(.data$`index_value`),0,.data$`index_value`)) %>%
      mutate("value_var_occur" = ifelse(is.na(.data$`value_var_occur`),0,.data$`value_var_occur`))

    col_set <- 
      col_set %>%  
      mutate(
        "valid_class" = case_when(
          
          `Variable name` == toString(col_id) & `index_value` != 0         ~ "0 - Col id",
          `madshapR::missing` == FALSE                                     ~ "2 - Valid categorical values",
          `madshapR::missing` == TRUE                                      ~ "3 - Non-valid categorical values",
          is.na(!!as.name(i)) &  `index_value` != 0                        ~ "4 - Empty values",
          is.na(`Categories in data dictionary long`) & `index_value` != 0 ~ "1 - Valid non-categorical values",
          # is.na(!!as.name(i))                                              ~ "4 - Empty values",
          TRUE                                                             ~ NA_character_)) %>%
      arrange(pick("index_value"))
  
    # add the rest of the palette
    col_set <-
      col_set %>% 
      # select(valid_class,color_palette_valid_class,name_palette) %>%
      # left_join(
      #   col_palette %>% rename(valid_class = values),by = "valid_class") %>%
      # mutate(color_palette_valid_class =
      #          ifelse(is.na(.data$`color_palette_valid_class`),
      #                 .data$`color_palette`,
      #                 .data$`color_palette_valid_class`)) %>%
      # mutate(name_palette =
      #          ifelse(is.na(.data$`name_palette`) & !is.na(.data$`color_palette_valid_class`),
      #                 .data$`valid_class`,
      #                 .data$`name_palette`)) %>%
      # mutate(across(c("color_palette_valid_class"), as.character)) %>%
      # select(-"color_palette") %>%
      mutate(name_palette = ifelse(valid_class == "1 - Valid non-categorical values","non_cat", name_palette)) %>%
      mutate(name_palette = ifelse(valid_class == "4 - Empty values","empty"                  , name_palette))
    
    valid_statuses <- 
      c(na.omit(unique(
        col_set %>%
          dplyr::filter(
            .data$`valid_class` %in%
              c("2 - Valid categorical values","3 - Non-valid categorical values","1 - Valid non-categorical values")) %>%
          pull("valid_class")))) %>% toString
    
    categorical_status <- 
      case_when(
        
        # i %in% col_id                                                                ~ "col id",
        # i %in% group_var                                                             ~ "group" ,
                               
        valid_statuses == ""                                                         ~ "no"    ,
        toString(valid_statuses) == "1 - Valid non-categorical values"               ~ "no"    ,
        str_detect(toString(valid_statuses),"1 - Valid non-categorical values")      ~ "mix"   ,
        str_detect(toString(valid_statuses),"3 - Non-valid categorical values|2 - Valid categorical values") ~ "yes"   ,
        TRUE  ~ "ERROR")
    
    
    # [GF] If col id and group are the same might cause an error
    valid_class_status <- 
      case_when(
        
        i %in% group_var                                                        ~ "group" ,
        i %in% col_id                                                           ~ "col id",
        valid_statuses == ""                                                    ~ "no"    ,
        toString(valid_statuses) == "1 - Valid non-categorical values"          ~ "no"    ,
        
        str_detect(toString(valid_statuses),"1 - Valid non-categorical values")              ~ "mix"   ,
        str_detect(toString(valid_statuses),"3 - Non-valid categorical values|2 - Valid categorical values") ~ "yes"   ,
        TRUE  ~ "ERROR")
    
    if(categorical_status == "ERROR" | valid_class_status == "ERROR") 
      stop(call. = FALSE, "error categorical status")
    
    col_set <-
      col_set %>%
      mutate("Categorical variable" = categorical_status) %>%
      mutate("Variable class" = valid_class_status) %>%
      select(
        "Index in data_dict",
        "name_var",
        "Variable name",
        "valueType",
        "Variable class",
        "Categorical variable",
        "index_value",
        "value_var long",
        "value_var short",
        "value_var_occur",
        "cat_index",
        "valid_class",
        "name_palette",
        # "color_palette_valid_class",
        "madshapR::group_occurrence",
        "madshapR::group_label long",
        "madshapR::group_label short")
    
    summary_tbl <- 
      summary_tbl %>%
      bind_rows(col_set)
    
  }

  final_resume <- rep(list(summary_tbl), nrow(group_tibble) + 2)
  names(final_resume) <- 
    c(group_tibble$`madshapR::group_label long`,'(all)', "madshapR::grouping_var")  # [GF] NOTE : long label
  
  if(group_var == "madshapR::no_group"){
    final_resume <- final_resume["(all)"]}
  
  for(p in names(final_resume)){
    # stop()}
    
    if(p == "(all)"){
      
      final_resume[[p]] <- 
        final_resume[[p]] %>%  
        dplyr::filter(.data$`name_var` != group_var) %>% 
        mutate(
          !! paste0('Grouping variable: ', group_name_short) := p,
          "madshapR::group_label short" = p,
          "madshapR::group_label long" = p) %>%
        mutate("madshapR::group_occurrence" = 1) 
    
    }else if(p == "madshapR::grouping_var"){
      
      final_resume[[p]] <-  
        final_resume[[p]] %>%
        dplyr::filter(.data$`name_var` == group_var) %>% 
        mutate("valid_class" = "2 - Valid categorical values") %>%
        mutate("madshapR::group_label long" = '(all)') %>%
        mutate("madshapR::group_label short" = '(all)') %>%
        mutate(across(c("value_var long","value_var short"),
                      ~ str_replace(.,'\\[Empty value\\]','[Unlabelled group]'))) %>%
        mutate("cat_index" = ifelse(is.na(.data$`cat_index`), 1 + max(0,.data$`cat_index`,na.rm = TRUE),.data$`cat_index`)) %>% 
        mutate(!! paste0('Grouping variable: ', group_name_short) := '(all)') %>% # [GF] NOTE : long label
        left_join(group_tibble %>% select('value_var long'='madshapR::group_label long','name_palette_group'),by = 'value_var long') %>%
        mutate(name_palette = name_palette_group) %>%
        select(-"name_palette_group") 

    }else{
      final_resume[[p]] <- 
        final_resume[[p]] %>%
        dplyr::filter(.data$`name_var` != group_var) %>%
        mutate(
          "value_var_occur" = 
            ifelse(.data$`madshapR::group_label long` %in% p ,                    # [GF] NOTE : long label
                   .data$`value_var_occur`,0)) %>%
        mutate(
          !! paste0('Grouping variable: ', group_name_short) := p) %>%  
        mutate(
          "madshapR::group_occurrence" = 
            (group_tibble %>% filter(p == `madshapR::group_label long`) %>% pull(`madshapR::group_occurrence`)))
      
    }}
  
  vT_list <- madshapR::valueType_list
  
  final_resume <-
    final_resume %>%
    lapply(function(x){
      
      x %>% 
        left_join(vT_list[c("valueType","genericType")], by = "valueType") %>% 
        mutate(
          "genericType" = ifelse(
            `Variable class` %in% c("yes","no","mix"),`genericType`,
            `Variable class`)) %>% select("Index in data_dict":"valueType","genericType",everything()) %>%
        rename("Index" = "Index in data_dict") %>%
        # left_join(
        #   group_tibble %>% 
        #     select(!! paste0('Grouping variable: ', group_name_short) := "madshapR::group_label long",
        #            "name_palette_group", "madshapR::group_index") %>%
        #     bind_rows(tibble(
        #       !! paste0('Grouping variable: ', group_name_short) := "(all)",
        #       "name_palette"    = ifelse(group_var == "madshapR::no_group", 
        #                                        col_palette$values[col_palette$values == "no_group"], 
        #                                        col_palette$values[col_palette$values == "group_total"])
        #       
        #       
        #       # "name_palette_group"    = ifelse(group_var == "madshapR::no_group", 
        #       #                                  col_palette$values[col_palette$values == "no_group"], 
        #       #                                  col_palette$values[col_palette$values == "group_total"]),
        #       # "color_palette_group"   = ifelse(group_var == "madshapR::no_group", 
        #       #                                  col_palette$color_palette[col_palette$values == "no_group"], 
        #       #                                  col_palette$color_palette[col_palette$values == "group_total"])
        #       )) %>% add_index("madshapR::group_index",.force = TRUE),
        #   by = paste0('Grouping variable: ', group_name_short)) %>%
        mutate("genericType" = as.character(.data$`genericType`)) 
    })

  # handle colors for visualization.
  
  # if(group_var == "madshapR::no_group"){
  #   
  #   final_resume <- 
  #     final_resume$`(all)` %>% 
  #     select(everything(),-contains("palette"),
  #            "name_palette" = name_palette) %>%
  #     left_join(col_palette %>% rename(name_palette = values))
  # 
  # }else{
    
  color_palette_maelstrom <- 
    madshapR::color_palette_maelstrom %>% 
    rename(name_palette = values)
  
  nb_group <- max(0,group_tibble[["madshapR::group_index"]])
  
  col_palette_group <-
    color_palette_maelstrom %>%
    filter(str_detect(.data$`name_palette`,"group") & !str_detect(.data$`name_palette`,"empty|total|no_group")) %>%
    pull(color_palette)
  # 
  col_palette_group <-
    rep(col_palette_group,ceiling(nb_group/length(col_palette_group)))[seq_along(1:nb_group)]
  names(col_palette_group) <- paste0("group_",seq_along(1:nb_group))
  
  col_palette_group <-
    tibble(
      name_palette  = names(col_palette_group),
      color_palette = col_palette_group) %>%
    bind_rows(
      color_palette_maelstrom %>% 
        filter(str_detect(.data$`name_palette`,"group_empty|group_total|no_group")))
  
  
  # add color palette to the categories (missing = FALSE)
  nb_cat <- max(0,data_dict_pps[['Categories']] %>%
                  dplyr::filter(.data$`madshapR::missing` == FALSE) %>%
                  group_by(pick("name_var")) %>% group_size())
  
  
  nb_miss <- max(0,data_dict_pps[['Categories']] %>%
                   dplyr::filter(.data$`madshapR::missing` == TRUE) %>%
                   group_by(pick("name_var")) %>% group_size())
  
  
  col_palette_cat <-
    color_palette_maelstrom %>%
    filter(str_detect(.data$`name_palette`,"cat_")) %>%
    pull(color_palette)
  
  col_palette_cat <-
    rep(col_palette_cat,ceiling(nb_cat/length(col_palette_cat)))[seq_len(nb_cat)]
  
  if(length(col_palette_cat > 0))
    names(col_palette_cat) <- paste0("cat_",seq_len(nb_cat))
  
  col_palette_cat <-
    tibble(
      name_palette  = names(col_palette_cat),
      color_palette = col_palette_cat)
  
  col_palette_miss <-
    color_palette_maelstrom %>%
    filter(str_detect(.data$`name_palette`,"miss_cat_")) %>%
    pull(color_palette)
  
  col_palette_miss <-
    rep(col_palette_miss,ceiling(nb_miss/length(col_palette_miss)))[seq_len(nb_miss)]
  
  if(length(col_palette_miss > 0))
    names(col_palette_miss) <- paste0("miss_cat_",seq_len(nb_miss))
  
  col_palette_miss <-
    tibble(
      name_palette = names(col_palette_miss),
      color_palette = col_palette_miss)
  
  col_palette_total <- 
    bind_rows(col_palette_group,col_palette_cat,col_palette_miss,color_palette_maelstrom) %>% distinct
  
  }
  
  
  if(group_var != "madshapR::no_group"){
    
    final_resume[["(all)"]] <- 
      
      final_resume[["(all)"]] %>%
      mutate(name_palette = if_else(`valid_class` == "1 - Valid non-categorical values","group_total",name_palette))
    
  }
        
  final_resume <- 
    final_resume %>% lapply(function(x){
      x %>%
        #     mutate(name_palette = ifelse(name_palette_group == "group_total" & genericType != "group",name_palette_group,name_palette))
        # })
        # select(everything(),contains("palette")) %>%
        # select(-c('color_palette_valid_class')) %>%
        # select(-c('name_palette_group','name_palette','color_palette_valid_class','color_palette_group')) %>%
        left_join(col_palette_total,by = "name_palette")
      # %>% select(color_palette_valid_class) %>% distinct
    })
  
  return(final_resume)
  
}

#' @title
#' Provide descriptive statistics for variables in a dataset
#'
#' @description
#' Summarizes (in a data frame) the columns in a dataset and its data dictionary 
#' (if any). The summary provides information about quality, type, composition, 
#' and descriptive statistics of variables. Statistics are generated by 
#' valueType.
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
#' @param dataset_preprocess A data frame which provides summary of the 
#' variables (used for internal processes and programming).
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param .dataset_preprocess `r lifecycle::badge("deprecated")`
#'
#' @returns
#' A data frame providing statistical description of variables present in
#' a dataset.
#'
#' @examples
#' {
#' 
#' # use madshapR_example provided by the package
#' dataset <- madshapR_example$`dataset_example`
#' dataset_preprocess <- dataset_preprocess(dataset)
#' summary_prep <- summary_variables(dataset_preprocess = dataset_preprocess)
#' head(summary_prep)
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
summary_variables <- function(
    dataset_preprocess = .dataset_preprocess,
    dataset = NULL,
    data_dict = NULL,
    .dataset_preprocess = NULL){

  #  (for internal processes and programming).
  if(is.null(dataset_preprocess)) dataset_preprocess <- 
      dataset_preprocess(dataset = dataset, data_dict = data_dict)
  
  summary_total <- dataset_preprocess
  
  # init
  for(group in names(dataset_preprocess)){
    # stop()}
  
    summary <- dataset_preprocess[[group]]
    summary_tbl <- tibble(
      "Variable name" = as.character(),
      "Quality assessment comment" = as.character(),
      "Categorical variable" = as.character(),
      "Variable class" = as.character(),
      "Number of rows" = as.numeric(),
      "Number of valid values" = as.numeric(),
      "Number of non-valid values" = as.numeric(),
      "Number of empty values" = as.numeric(),
      "% Valid values" = as.numeric(),
      "% Non-valid values" = as.numeric(),
      "% Empty values" = as.numeric(),
      "Number of distinct values" = as.numeric())
    
    for(i in unique(summary$`name_var`)){
      # stop()}
      
      summary_i <- summary %>% dplyr::filter(.data$`name_var` == i)
      
      # summary the output
      summary_i <-
        tibble(
          `Variable name` = unique(summary_i$`Variable name`),
          
          `Categorical variable` = unique(summary_i$`Categorical variable`),
          
          `Variable class` = unique(summary_i$`Variable class`),
          
          `Number of rows` = sum(summary_i$`value_var_occur`,na.rm = TRUE),
          
          `Number of valid values` =
            sum(summary_i[
              summary_i$`valid_class` %in%
                c("0 - Col id","2 - Valid categorical values","1 - Valid non-categorical values"),]$`value_var_occur`),
          
          `Number of non-valid values` =
            sum(summary_i[
              summary_i$`valid_class` %in%
                c("3 - Non-valid categorical values"),]$`value_var_occur`),
          
          `Number of empty values` =  
            sum(summary_i[
              summary_i$`valid_class` %in%
                c("4 - Empty values"),]$`value_var_occur`),
          
          `% Valid values` = ifelse(.data$`Number of rows` == 0,0,
                                    round(100*(.data$`Number of valid values`/.data$`Number of rows`),2)),
          
          `% Non-valid values` = ifelse(.data$`Number of rows` == 0,0,
                                        round(100*(.data$`Number of non-valid values`/.data$`Number of rows`),2)),
          
          `% Empty values` = ifelse(.data$`Number of rows` == 0,0,
                                    round(100*(.data$`Number of empty values`/.data$`Number of rows`),2)),
          
          `Number of distinct values` =
            length(unique(summary_i[
              summary_i$`value_var_occur` == 1 &
                !(summary_i$`valid_class` %in%
                    c("4 - Empty values")),]$`value_var long`)),
        )
      
      summary_tbl <- bind_rows(summary_tbl, summary_i)
    }

    # [GF] - messages to validate
        
    summary_tbl <-
      summary_tbl %>%
      mutate(
        `Quality assessment comment` = case_when(

          sum(bind_rows(dataset_preprocess) %>% pull(`value_var_occur`),na.rm = TRUE) == 0 ~         
            "[INFO] - The dataset has 0 rows." ,
          
          sum(summary$`madshapR::group_occurrence`) == 0 & 
            "group" %in% (bind_rows(dataset_preprocess) %>% pull(`Variable class`))  ~
            "[INFO] - Empty group." ,

          .data$`Number of rows` == .data$`Number of empty values` ~
            "[INFO] - Empty variable." ,
                    
          .data$`Number of rows` == .data$`Number of distinct values`           ~
            "[INFO] - All rows are unique." ,
          
          .data$`Number of distinct values` > 0 & .data$`Number of valid values` == 0   ~
            "[INFO] - All categorical values present in variable indicate non-valid ('missing') values.",
          
          .data$`Number of distinct values` == 1                                              ~
            "[INFO] - Variable has a constant value.",
          
          # summary_tbl$`Number of valid values` > 0 & summary_tbl$`Number of non-valid values` == 0      ~
          # "[INFO] - Categorical values present in dataset that do not match categorical values in data dictionary." ,
          
          TRUE       ~   NA_character_
        )) %>%
      select("Variable name","Quality assessment comment", everything())

    
    summary_total[[group]] <- summary_tbl
    
  }
  
  return(summary_total)
  
}

#' @title
#' Provide descriptive statistics for variables of type 'text' in a dataset
#'
#' @description
#' Summarizes (in a data frame) the columns of type 'text' in a dataset and its 
#' data dictionary (if any). The summary provides information about quality, 
#' type, composition, and descriptive statistics of variables. Statistics are 
#' generated by valueType.
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
#' @param dataset_preprocess A data frame which provides summary of the variables
#' (for internal processes and programming).
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param .dataset_preprocess `r lifecycle::badge("deprecated")`
#'
#' @returns
#' A data frame providing statistical description of 'text' variables present
#' in a dataset.
#'
#' @examples
#' {
#'    
#' # use madshapR_example provided by the package
#' dataset <- madshapR_example$`dataset_example`
#' dataset_preprocess <- dataset_preprocess(dataset)
#' summary_prep <- summary_variables_text(dataset_preprocess = dataset_preprocess)
#' head(summary_prep)
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
summary_variables_text <- function(
    dataset_preprocess = .dataset_preprocess,
    dataset = NULL,
    data_dict = NULL,
    .dataset_preprocess = NULL){
  
  # init
  summary <-
    dataset_preprocess %>%
    dplyr::filter(.data$`valid_class`  == "1 - Valid non-categorical values") %>%
    dplyr::filter(.data$`genericType`  == "character") 
  
  # init
  summary_tbl <- tibble(
    "Variable name" = as.character(),
    "Most common values" = as.character(),
    "Least common values" = as.character())
  
  # for(i in unique(summary$`Variable name`)){
  #   # stop()}
  #   
    
  for(i in unique(summary$`name_var`)){
    # stop()}
    
    summary_i <- 
      summary %>% dplyr::filter(.data$`name_var` == i) %>%
      mutate(value_var = ifelse(value_var_occur == 0,NA_character_,.data$`value_var long`))   # [GF] NOTE : long label
    
    summary_i$`value_var` <- 
      as_valueType(summary_i$`value_var`,unique(summary_i$`valueType`))
    
    # turn the output to be readable
    summary_i <-
      tibble(
        
          `Variable name` = unique(summary_i$`Variable name`),
          `Most common values` =
            summary_i %>%
            count(.data$`value_var`) %>%
            dplyr::filter(!is.na(.data$`value_var`)) %>%
            dplyr::filter(if_any(any_of("n"), ~ . == max(.))) %>%
            slice(1:6) %>%
            mutate(
              value_var = str_trunc(value_var,width = 30,ellipsis = "..."),
              value_var = ifelse(
                row_number() == 6,
                '[...]', 
                .data$`value_var`)) %>%
            pull(.data$`value_var`) %>% paste0(collapse = " ; ") %>%
            str_replace('; \\[\\.\\.\\.\\]$','[...]'),
          
          `Least common values` =
            summary_i %>%
            count(.data$`value_var`) %>%
            dplyr::filter(!is.na(.data$`value_var`)) %>%
            dplyr::filter(if_any(any_of("n"), ~ . == min(.))) %>%
            slice(1:6) %>%
            mutate(
              value_var = str_trunc(value_var,width = 30,ellipsis = "..."),
              value_var = ifelse(
                row_number() == 6,
                '[...]', 
                .data$`value_var`)) %>%
            pull(.data$`value_var`) %>% paste0(collapse = " ; ") %>%
            str_replace('; \\[\\.\\.\\.\\]$','[...]')
        )
  
    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }
  
  summary_tbl <- 
    summary_tbl %>% full_join(
      dataset_preprocess[c("Variable name","genericType","Variable class")] %>%
        dplyr::filter(.data$`Variable class`  %in% c("no","mix")) %>% 
        dplyr::filter(.data$`genericType`  == "character") %>% distinct,
      by = "Variable name") %>%
    select(-"genericType",-"Variable class")
  
  return(summary_tbl)
}

#' @title
#' Provide descriptive statistics for variables of type 'date' in a dataset
#'
#' @description
#' Summarizes (in a data frame) the columns of type 'date' in a dataset and its 
#' data dictionary (if any). The summary provides information about quality, 
#' type, composition, and descriptive statistics of variables. Statistics are 
#' generated by valueType.
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
#' @param dataset_preprocess A data frame which provides summary of the 
#' variables (for internal processes and programming).
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param .dataset_preprocess `r lifecycle::badge("deprecated")`
#'
#' @returns
#' A data frame providing statistical description of 'date' variables present
#' in a dataset.
#'
#' @examples
#' {
#'    
#' library(dplyr)
#' library(fabR)
#' 
#' # use madshapR_example provided by the package
#' dataset <- madshapR_example$`dataset_example` %>%
#'   mutate(dob = as_any_date(dob)) %>%
#'   select(dob)
#' 
#' dataset_preprocess <- dataset_preprocess(dataset)
#' summary_prep <- summary_variables_date(dataset_preprocess = dataset_preprocess)
#' head(summary_prep)
#'
#' }
#'
#' @import dplyr tidyr lubridate fabR
#' @importFrom rlang .data
#'
#' @export
summary_variables_date <- function(
    dataset_preprocess = .dataset_preprocess,
    dataset = NULL,
    data_dict = NULL,
    .dataset_preprocess = NULL){

  # init
  summary <-
    dataset_preprocess %>%
    dplyr::filter(.data$`valid_class`  == "1 - Valid non-categorical values") %>%
    dplyr::filter(.data$`genericType`  == "date") 
  
  # init
  summary_tbl <- 
    tibble(
    "Variable name" = as.character(),
    "Oldest date" = as_any_date(),
    "Most recent date" = as_any_date(),
    "Minimum (year)" = as.numeric(),
    "1st quartile (year)" = as.numeric(),
    "Median (year)" = as.numeric(),
    "3rd quartile (year)" = as.numeric(),
    "Maximum (year)" = as.numeric(),
    "Mean (year)" = as.numeric(),
    "Span (year)" = as.numeric())
  
  for(i in unique(summary$`name_var`)){
    # stop()}
    
    summary_i <- 
      summary %>% dplyr::filter(.data$`name_var` == i) %>%
      mutate(value_var = ifelse(.data$`value_var_occur` == 0,NA_character_,.data$`value_var long`))
    
    date_format <-
      guess_date_format(distinct(summary_i['value_var']))
      
    if(date_format$`% values formated` < 100){
      warning(
        "Problem while computing date type variables due to ambiguous format.\n",
        "They will be analysed as text variables\n",
        bold("Useful tip:"),
        "Use dataset_evaluate(dataset) to get an assessment of your dataset.")
      
      summary_i <- 
        summary_variables_text(
          dataset_preprocess = 
            summary_i %>% mutate(
              valueType = "text",genericType = "character"))
    }else{

      summary_i$`value_var` <- 
        as_valueType(summary_i$`value_var`,unique(summary_i$`valueType`))
        
      # turn the output to be readable
      summary_i <-
        tibble( 
          `Variable name`       = unique(summary_i$`Variable name`),
          `Oldest date`         = if(all(is.na(summary_i$`value_var`))) NA_Date_  else 
            min((summary_i$`value_var`),na.rm = TRUE),
          `Most recent date`    = if(all(is.na(summary_i$`value_var`))) NA_Date_  else 
            max((summary_i$`value_var`),na.rm = TRUE),
          `Minimum (year)`      = 
            round(summary(as_any_integer(year(summary_i$`value_var`)))[[1]]),
          `1st quartile (year)` =
            round(summary(as_any_integer(year(summary_i$`value_var`)))[[2]]),
          `Median (year)`       =
            round(summary(as_any_integer(year(summary_i$`value_var`)))[[3]]),
          `3rd quartile (year)` =
            round(summary(as_any_integer(year(summary_i$`value_var`)))[[5]]),
          `Maximum (year)`      =
            round(summary(as_any_integer(year(summary_i$`value_var`)))[[6]]),
          `Mean (year)`         =
            round(summary(as_any_integer(year(summary_i$`value_var`)))[[4]]),
          `Span (year)`         = if(all(is.na(summary_i$`value_var`))) NA_real_  else 
            year(.data$`Most recent date`) - year(.data$`Oldest date`))
    }
    
    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }

  summary_tbl <- 
    summary_tbl %>% full_join(
      dataset_preprocess[c("Variable name","genericType","Variable class")] %>%
        dplyr::filter(.data$`Variable class`  %in% c("no","mix")) %>% 
        dplyr::filter(.data$`genericType`  == "date") %>% distinct,
      by = "Variable name") %>%
    select(-"genericType",-"Variable class")
  
  return(summary_tbl)
}


#' @title
#' Provide descriptive statistics for variables of type 'datetime' in a dataset
#'
#' @description
#' Summarizes (in a data frame) the columns of type 'datetime' in a dataset and 
#' its data dictionary (if any). The summary provides information about quality, 
#' type, composition, and descriptive statistics of variables. Statistics are 
#' generated by valueType.
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
#' @param dataset_preprocess A data frame which provides summary of the 
#' variables (for internal processes and programming).
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param .dataset_preprocess `r lifecycle::badge("deprecated")`
#'
#' @returns
#' A data frame providing statistical description of 'datetime' variables present
#' in a dataset.
#'
#' @examples
#' {
#'    
#' library(dplyr)
#' library(lubridate)
#' library(fabR)
#' 
#' # use madshapR_example provided by the package
#' dataset <- madshapR_example$`dataset_example` %>%
#'   mutate(dob = as_datetime(as_any_date(dob))) %>%
#'   select(dob)
#' 
#' dataset_preprocess <- dataset_preprocess(dataset)
#' summary_prep <- summary_variables_datetime(dataset_preprocess = dataset_preprocess)
#' head(summary_prep)
#'
#' }
#'
#' @import dplyr tidyr lubridate fabR
#' @importFrom rlang .data
#'
#' @export
summary_variables_datetime <- function(
    dataset_preprocess = .dataset_preprocess,
    dataset = NULL,
    data_dict = NULL,
    .dataset_preprocess = NULL){
  
  # init
  summary <-
    dataset_preprocess %>%
    dplyr::filter(.data$`valid_class`  == "1 - Valid non-categorical values") %>%
    dplyr::filter(.data$`genericType`  == "datetime") 
  
  # init
  summary_tbl <- tibble(
    "Variable name" = as.character(),
    "Most common values" = as.character(),
    "Least common values" = as.character())
  
  for(i in unique(summary$`name_var`)){
    # stop()}
    
    summary_i <- 
      summary %>% dplyr::filter(.data$`name_var` == i) %>%
      mutate("value_var" = ifelse(value_var_occur == 0,NA_character_,.data$`value_var long`))
      
    summary_i <- 
      summary_variables_text(
        dataset_preprocess = 
          summary_i %>% mutate(
            valueType = "text",genericType = "character"))

    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }
  
  summary_tbl <- 
    summary_tbl %>% full_join(
      dataset_preprocess[c("Variable name","genericType","Variable class")] %>%
        dplyr::filter(.data$`Variable class`  %in% c("no","mix")) %>% 
        dplyr::filter(.data$`genericType`  == "datetime") %>% distinct,
      by = "Variable name") %>%
    select(-"genericType",-"Variable class")
  
  return(summary_tbl)
}




#' @title
#' Provide descriptive statistics for variables of type 'numeric' in a dataset
#'
#' @description
#' Summarizes (in a data frame) the columns of type 'numeric' in a dataset and 
#' its data dictionary (if any). The summary provides information about quality, 
#' type, composition, and descriptive statistics of variables. Statistics are 
#' generated by valueType.
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
#' @param dataset_preprocess A data frame which provides summary of the 
#' variables (for internal processes and programming).
#' 
#' @returns
#' A data frame providing statistical description of 'numerical' variables
#' present in a dataset.
#'
#' @examples
#' {
#' 
#' # use madshapR_example provided by the package
#' dataset <- madshapR_example$`dataset_example`
#' dataset_preprocess <- dataset_preprocess(dataset)
#' summary_prep <- summary_variables_numeric(dataset_preprocess = dataset_preprocess)
#' head(summary_prep)
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#' @importFrom stats sd
#'
#' @export
summary_variables_numeric <- function(
    dataset_preprocess = .dataset_preprocess,
    dataset = NULL,
    data_dict = NULL,
    .dataset_preprocess = NULL){
  
  # init
  summary <-
    dataset_preprocess %>%
    dplyr::filter(.data$`valid_class`  == "1 - Valid non-categorical values") %>%
    dplyr::filter(.data$`genericType`  == "numeric") 

  summary_tbl <-  
    tibble(
      "Variable name" = as.character(),
      "Minimum" = as.numeric(),
      "1st quartile" = as.numeric(),
      "Median" = as.numeric(),
      "3rd quartile" = as.numeric(),
      "Maximum" = as.numeric(),
      "Mean" = as.numeric(),
      "Standard deviation" = as.numeric())
  
  for(i in unique(summary$`name_var`)){
    # stop()}
    
    summary_i <- 
      summary %>% dplyr::filter(.data$`name_var` == i) %>%
      mutate('value_var' = ifelse(value_var_occur == 0,NA_character_,.data$`value_var long`))
    
    summary_i$`value_var` <- 
      as_valueType(summary_i$`value_var`,unique(summary_i$`valueType`))
    
    summary_i$`value_var` <- 
      if(unique(summary_i$`valueType`) == "boolean"){
        as_valueType(summary_i$`value_var`,"integer")      
      }else{
        as_valueType(summary_i$`value_var`,unique(summary_i$`valueType`))
      }

    # turn the output to be readable

    summary_i <-
      tibble(
        `Variable name`        = unique(summary_i$`Variable name`),
        `Minimum`              = ifelse(all(is.na(summary_i$`value_var`)),NA,round(summary(summary_i$`value_var`)[[1]],2)),
        `1st quartile`         = ifelse(all(is.na(summary_i$`value_var`)),NA,round(summary(summary_i$`value_var`)[[2]],2)),
        `Median`               = ifelse(all(is.na(summary_i$`value_var`)),NA,round(summary(summary_i$`value_var`)[[3]],2)),
        `3rd quartile`         = ifelse(all(is.na(summary_i$`value_var`)),NA,round(summary(summary_i$`value_var`)[[5]],2)),
        `Maximum`              = ifelse(all(is.na(summary_i$`value_var`)),NA,round(summary(summary_i$`value_var`)[[6]],2)),
        `Mean`                 = ifelse(all(is.na(summary_i$`value_var`)),NA,round(summary(summary_i$`value_var`)[[4]],2)),
        `Standard deviation`   = ifelse(all(is.na(summary_i$`value_var`)),NA,round(sd(summary_i$`value_var`,na.rm = TRUE),2)),
      ) %>%
      mutate(
        `Standard deviation`   = ifelse(is.na(Mean),NA,replace_na(.data$`Standard deviation`,0)))
    
    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }

  summary_tbl <- 
    summary_tbl %>% full_join(
      dataset_preprocess[c("Variable name","genericType","Variable class")] %>%
        dplyr::filter(.data$`Variable class`  %in% c("no","mix")) %>% 
        dplyr::filter(.data$`genericType`  == "numeric") %>% distinct,
      by = "Variable name") %>%
    select(-"genericType",-"Variable class")
  
  return(summary_tbl)
}

#' @title
#' Provide descriptive statistics for variables of categorical in a dataset
#'
#' @description
#' Summarizes (in a data frame) the columns of type 'categorical' in a dataset and 
#' its data dictionary (if any). The summary provides information about 
#' quality, type, composition, and descriptive statistics of variables. 
#' Statistics are generated by valueType.
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
#' @param dataset_preprocess A data frame which provides summary of the variables 
#' (for internal processes and programming).
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param .dataset_preprocess `r lifecycle::badge("deprecated")`
#'
#' @returns
#' A data frame providing statistical description of 'categorical' variables
#' present in a dataset.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' library(fabR)
#' 
#' # use madshapR_example provided by the package
#' dataset <- madshapR_example$`dataset_example` %>%
#'   mutate(prg_ever = as_category(prg_ever)) %>%
#'   select(prg_ever)
#' 
#' dataset_preprocess <- dataset_preprocess(dataset)
#' summary_prep <- summary_variables_categorical(dataset_preprocess = dataset_preprocess)
#' head(summary_prep)
#' 
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom rlang .data
#'
#' @export
summary_variables_categorical <- function(
    dataset_preprocess = .dataset_preprocess,
    dataset = NULL,
    data_dict = NULL,
    .dataset_preprocess = NULL){
  
  # init
  summary_tbl <- 
    tibble(
      "Variable name" = as.character(),
      "Values present in dataset" = as.character(),
      "Data dictionary categories not present in dataset" = as.character(),
      "Dataset values not present in data dictionary" = as.character())
  
  if(is.null(dataset_preprocess)) return(summary_tbl)
  if(!nrow(dataset_preprocess)) return(summary_tbl)
  
  # anchor
  summary <-
    dataset_preprocess %>%
    dplyr::filter(!(.data$`value_var_occur` == 0 & is.na(.data$`cat_index`))) %>%
    group_by(across(c(-"value_var_occur",-"index_value"))) %>%
    reframe(
      n = sum(as.integer(.data$`value_var_occur`))) %>%
    arrange(.data$`Index`, .data$`valid_class`,.data$`cat_index`) %>%
    ungroup 
  
  # if(nrow(summary) == 0) return(summary_tbl)
  
  for(i in unique(summary$`name_var`)){
    # stop()}

    summary_i <- summary %>% dplyr::filter(.data$`name_var` == i) 
    
    summary_category <- 
      summary_i %>%
      mutate(
        cat_order = .data$`cat_index`,
        cat_index = ifelse(!is.na(.data$`cat_index`), .data$`value_var long`,NA_character_)) %>%
      ungroup %>%
      select("valid_class","cat_index","cat_order","value_var" = "value_var long","n") %>%
      group_by(.data$`valid_class`,.data$`cat_index`, .data$`cat_order`) %>%
      reframe(
        n = sum(.data$`n`),
        value_var = paste0(.data$`value_var`, collapse = "{semicolon}")) %>%
      separate_rows("value_var",sep = "{semicolon}") %>%
      distinct() %>%
      group_by(.data$`valid_class`,.data$`cat_index`,.data$`cat_order`,.data$`n`) %>%
      reframe(
        name_var = paste0(.data$`value_var`, collapse = "{semicolon}")) %>%
      arrange(.data$`valid_class`,.data$`cat_order`) %>%
      mutate(
        cat_index = replace_na(.data$`cat_index`,'{blank}'),
        name_var  = ifelse(str_squish(.data$`name_var`) == "",paste0("[",(.data$`name_var`),"]"),.data$`name_var`),
        name_var  = str_replace(.data$`name_var`, "^NA$","")) %>%
      
      # ) %>% View
      # handle the round
      mutate(n_perc = ifelse(.data$`n` == 0,"0%",
               paste0(round(100*(.data$`n` / sum(.data$`n`)),2),"%"))) %>%
      rowwise() %>%
      mutate(
        name_var2 = ifelse(
          .data$`valid_class` == "1 - Valid non-categorical values",
          unlist(.data$`name_var` %>% str_split("\\{semicolon\\}"))[6],.data$`name_var`),
        name_var = ifelse(
          .data$`valid_class` == "1 - Valid non-categorical values",
          unlist(.data$`name_var` %>% str_split("\\{semicolon\\}"))[1:5] %>%
            paste0(collapse = "{semicolon}"),.data$`name_var`)
      ) %>%
      mutate(
        name_var = str_replace_all(.data$`name_var`,'\\{semicolon\\}NA',''),
        name_var = ifelse(.data$`valid_class` == "1 - Valid non-categorical values" &
                            !is.na(.data$`name_var2`),
                          paste0(.data$`name_var`," [...]"),
                          .data$`name_var`),
        name_var = str_replace_all(.data$`name_var`,'\\{semicolon\\}',' ; '),
      ) %>% select(-'name_var2') %>%
      ungroup %>%
      mutate(
        cat_var_absence    =
          ifelse(.data$`n` == 0, .data$`cat_index`, ""),
        other_val_presence =
          ifelse(.data$`valid_class` == "1 - Valid non-categorical values",
                 .data$`name_var`, ""),
        list_values        =
          ifelse(.data$`valid_class` == "1 - Valid non-categorical values", "",
                 .data$`cat_index`),
        list_values        = na_if(.data$`list_values`,'{blank}'),
        n_perc             =
          paste0(" : ", .data$`n_perc`)) %>%
      unite("list_values",
            c('list_values','n_perc'),
            sep = "",remove = TRUE, na.rm = TRUE) %>%
      mutate(categorical_index = str_sub(.data$`valid_class`,1,1)) %>%
      group_by(.data$`valid_class`,.data$`categorical_index`) %>%
      mutate(
        "valid_class" = case_when(
          .data$`valid_class` == "2 - Valid categorical values"       ~
            "Valid values : \n",
          .data$`valid_class` == "3 - Non-valid categorical values"     ~
            "\nNon-valid values : \n",
          .data$`valid_class` == "1 - Valid non-categorical values" ~
            "\nOther values (non-categorical)",
          .data$`valid_class` == "4 - Empty values"          ~
            "\nEmpty values",
          TRUE                             ~ .data$`valid_class`)) %>%
      select(-'name_var') %>%
      
      mutate(across(c(
        'list_values','cat_var_absence','other_val_presence'),
        ~ ifelse(.data$`categorical_index` == 4 ,.,paste0(.,"\n")))) %>%
      mutate(
        "valid_class" =
          ifelse(.data$`cat_index` == '{blank}' | .data$`cat_order` %in% 1 ,.data$`valid_class`,"")) %>%
      mutate(
        category_space_prefix =
          ifelse(
            .data$`cat_index` == '{blank}' & .data$`categorical_index` %in% c(2,3,4),
            "\n","")) %>%
      mutate(
        category_space_suffix =
          ifelse(
            .data$`cat_index` == '{blank}' & .data$`categorical_index` %in% c(1,2),
            "\n","")) %>%
      unite(
        "list_values",
        c('valid_class','list_values'),
        sep = "",remove = TRUE, na.rm = TRUE) %>%
      unite(
        "cat_var_absence",
        c('category_space_prefix','cat_var_absence','category_space_suffix'),
        sep = "",remove = FALSE, na.rm = TRUE) %>%
      unite(
        "other_val_presence",
        c(
          # 'category_space_prefix',
          'other_val_presence','category_space_suffix'),
        sep = "",remove = TRUE, na.rm = TRUE) %>%
      mutate(
        cat_var_absence =
          ifelse(.data$`cat_var_absence` %>% str_squish() == "",
                 "",.data$`cat_var_absence`),
        other_val_presence =
          ifelse(.data$`other_val_presence` %>% str_squish() == "",
                 "",.data$`other_val_presence`)) %>%
      ungroup() %>%
      select(-"categorical_index", -"n") %>%
      reframe(across(everything(), ~ paste0(.,collapse = ""))) %>%
      mutate(
        list_values =
          ifelse(str_detect(.data$`list_values`,"^\n"),
                 str_replace(.data$`list_values`,"^\n",""),
                 .data$`list_values`))
    
    
    if(nrow(dplyr::filter(
      summary_i,
      .data$`valid_class` %in% c("2 - Valid categorical values","3 - Non-valid categorical values"))) > 0){
      
      summary_i <-
        tibble(
          
          `Variable name`                   = 
            unique(summary_i$`Variable name`),
          
          `Values present in dataset`                           =
            if(sum(summary_i$`madshapR::group_occurrence`,na.rm = TRUE) == 0) NA_character_ else
              if(summary_category$list_values == "") NA_character_ else
                summary_category$list_values,
          
          `Data dictionary categories not present in dataset`   =
            if(sum(summary_i$`madshapR::group_occurrence`,na.rm = TRUE) == 0) NA_character_ else
              if(summary_category$cat_var_absence == "") NA_character_ else
                summary_category$cat_var_absence,
          
          `Dataset values not present in data dictionary`        =
            if(sum(summary_i$`madshapR::group_occurrence`,na.rm = TRUE) == 0) NA_character_ else
              if(summary_category$other_val_presence == "") NA_character_ else
                summary_category$other_val_presence
          
        )
      
    }else{summary_i <- tibble()}
    
    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }
  
  # summary_tbl <- 
  #   summary_tbl %>% full_join(
  #     dataset_preprocess[c("Variable name","genericType")] %>%
  #       dplyr::filter(.data$`genericType`  == "categorical") %>% distinct,
  #     by = "Variable name") %>%
  #   select(-"genericType")
  
  return(summary_tbl)
}
