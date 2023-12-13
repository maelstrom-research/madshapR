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
#' [OBiBa data type of a variable](https://opaldoc.obiba.org/en/variables-data.html#value-types). 
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
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#' @param dataset_name A character string specifying the name of the dataset
#' (internally used in the function [dossier_evaluate()]).
#' @param valueType_guess Whether the output should include a more accurate 
#' valueType that could be applied to the dataset. FALSE by default.
#' @param .dataset_name `r lifecycle::badge("deprecated")`
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
#' # use madshapR_DEMO provided by the package
#' library(dplyr)
#' 
#' #' ###### Example : Any data frame can be summarized
#' dataset <- iris['Sepal.Width']
#' glimpse(dataset_summarize(dataset))
#'  
#' }
#'
#' @import dplyr stringr tidyr fabR
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
dataset_summarize <- function(
    dataset,
    data_dict = data_dict_extract(dataset),
    group_by = NULL,
    taxonomy = NULL,
    dataset_name = .dataset_name,
    valueType_guess = FALSE,
    .dataset_name = NULL){
  
  fargs <- as.list(match.call(expand.dots = TRUE))
  
  if(!is.logical(valueType_guess))
    stop(call. = FALSE,
         '`valueType_guess` must be TRUE or FALSE (FALSE by default)')
  
  dataset <- as_dataset(dataset, col_id(dataset))
  col_id <- col_id(dataset)
  
  dataset_name <- 
    ifelse(
      !is.null(dataset_name),
      dataset_name,
      make_name_list(
        as.character(fargs[['dataset']]),list_elem = list(NULL)))
  
  # check on argument : taxonomy
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  
  if(toString(substitute(group_by)) == '') group_by <- NULL
  # attempt to catch group_by from the group_vars if the dataset is grouped
  if(length(group_vars(dataset)) == 1 & toString(substitute(group_by)) == ''){
    group_by <- group_vars(dataset)
  }

  dataset <- 
    as_dataset(ungroup(dataset),col_id)
    
  dataset <-
    suppressWarnings({
    data_dict_match_dataset(
      dataset,
      data_dict,
      output = 'dataset') %>%
    as_dataset(col_id)})
  
  data_dict <- 
    suppressWarnings({
    data_dict_match_dataset(
      dataset,data_dict,
      output = 'data_dict') %>%
    as_data_dict_mlstr(name_standard = FALSE)
      })
  
  # attempt to catch group_by
  if(toString(substitute(group_by)) != ''){
    group_by <- tryCatch(
      expr  = {toString(names(dataset[toString(substitute(group_by))]))},
      error = function(cond){return(toString(names(dataset[group_by])))})    
    
    if(! group_by %in% data_dict[['Categories']][['variable']]) group_by <- ''
    
  }else{ group_by <- ''}
  
  if(group_by != ''){
    
    preprocess_group <- 
      dataset_preprocess(dataset[c(group_by)], data_dict)
    
    if(toString(unique(preprocess_group$`Categorical variable`)) %in% 
       c('mix','no'))
      stop(call. = FALSE,
           'Your grouping variable must be a categorical variable.')
    
    cat_lab <-  
      data_dict[['Categories']] %>% 
      dplyr::filter(if_any('variable') == group_by) %>%
      select(
        !! group_by := 'name', 
        `___labels___` = matches(c("^label$","^label:[[:alnum:]]"))[1]) %>%
      mutate(!! as.symbol(group_by) := as.character(!!as.symbol(group_by))) %>%
      add_index('___category_level___') %>%
      mutate(
        `___labels___` = 
          ifelse(!! as.symbol(group_by) == .data$`___labels___`,'',
                 paste0(' [',str_trunc(.data$`___labels___`,width = 19,
                                       ellipsis = '...'),']'))) %>%
      unite('___labels___',c(group_by,'___labels___'),sep = '', 
            remove = FALSE,na.rm = TRUE)
    
    # create group
    dataset_group <- dataset %>% group_by(!! as.symbol(group_by))
    name_group <- group_keys(dataset_group)
    dataset_group <- group_split(dataset_group)
    names(dataset_group) <- as.character(name_group[[1]])
    
    # arrange group
    dataset_group <- 
      dataset_group[unique(
        c(intersect(cat_lab[[group_by]],names(dataset_group)),
          names(dataset_group)[length(names(dataset_group))]))]
    
    # rename group
    name_group <- 
      name_group %>%
      mutate(!! as.symbol(group_by) := as.character(!!as.symbol(group_by))) %>%
      left_join(cat_lab, by = join_by(!!as.symbol(group_by))) %>%
      arrange(.data$`___category_level___`) %>% 
      pull('___labels___') %>%
      str_replace_na('NA Values') 
    
    names(dataset_group) <- name_group
    
  } else { dataset_group <- list(no_group = dataset)}
  
  # evaluate the dataset
  report <- list()
  report <- 
    dataset_evaluate(
      dataset,
      data_dict,
      taxonomy = taxonomy,
      dataset_name = dataset_name,
      as_data_dict_mlstr = TRUE)

  message(
    "- DATASET SUMMARIZE: ",
    bold(dataset_name), if(dataset %>% nrow == 0) " (empty dataset)",
    " --------------------------")

  # 
  # if(is.null(col_id) | ncol(dataset) == 1){
  #   dataset <- madshapR::dataset %>% add_index("___mlstr_index___")
  #   dataset <-   as_dataset(dataset, names(dataset)[1])}
  # 
  # if(!is.null(preserve_attributes)) col_id <- preserve_attributes

  # exclude id col if is the index
  dataset_valueType <- tibble(
      `___name_var___` = as.character(),
      `Actual dataset valueType` = as.character())
  
  estimated_valueType <- tibble(
    `___name_var___` = as.character(),
    `Estimated dataset valueType` = as.character())
  
  if(ncol(dataset) > 0){
    dataset_valueType <-
      dataset %>%
      # select(-matches("^___mlstr_index___$")) %>%
      summarise(across(
        everything(),
        ~ valueType_of(.))) %>%
      pivot_longer(cols = everything()) %>%
      rename(
        `___name_var___` = "name",
        `Actual dataset valueType` = "value")

  if(valueType_guess == TRUE){
    estimated_valueType <-
      dataset %>%
      # select(-matches("^___mlstr_index___$")) %>%
      summarise(across(
        everything(),
        ~ valueType_guess(.))) %>%
      pivot_longer(cols = everything()) %>%
      rename(
        `___name_var___` = "name",
        `Estimated dataset valueType` = "value")
  }else{
    estimated_valueType <-
      dataset_valueType %>%
      select(
        "___name_var___",
        `Estimated dataset valueType` = "Actual dataset valueType")}

  }
  ## variables
  data_dict_var <-
    data_dict[['Variables']] %>%
    select(-matches("^___name_var___$")) %>%
    rename(`___name_var___` = "name") %>%
    mutate(across(everything(),as.character)) %>%
    add_index("index in data dict.", .force = TRUE) %>%
    select("index in data dict.", "___name_var___",
           matches(c("^label$","^label:[[:alnum:]]"))[1],
           `Data Dictionary valueType` = "valueType") %>%
    full_join(estimated_valueType, by = "___name_var___") %>%
    full_join(dataset_valueType, by = "___name_var___")

  ## categories
  if(sum(nrow(data_dict[['Categories']])) > 0){
    data_dict_cat <-
      data_dict[['Categories']] %>% 
      select(-matches("^___name_var___$")) %>%
      rename("___name_var___" = "variable") %>%
      select("___name_var___","name",
             matches(c("^label$","^label:[[:alnum:]]"))[1], 
             "missing") %>%
      mutate(
        missing =
          ifelse(
            .data$`missing` == TRUE,
            "Missing categorical values :",
            "Valid categorical values :")) %>%
      unite(
        "Categories in data dictionary",
        c("name",matches(c("^label$","^label:[[:alnum:]]"))[1]),
        sep = " = ",remove = TRUE) %>%
      group_by(pick(c(-"Categories in data dictionary"))) %>%
      summarise(across(c("Categories in data dictionary"),
                       ~ paste0(.,collapse = "\n")),.groups = "drop") %>%
      arrange(.data$`___name_var___`,desc(.data$`missing`)) %>%
      unite("Categories in data dictionary",
            c("missing","Categories in data dictionary"),
            sep = "\n",remove = TRUE) %>%
      group_by(pick(c(-"Categories in data dictionary"))) %>%
      summarise(across(c("Categories in data dictionary"),
                       ~ paste0(.,collapse = "\n\n")),.groups = "drop") %>%
      ungroup %>%
      select("___name_var___","Categories in data dictionary")
  }else{
    data_dict[['Categories']] <-
      tibble(
        name = as.character(),
        variable = as.character(),
        missing = as.logical())

    data_dict_cat <-
      tibble(
        `___name_var___` = as.character(),
        `Categories in data dictionary` = as.character())}

  report$`Variables summary (all)` <-
    data_dict_var %>%
    left_join(data_dict_cat, by = "___name_var___") %>%
    rename(name = "___name_var___") %>%
    tibble

  message("    Summarize the data type of each variable across the dataset")

  ### SUMMARIZE VARIABLES VALUES ###

  vT <- madshapR::valueType_list
  vT_text <- vT[vT$`genericType` == 'character',][['valueType']]
  report$`Text variable summary` <-
    report$`Variables summary (all)`[
      report$`Variables summary (all)`$`Estimated dataset valueType` %in%
        vT_text,] %>% dplyr::filter(!.data$`name` %in% col_id)

  vT_num  <- vT[vT$`genericType` == 'numeric',][['valueType']]
  report$`Numerical variable summary` <-
    report$`Variables summary (all)`[
      report$`Variables summary (all)`$`Estimated dataset valueType` %in%
        vT_num,] %>% dplyr::filter(!.data$`name` %in% col_id)

  vT_date <- vT[vT$`genericType` == 'date',][['valueType']]
  report$`Date variable summary` <-
    report$`Variables summary (all)`[
      report$`Variables summary (all)`$`Estimated dataset valueType` %in%
        vT_date,] %>% dplyr::filter(!.data$`name` %in% col_id)
  
  vT_datetime <- vT[vT$`genericType` == 'datetime',][['valueType']]
  report$`Datetime variable summary` <-
    report$`Variables summary (all)`[
      report$`Variables summary (all)`$`Estimated dataset valueType` %in%
        vT_datetime,] %>% dplyr::filter(!.data$`name` %in% col_id)

  report$`Categorical variable summary` <-
    report$`Variables summary (all)`[
      !is.na(report$`Variables summary (all)`$`Categories in data dictionary`),
      ] %>% dplyr::filter(!.data$`name` %in% col_id)

  if(nrow(dataset) > 0){

    message("    Summarize information for all variables")
    
    dataset_preprocess <- 
      lapply(dataset_group,function(x){
        dataset_preprocess(select(x,-any_of(col_id)),data_dict)})
    
    summary_var <- 
      lapply(dataset_preprocess,function(x){
        summary_variables(dataset_preprocess = x)})
    
    if(group_by != ''){
      summary_group <- summary_variables(dataset_preprocess = preprocess_group)
      summary_var <- 
        lapply(summary_var,function(x){
          x %>% dplyr::filter(.data$`name` != group_by) %>%
            bind_rows(summary_group)})}

    message("    Summarize information for numerical variables")
    dataset_preprocess_num <-
      lapply(dataset_preprocess,function(x){
      x[x$`name` %in% report$`Numerical variable summary`$name,] %>%
          dplyr::filter(.data$`Categorical variable` != 'yes')})
    summary_num <-
      lapply(dataset_preprocess_num,function(x){
        summary_variables_numeric(dataset_preprocess = x)})
    
    message("    Summarize information for text variables")
    dataset_preprocess_text <-
      lapply(dataset_preprocess,function(x){
        x[x$`name` %in% report$`Text variable summary`$name,] %>%
          dplyr::filter(.data$`Categorical variable` != 'yes')})
    summary_text <-
      lapply(dataset_preprocess_text,function(x){
        summary_variables_text(dataset_preprocess = x)})

    message("    Summarize information for date variables")
    dataset_preprocess_date <-
      lapply(dataset_preprocess,function(x){
        x[x$`name` %in% report$`Date variable summary`$name,] %>%
          dplyr::filter(.data$`Categorical variable` != 'yes')})
    summary_date <-
      lapply(dataset_preprocess_date,function(x){
        summary_variables_date(dataset_preprocess = x)})
    
    message("    Summarize information for datetime variables")
    dataset_preprocess_datetime <-
      lapply(dataset_preprocess,function(x){
        x[x$`name` %in% report$`Datetime variable summary`$name,] %>%
          dplyr::filter(.data$`Categorical variable` != 'yes')})
    summary_datetime <-
      lapply(dataset_preprocess_datetime,function(x){
        summary_variables_datetime(dataset_preprocess = x)})
    
    message("    Summarize information for categorical variables")
    dataset_preprocess_cat <-
      lapply(dataset_preprocess,function(x){
        x[x$`name` %in% report$`Categorical variable summary`$name,] %>%
          dplyr::filter(.data$`Categorical variable` != 'no')}) 
    summary_cat <-
      lapply(dataset_preprocess_cat,function(x){
        summary_variables_categorical(dataset_preprocess = x)})
    
    if(group_by != ''){
      summary_group_cat <- 
        summary_variables_categorical(dataset_preprocess = preprocess_group)
      summary_cat <- 
        lapply(summary_cat,function(x){
          x %>% dplyr::filter(.data$`name` != group_by) %>%
            bind_rows(summary_group_cat)})}
    
    # add grouping variable to each group
    if(group_by != ''){
      for(i in names(summary_var)) {
        summary_var [[i]] <- summary_var [[i]] %>% 
          mutate(!! paste0('Grouping variable: ', group_by) := as.character(
            ifelse(.data$`name` == group_by, paste0(group_by,' (all)'),i)))
        
        summary_num [[i]] <- summary_num [[i]] %>% 
          mutate(!! paste0('Grouping variable: ', group_by) := as.character(
            ifelse(.data$`name` == group_by, paste0(group_by,' (all)'),i)))
        
        summary_text[[i]] <- summary_text[[i]] %>% 
          mutate(!! paste0('Grouping variable: ', group_by) := as.character(
            ifelse(.data$`name` == group_by, paste0(group_by,' (all)'),i)))
        
        summary_date[[i]] <- summary_date[[i]] %>% 
          mutate(!! paste0('Grouping variable: ', group_by) := as.character(
            ifelse(.data$`name` == group_by, paste0(group_by,' (all)'),i)))
        
        summary_datetime[[i]] <- summary_datetime[[i]] %>% 
          mutate(!! paste0('Grouping variable: ', group_by) := as.character(
            ifelse(.data$`name` == group_by, paste0(group_by,' (all)'),i)))
        
        summary_cat [[i]] <- summary_cat [[i]] %>% 
          mutate(!! paste0('Grouping variable: ', group_by) := as.character(
            ifelse(.data$`name` == group_by, paste0(group_by,' (all)'),i)))
      }}
    
    # binding information
    summary_var      <- bind_rows(summary_var)      %>% distinct()
    summary_num      <- bind_rows(summary_num)      %>% distinct()
    summary_text     <- bind_rows(summary_text)     %>% distinct()
    summary_date     <- bind_rows(summary_date)     %>% distinct()
    summary_datetime <- bind_rows(summary_datetime) %>% distinct()
    summary_cat      <- bind_rows(summary_cat)      %>% distinct()
    
    report$`Variables summary (all)` <-
      report$`Variables summary (all)` %>%
      left_join(summary_var, by = "name", multiple = "all") %>%
      select(
        "index in data dict." ,
        "name",
        matches("Quality assessment comment"),
        starts_with("label")[1],
        "Data Dictionary valueType",
        "Estimated dataset valueType",
        "Actual dataset valueType",
        matches("Categorical variable"),
        matches("Categories in data dictionary"),
        starts_with('Grouping variable: '),
        matches("Total number of observations"),
        matches("Nb. distinct values"),
        matches("% total Valid values"),
        matches("% NA"),
        matches("% Valid categorical values (if applicable)"),
        matches("% Missing categorical values (if applicable)"))
    
    report$`Text variable summary` <-
      suppressMessages(report$`Text variable summary` %>%
      inner_join(summary_var , by = "name", multiple = "all") %>%
      inner_join(summary_text, multiple = "all")) %>%
      select(
        "index in data dict." ,
        "name",
        matches("Quality assessment comment"),
        starts_with("label")[1],
        "Data Dictionary valueType",
        "Estimated dataset valueType",
        "Actual dataset valueType",
        matches("Categorical variable"),
        matches("Categories in data dictionary"),
        starts_with('Grouping variable: '),
        matches("Total number of observations"),
        matches("Nb. distinct values"),
        matches("% total Valid values"),
        matches("% NA"),
        matches("% Valid categorical values (if applicable)"),
        matches("% Missing categorical values (if applicable)"),everything())

    report$`Date variable summary` <-
      suppressMessages(report$`Date variable summary` %>%
      inner_join(summary_var, by = "name", multiple = "all") %>%
      inner_join(summary_date, multiple = "all")) %>%
      select(
        "index in data dict." ,
        "name",
        matches("Quality assessment comment"),
        starts_with("label")[1],
        "Data Dictionary valueType",
        "Estimated dataset valueType",
        "Actual dataset valueType",
        matches("Categorical variable"),
        matches("Categories in data dictionary"),
        starts_with('Grouping variable: '),
        matches("Total number of observations"),
        matches("Nb. distinct values"),
        matches("% total Valid values"),
        matches("% NA"),
        matches("% Valid categorical values (if applicable)"),
        matches("% Missing categorical values (if applicable)"),everything())

    report$`Datetime variable summary` <-
      suppressMessages(report$`Datetime variable summary` %>%
      inner_join(summary_var , by = "name", multiple = "all") %>%
      inner_join(summary_datetime, multiple = "all")) %>%
      select(
        "index in data dict." ,
        "name",
        matches("Quality assessment comment"),
        starts_with("label")[1],
        "Data Dictionary valueType",
        "Estimated dataset valueType",
        "Actual dataset valueType",
        matches("Categorical variable"),
        matches("Categories in data dictionary"),
        starts_with('Grouping variable: '),
        matches("Total number of observations"),
        matches("Nb. distinct values"),
        matches("% total Valid values"),
        matches("% NA"),
        matches("% Valid categorical values (if applicable)"),
        matches("% Missing categorical values (if applicable)"),everything())
    
    report$`Numerical variable summary` <-
      suppressMessages(report$`Numerical variable summary` %>%
      inner_join(summary_var, by = "name", multiple = "all") %>%
      inner_join(summary_num, multiple = "all")) %>%
      select(
        "index in data dict." ,
        "name",
        matches("Quality assessment comment"),
        starts_with("label")[1],
        "Data Dictionary valueType",
        "Estimated dataset valueType",
        "Actual dataset valueType",
        matches("Categorical variable"),
        matches("Categories in data dictionary"),
        starts_with('Grouping variable: '),
        matches("Total number of observations"),
        matches("Nb. distinct values"),
        matches("% total Valid values"),
        matches("% NA"),
        matches("% Valid categorical values (if applicable)"),
        matches("% Missing categorical values (if applicable)"),everything())

    report$`Categorical variable summary` <-
      suppressMessages(report$`Categorical variable summary` %>%
      inner_join(summary_var, by = "name", multiple = "all") %>%
      inner_join(summary_cat, multiple = "all")) %>% 
      select(
        "index in data dict." ,
        "name",
        matches("Quality assessment comment"),
        starts_with("label")[1],
        "Data Dictionary valueType",
        "Estimated dataset valueType",
        "Actual dataset valueType",
        matches("Categorical variable"),
        matches("Categories in data dictionary"),
        starts_with('Grouping variable: '),
        matches("Total number of observations"),
        matches("Nb. distinct values"),
        matches("% total Valid values"),
        matches("% NA"),
        matches("% Valid categorical values (if applicable)"),
        matches("% Missing categorical values (if applicable)"),everything())
    }
    
  message("    Summarize global information (Overview)")
  
  Overview <-
    tibble(`---` = c(
      'Quality control of dataset'                                             ,
      'Date'                                                                   ,
      '1_Name of the dataset'                                                  ,
      '    1_Identifier Variable'                                              ,
      '    1_Grouping variable'                                                ,
      '    1_Variables'                                                        ,
      '        1_Total number of variables (incl. identifier)'                 ,
      '        1_Total number of empty columns'                                ,
      '    1_Data type in dictionary (valueType)'                              ,
      '        1_Nb. text variables'                                           ,
      '        1_Nb. date variables'                                           ,
      '        1_Nb. datetime variables'                                       ,
      '        1_Nb. numerical variables'                                      ,
      '        1_Nb. categorical variables'                                    ,
      '    2_Rows'                                                             ,
      '        2_Total number of observations'                                 ,
      '        2_Nb. distinct observations'                                   ))
  

  Overview_group <- c(list(`(all)` = dataset),dataset_group)
  
  for(i in names(Overview_group)){
    
    Overview_group[[i]] <-
      Overview %>%
      mutate(`-----` = case_when(
        .data$`---` == 'Quality control of dataset'                            ~
          " ",
        .data$`---` == 'Date'                                                  ~
          as.character(Sys.Date()),
        .data$`---` == '1_Name of the dataset'                                 ~
          dataset_name %>% str_remove_all("`"),
        .data$`---` == '    1_Identifier Variable'                             ~
          toString(col_id),
        .data$`---` == '    1_Grouping variable'                               ~
          toString(group_by),  
        .data$`---` == '    1_Variables'                                       ~
          " ",
        .data$`---` == '        1_Total number of variables (incl. identifier)'~
          as.character(length(unique(report$`Data dictionary summary`$name))),
        .data$`---` == '        1_Total number of empty columns'               ~
          as.character(
            ncol(Overview_group[[i]][vapply(X = Overview_group[[i]],
                          FUN = function(x) all(is.na(x)),
                          FUN.VALUE = logical(1))])),
        .data$`---` == '    1_Data type in dictionary (valueType)'             ~
          " ",
        .data$`---` == '        1_Nb. text variables'                          ~
      as.character(length(unique(report$`Text variable summary`$name))),
        .data$`---` == '        1_Nb. date variables'                          ~
      as.character(length(unique(report$`Date variable summary`$name))),
        .data$`---` == '        1_Nb. datetime variables'                      ~
      as.character(length(unique(report$`Datetime variable summary`$name))),
        .data$`---` == '        1_Nb. numerical variables'                     ~
      as.character(length(unique(report$`Numerical variable summary`$name))),
        .data$`---` == '        1_Nb. categorical variables'                   ~
      as.character(length(unique(report$`Categorical variable summary`$name))),
        .data$`---` == '    2_Rows'                                            ~
          i,
        .data$`---` == '        2_Total number of observations'                ~
      as.character(nrow(Overview_group[[i]])),
        .data$`---` == '        2_Nb. distinct observations'                   ~
        ifelse(is.null(col_id),as.character(nrow(Overview_group[[i]])),
        as.character(nrow(distinct(Overview_group[[i]][col_id])))),
        TRUE                                                                   ~
          "EMPTY",
      )) %>%
      mutate(
        `---` = str_remove_all(.data$`---`, "1_"),
        `---` = str_remove_all(.data$`---`, "2_")) %>%
      rename(
        `Quality control of dataset` = .data$`---`,
        !! as.symbol(i) := .data$`-----`) %>% slice(-1)}
  
  for(i in names(Overview_group)[-1]){
    Overview_group[[i]][c(1:11),2] <- " " 
    Overview_group[[i]][,1] <- NULL
  }
  
  report$Overview <- bind_cols(Overview_group)
  if(group_by == ''){report$Overview <- 
    report$Overview %>% slice(-4) %>% select(-3)}
  
  message("    Generate report\n")

  # create report structure

  report$`Data dictionary summary`       <- NULL
  report <- report[vapply(X = report,
                          FUN = function(x) sum(nrow(x)) > 0,
                          FUN.VALUE = logical(1))]
  report <- report[unique(c('Overview', names(report)))]

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
#' [OBiBa data type of a variable](https://opaldoc.obiba.org/en/variables-data.html#value-types). 
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
#' valueType that could be applied to the dataset. FALSE by default.
#'
#' @returns
#' A list of data frames containing overall assessment reports and summaries grouped by dataset.
#'
#' @examples
#' {
#' 
#' # use madshapR_DEMO provided by the package
#' library(dplyr)
#' 
#' ###### Example 1: Combine functions and summarize datasets.
#' dossier <- list(iris = tibble())
#' 
#' dossier_summary <- dossier_summarize(dossier)
#' glimpse(dossier_summary)
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
    valueType_guess = FALSE){
  
  # amelioration :rajouter taxonomy
  
  # check on arguments
  as_dossier(dossier)
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  if(!is.logical(valueType_guess))
    stop(call. = FALSE,
         '`as_data_dict_mlstr` must be TRUE or FALSE (TRUE by default)')
  
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
        group_by = group_by,
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
#' valid/non valid/missing values (based on the data dictionary information if 
#' provided). This function can be used to personalize report parameters and is 
#' internally used in the function [dataset_summarize()].
#' 
#' Generates a data frame that evaluates and aggregates all columns
#' in a dataset with (if any) its data dictionary. The data dictionary (if
#' present) separates observations between open values, missing values,
#' categorical values , and categorical missing values (which corresponds to the
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
#' ###### Example : Any data frame can be a dataset by definition.
#' head(dataset_preprocess(iris))
#'
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
dataset_preprocess <- function(dataset, data_dict = NULL){
  
  # handle atomics
  summary_tbl <-
    tibble(
      `index` = as.integer(),
      `name` = as.character(),
      `Categorical variable` = as.character(),
      `valid_class` = as.character(),
      `value_var_occur` = as.numeric(),
      `value_var` = as.character(),
      `index_value` = as.integer(),
      `cat_index` = as.integer(),
      `cat_label` = as.character())
  
  # handle atomics
  if(is.atomic(dataset) & length(dataset) == 0){return(summary_tbl)}
  if(is.atomic(dataset))
    return(dataset_preprocess(dataset = tibble(name = dataset), data_dict))
  
  # tests
  as_dataset(dataset)
  
  # if no data_dict
  if(is.null(data_dict)){
    data_dict <- data_dict_extract(dataset,as_data_dict_mlstr = TRUE)
  }else{
    data_dict <- as_data_dict_mlstr(data_dict,name_standard = FALSE)}
  
  data_dict_var  <-
    data_dict[['Variables']] %>%
    select('name') %>%
    mutate(`Categorical variable` = NA_character_) %>%
    add_index()
  
  if(sum(nrow(data_dict[['Categories']])) > 0){
    data_dict_cat <-
      data_dict[['Categories']] %>%
      select(
        name = .data$`variable`, 
        value_var = .data$`name`,
        cat_label = matches(c("^label$","^label:[[:alnum:]]","^labels$"))[1],
        valid_class = .data$`missing`) %>%
      group_by(.data$`name`, .data$`valid_class`) %>%
      add_index('cat_index') %>%
      ungroup() %>%
      mutate(
        valid_class =
          ifelse(
            .data$`valid_class` == TRUE,
            "2_Missing values", "1_Valid values")) %>%
      mutate(value_var = as.character(.data$`value_var`))
    
  }else{
    data_dict_cat <-
      tibble(cat_index = as.integer(),name = as.character(),
             value_var = as.character(),cat_label = as.character(),
             valid_class = as.character())}
  
  data_dict_var  <-
    data_dict_var %>%
    full_join(data_dict_cat,by = "name",multiple = "all") %>%
    mutate(`Categorical variable` = 
             ifelse(is.na(.data$`valid_class`),"no","yes"))
  
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
        mutate(name = i) %>%
        add_index('index_in_dataset')
      
      # count observations in the dataset, including no-observation of
      # categorical outcomes
      # classification of observations not in the data_dict(3) and 'true'
      # NA as na values(4)
      # arrange (1),(2),(3),(4), by original index
      summary   <-
        tbl_var %>%
        full_join(
          data_dict_var[data_dict_var$`name` == i,],
          by = c("value_var", "name")) %>%
        mutate(value_var_occur = replace_na(.data$`value_var_occur`, 0)) %>%
        fill(
          .data$`index`,.direction = "downup") %>%
        mutate(
          valid_class =
            case_when(
              is.na(.data$`value_var`)  ~ "4_NA values",
              TRUE ~ ifelse(
                is.na(.data$`valid_class`),
                "3_Valid other values",.data$`valid_class`))) %>%
        arrange(.data$`valid_class`) %>%
        group_by(.data$`valid_class`) %>%
        add_index('index_value', .force = TRUE) %>%
        ungroup
      
      # handle categories which are non-categorical, categorical and mixed
      summary$`Categorical variable` <-
        ifelse(
          nrow(summary[
            summary$`valid_class` ==
              '3_Valid other values',]) > 0 &
            nrow(summary[
              summary$`valid_class` %in%
                c('1_Valid values','2_Missing values'),]) >= 1,
          "mix",
          unique(
            summary[!is.na(summary$`Categorical variable`),
                    ][['Categorical variable']]))
      
      summary_tbl <- bind_rows(summary_tbl, summary)
    }
  }
  
  final_resume <-
    summary_tbl %>%
    mutate(value_var_occur = replace_na(.data$`value_var_occur`, 0)) %>%
    mutate(
      valid_class = replace_na(.data$`valid_class`, '3_Valid other values')) %>%
    dplyr::filter(
      !(.data$`value_var_occur`== 0 & .data$`valid_class` == "4_NA values")) %>%
    select(.data$index,
           .data$`name`, everything()) %>%
    arrange(.data$`index`,.data$`valid_class`)
  
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
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param dataset_preprocess A data frame which provides summary of the 
#' variables (used for internal processes and programming).
#' @param .dataset_preprocess `r lifecycle::badge("deprecated")`
#'
#' @returns
#' A data frame providing statistical description of variables present in
#' a dataset.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' ###### Example : Any data frame can be a dataset by definition.
#' dataset_preprocess <- dataset_preprocess(iris)
#' glimpse(summary_variables(dataset_preprocess = dataset_preprocess))
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
summary_variables <- function(
    dataset = NULL,
    data_dict = NULL,
    dataset_preprocess = .dataset_preprocess,
    .dataset_preprocess = NULL){
  
  #  (for internal processes and programming).
  if(is.null(dataset_preprocess)) dataset_preprocess <- 
      dataset_preprocess(dataset, data_dict)
  summary <- dataset_preprocess
  
  # init
  summary_tbl <- tibble(name = as.character())
  if(nrow(summary) == 0) return(summary_tbl)
  
  for(i in unique(summary$name)){
    # stop()}
    
    summary_i <- summary %>% dplyr::filter(.data$`name` == i)
    
    # summary the output
    summary_i <-
      tibble(
        `name` = i,
        
        `Categorical variable` = unique(summary_i$`Categorical variable`),
        
        `Total number of observations` = sum(summary_i$`value_var_occur`),
        
        `Nb. distinct values` =
          length(unique(summary_i[
            summary_i$`value_var_occur` == 1 &
              !is.na(summary_i$`value_var`),]$`value_var`)),
        
        `% total Valid values` =
          round(100*(sum(summary_i[
            summary_i$`valid_class` %in%
              c("1_Valid values","3_Valid other values"),]$value_var_occur)/
          sum(summary_i$`value_var_occur`)),2),
        
        `% NA` =
          round(100*(sum(summary_i[
            summary_i$`valid_class` %in%
              c("4_NA values"),]$value_var_occur)/
          sum(summary_i$`value_var_occur`)),2),
        
        `% Valid categorical values (if applicable)` =
          ifelse(all(summary_i$`Categorical variable` != 'no'),
                 round(100*(sum(summary_i[
                   summary_i$`valid_class` %in%
                     c("1_Valid values"),]$value_var_occur)/
                   sum(summary_i$`value_var_occur`)),2),
                 NA_real_),
        
        `% Missing categorical values (if applicable)` =
          ifelse(all(summary_i$`Categorical variable` != 'no'),
                 round(100*(sum(summary_i[
                   summary_i$`valid_class` %in%
                     c("2_Missing values"),]$value_var_occur)/
                   sum(summary_i$`value_var_occur`)),2),
                 NA_real_)
      )
    
    summary_tbl <- bind_rows(summary_tbl, summary_i)
  }
  
  summary_tbl <-
    summary_tbl %>%
    mutate(
      `Quality assessment comment` = case_when(
        
        .data$`Total number of observations` == .data$`Nb. distinct values`    ~
"[INFO] - All observations are unique" ,
        
        .data$`Nb. distinct values` == 0                                       ~
"[INFO] - The column is empty" ,
        
        .data$`Nb. distinct values` == 1                                       ~
"[INFO] - The column has a constant value",
        
        .data$`Nb. distinct values` > 0 & .data$`% total Valid values` == 0    ~
"[INFO] - All the categorical values present in the dataset are 'missing'",
        
        .data$`% total Valid values` > 0 &
          .data$`% Valid categorical values (if applicable)` == 0              ~
"[INFO] - All the categorical values found in the dataset are non categorical" ,
        
        TRUE                                                                   ~
          NA_character_
      )) %>%
    select(.data$`name`,.data$`Quality assessment comment`, everything())
  
  return(summary_tbl)
  
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
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param dataset_preprocess A data frame which provides summary of the variables
#' (for internal processes and programming).
#' @param .dataset_preprocess `r lifecycle::badge("deprecated")`
#'
#' @returns
#' A data frame providing statistical description of 'text' variables present
#' in a dataset.
#'
#' @examples
#' {
#'    
#' ###### Example : Any data frame can be a dataset by definition.
#' library(dplyr)
#' 
#' dataset_preprocess <- dataset_preprocess(starwars['homeworld'])
#' glimpse(summary_variables_text(dataset_preprocess = dataset_preprocess))
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
summary_variables_text <- function(
    dataset = NULL,
    data_dict = NULL,
    dataset_preprocess = .dataset_preprocess,
    .dataset_preprocess = NULL){
  
  # init
  summary_tbl <- tibble(name = as.character())
  if(is.null(dataset_preprocess)) return(summary_tbl)
  if(!nrow(dataset_preprocess)) return(summary_tbl)
  
  summary <-
    dataset_preprocess %>%
    dplyr::filter(.data$`value_var_occur` == 1 ) %>%
    dplyr::filter(.data$`valid_class`  == "3_Valid other values")
  
  # init
  summary_tbl <- tibble(name = as.character())
  if(nrow(summary) == 0) return(summary_tbl)
  
  for(i in unique(summary$name)){
    # stop()}
    
    summary_i <-
      summary %>%
      dplyr::filter(.data$`name` == i)
    
    # turn the output to be readable
    if(summary_i %>% nrow > 0){
      summary_i <-
        tibble(
          `name` = i,
          
          `Most seen value(s)` =
            summary_i %>%
            count(.data$`value_var`) %>%
            dplyr::filter(if_any(.data$`n`, ~ . == max(.))) %>%
            slice(1:6) %>% 
            mutate(value_var = ifelse(
                row_number() == 6,
                '[...]', 
                .data$`value_var`)) %>%
            pull(.data$`value_var`) %>% paste0(collapse = " ; ") %>%
            str_replace('; \\[\\.\\.\\.\\]$','[...]'),
          
          `Rarest value(s)` =
            summary_i %>%
            count(.data$`value_var`) %>%
            dplyr::filter(if_any(.data$`n`, ~ . == min(.))) %>%
            slice(1:6) %>% mutate(value_var = ifelse(row_number() == 6,'[...]',
                                                     .data$`value_var`)) %>%
            pull(.data$`value_var`) %>% paste0(collapse = " ; ") %>%
            str_replace('; \\[\\.\\.\\.\\]$','[...]'),
        )
      
    }else{summary_i <- tibble(name = as.character())}
    
    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }
  
  # final_summary <-
  #   summary_variables(dataset, data_dict, dataset_preprocess) %>%
  #   dplyr::filter(.data$`categorical` != 'yes') %>%
  #   full_join(summary_tbl, by = 'name')
  
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
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param dataset_preprocess A data frame which provides summary of the 
#' variables (for internal processes and programming).
#' @param .dataset_preprocess `r lifecycle::badge("deprecated")`
#'
#' @returns
#' A data frame providing statistical description of 'date' variables present
#' in a dataset.
#'
#' @examples
#' {
#'    
#' # use madshapR_DEMO provided by the package
#' library(dplyr)
#' library(fabR)
#' 
#' dataset_preprocess <- 
#'   madshapR_DEMO$dataset_TOKYO %>%
#'     mutate(dob = as_any_date(dob)) %>%
#'     select(dob) %>%
#'     head() %>%
#'     dataset_preprocess
#'
#' glimpse(summary_variables_date(dataset_preprocess = dataset_preprocess))
#'
#' }
#'
#' @import dplyr tidyr lubridate fabR
#' @importFrom rlang .data
#'
#' @export
summary_variables_date <- function(
    dataset = NULL,
    data_dict = NULL,
    dataset_preprocess = .dataset_preprocess,
    .dataset_preprocess = NULL){
  
  # init
  summary_tbl <- tibble(name = as.character())
  if(is.null(dataset_preprocess)) return(summary_tbl)
  if(!nrow(dataset_preprocess)) return(summary_tbl)
  
  date_format <-
    guess_date_format(distinct(dataset_preprocess['value_var']))
  
  if(date_format$`% values formated` < 100){
    warning(
      "Problem while computing date type variables due to ambiguous format.\n",
      "They will be analysed as text variables\n",
      bold("Useful tip:"),
      "Use dataset_evaluate(dataset) to get an assessment of your dataset.")
    
    final_summary <- 
      summary_variables_text(dataset_preprocess = dataset_preprocess)
    return(final_summary)
  }
  
  summary <-
    dataset_preprocess %>%
    mutate(
      value_var =
        as_any_date(.data$`value_var`,date_format$`Date format`)) %>%
    dplyr::filter(.data$`value_var_occur` == 1 ) %>%
    dplyr::filter(.data$`valid_class`  == "3_Valid other values")
  
  # init
  summary_tbl <- tibble(name = as.character())
  if(nrow(summary) == 0) return(summary_tbl)
  
  for(i in unique(summary$name)){
    # stop()}
    
    summary_i <-
      summary %>%
      dplyr::filter(.data$`name` == i)
    
    # turn the output to be readable
    if(summary_i %>% nrow > 0){
      summary_i <-
        tibble(
          `name`         =  i,
          `Lowest date`      =
            min((summary_i$`value_var`),na.rm = TRUE),
          `Highest date`     =
            max((summary_i$`value_var`),na.rm = TRUE),
          `MIN\n(year)`      =
            summary(as.integer(year(summary_i$`value_var`)))[[1]],
          `Q1\n(year)`       =
            summary(as.integer(year(summary_i$`value_var`)))[[2]] %>% round,
          `MEDIAN\n(year)`   =
            summary(as.integer(year(summary_i$`value_var`)))[[3]] %>% round,
          `Q3\n(year)`       =
            summary(as.integer(year(summary_i$`value_var`)))[[5]] %>% round,
          `MAX\n(year)`      =
            summary(as.integer(year(summary_i$`value_var`)))[[6]],
          `MEAN\n(year)`     =
            summary(as.integer(year(summary_i$`value_var`)))[[4]] %>% round,
          `Span\n(year)`     =
            year(.data$`Highest date`) - year(.data$`Lowest date`))
      
    }else{summary_i <- tibble(name = as.character())}
    
    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }
  
  # final_summary <-
  #   summary_variables(dataset, data_dict, dataset_preprocess) %>%
  #   dplyr::filter(.data$`categorical` != 'yes') %>%
  #   full_join(summary_tbl, by = 'name')
  
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
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param dataset_preprocess A data frame which provides summary of the 
#' variables (for internal processes and programming).
#' @param .dataset_preprocess `r lifecycle::badge("deprecated")`
#'
#' @returns
#' A data frame providing statistical description of 'datetime' variables present
#' in a dataset.
#'
#' @examples
#' {
#'    
#' # use madshapR_DEMO provided by the package
#' library(dplyr)
#' library(lubridate)
#' library(fabR)
#' 
#' dataset_preprocess <-
#'   madshapR_DEMO$dataset_TOKYO %>%
#'   mutate(dob = as_datetime(as_any_date(dob))) %>%
#'   select(dob) %>%
#'   head() %>%
#'   dataset_preprocess
#'
#' glimpse(summary_variables_datetime(dataset_preprocess = dataset_preprocess))
#'
#' }
#'
#' @import dplyr tidyr lubridate fabR
#' @importFrom rlang .data
#'
#' @export
summary_variables_datetime <- function(
    dataset = NULL,
    data_dict = NULL,
    dataset_preprocess = .dataset_preprocess,
    .dataset_preprocess = NULL){
  
  # init
  summary_tbl <- tibble(name = as.character())
  if(is.null(dataset_preprocess)) return(summary_tbl)
  if(!nrow(dataset_preprocess)) return(summary_tbl)
  
  final_summary <- 
    summary_variables_text(dataset_preprocess = dataset_preprocess)
  return(final_summary)

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
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param dataset_preprocess A data frame which provides summary of the 
#' variables (for internal processes and programming).
#' @param .dataset_preprocess `r lifecycle::badge("deprecated")`
#' 
#' @returns
#' A data frame providing statistical description of 'numerical' variables
#' present in a dataset.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' ###### Example : Any data frame can be a dataset by definition.
#' dataset_preprocess <- dataset_preprocess(iris)
#' glimpse(summary_variables_numeric(dataset_preprocess = dataset_preprocess))
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#' @importFrom stats sd
#'
#' @export
summary_variables_numeric <- function(
    dataset = NULL,
    data_dict = NULL,
    dataset_preprocess = .dataset_preprocess,
    .dataset_preprocess = NULL){
  
  # init
  summary_tbl <- tibble(name = as.character())
  if(is.null(dataset_preprocess)) return(summary_tbl)
  if(!nrow(dataset_preprocess)) return(summary_tbl)
  
  summary <-
    dataset_preprocess %>%
    dplyr::filter(.data$`value_var_occur` == 1 ) %>%
    dplyr::filter(.data$`valid_class`  == "3_Valid other values")
  
  # init
  summary_tbl <- tibble(name = as.character())
  if(nrow(summary) == 0) return(summary_tbl)
  
  for(i in unique(summary$name)){
    # stop()}
    
    summary_i <-
      summary %>%
      dplyr::filter(.data$`name` == i)
    
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
          `MEAN`    = summary(summary_i$`value_var`)[[4]],
          `STDEV`   = sd(summary_i$`value_var`,na.rm = TRUE),
          )
      
    }else{summary_i <- tibble(name = as.character())}
    
    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }
  
  # final_summary <-
  #   summary_variables(dataset, data_dict, dataset_preprocess) %>%
  #   dplyr::filter(.data$`categorical` != 'yes') %>%
  #   full_join(summary_tbl, by = 'name')
  
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
#' [OBiBa data type of a variable](https://opaldoc.obiba.org/en/variables-data.html#value-types). 
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
#' @param dataset_preprocess A data frame which provides summary of the variables 
#' (for internal processes and programming).
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
#' 
#' ###### Example : Any data frame can be a dataset by definition.
#' dataset_preprocess <- dataset_preprocess(iris['Species'])
#' glimpse(summary_variables_categorical(dataset_preprocess = dataset_preprocess))
#' 
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom rlang .data
#'
#' @export
summary_variables_categorical <- function(
    dataset = NULL,
    data_dict = NULL,
    dataset_preprocess = .dataset_preprocess,
    .dataset_preprocess = NULL){
  
  # init
  summary_tbl <- tibble(name = as.character())
  if(is.null(dataset_preprocess)) return(summary_tbl)
  if(!nrow(dataset_preprocess)) return(summary_tbl)
  
  summary <-
    dataset_preprocess %>%
    group_by(across(c(-"value_var_occur",-"index_value"))) %>%
    summarise(
      n = sum(as.integer(.data$`value_var_occur`)),
      .groups = 'drop') %>%
    arrange(.data$`index`, .data$`valid_class`,.data$`cat_index`) %>%
    ungroup
  
  if(nrow(summary) == 0) return(summary_tbl)
  
  for(i in unique(summary$name)){
    # stop()}
    
    summary_i <-
      summary %>%
      dplyr::filter(.data$`name` == i)
    
    summary_category <- 
      summary_i %>%
      mutate(
        cat_order = .data$`cat_index`,
        cat_index = 
          ifelse(
            .data$`value_var` == .data$`cat_label`,
            .data$`cat_label`,
            paste0('[',.data$`value_var`,'] - ',.data$`cat_label`)),
        
        cat_index = 
          ifelse(nchar(.data$`cat_index`) > 40,
                 paste0(str_sub(.data$`cat_index`,1,40),' [...]'),
                 .data$`cat_index`),
        cat_index = 
          ifelse(
            is.na(.data$`cat_label`),NA,.data$`cat_index`)) %>%
      
      ungroup %>%
      select("valid_class","cat_index","cat_order","value_var","n") %>%
      group_by(.data$`valid_class`,.data$`cat_index`, .data$`cat_order`) %>%
      summarise(
        n = sum(.data$`n`),
        value_var = paste0(.data$`value_var`, collapse = "{semicolon}"),
        .groups = "drop") %>%
      separate_rows("value_var",sep = "{semicolon}") %>%
      distinct() %>%
      group_by(.data$`valid_class`,.data$`cat_index`,.data$`cat_order`,.data$`n`) %>%
      summarise(
        name_var = paste0(.data$`value_var`, collapse = "{semicolon}"),
        .groups = "drop") %>%
      arrange(.data$`valid_class`,.data$`cat_order`) %>%
      mutate(
        cat_index = replace_na(.data$`cat_index`,'{blank}'),
        name_var  = str_replace(.data$`name_var`, "^NA$","")) %>%
      
      # ) %>% View
      # handle the round
      mutate(n_perc =
               paste0(round(100*(.data$`n` / sum(.data$`n`)),2),"%")) %>%
      rowwise() %>%
      mutate(
        name_var2 = ifelse(
          .data$`valid_class` == "3_Valid other values",
          unlist(.data$`name_var` %>% str_split("\\{semicolon\\}"))[6],.data$`name_var`),
        name_var = ifelse(
          .data$`valid_class` == "3_Valid other values",
          unlist(.data$`name_var` %>% str_split("\\{semicolon\\}"))[1:5] %>%
            paste0(collapse = "{semicolon}"),.data$`name_var`)
      ) %>%
      mutate(
        name_var = str_replace_all(.data$`name_var`,'\\{semicolon\\}NA',''),
        name_var = ifelse(.data$`valid_class` == "3_Valid other values" &
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
          ifelse(.data$`valid_class` == "3_Valid other values",
                 .data$`name_var`, ""),
        list_values        =
          ifelse(.data$`valid_class` == "3_Valid other values", "",
                 .data$`cat_index`),
        list_values        = na_if(.data$`list_values`,'{blank}'),
        n_perc             =
          paste0(" : ", .data$`n_perc`)) %>%
      unite("list_values",.data$`list_values`,.data$`n_perc`,
            sep = "",remove = TRUE, na.rm = TRUE) %>%
      mutate(categorical_index = str_sub(.data$`valid_class`,1,1)) %>%
      group_by(.data$`valid_class`,.data$`categorical_index`) %>%
      mutate(
        valid_class = case_when(
          .data$`valid_class` == "1_Valid values"       ~
            "Valid categorical values : \n",
          .data$`valid_class` == "2_Missing values"     ~
            "\nMissing categorical values : \n",
          .data$`valid_class` == "3_Valid other values" ~
            "\nOther values (non categorical)",
          .data$`valid_class` == "4_NA values"          ~
            "\nNA values",
          TRUE                             ~ .data$`valid_class`)) %>%
      select(-'name_var') %>%
      
      mutate(across(c(
        'list_values','cat_var_absence','other_val_presence'),
        ~ ifelse(.data$`categorical_index` == 4 ,.,paste0(.,"\n")))) %>%
      mutate(
        valid_class =
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
        "list_values",.data$`valid_class`,.data$`list_values`,
        sep = "",remove = TRUE, na.rm = TRUE) %>%
      unite(
        "cat_var_absence",.data$`category_space_prefix`,
        .data$`cat_var_absence`,.data$`category_space_suffix`,
        sep = "",remove = FALSE, na.rm = TRUE) %>%
      unite(
        "other_val_presence",
        .data$`category_space_prefix`,.data$`other_val_presence`,
        .data$`category_space_suffix`,
        sep = "",remove = TRUE, na.rm = TRUE) %>%
      mutate(
        cat_var_absence =
          ifelse(.data$`cat_var_absence` %>% str_squish() == "",
                 "",.data$`cat_var_absence`),
        other_val_presence =
          ifelse(.data$`other_val_presence` %>% str_squish() == "",
                 "",.data$`other_val_presence`)) %>%
      ungroup() %>%
      select(-.data$`categorical_index`, -.data$`n`) %>%
      summarise(across(everything(), ~ paste0(.,collapse = "")))
    
    
    if(nrow(dplyr::filter(
      summary_i,
      .data$`valid_class` %in% c("1_Valid values","2_Missing values"))) > 0){
      
      summary_i <-
        tibble(
          
          `name`                   =
            unique(summary_i$name),
          
          `Values present in dataset`                           =
            summary_category$list_values,
          
          `Data dictionary categories not present in dataset`   =
            summary_category$cat_var_absence,
          
          `Dataset value not present in data dictionary`        =
            summary_category$other_val_presence
          
        )
      
    }else{summary_i <- tibble(name = as.character())}
    
    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }
  
  # final_summary <-
  #   summary_variables(dataset, data_dict, dataset_preprocess) %>%
  #   dplyr::filter(.data$`categorical` == 'yes' | 
  #   .data$`categorical` == 'mix') %>%
  #   full_join(summary_tbl, by = 'name')
  
  return(summary_tbl)
}
