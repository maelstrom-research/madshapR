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
#' valueType that could be applied to the dataset. FALSE by default.
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#' @param dataset_name A character string specifying the name of the dataset
#' (internally used in the function [dossier_evaluate()]).
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
#' library(dplyr)
#' 
#' 
#' ###### Example 1: use madshapR_example provided by the package
#' dataset <- as_dataset(madshapR_example$`dataset_example`, col_id = 'part_id')
#' data_dict <- as_data_dict(madshapR_example$`data_dict_example`)
#'   
#' summary_dataset <- dataset_summarize(dataset, data_dict)
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
    group_by = NULL,
    taxonomy = NULL,
    dataset_name = .dataset_name,
    valueType_guess = TRUE,
    .dataset_name = NULL){
  
  # fargs <- list()
  fargs <- as.list(match.call(expand.dots = TRUE))
  
  if(!is.logical(valueType_guess))
    stop(call. = FALSE,
         '`valueType_guess` must be TRUE or FALSE (FALSE by default)')

  # check on arguments : dataset
  as_dataset(dataset) # no col_id
  
  # check on arguments : data_dict. 
  as_data_dict_shape(data_dict)
  
  dataset_name <- 
    ifelse(!is.null(dataset_name),dataset_name,
           make_name_list(
             as.character(fargs[['dataset']]),list_elem = list(NULL)))
  
  # check on argument : taxonomy
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  
  # attempt to catch group_by from the group_vars if the dataset is grouped
  
  if(toString(substitute(group_by)) == '') group_by <- NULL
  if(length(group_vars(dataset)) == 1 & toString(substitute(group_by)) == ''){
    group_by <- group_vars(dataset)
  }
  
  col_id <- col_id(dataset)
  dataset <- as_dataset(ungroup(dataset),col_id)
    
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
        as_data_dict_mlstr()})
  
  # evaluate the dataset
  report <- list()
  
  # [GF] - note : this step adds time in the process
  # dataset_with_data_dict <- data_dict_apply(dataset,data_dict)
  
  # if the dataset has no participant, group_by is null
  if(nrow(dataset) == 0) group_by <- NULL
  
  # attempt to catch group_by
  if(toString(substitute(group_by)) != ''){
    group_by <- tryCatch(
      expr  = {toString(names(dataset[toString(substitute(group_by))]))},
      error = function(cond){return(toString(names(dataset[group_by])))})
    
    # if the group is not in the data dictionary, create it. Adds the 
    # group values that are in the dataset but not present in the data dictionary
    if(group_by != ''){
      
      data_dict_group_by <- 
        dataset_zap_data_dict(dataset) %>% 
        mutate(across(!!group_by, as_category)) %>%
        select(all_of(group_by)) %>% data_dict_extract(as_data_dict_mlstr = TRUE)
      
      data_dict_group_by <- 
        data_dict_group_by[["Categories"]] %>%
        rename_with(~ case_when(. == "label" ~ first_lab_var,TRUE ~ .))
        
      anti_group_cat_lab <- 
        data_dict_filter(
        data_dict,filter_cat = paste0("!variable %in% '", group_by,"'"))
      
      group_cat_lab <- data_dict_filter(
        data_dict,filter_cat = paste0("variable %in% '", group_by,"'"))
      
      data_dict <- data_dict
      data_dict[['Categories']] <- 
        bind_rows(
          anti_group_cat_lab[['Categories']],
          group_cat_lab[['Categories']],
          data_dict_group_by) %>%
        add_index(.force = TRUE) %>%
        group_by(.data$variable,.data$`name`) %>% slice(1) %>%
        ungroup %>% arrange(.data$`index`) %>% select(-"index")
      
      data_dict <- as_data_dict_mlstr(data_dict)
    }
    
  }else{ group_by <- ''}
  
  # [GF resolved] - question : now it is not mandatory that the grouping variable must
  # have no NA . the group is called (Empty) and works as any other group. A 
  # decision has to be made.
  
#   if(group_by != ''){
# 
#     has_empty <-
#       dataset %>%
#       reframe(across(!! group_by, ~ !all(!is.na(.)))) %>%
#       unlist %>% all
# 
#     if(has_empty){
#       stop(call. = FALSE,
# "Grouping variable contains empty values, and cannot be used as a grouping variable.")
# 
#     }
#   }
  
  # catch variable labels for documentation
  data_dict_labels <- data_dict_add_labels_short(data_dict,.keep_columns = TRUE)
  
  if(group_by != ''){
    
    preprocess_group <- 
      dataset_preprocess(
        dataset = dataset[c(group_by)],
        data_dict = data_dict)
    
    cat_lab <- data_dict_filter(
      data_dict_labels,filter_cat = paste0("variable == '", group_by,"'"))

    cat_lab <- 
      cat_lab[['Categories']] %>% 
      select("name",'madshapR::label_short_cat') %>%
      rename(
        !! group_by := 'name') %>%
      mutate(across(everything(),as.character)) %>%
      add_index('madshapR::category_level')

    # adjust the valueType of the dataset(s) according to the data dictionary
    # dataset_group <- valueType_adjust(from = data_dict, to = dataset)
      
    # create group
    dataset_group <- 
      dataset %>%
      group_by(!! as.symbol(group_by))
      
    name_group <- 
      group_keys(dataset_group) %>% 
      mutate(across(everything(),as.character)) %>%
      mutate(across(!! as.symbol(group_by), ~ replace_na(.,"Empty Values")))
    
    dataset_group <- group_split(dataset_group)
    names(dataset_group) <- as.character(name_group[[1]])
    
    # arrange group
    dataset_group <- 
      dataset_group[unique(
        c(intersect(cat_lab[[group_by]],names(dataset_group)),
          names(dataset_group)[length(names(dataset_group))]))] %>%
      as.list()
    
    # create data dictionary per group
    for(i in names(dataset_group)){
      # stop()}
      
      other_groups <- names(dataset_group)[names(dataset_group)!=i]
      data_dict_group <- data_dict
      data_dict_group[['Categories']] <- 
        data_dict_group[['Categories']] %>%
        rowwise %>%
        dplyr::filter(!(.data$`variable` == !!group_by & .data$`name` %in% !! other_groups))
      
      dataset_group[[i]] <- as_dataset(dataset_group[[i]],col_id)
      attributes(dataset_group[[i]])$`madshapR::Data dictionary` <- data_dict_group
    }
    
    # rename group and data dict group
    name_group <- 
      name_group %>%
      mutate(!! as.symbol(group_by) := as.character(!!as.symbol(group_by))) %>%
      left_join(cat_lab, by = join_by(!!as.symbol(group_by))) %>%
      arrange(.data$`madshapR::category_level`) %>% 
      pull('madshapR::label_short_cat') %>%
      str_replace_na('Empty value')
    
    names(dataset_group) <- name_group
    
  } else {
    
    name_group <- "no_group"
    # dataset_group <- valueType_adjust(from = data_dict, to = dataset)
    dataset_group <- list(no_group = dataset)
    dataset_group$no_group <- as_dataset(dataset_group$no_group,col_id)
    attributes(dataset_group$no_group)$`madshapR::Data dictionary` <- data_dict
  }
  
  report <- 
    dataset_evaluate(
      dataset,
      data_dict,
      taxonomy = taxonomy,
      dataset_name = dataset_name,
      is_data_dict_mlstr = TRUE)
  
  if(as_tibble(report[['Dataset assessment']]) %>% 
     bind_rows(tibble(`Dataset assessment` = as.character())) %>%
     dplyr::filter(str_detect(.data$`Dataset assessment`,"\\[ERROR\\]")) %>% nrow > 0){
    stop(call. = FALSE,
"The dataset and/or the data dictionary contain errors.",
bold("\n\nUseful tip:"),
" Use dataset_evaluate(dataset, data_dict) to get a full assessment of
your dataset")}
  
  
  message(
    "- DATASET SUMMARIZE: ",
    bold(dataset_name), if(dataset %>% nrow == 0) " (empty dataset)",
    " --------------------------")

  dataset_valueType <- tibble(
      `madshapR::variable_name` = as.character(),
      `Dataset valueType` = as.character())
  
  suggested_valueType <- tibble(
    `madshapR::variable_name` = as.character(),
    `Suggested valueType` = as.character())
  
  if(ncol(dataset) > 0){
    dataset_valueType <-
      dataset %>%
      # select(-matches("^___mlstr_index___$")) %>%
      reframe(across(
        everything(),
        ~ valueType_of(.))) %>%
      pivot_longer(cols = everything()) %>%
      rename(
        `madshapR::variable_name` = "name",
        `Dataset valueType` = "value")

    if(valueType_guess == TRUE){
      suggested_valueType <-
        dataset %>%
        # select(-matches("^___mlstr_index___$")) %>%
        reframe(across(
          everything(),
          ~ valueType_guess(.))) %>%
        pivot_longer(cols = everything()) %>%
        rename(
          `madshapR::variable_name` = "name",
          `Suggested valueType` = "value")
    }else{
      suggested_valueType <-
        dataset_valueType %>%
        select(
          "madshapR::variable_name",
          `Suggested valueType` = "Dataset valueType")}

  }
  ## variables
  first_labs <- first_label_get(data_dict)
  first_lab_var <- first_labs[['Variables']]

  data_dict_var <-
    data_dict[['Variables']] %>%
    select(-matches("^madshapR::variable_name$")) %>%
    rename(`madshapR::variable_name` = "name") %>%
    mutate(across(everything(),as.character)) %>%
    add_index("Index", .force = TRUE) %>%
    select("Index", "madshapR::variable_name",
           "Variable label" = !! first_lab_var,
           "Data dictionary valueType" = "valueType") %>%
    full_join(dataset_valueType, by = "madshapR::variable_name") %>%
    full_join(suggested_valueType, by = "madshapR::variable_name") %>%
    mutate(`Suggested valueType` = ifelse(
      .data$`Dataset valueType` == .data$`Data dictionary valueType`,NA_character_,
      .data$`Suggested valueType`))
  # %>% remove_empty('cols')
  
  # always treat id col as text

  ## categories
  if(has_categories(data_dict)){

        
    first_lab_cat <- first_labs[['Categories']]
    
    data_dict_cat <-
      data_dict_labels[['Categories']] %>%
      select(-matches("^madshapR::variable_name$")) %>%
      rename("madshapR::variable_name" = "variable") %>%
      select("madshapR::variable_name",'madshapR::label_short_cat', 
             "missing") %>%
      mutate(
        missing =
          ifelse(
            .data$`missing` == TRUE,
            "Non-valid values:",
            "Valid values:")) %>%
      group_by(pick(c(-"madshapR::label_short_cat"))) %>%
      reframe(across(c("madshapR::label_short_cat"),
                       ~ paste0(.,collapse = "\n"))) %>%
      arrange(.data$`madshapR::variable_name`,desc(.data$`missing`)) %>%
      unite("Categories in data dictionary",
            c("missing","madshapR::label_short_cat"),
            sep = "\n",remove = TRUE) %>%
      group_by(pick(c(-"Categories in data dictionary"))) %>%
      reframe(across(c("Categories in data dictionary"),
                       ~ paste0(.,collapse = "\n\n"))) %>%
      select("madshapR::variable_name","Categories in data dictionary")
    
  }else{
    data_dict[['Categories']] <-
      tibble(
        name = as.character(),
        variable = as.character(),
        missing = as.logical())

    data_dict_cat <-
      tibble(
        `madshapR::variable_name` = as.character(),
        `Categories in data dictionary` = as.character())}

  report$`Variables summary (all)` <-
    data_dict_var %>%
    left_join(data_dict_cat, by = "madshapR::variable_name") %>%
    rename('Variable name' = "madshapR::variable_name") %>%
    tibble

  message("    Summarize the data type of each variable across the dataset")
  
  ### SUMMARIZE VARIABLES VALUES ###
  # [GF] - NOTE : handle better the id participant.

  vT <- madshapR::valueType_list
  vT_text <- vT[vT$`genericType` == 'character',][['valueType']]
  report$`Text variable summary` <-
    report$`Variables summary (all)`[
      report$`Variables summary (all)`$`Data dictionary valueType` %in%
        vT_text,] %>% 
    rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
    dplyr::filter(!.data$`Variable name` %in% col_id) %>% ungroup %>%
    select(-any_of("Dataset valueType"),
           -any_of("Suggested valueType"),
           -any_of("Categories in data dictionary"))

  vT_num  <- vT[vT$`genericType` == 'numeric',][['valueType']]
  report$`Numerical variable summary` <-
    report$`Variables summary (all)`[
      report$`Variables summary (all)`$`Data dictionary valueType` %in%
        vT_num,] %>% 
    rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
    dplyr::filter(!.data$`Variable name` %in% col_id) %>% ungroup %>%
    select(-any_of("Dataset valueType"),
           -any_of("Suggested valueType"),
           -any_of("Categories in data dictionary"))

  vT_date <- vT[vT$`genericType` == 'date',][['valueType']]
  report$`Date variable summary` <-
    report$`Variables summary (all)`[
      report$`Variables summary (all)`$`Data dictionary valueType` %in%
        vT_date,] %>% 
    rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
    dplyr::filter(!.data$`Variable name` %in% col_id) %>% ungroup %>%
    select(-any_of("Dataset valueType"),
           -any_of("Suggested valueType"),
           -any_of("Categories in data dictionary"))
  
  vT_datetime <- vT[vT$`genericType` == 'datetime',][['valueType']]
  report$`Datetime variable summary` <-
    report$`Variables summary (all)`[
      report$`Variables summary (all)`$`Data dictionary valueType` %in%
        vT_datetime,] %>% 
    rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
    dplyr::filter(!.data$`Variable name` %in% col_id) %>% ungroup %>%
    select(-any_of("Dataset valueType"),
           -any_of("Suggested valueType"),
           -any_of("Categories in data dictionary"))

  report$`Categorical variable summary` <-
    report$`Variables summary (all)`[
      !is.na(report$`Variables summary (all)`$`Categories in data dictionary`),] %>% 
    rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
    dplyr::filter(!.data$`Variable name` %in% col_id) %>% ungroup %>%
    select(-any_of("Dataset valueType"),
           -any_of("Suggested valueType"))

  if(nrow(dataset) > 0){

    message("    Summarize information for all variables")
    dataset_preprocess <-
      lapply(dataset_group,function(x){
        dataset_preprocess(x)})

    summary_var <- 
      lapply(dataset_preprocess,function(x){
        summary_variables(dataset_preprocess = x)})
    
    if(group_by != ''){
      summary_group <- summary_variables(dataset_preprocess = preprocess_group)
      summary_var <- 
        lapply(summary_var,function(x){
          x %>% dplyr::filter(.data$`Variable name` != group_by) %>%
            bind_rows(summary_group)})}

    dataset_preprocess_num <-
      lapply(dataset_preprocess,function(x){
      x[x$`Variable name` %in% report$`Numerical variable summary`$`Variable name`,] %>%
          dplyr::filter(.data$`Categorical variable` != 'yes')})
    summary_num <-
      lapply(dataset_preprocess_num,function(x){
        summary_variables_numeric(dataset_preprocess = x)})
    if(nrow(bind_rows(summary_num)) > 0)
    message("    Summarize information for numerical variables")
    
    dataset_preprocess_text <-
      lapply(dataset_preprocess,function(x){
        x[x$`Variable name` %in% report$`Text variable summary`$`Variable name`,] %>%
          dplyr::filter(.data$`Categorical variable` != 'yes')})
    summary_text <-
      lapply(dataset_preprocess_text,function(x){
        summary_variables_text(dataset_preprocess = x)})
    if(nrow(bind_rows(summary_text)) > 0)
    message("    Summarize information for text variables")

    dataset_preprocess_date <-
      lapply(dataset_preprocess,function(x){
        x[x$`Variable name` %in% report$`Date variable summary`$`Variable name`,] %>%
          dplyr::filter(.data$`Categorical variable` != 'yes')})
    summary_date <-
      lapply(dataset_preprocess_date,function(x){
        summary_variables_date(dataset_preprocess = x)})
    if(nrow(bind_rows(summary_date)) > 0)
    message("    Summarize information for date variables")
    
    dataset_preprocess_datetime <-
      lapply(dataset_preprocess,function(x){
        x[x$`Variable name` %in% report$`Datetime variable summary`$`Variable name`,] %>%
          dplyr::filter(.data$`Categorical variable` != 'yes')})
    summary_datetime <-
      lapply(dataset_preprocess_datetime,function(x){
        summary_variables_datetime(dataset_preprocess = x)})
    
    if(nrow(bind_rows(summary_datetime)) > 0)
    message("    Summarize information for datetime variables")
    
    dataset_preprocess_cat <-
      lapply(dataset_preprocess,function(x){
        x[x$`Variable name` %in% report$`Categorical variable summary`$`Variable name`,] %>%
          dplyr::filter(.data$`Categorical variable` != 'no')})
    summary_cat <-
      lapply(dataset_preprocess_cat,function(x){
        summary_variables_categorical(dataset_preprocess = x)}) 
    if(nrow(bind_rows(summary_cat)) > 0)
    message("    Summarize information for categorical variables")
    
    if(group_by != ''){
      summary_group_cat <- 
        summary_variables_categorical(dataset_preprocess = preprocess_group)
      summary_cat <- 
        lapply(summary_cat,function(x){
          x %>% dplyr::filter(.data$`Variable name` != group_by) %>%
            bind_rows(summary_group_cat)})}
    
    # add grouping variable to each group
    if(group_by != ''){
      for(i in names(summary_var)) {
        # stop()}
        summary_var [[i]] <- summary_var [[i]] %>% 
          mutate(!! paste0('Grouping variable: ', group_by) := as.character(
            ifelse(.data$`Variable name` == group_by, paste0(group_by,' (all)'),i)))
        
        summary_num [[i]] <- summary_num [[i]] %>% 
          mutate(!! paste0('Grouping variable: ', group_by) := as.character(
            ifelse(.data$`Variable name` == group_by, paste0(group_by,' (all)'),i)))
        
        summary_text[[i]] <- summary_text[[i]] %>% 
          mutate(!! paste0('Grouping variable: ', group_by) := as.character(
            ifelse(.data$`Variable name` == group_by, paste0(group_by,' (all)'),i)))
        
        summary_date[[i]] <- summary_date[[i]] %>% 
          mutate(!! paste0('Grouping variable: ', group_by) := as.character(
            ifelse(.data$`Variable name` == group_by, paste0(group_by,' (all)'),i)))
        
        summary_datetime[[i]] <- summary_datetime[[i]] %>% 
          mutate(!! paste0('Grouping variable: ', group_by) := as.character(
            ifelse(.data$`Variable name` == group_by, paste0(group_by,' (all)'),i)))
        
        summary_cat [[i]] <- summary_cat [[i]] %>% 
          mutate(!! paste0('Grouping variable: ', group_by) := as.character(
            ifelse(.data$`Variable name` == group_by, paste0(group_by,' (all)'),i)))
      }}
    
    # binding information
    summary_var      <- bind_rows(summary_var)      %>% distinct()
    summary_num      <- bind_rows(summary_num)      %>% distinct()
    summary_text     <- bind_rows(summary_text)     %>% distinct()
    summary_date     <- bind_rows(summary_date)     %>% distinct()
    summary_datetime <- bind_rows(summary_datetime) %>% distinct()
    summary_cat      <- bind_rows(summary_cat)      %>% distinct()
    
    minimum_cols <- c(
        "Index" ,
        paste0('Grouping variable: ', group_by),
        "Variable name",
        "Variable label",
        "Quality assessment comment",
        "Data dictionary valueType",
        "Dataset valueType",
        "Suggested valueType",
        "Categorical variable",
        "Categories in data dictionary",
        "Number of rows",
        "Number of valid values",
        "Number of non-valid values",
        "Number of empty values",
        "% Valid values",
        "% Non-valid values",
        "% Empty values",
        "Number of distinct values")
    
    report$`Variables summary (all)` <-
      report$`Variables summary (all)` %>%
      left_join(summary_var, by = "Variable name", multiple = "all") %>% 
      select(any_of(minimum_cols)) %>%
      mutate(
        `Quality assessment comment` = ifelse(.data$`Variable name` %in% col_id,
        "[INFO] - Identifier variable.",.data$`Quality assessment comment`),       
        `Quality assessment comment` = ifelse(.data$`Variable name` %in% group_by,
        "[INFO] - Grouping variable.",.data$`Quality assessment comment`))         
    
    report$`Text variable summary` <-
      suppressMessages(report$`Text variable summary` %>%
      inner_join(summary_var , by = "Variable name", multiple = "all") %>%
      inner_join(summary_text, multiple = "all")) %>%
      select(any_of(minimum_cols),everything())

    report$`Date variable summary` <-
      suppressMessages(report$`Date variable summary` %>%
      inner_join(summary_var , by = "Variable name", multiple = "all") %>%
      inner_join(summary_date, multiple = "all")) %>%
      select(any_of(minimum_cols),everything())

    report$`Datetime variable summary` <-
      suppressMessages(report$`Datetime variable summary` %>%
      inner_join(summary_var , by = "Variable name", multiple = "all") %>%
      inner_join(summary_datetime, multiple = "all")) %>%
      select(any_of(minimum_cols),everything())
    
    report$`Numerical variable summary` <-
      suppressMessages(report$`Numerical variable summary` %>%
      inner_join(summary_var , by = "Variable name", multiple = "all") %>%
      inner_join(summary_num, multiple = "all")) %>%
      select(any_of(minimum_cols),everything())
      
    report$`Categorical variable summary` <-
      suppressMessages(report$`Categorical variable summary` %>%
      inner_join(summary_var , by = "Variable name", multiple = "all") %>%
      inner_join(summary_cat, multiple = "all")) %>%
      select(any_of(minimum_cols),everything())  %>%
      mutate(
        `Quality assessment comment` = ifelse(.data$`Variable name` %in% group_by,
        "[INFO] - Grouping variable.",.data$`Quality assessment comment`))      
    }
    
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
      '        1_Number of empty variables'                                    ,
      '    1_ Variable data types (valueType)'                                 ,
      '        1_Number of text variables'                                     ,
      '        1_Number of date variables'                                     ,
      '        1_Number of datetime variables'                                 ,
      '        1_Number of numerical variables'                                ,
      '        1_Number of categorical variables'                              ,
      '    2_Rows'                                                             ,
      '        2_Number of rows'                                               ,
      '        2_Number of distinct identifier variable values'                ))
  

  Overview_group <- c(list(`(all)` = dataset),dataset_group)
  
  for(i in names(Overview_group)){
    
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
          as.character(ifelse(toString(group_by) != '',
                              toString(group_by), "madshapR::remove")),
        .data$`---` == '    1_Variables'                                       ~
          " ",
        .data$`---` == '        1_Number of variables'                         ~
          as.character(length(unique(
            report$`Data dictionary summary`$`Variable name`))),
        .data$`---` == '        1_Number of empty variables'                   ~
          as.character(
            ncol(Overview_group[[i]][vapply(X = Overview_group[[i]],
                          FUN = function(x) all(is.na(x)),
                          FUN.VALUE = logical(1))])),
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
        
      # [GF resolved comment] Leave categorical even if empty
        .data$`---` == '        1_Number of categorical variables'                   ~
      as.character(ifelse(length(unique(report$`Categorical variable summary`$`Variable name`)) > 0,
                          length(unique(report$`Categorical variable summary`$`Variable name`)), " ")),
        .data$`---` == '    2_Rows'                                                  ~
          i,
        .data$`---` == '        2_Number of rows'                                    ~
      as.character(nrow(Overview_group[[i]])),
        .data$`---` == '        2_Number of distinct identifier variable values'     ~
        ifelse(is.null(col_id),as.character(nrow(Overview_group[[i]])),
        as.character(nrow(distinct(Overview_group[[i]][col_id])))),
        TRUE                                                                         ~
          "EMPTY",
      )) %>%
      mutate(
        `---` = str_remove_all(.data$`---`, "1_"),
        `---` = str_remove_all(.data$`---`, "2_")) %>%
      rename(
        `Overview` = "---",
        !! as.symbol(i) := "-----") %>% slice(-1)}
  
  index <- Overview_group[[2]] %>%
    add_index %>% 
    add_index("index2",start = 0) %>%
    mutate(
      index = ifelse(.data$`Overview` == "    Rows", 0,.data$`index`)) %>%
    dplyr::filter(.data$`index` == 0) %>% pull("index2")
  
  for(i in names(Overview_group)[-1]){
    Overview_group[[i]][c(1:index),2] <- " "
    Overview_group[[i]][,1] <- NULL
  }
  
  report$Overview <- bind_cols(Overview_group)
  report$Overview <-
    report$Overview %>%
    dplyr::filter(!.data$`(all)` %in% "madshapR::remove")

  if(group_by == '') report$Overview <- report$Overview %>% select(-"no_group")
  
  if(all("Empty value" %in% name_group)){

    qual_comment = "[INFO] - Grouping variable contains Empty values (NA)."

    report[["Dataset assessment"]] <-
      bind_rows(
        report[["Dataset assessment"]],
        tibble(
          `Variable name` = group_by,
          `Dataset assessment` = qual_comment,
          Value = NA_character_))
  }

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
#' valueType that could be applied to the dataset. FALSE by default.
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
    valueType_guess = FALSE){
  
  # amelioration :rajouter taxonomy
  
  # check on arguments
  as_dossier(dossier)
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  if(!is.logical(valueType_guess))
    stop(call. = FALSE,
         '`as_data_dict_mlstr` must be TRUE or FALSE (TRUE by default)')
  
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
dataset_preprocess <- function(dataset, data_dict = NULL){
  
  # handle atomics
  summary_tbl <-
    tibble(
      `Index` = as.integer(),
      `Variable name` = as.character(),
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
    return(dataset_preprocess(
      dataset = tibble(name = dataset), 
      data_dict = data_dict))
  
  # tests
  as_dataset(dataset)
  
  # if no data_dict
  if(is.null(data_dict)){
    data_dict <- data_dict_extract(dataset,as_data_dict_mlstr = TRUE)
  }else{
    data_dict <- as_data_dict_mlstr(data_dict)}
  
  data_dict_var  <-
    data_dict[['Variables']] %>%
    select("Variable name" = 'name') %>%
    mutate(`Categorical variable` = NA_character_) %>%
    add_index("Index")
  
  if(has_categories(data_dict)){
    
    first_lab_var <- first_label_get(data_dict)[['Variables']]
    
    data_dict_cat <-
      data_dict[['Categories']] %>%
      select(
        "Variable name" = "variable", 
        value_var = "name",
        cat_label = !!first_lab_var,
        valid_class = "missing") %>%
      group_by(.data$`Variable name`, .data$`valid_class`) %>%
      add_index('cat_index') %>%
      ungroup() %>%
      mutate(
        valid_class =
          ifelse(
            .data$`valid_class` == TRUE,
            "2_Non-valid values", "1_Valid values")) %>%
      mutate(value_var = as.character(.data$`value_var`))
    
  }else{
    data_dict_cat <-
      tibble(cat_index = as.integer(),
             'Variable name' = as.character(),
             value_var = as.character(),
             cat_label = as.character(),
             valid_class = as.character())}
  
  data_dict_var  <-
    data_dict_var %>%
    full_join(data_dict_cat,by = "Variable name",multiple = "all") %>%
    mutate(`Categorical variable` = 
             ifelse(is.na(.data$`valid_class`),"no","yes"))
  
  summary <- tibble("Variable name" = as.character())
  
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
        mutate("Variable name" = i) %>%
        add_index('index_in_dataset')
      
      # count observations in the dataset, including no-observation of
      # categorical outcomes
      # classification of observations not in the data_dict(3) and 'true'
      # NA as empty values(4)
      # arrange (1),(2),(3),(4), by original index
      summary   <-
        tbl_var %>%
        full_join(
          data_dict_var[data_dict_var$`Variable name` == i,],
          by = c("value_var", "Variable name")) %>%
        mutate(value_var_occur = replace_na(.data$`value_var_occur`, 0)) %>%
        fill("Index",.direction = "downup") %>%
        mutate(
          valid_class =
            case_when(
              is.na(.data$`value_var`)  ~ "4_Empty values",
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
                c('1_Valid values','2_Non-valid values'),]) >= 1,
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
      !(.data$`value_var_occur`== 0 & .data$`valid_class` == "4_Empty values")) %>%
    select("Index","Variable name", everything()) %>%
    arrange(.data$`Index`,.data$`valid_class`)
  
  
  # to lower text if not categories
  final_resume <- 
    final_resume %>%
    mutate(
      value_var = ifelse(
        .data$`valid_class` == "3_Valid other values",
        tolower(.data$`value_var`),.data$`value_var`))
  
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
  summary <- dataset_preprocess
  
  # init
  summary_tbl <- tibble("Variable name" = as.character())
  if(nrow(summary) == 0) return(summary_tbl)
  
  for(i in unique(summary$`Variable name`)){
    # stop()}
    
    summary_i <- summary %>% dplyr::filter(.data$`Variable name` == i)
    
    # summary the output
    summary_i <-
      tibble(
        `Variable name` = i,
        
        `Categorical variable` = unique(summary_i$`Categorical variable`),
        
        `Number of rows` = sum(summary_i$`value_var_occur`),
        
        `Number of valid values` =
          sum(summary_i[
            summary_i$`valid_class` %in%
              c("1_Valid values","3_Valid other values"),]$value_var_occur),
        
        `Number of non-valid values` =
          sum(summary_i[
            summary_i$`valid_class` %in%
              c("2_Non-valid values"),]$value_var_occur),
        
        `Number of empty values` =        
          sum(summary_i[
            summary_i$`valid_class` %in%
              c("4_Empty values"),]$value_var_occur),
                
        `% Valid values` =
          round(100*(.data$`Number of valid values`/.data$`Number of rows`),2),
        
        `% Non-valid values` =
          round(100*(.data$`Number of non-valid values`/.data$`Number of rows`),2),
        
        `% Empty values` =
          round(100*(.data$`Number of empty values`/.data$`Number of rows`),2),

        `Number of distinct values` =
          length(unique(summary_i[
            summary_i$`value_var_occur` == 1 &
              !is.na(summary_i$`value_var`),]$`value_var`))
      )
    
    summary_tbl <- bind_rows(summary_tbl, summary_i)
  }
  
  summary_tbl <-
    summary_tbl %>%
    mutate(
      `Quality assessment comment` = case_when(
        
        summary_tbl$`Number of rows` == summary_tbl$`Number of distinct values`    ~
"[INFO] - All rows are unique." ,
        
summary_tbl$`Number of distinct values` == 0                                       ~
"[INFO] - Empty variable." ,
        
summary_tbl$`Number of distinct values` > 0 & summary_tbl$`Number of valid values` == 0       ~
"[INFO] - All categorical values present in variable indicate non-valid ('missing') values.",

summary_tbl$`Number of distinct values` == 1                                       ~
"[INFO] - Variable has a constant value.",
        
# summary_tbl$`Number of valid values` > 0 & summary_tbl$`Number of non-valid values` == 0      ~
# "[INFO] - Categorical values present in dataset that do not match categorical values in data dictionary." ,
        
        TRUE       ~   NA_character_
      )) %>%
    select("Variable name","Quality assessment comment", everything())
  
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
  summary_tbl <- tibble("Variable name" = as.character())
  if(is.null(dataset_preprocess)) return(summary_tbl)
  if(!nrow(dataset_preprocess)) return(summary_tbl)
  
  summary <-
    dataset_preprocess %>%
    dplyr::filter(.data$`value_var_occur` == 1 ) %>%
    dplyr::filter(.data$`valid_class`  == "3_Valid other values")
  
  # init
  summary_tbl <- tibble("Variable name" = as.character())
  if(nrow(summary) == 0) return(summary_tbl)
  
  for(i in unique(summary$`Variable name`)){
    # stop()}
    
    summary_i <- summary %>% dplyr::filter(.data$`Variable name` == i)
    
    # turn the output to be readable
    if(summary_i %>% nrow > 0){
      summary_i <-
        tibble(
          `Variable name` = i,
          
          `Most common values` =
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
          
          `Least common values` =
            summary_i %>%
            count(.data$`value_var`) %>%
            dplyr::filter(if_any(.data$`n`, ~ . == min(.))) %>%
            slice(1:6) %>% mutate(value_var = ifelse(row_number() == 6,'[...]',
                                                     .data$`value_var`)) %>%
            pull(.data$`value_var`) %>% paste0(collapse = " ; ") %>%
            str_replace('; \\[\\.\\.\\.\\]$','[...]')
        )
      
    }else{summary_i <- tibble("Variable name" = as.character())}
    
    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }
  
  # final_summary <-
  #   summary_variables(
  #     dataset = dataset,data_dict data_dict, 
  #     dataset_preprocess = dataset_preprocess) %>%
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
  summary_tbl <- tibble("Variable name" = as.character())
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
  summary_tbl <- tibble("Variable name" = as.character())
  if(nrow(summary) == 0) return(summary_tbl)
  
  for(i in unique(summary$`Variable name`)){
    # stop()}
    
    summary_i <- summary %>% dplyr::filter(.data$`Variable name` == i)
    
    # turn the output to be readable
    if(summary_i %>% nrow > 0){
      summary_i <-
        tibble(
          `Variable name`         =  i,
          `Oldest date`      =
            min((summary_i$`value_var`),na.rm = TRUE),
          `Most recent date`     =
            max((summary_i$`value_var`),na.rm = TRUE),
          `Minimum\n(year)`      =
            summary(as.integer(year(summary_i$`value_var`)))[[1]],
          `1st quartile\n(year)`       =
            round(summary(as.integer(year(summary_i$`value_var`)))[[2]]),
          `Median\n(year)`   =
            round(summary(as.integer(year(summary_i$`value_var`)))[[3]]),
          `3rd quartile\n(year)`       =
            round(summary(as.integer(year(summary_i$`value_var`)))[[5]]),
          `Maximum\n(year)`      =
            summary(as.integer(year(summary_i$`value_var`)))[[6]],
          `Mean\n(year)`     =
            round(summary(as.integer(year(summary_i$`value_var`)))[[4]]),
          `Span\n(year)`     =
            year(.data$`Most recent date`) - year(.data$`Oldest date`))
      
    }else{summary_i <- tibble("Variable name" = as.character())}
    
    summary_tbl <- bind_rows(summary_tbl, summary_i)
    summary_i <- tibble()
  }
  
  # final_summary <-
  #   summary_variables(
  #     dataset = dataset, data_dict = data_dict, 
  #     dataset_preprocess = dataset_preprocess) %>%
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
  summary_tbl <- tibble("Variable name" = as.character())
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
#' @param dataset_preprocess A data frame which provides summary of the 
#' variables (for internal processes and programming).
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param .dataset_preprocess `r lifecycle::badge("deprecated")`
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
  summary_tbl <- tibble('Variable name' = as.character())
  if(is.null(dataset_preprocess)) return(summary_tbl)
  if(!nrow(dataset_preprocess)) return(summary_tbl)
  
  summary <-
    dataset_preprocess %>%
    dplyr::filter(.data$`value_var_occur` == 1 ) %>%
    dplyr::filter(.data$`valid_class`  == "3_Valid other values")
  
  # init
  summary_tbl <- tibble("Variable name" = as.character())
  if(nrow(summary) == 0) return(summary_tbl)
  
  for(i in unique(summary$`Variable name`)){
    # stop()}
    
    summary_i <- summary %>% dplyr::filter(.data$`Variable name` == i)
    
    summary_i$`value_var` <- suppressWarnings(as.numeric(summary_i$`value_var`))
    
    # turn the output to be readable
    if(summary_i %>% nrow > 0 & !all(is.na(summary_i$value_var))){
      summary_i <-
        tibble(
          `Variable name`        =  i,
          `Minimum`              = round(summary(summary_i$`value_var`)[[1]],2),
          `1st quartile`         = round(summary(summary_i$`value_var`)[[2]],2),
          `Median`               = round(summary(summary_i$`value_var`)[[3]],2),
          `3rd quartile`         = round(summary(summary_i$`value_var`)[[5]],2),
          `Maximum`              = round(summary(summary_i$`value_var`)[[6]],2),
          `Mean`                 = round(summary(summary_i$`value_var`)[[4]],2),
          `Standard deviation`   = round(sd(summary_i$`value_var`,na.rm = TRUE),2),
          ) %>%
        mutate(
          `Standard deviation`   = replace_na(.data$`Standard deviation`,0))
        
    }else{summary_i <- tibble('Variable name' = as.character())}
    
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
  summary_tbl <- tibble("Variable name" = as.character())
  if(is.null(dataset_preprocess)) return(summary_tbl)
  if(!nrow(dataset_preprocess)) return(summary_tbl)
  
  summary <-
    dataset_preprocess %>%
    dplyr::filter(.data$`Categorical variable` != 'no') %>%
    group_by(across(c(-"value_var_occur",-"index_value"))) %>%
    reframe(
      n = sum(as.integer(.data$`value_var_occur`))) %>%
    arrange(.data$`Index`, .data$`valid_class`,.data$`cat_index`) %>%
    ungroup
  
  if(nrow(summary) == 0) return(summary_tbl)
  
  for(i in unique(summary$`Variable name`)){
    # stop()}
    
    summary_i <- summary %>% dplyr::filter(.data$`Variable name` == i)
    
    summary_category <- 
      summary_i %>%
      # slice(1:4,8) %>%
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
      unite("list_values",
            c('list_values','n_perc'),
            sep = "",remove = TRUE, na.rm = TRUE) %>%
      mutate(categorical_index = str_sub(.data$`valid_class`,1,1)) %>%
      group_by(.data$`valid_class`,.data$`categorical_index`) %>%
      mutate(
        valid_class = case_when(
          .data$`valid_class` == "1_Valid values"       ~
            "Valid values : \n",
          .data$`valid_class` == "2_Non-valid values"     ~
            "\nNon-valid values : \n",
          .data$`valid_class` == "3_Valid other values" ~
            "\nOther values (non categorical)",
          .data$`valid_class` == "4_Empty values"          ~
            "\nEmpty values",
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
      .data$`valid_class` %in% c("1_Valid values","2_Non-valid values"))) > 0){
      
      summary_i <-
        tibble(
          
          `Variable name`                   =
            unique(summary_i$`Variable name`),
          
          `Values present in dataset`                           =
            summary_category$list_values,
          
          `Data dictionary categories not present in dataset`   =
            summary_category$cat_var_absence,
          
          `Dataset values not present in data dictionary`        =
            summary_category$other_val_presence
          
        )
      
    }else{summary_i <- tibble("Variable name" = as.character())}
    
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
