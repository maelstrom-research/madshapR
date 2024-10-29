#' @title
#' Generate a list of charts, figures and summary tables of a variable
#'
#' @description
#' Analyses the content of a variable and its data dictionary (if any), 
#' identifies its data type and values accordingly and generates figures and 
#' summaries (datatable format). The figures and tables are representations of
#' data distribution, statistics and valid/non valid/empty values (based on 
#' the data dictionary information if provided and the data type of the 
#' variable). This function can be used to personalize report parameters and is 
#' internally used in the function [dataset_visualize()]. Up to seven objects 
#' are generated which include : One datatable of the key elements of the 
#' data dictionary, one datatable summarizing statistics (such as mean, 
#' quartile, most common values, most recent date, ... , depending on the 
#' data type of the variable), two graphs showing the distribution of the 
#' variable, One bar chart for categorical values (if any), One bar chart for 
#' non valid values (if any), One pie chart for the proportion of valid and 
#' non-valid values (if any). The variable can be grouped using `group_by` 
#' parameter, which is a (categorical) column in the dataset. The user may need 
#' to use [as_category()] in this context. To fasten the process (and allow 
#' recycling object in a workflow) the user can feed the function with a 
#' `variable_summary`, which is the output of the function [dataset_summarize()] 
#' of the column(s) `col` and  `group_by`. The summary must have the same 
#' parameters to operate. 
#'
#' @details
#' A dataset is a data table containing variables. A dataset object is a 
#' data frame and can be associated with a data dictionary. If no 
#' data dictionary is provided with a dataset, a minimum workable 
#' data dictionary will be generated as needed within relevant functions.
#' Identifier variable(s) for indexing can be specified by the user. 
#' The id values must be non-missing and will be used in functions that 
#' require it. If no identifier variable is specified, indexing is 
#' handled automatically by the function.
#' 
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
#' @param dataset A dataset object.
#' @param col A character string specifying the name of the column.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param group_by A character string identifying the column in the dataset
#' to use as a grouping variable. Elements will be grouped by this 
#' column.
#' @param valueType_guess Whether the output should include a more accurate 
#' valueType that could be applied to the dataset. FALSE by default.
#' @param variable_summary A summary list which is the summary of the variables.
#' @param .summary_var `r lifecycle::badge("deprecated")`
#'
#' @seealso
#' [DT::datatable()], [ggplot2::ggplot()]
#' [dataset_summarize()], [dataset_visualize()]
#'
#' @returns
#' A list of up to seven elements (charts and figures and datatables) which can 
#' be used to summarize visualize data.
#' 
#' @examples
#' {
#' 
#' library(dplyr)
#' library(fs)
#' 
#' # use madshapR_example provided by the package 
#' dataset <- 
#'   madshapR_example$`dataset_example` %>% 
#'   group_by(pick('gndr')) %>% 
#'   as_dataset(col_id = "part_id")
#'   
#' data_dict <- madshapR_example$`data_dict_example`
#' variable_summary <- dataset_summarize(dataset,data_dict)
#'   
#' plots <- variable_visualize(
#'  dataset,data_dict, col = 'prg_ever',
#'  variable_summary =  variable_summary,valueType_guess = TRUE)
#'  
#' print(plots$main_values_1)
#'  
#' }
#'
#' @import dplyr fabR
#' @import ggplot2 tidytext janitor forcats
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics hist
#' @importFrom stats IQR
#' @importFrom rlang .data
#' @importFrom rlang :=
#'
#' @export
variable_visualize <- function(
    dataset = tibble(id = as.character()),
    col,
    data_dict = NULL, 
    group_by = NULL, 
    valueType_guess = FALSE,
    variable_summary = .summary_var,
    .summary_var = NULL){
  
  if(toString(col_id(dataset)) == col) {
    warning(call. = FALSE,'Your column is identifier. It will not be analysed.')
    return(ggplot())}
  
  # dataset <- 
  #   as_dataset(dataset) %>%
  #   mutate(across(where(is.character),tolower))
  
  if(nrow(dataset) == 0) {
    warning(call. = FALSE,'Your column has no observation.')
    return(ggplot())}
  
  if(toString(substitute(group_by)) == '') group_by <- NULL
  # attempt to catch group_by from the group_vars if the dataset is grouped
  if(length(group_vars(dataset)) == 1 & toString(substitute(group_by)) == ''){
    group_by <- group_vars(dataset)
  }

  dataset <- as_dataset(ungroup(dataset))
  
  ## future dev
  # theme_minimal() + 
  # labs(title = "Change in Life Expectancy",
  #      subtitle = "1952 to 2007",
  #      x = "Life Expectancy (years)",
  #      y = "")
  
  # attempt to catch col
  colset_temp_1 <- tryCatch(
    expr  = {dataset[toString(substitute(col))]},
    error = function(cond){return(dataset[col])})
  
  # attempt to catch group_by
  colset_temp_2 <- tryCatch(
    expr  = {dataset[toString(substitute(group_by))]},
    error = function(cond){return(dataset[group_by])})
  
  
  if(toString(names(colset_temp_1)) == toString(names(colset_temp_2))){
    colset <- colset_temp_1
  } else {
    colset <- bind_cols(colset_temp_1,colset_temp_2)}
  
  if(ncol(colset)== 1){col <- names(colset)[1] ; group_by <- ''}
  if(ncol(colset)== 2){col <- names(colset)[1] ; group_by <- names(colset)[2]}
  
  if(group_by != ''){
    
    if(!is_category(colset[[group_by]])) 
      colset <- as_dataset(colset) %>% 
        mutate(across(all_of(group_by), as_category))
  }

  if(!is.null(data_dict)){
    
      tryCatch(
        expr = {
          col_dict <- 
            data_dict %>%
            data_dict_match_dataset(dataset = colset,output = 'data_dict') %>%
            as_data_dict_mlstr()
        },
        warning = function(cond){
          stop(cond)
        })
    
  }else{
    
    col_dict <- 
      data_dict_extract(colset,as_data_dict_mlstr = TRUE)
  }
  
  if(! group_by %in% col_dict[['Categories']][['variable']] & group_by != ''){
    
    col_dict_group_by <- 
      as_dataset(colset) %>% 
      select(all_of(group_by)) %>% data_dict_extract()
    
    col_dict[['Categories']] <- 
      bind_rows(
        col_dict[['Categories']], 
        col_dict_group_by$Categories)
  }
  
  
  if(group_by != ''){
    
    preprocess_var <- 
      preprocess_group <- 
      dataset_preprocess(dataset = colset[c(col,group_by)], data_dict = col_dict)
    
    preprocess_var <- preprocess_var[preprocess_var$`Variable name` == col,] 
    preprocess_group <- preprocess_group[preprocess_group$`Variable name` == group_by,] 
    
  } else {
    preprocess_var <- 
      dataset_preprocess(dataset = colset[col], data_dict = col_dict)
  }
  
  colset <- as_dataset(dataset_zap_data_dict(colset))

  # if(group_by != ''){
  #   if(toString(unique(preprocess_group$`Categorical variable`)) %in% 
  #      c('mix','no'))
  #     stop(call. = FALSE,
  #          'Your grouping variable must be a categorical variable.')}
  
  preprocess_var_values <-
    preprocess_var[preprocess_var$valid_class == '3_Valid other values',]
  preprocess_var_cat_values <- 
    preprocess_var[preprocess_var$valid_class == '1_Valid values',]
  preprocess_var_cat_miss_values <- 
    preprocess_var[preprocess_var$valid_class %in% 
                     c('2_Non-valid values','4_Empty values'),]
  
  if(group_by != ''){
    
    first_lab_var <- 
      names(col_dict[['Variables']] %>%
      select(matches(c("^label$","^label:[[:alnum:]]"))))[1]
    
    cat_lab <- 
      col_dict[['Categories']] %>% 
      dplyr::filter(if_any('variable') == group_by) %>%
      select(
        !! group_by := 'name', 
        `___labels___` = !! first_lab_var) %>%
      mutate(!! as.symbol(group_by) := as.character(!!as.symbol(group_by))) %>%
      add_index('___category_level___')
    
    colset <-  
      colset %>%
      mutate(!! group_by := as.character(!!as.symbol(group_by))) %>%
      left_join(cat_lab,by = group_by) %>%
      mutate(
        `___labels___` = 
          ifelse(!! as.symbol(group_by) == .data$`___labels___`,'',
                 paste0(' [',str_trunc(.data$`___labels___`,width = 19,
                                       ellipsis = '...'),']'))) %>%
      unite(!! group_by,c(!! group_by,'___labels___'),sep = '', 
            remove = TRUE,na.rm = TRUE)
  }
  
  # levels if group_by
  if(group_by != '') {
    cat_levels <- 
      colset %>% 
      arrange(.data$`___category_level___`) %>%
      pull(group_by) %>% unique
    
    colset <- 
      colset %>% 
      mutate(across(!! group_by, ~ factor(.,levels=c(cat_levels)))) %>%
      select(-'___category_level___')
  }
  
  if(sum(preprocess_var$index_in_dataset,na.rm = TRUE)*2 / 
     sum(!is.na(preprocess_var$index_in_dataset))-1 != 
     max(preprocess_var$index_in_dataset,na.rm = TRUE)){
    stop("error in the function variable_visualize(). Contact Maintainer")
  }
  
  colset[[1]] <- 
    preprocess_var %>%
    arrange(.data$`index_in_dataset`) %>%
    dplyr::filter(!is.na(.data$`index_in_dataset`)) %>%
    pull('value_var') %>% as_valueType(valueType = valueType_of(colset[[1]]))
    
    # colset <- 
    # colset %>%
    # rename_with(.cols = any_of(names(colset)), 
    #             .fn = ~ c("variable","group")[1:ncol(colset)]) %>%
    # bind_cols(
    #   preprocess_var %>%
    #     arrange(.data$`index_in_dataset`) %>%
    #     dplyr::filter(!is.na(.data$`index_in_dataset`)) %>%
    #     select(c('valid_class','value_var'))) %>%
    # mutate("variable" = 
    #          ifelse(.data$`valid_class` == "3_Valid other values",
    #                 .data$`value_var`,
    #                 .data$`variable`)) %>%
    # select(-c('valid_class','value_var')) %>%
    # rename_with(.cols = c("variable","group")[1:ncol(colset)], 
    #             .fn = ~ names(colset))
    
  if(is.null(variable_summary)){
    temp_group <- if(group_by == ''){NULL}else{group_by}
    variable_summary <- dataset_summarize(
      dataset = as_dataset(dataset[c(names(colset))]),
      data_dict = col_dict,
      valueType_guess = valueType_guess,
      group_by = temp_group, 
      dataset_name = 'dataset')}
  
  colset_values <-
    colset %>% 
    mutate(temp_val = as.character(!! as.symbol(col))) %>%
    rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
    dplyr::filter(.data$`temp_val` %in% 
                    preprocess_var_values$value_var) %>% ungroup %>%
    select(-'temp_val')
  
  colset_cat_values <-
    colset %>% 
    mutate(temp_val = as.character(!! as.symbol(col))) %>%
    rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
    dplyr::filter(.data$`temp_val` %in% 
                    preprocess_var_cat_values$value_var) %>% ungroup %>%
    select(-'temp_val')
  
  colset_cat_miss_values <-
    colset  %>% 
    mutate(temp_val = as.character(!! as.symbol(col))) %>%
    rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
    dplyr::filter(.data$`temp_val` %in% 
                    preprocess_var_cat_miss_values$value_var) %>% ungroup %>%
    select(-'temp_val')
  
  # guess the generic valueType of the variable (excluding categories):

  vT_col <-   
  if(valueType_guess == TRUE){
      madshapR::valueType_list[
        madshapR::valueType_list$valueType %in% 
          valueType_guess(dataset[[col]]),]    
  }else{

      madshapR::valueType_list[
        madshapR::valueType_list$valueType %in% 
          valueType_of(dataset[[col]]),] 
  }
  
  n_part <- nrow(colset)
  
  `Variables summary (all)` <- variable_summary[
    str_detect(names(variable_summary), "Variables summary \\(all\\)")][[1]]
  
  summary_1 <- 
    as.data.frame(t(
      
      `Variables summary (all)` %>% 
        rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
        dplyr::filter(.data$`Variable name` %in% col) %>% ungroup %>%
        select(-("Index":"Categories in data dictionary"))
    ))
  
  if(group_by != ''){
    names(summary_1) <- 
      
      unique(pull(
        `Variables summary (all)` %>%
          rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
          dplyr::filter(.data$`Variable name` %in% col) %>% ungroup %>%
          select(starts_with('Grouping variable:'))
      ))
    
  } else { names(summary_1) <- col}
  
  
  #### summary_1 ####
  summary_1 <-
    summary_1 %>% 
    mutate(col = row.names(summary_1)) %>%
    mutate(across(-c("col"), 
                  ~ ifelse(. == 0,NA_real_,.))) %>%
    select(-'col') %>%
    mutate(across(everything(),as.character))
    
  #### palettes ####
  palette_mlstr <- c("#7ab51d","#e9b400","#cc071e","dodgerblue3")
  palette_mlstr_fct <- colorRampPalette(palette_mlstr, 1)
  
  # levels if group_by
  palette_mlstr_first <- palette_mlstr[(length(colset_values[[col]]) >= 1)*1]
  palette_values <- 
    c(palette_mlstr_first, 
      palette_mlstr[seq_len(length(unique(colset_values[[group_by]])))],
      sample(palette_mlstr_fct(length(unique(colset_values[[group_by]]))))
      ) %>%
    tolower() %>% unique

  palette_values <- 
    palette_values[(!palette_values %in% c(NA))][
      unique(c(length(palette_mlstr_first),
      seq_len(length(unique(colset_values[[group_by]])))))]
  
  palette_categories <- 
    c(palette_values,
      palette_mlstr[seq_len(length(palette_values) + 
                              length(unique(colset_cat_values[[col]])))],
      sample(palette_mlstr_fct(
        length(unique(colset_cat_values[[col]])) +
          length(palette_values)
        ))) %>%
    tolower() %>% unique
  
  palette_categories <- 
    palette_categories[(!palette_categories %in% c(palette_values,NA))][
      seq_len(length(unique(colset_cat_values[[col]])))]
  
  palette_group <- 
    c(palette_values,palette_categories,
      palette_mlstr[seq_len(length(palette_values)+length(palette_categories)+
                              length(unique(colset[[group_by]])))],
      sample(palette_mlstr_fct(
        length(unique(colset[[group_by]])) +
          length(palette_values) +
          length(palette_categories)
      ))) %>%
    tolower() %>% unique
  
  palette_group <- 
    palette_group[(!palette_group %in% 
                     c(palette_values,palette_categories,NA))][seq_len(
                       length(unique(colset[[group_by]])))]
  
  palette_Empty <- "#afb1b2"
  palette_missing     <- 
    c("darkseagreen3", "lemonchiffon3","darksalmon","slategray3")
  palette_missing_fct <- colorRampPalette(palette_missing, 1)
  
  palette_missing <- 
    c(palette_Empty, palette_missing,
      palette_missing[seq_len(length(unique(colset_cat_miss_values[[col]])))],
      sample(palette_mlstr_fct(
        length(unique(colset_cat_miss_values[[col]])) +
        length(palette_Empty) +
        length(palette_missing)
      ))) %>%
    tolower() %>% unique
  
  palette_missing <- 
    palette_missing[(!palette_missing %in% c(palette_Empty,NA))][
      seq_len(length(unique(colset_cat_miss_values[[col]])))]
  
  # a ameliorer
  # names(palette_missing) <- levels(colset_cat_miss_values[[col]])
  # palette_missing['Empty'] <- palette_Empty
  #FCDF5C
  palette_pie <- c()
  # palette_pie["Valid values"]       <- "#88C79A" # green
  palette_pie["Valid values"]         <- "#FCDF5C" # yellow
  palette_pie["Valid other values"]   <- "#6581C0" # blue
  palette_pie["Non-valid values"]     <- "#EE7765" # red
  palette_pie["Empty values"]         <- palette_Empty # grey
  
  if(nrow(colset_values) > 0) {

    if(vT_col$`genericType` == "numeric"){
      
      `Numerical variable summary` <- variable_summary[
        str_detect(names(variable_summary), "Numerical variable summary")][[1]]

      #### summary_2 numeric ####
      summary_2 <- 
        as.data.frame(t(
          
          `Numerical variable summary` %>%
            rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
            dplyr::filter(.data$`Variable name` %in% col) %>% ungroup %>%
            select(-c(1:"% Non-valid values"))
          
          ))
    
      if(group_by != ''){
        names(summary_2) <- 
          
          unique(pull(
            `Numerical variable summary` %>%
              rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
              dplyr::filter(.data$`Variable name` %in% col) %>% ungroup %>%
              select(starts_with('Grouping variable:'))
          ))
            
      } else { names(summary_2) <- col}
      
      summary_2 <-
        summary_2 %>% 
        mutate(col = row.names(summary_2)) %>%
        mutate(across(-c("col"), ~ round(as.numeric(.),2))) %>%
        select(-'col') %>%
        mutate(across(everything(),as.character))
      
      ## PLOT graphs ##
      colset_values <- 
        colset_values %>% 
        mutate(across(
          all_of(col), ~ as_valueType(.,vT_col$`valueType`)))
      
      #### plot_1 numeric ####
      n_obs <- nrow(colset_values)
      
      title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
      if(group_by != '') title <- paste0(title, ' - per ',group_by)
    
      aes <-
        if(group_by == ''){
            aes(x = '',
                y = !! as.symbol(col),
                fill = '')}else{ 

            aes(x = fct_rev(!! as.symbol(group_by)),
                y = !! as.symbol(col),
                fill =  !! as.symbol(group_by))}
        
      plot_1 <-
        ggplot(colset_values) + aes +
        geom_boxplot(outlier.color = 'red') +
        theme_bw() +
        coord_flip() + 
        theme(legend.position="none",plot.title = 
                element_text(size=8, face = "bold")) +
        ggtitle(paste0('Box plot', title)) +
        ylab("") +
        xlab("") +
        scale_fill_manual(values = palette_values)
      
      #### plot_2 numeric ####
      aes <- 
        if(group_by == ''){
          aes(x     = !! as.symbol(col),group = '',fill  = ''
          )
        }else{
            
          aes(x     = !! as.symbol(col),
              group = !! as.symbol(group_by),
              fill  = !! as.symbol(group_by))
        }
    
      if(vT_col$valueType == "decimal") {
         geom_viz <- geom_density(color="black",na.rm = FALSE)
         title <- paste0('Density graph', title)}
      
      if(vT_col$valueType %in% c("integer","boolean")) {
        bin <- length(hist(colset_values[[col]],plot = FALSE)$breaks)
        geom_viz <- geom_histogram(bins = bin)
        title <- paste0('Histogram', title)}
      
        plot_2 <- 
          ggplot(colset_values) + aes +
          geom_viz +
          theme_bw() +
          ggtitle(paste0(title)) +
          theme(legend.position = "none",plot.title = 
                  element_text(size = 8, face = "bold"),
                strip.background = element_rect(color="white", fill="white")) +
          ylab("") +
          xlab("") +
          scale_fill_manual(values = palette_values)
        # no coord flip
        
      if(group_by != '') {plot_2 <- plot_2 + facet_wrap(as.symbol(group_by))}
        
    }
    
    if(vT_col$`genericType` == "character"){
      
      `Text variable summary` <- variable_summary[
        str_detect(names(variable_summary), "Text variable summary")][[1]]
      
      #### summary_2 character ####
      summary_2 <- 
        as.data.frame(t(
          
          `Text variable summary` %>%
            rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
            dplyr::filter(.data$`Variable name` %in% col) %>% ungroup %>%
            select(-c(1:"% Non-valid values"))
          
        ))
      
      if(group_by != ''){
        names(summary_2) <- 
          
          unique(pull(
            `Text variable summary` %>%
              rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
              dplyr::filter(.data$`Variable name` %in% col) %>% ungroup %>%
              select(starts_with('Grouping variable:'))
          ))
        
      } else { names(summary_2) <- col}
      
      summary_2 <-
        summary_2 %>% 
        mutate(col = row.names(summary_2)) %>%
        mutate(across(-c("col"), 
                      ~ str_trunc(.,width = 39,ellipsis = ' [...]'))) %>%
        select(-'col') %>%
        mutate(across(everything(),as.character))
      
      colset_values_main_word <- 
        colset_values_all_word <- 
        colset_values %>% 
        mutate(across(
          all_of(col), ~ as_valueType(.,vT_col$`valueType`)))
      
      if(group_by != ''){
        colset_values_main_word <- 
          colset_values_main_word %>%
          group_by(!! as.symbol(group_by))}
      
      colset_values_main_word <- 
        colset_values_main_word %>%
        unnest_tokens(output = word, input = !! as.symbol(col)) %>%
        anti_join(tidytext::stop_words,by = 'word') %>%
        count(word, sort = TRUE) %>%
        rename(`___n___` = last_col()) %>%
        rename(!! as.symbol(col) := word) %>%
        arrange(desc(.data$`___n___`)) %>%
        slice(1:10)
      
      if(group_by != ''){
        colset_values_all_word <- 
          colset_values_all_word %>%
          group_by(!! as.symbol(group_by))}
      
      colset_values_all_word <- 
        colset_values_all_word %>%
        count(!! as.symbol(col), sort = TRUE) %>%
        rename(`___n___` = last_col()) %>%
        mutate(!! as.symbol(col) := 
                 str_trunc(!! as.symbol(col),
                           width = 50,
                           ellipsis = '...')) %>%
        arrange(desc(.data$`___n___`)) %>%
        slice(1:10)
      
      
      #### plot_2 character ####      
      n_obs <- nrow(colset_values)
      
      title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
      if(group_by != '') title <- paste0(title, ' - per ',group_by)
      
      group_n <- "___n___"
      aes <- 
        if(group_by == ''){
          aes(
            x = fct_reorder(!! as.symbol(col),!! as.symbol(group_n)), 
            y = !! as.symbol(group_n),
            fill = '')
        }else{
          aes(
            x = fct_reorder(!! as.symbol(col),!! as.symbol(group_n)), 
            y = !! as.symbol(group_n), 
            fill = fct_rev(!! as.symbol(group_by)))}
      
      plot_1 <- 
        ggplot(colset_values_all_word) + aes + 
        geom_col() +
        theme_bw() +
        theme(legend.position = "none",plot.title =
                element_text(size = 8,face = "bold"),
              strip.background = element_rect(color = "white", fill="white")) +
        ggtitle(paste0('Bar plot', title)) +
        ylab("") +
        xlab("") +
        scale_fill_manual(values = rev(palette_values)) +
        coord_flip()
      
      if(group_by != '') {plot_1 <- plot_1 + facet_wrap(as.symbol(group_by))}
      
      #### plot_2 character ####      
      
      if(nrow(colset_values_main_word) == 0){
        
        plot_2 = NULL
        
      }else{
        
        n_obs <- nrow(colset_values)
        
        title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
        if(group_by != '') title <- paste0(title, ' - per ',group_by)
        
        group_n <- "___n___"
        
        aes <- 
          if(group_by == ''){
            aes(
              x = fct_reorder(!! as.symbol(col),!! as.symbol(group_n)), 
              y = !! as.symbol(group_n),
              fill = '')
          }else{
            aes(
              x = fct_reorder(!! as.symbol(col),!! as.symbol(group_n)), 
              y = !! as.symbol(group_n), 
              fill = fct_rev(!! as.symbol(group_by)))}
        
        plot_2 <- 
            ggplot(colset_values_main_word) + aes + 
            geom_col() +
            theme_bw() +
            theme(legend.position = "none",plot.title = 
                    element_text(size = 8,face = "bold"),
                  strip.background = element_rect(color = "white", fill="white")) +
            ggtitle(paste0('Most common entry', title)) +
            ylab("") +
            xlab("") +
            scale_fill_manual(values = rev(palette_values)) +
            coord_flip()
          
          if(group_by != '') {plot_2 <- plot_2 + facet_wrap(as.symbol(group_by))}
        }
      
      
      
      
    }
    
    if(vT_col$`genericType` == "datetime"){
    
      `Datetime variable summary` <- variable_summary[
        str_detect(names(variable_summary), "Datetime variable summary")][[1]]
        
      #### summary_2 datetime ####
      summary_2 <- 
        as.data.frame(t(
          
          `Datetime variable summary` %>%
            rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
            dplyr::filter(.data$`Variable name` %in% col) %>% ungroup %>%
            select(-c(1:"% Non-valid values"))
          
        ))
      
      if(group_by != ''){
        names(summary_2) <- 
          
          unique(pull(
            `Datetime variable summary` %>%
              rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
              dplyr::filter(.data$`Variable name` %in% col) %>% ungroup %>%
              select(starts_with('Grouping variable:'))
          ))
        
      } else { names(summary_2) <- col}
      
      summary_2 <-
        summary_2 %>% 
        mutate(col = row.names(summary_2)) %>%
        mutate(across(-c("col"), 
                      ~ str_trunc(.,width = 39,ellipsis = ' [...]'))) %>%
        select(-'col') %>%
        mutate(across(everything(),as.character))
      
      colset_values_main_word <- 
        colset_values_all_word <- 
        colset_values %>% 
        mutate(across(
          all_of(col), ~ as.character(.)))
      
      if(group_by != ''){
        colset_values_main_word <- 
          colset_values_main_word %>%
          group_by(!! as.symbol(group_by))}
      
      colset_values_main_word <- 
        colset_values_main_word %>%
        unnest_tokens(output = word, input = !! as.symbol(col)) %>%
        anti_join(tidytext::stop_words,by = 'word') %>%
        count(word, sort = TRUE) %>%
        rename(`___n___` = last_col()) %>%
        rename(!! as.symbol(col) := word) %>%
        arrange(desc(.data$`___n___`)) %>%
        slice(1:10)
      
      if(group_by != ''){
        colset_values_all_word <- 
          colset_values_all_word %>%
          group_by(!! as.symbol(group_by))}
      
      colset_values_all_word <- 
        colset_values_all_word %>%
        count(!! as.symbol(col), sort = TRUE) %>%
        rename(`___n___` = last_col()) %>%
        mutate(!! as.symbol(col) := 
                 str_trunc(!! as.symbol(col),
                           width = 50,
                           ellipsis = '...')) %>%
        arrange(desc(.data$`___n___`)) %>%
        slice(1:10)
      
      #### plot_1 datetime ####      
      n_obs <- nrow(colset_values)
      
      title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
      if(group_by != '') title <- paste0(title, ' - per ',group_by)
      
      group_n <- "___n___"
      
      aes <- 
        if(group_by == ''){
          aes(
            x = fct_reorder(!! as.symbol(col),!! as.symbol(group_n)), 
            y = !! as.symbol(group_n),
            fill = '')
        }else{
          aes(
            x = fct_reorder(!! as.symbol(col),!! as.symbol(group_n)), 
            y = !! as.symbol(group_n), 
            fill = fct_rev(!! as.symbol(group_by)))}
      
      plot_1 <- 
        ggplot(colset_values_main_word) + aes + 
        geom_col() +
        theme_bw() +
        theme(legend.position="none",plot.title = 
                element_text(size=8,face = "bold"),
              strip.background = element_rect(color = "white", fill="white")) +
        ggtitle(paste0('Most common entry', title)) +
        ylab("") +
        xlab("") +
        scale_fill_manual(values = rev(palette_values)) +
        coord_flip()
      
      if(group_by != '') {plot_1 <- plot_1 + facet_wrap(as.symbol(group_by))}
      
      #### plot_2 datetime ####      
      n_obs <- nrow(colset_values)
      
      title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
      if(group_by != '') title <- paste0(title, ' - per ',group_by)
      
      group_n <- "___n___"
      aes <- 
        if(group_by == ''){
          aes(
            x = fct_reorder(!! as.symbol(col),!! as.symbol(group_n)), 
            y = !! as.symbol(group_n),
            fill = '')
        }else{
          aes(
            x = fct_reorder(!! as.symbol(col),!! as.symbol(group_n)), 
            y = !! as.symbol(group_n), 
            fill = fct_rev(!! as.symbol(group_by)))}
      
      plot_2 <- 
        ggplot(colset_values_all_word) + aes + 
        geom_col() +
        theme_bw() +
        theme(legend.position="none",plot.title =
                element_text(size=8,face = "bold"),
              strip.background = element_rect(color = "white", fill="white")) +
        ggtitle(paste0('Bar plot', title)) +
        ylab("") +
        xlab("") +
        scale_fill_manual(values = rev(palette_values)) +
        coord_flip()
      
      if(group_by != '') {plot_2 <- plot_2 + facet_wrap(as.symbol(group_by))}
      
    }
    
    if(vT_col$`genericType` == "date"){
      
      `Date variable summary` <- variable_summary[
        str_detect(names(variable_summary), "Date variable summary")][[1]]
      
      #### summary_2 date ####    
      summary_2 <- 
        as.data.frame(t(
          
          `Date variable summary` %>%
            rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
            dplyr::filter(.data$`Variable name` %in% col) %>% ungroup %>%
            select(-c(1:"% Non-valid values"))
          
        ))
      
      if(group_by != ''){
        names(summary_2) <- 
          
          unique(pull(
            `Date variable summary` %>%
              rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
              dplyr::filter(.data$`Variable name` %in% col) %>% ungroup %>%
              select(starts_with('Grouping variable:'))
          ))
        
      } else { names(summary_2) <- col}
      
      summary_2 <-
        summary_2 %>% 
        mutate(col = row.names(summary_2)) %>%
        mutate(across(-c("col"), ~ .)) %>%
        select(-'col') %>%
        mutate(across(everything(),as.character))
      
      colset_values <- 
        colset_values %>% 
        mutate(across(
          all_of(col), ~ as_valueType(.,vT_col$`valueType`)))
      
      if(group_by != '') {
        colset_values <- colset_values %>% group_by(!! as.symbol(group_by))}
      
      # convert dataset to wide format
      colset_span <- 
        colset_values %>%
        dplyr::filter(if_any(col) == min(!! as.symbol(col)) | 
                 if_any(col) == max(!! as.symbol(col))) 
      
      n_obs <- nrow(colset_values)
      
      #### plot_1 date ####    
      
      title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
      if(group_by != '') title <- paste0(title, ' - per ',group_by)
      
      aes <-
        if(group_by == ''){
            aes(x = !! as.symbol(col),
                y = '',
                color = '')}else{

            aes(x = !! as.symbol(col),
                y = fct_rev(!! as.symbol(group_by)),
                color = !! as.symbol(group_by))}

      plot_1 <- 
        ggplot(colset_span) + aes +
        geom_line() +
        geom_point(size = 3) +
        ggtitle(paste0('Span date', title)) +
        theme_bw()+
        theme(legend.position="none",plot.title = 
                element_text(size=8,face = "bold")) +
        ylab("") +
        xlab("") +
        scale_color_manual(values = palette_values) 
      
      #### plot_2 date ####    
      
      title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
      if(group_by != '') title <- paste0(title, ' - per ',group_by)
      
      aes <-
        if(group_by == ''){
          aes(x = '',
              y = !! as.symbol(col),
              fill = '')}else{ 
                
                aes(x = fct_rev(!! as.symbol(group_by)),
                    y = !! as.symbol(col),
                    fill =  !! as.symbol(group_by))}
      
      plot_2 <-
        ggplot(colset_values) + aes +
        geom_boxplot(outlier.color = 'red') +
        theme_bw() +
        coord_flip() + 
        theme(legend.position="none",plot.title = 
                element_text(size=8, face = "bold")) +
        ggtitle(paste0('Box plot', title)) +
        ylab("") +
        xlab("") +
        scale_fill_manual(values = (palette_values))
      
      
      # n_obs <- nrow(colset_values)
      # 
      # title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
      # if(group_by != '') title <- paste0(title, ' - per ',group_by)
      # 
      # aes <-
      #   if(group_by == ''){
      #     aes(x = !! as.symbol(col),
      #         fill = '')
      #   }else{
      #     aes(
      #       x = !! as.symbol(col),
      #       fill = !! as.symbol(group_by))}
      # 
      # max_span <- 
      #   max(ungroup(colset_span) %>%
      #         pull(!! as.symbol(col))) -
      #   min(ungroup(colset_span) %>%
      #         pull(!! as.symbol(col))) + 1
      # 
      # bins <- ceiling(as.integer(max_span) / 365 / 5)
      # 
      # plot_2 <- 
      #   ggplot(colset_values) + aes +
      #   geom_histogram(bins = bins) +
      #   theme_bw() +
      #   ggtitle(paste0('Histogram', title)) +
      #   theme(legend.position="none",plot.title = 
      #           element_text(size=8,face = "bold"),
      #         strip.background = element_rect(color = "white", fill="white")) +
      #   ylab("") +
      #   xlab("") +
      #   scale_fill_manual(values = palette_values)
      # # no coord flip
      # 
      # if(group_by != '') {plot_2 <- plot_2 + facet_wrap(as.symbol(group_by))}
      
    }
    
  }else{plot_1 <- plot_2 <- summary_2 <- NULL}

  # categorical part of variable
  plot_3 <- NULL

  if(nrow(colset_cat_values) > 0){
    
    first_lab_var <- 
      names(col_dict[['Variables']] %>%
      select(matches(c("^label$","^label:[[:alnum:]]"))))[1]
    
    n_obs <- nrow(colset_cat_values)
    cat_lab_var <- 
      col_dict[['Categories']] %>% 
      dplyr::filter(if_any('variable') == col) %>%
      select(
        !!as.symbol(col) := 'name', 
        `___labels___` = !! first_lab_var) %>%
      mutate(!! as.symbol(col) := as.character(!!as.symbol(col))) %>%
      add_index('___category_level___')

    colset_cat_values <-  
      colset_cat_values %>%
      mutate(!! as.symbol(col) := as.character(!!as.symbol(col))) %>%
      left_join(cat_lab_var,by = col) %>%
      mutate(
        `___labels___` = 
          ifelse(!! as.symbol(col) == .data$`___labels___`,'',
                 paste0(' [',str_trunc(.data$`___labels___`,width = 19,
                                       ellipsis = '...'),']'))) %>%
      unite(!! col,c(col,'___labels___'),sep='', remove = TRUE,na.rm = TRUE) %>%
      mutate(across(all_of(col), ~ na_if(.,'')))
    
    cat_var_levels <- 
      colset_cat_values %>% 
      arrange(.data$`___category_level___`) %>%
      dplyr::filter(!is.na(!!as.symbol(col))) %>%
      pull(col) %>% unique 
    
    if(length(cat_var_levels) == 0) cat_var_levels <- 0
    
    #### plot_3 categorical_values ####    
    colset_cat_values <- 
      colset_cat_values %>% 
      mutate(across(!! as.symbol(col), ~factor(.,levels=c(cat_var_levels)))) %>%
      select(-'___category_level___')
    
    title <- paste0(' representation of categorical values in ',col,
                    ' (N obs. : ',n_obs,')')
    if(group_by != '') title <- paste0(title, ' - per ',group_by)
    
    aes <- aes(x    = fct_rev(!! as.symbol(col)), 
               fill =  !! as.symbol(col))

    plot_3 <- 
      ggplot(colset_cat_values) + aes +
      geom_bar() +
      theme_bw() + 
      theme(legend.position="none",plot.title = 
              element_text(size = 8, face = "bold"),
            strip.background = element_rect(color = "white", fill="white")) +
      ggtitle(paste0('Bar plot', title)) +
      ylab("") +
      xlab("") +
      scale_fill_manual(values = palette_categories) +
      coord_flip()
    
      if(group_by != '') {plot_3 <- plot_3 + facet_wrap(as.symbol(group_by))}
    }

  plot_4 <- NULL
  if(nrow(colset_cat_miss_values[col]) > 0 & 
     nrow(unique(colset_cat_miss_values[col])) > 1){
    
    n_obs <- nrow(colset_cat_miss_values)
    
    if(sum(nrow(col_dict[['Categories']])) > 0){
    
      first_lab_var <- 
        names(col_dict[['Variables']] %>%
        select(matches(c("^label$","^label:[[:alnum:]]"))))[1]
      
      cat_lab_miss_var <- 
        col_dict[['Categories']] %>% 
        dplyr::filter(if_any('variable') == col) %>%
        select(
          !!as.symbol(col) := 'name', 
          `___labels___` = !!first_lab_var) %>%
        mutate(!! as.symbol(col) := as.character(!!as.symbol(col))) %>%
        add_index('___category_level___') 
      
    } else { cat_lab_miss_var <- 
      tibble(
        col = as.character(),
        `___labels___` = as.character(),
        `___category_level___` = as.character()) %>%
      rename(!!as.symbol(col) := col)}

    colset_cat_miss_values <-  
      colset_cat_miss_values %>%
      mutate(!! as.symbol(col) := as.character(!!as.symbol(col))) %>%
      left_join(cat_lab_miss_var,by = col) %>%
      mutate(
        `___labels___` = 
          ifelse(!! as.symbol(col) == .data$`___labels___`,'',
                 paste0(' [',str_trunc(.data$`___labels___`,width = 19,
                                       ellipsis = '...'),']'))) %>%
      unite(!! col,c(any_of(col),'___labels___'),sep = '', 
            remove = TRUE,na.rm = TRUE) %>%
      mutate(across(all_of(col), ~ na_if(.,'')))
    
    cat_var_levels <- 
      colset_cat_miss_values %>% 
      arrange(.data$`___category_level___`) %>%
      dplyr::filter(!is.na(!!as.symbol(col))) %>%
      pull(col) %>% unique 
    
    if(length(cat_var_levels) == 0) cat_var_levels <- 0
    
    colset_cat_miss_values <- 
      colset_cat_miss_values %>% 
      mutate(across(!! as.symbol(col), ~factor(.,levels=c(cat_var_levels)))) %>%
      select(-'___category_level___')
    
    # a ameliorer
    names(palette_missing) <- levels(colset_cat_miss_values[[col]])

    #### plot_4 missing_values ####    
    title <- paste0(' representation of missing categorical values in ',col,
                    ' (N obs. : ',n_obs,')')
    if(group_by != '') title <- paste0(title, ' - per ',group_by)
    
    aes <- aes(x = fct_rev(!! as.symbol(col)), 
               fill =  !! as.symbol(col))
    
    plot_4 <- 
      ggplot(colset_cat_miss_values) + aes +
      geom_bar() +
      theme_bw() +
      theme(legend.position = "none",plot.title = 
              element_text(size = 8, face = "bold"),
            strip.background = element_rect(color = "white", fill = "white")) +
      ggtitle(paste0('Bar plot', title)) +
      ylab("") +
      xlab("") +
      scale_fill_manual(na.value = palette_Empty, values = palette_missing) +
      coord_flip()
    
      if(group_by != '') {plot_4 <- plot_4 + facet_wrap(as.symbol(group_by))}
  }

  # categorization of variable (valid/missing/others/NA)
  preprocess_var <-
    preprocess_var %>%
    select('valid_class', 'value_var') %>% 
    rename_with(.cols = 'valid_class', ~ '___valid_class___') %>%
    rename_with(.cols = 'value_var', ~ col) %>%
    distinct
  
  colset_valid <-
    colset %>%
    mutate(across(all_of(col),as.character)) %>%
    left_join(preprocess_var,by = 
                intersect(names(colset), names(preprocess_var))) %>%
    select(- !! col) %>%
    mutate(`___valid_class___` = str_sub(.data$`___valid_class___`,3)) %>%
    rename_with(.cols = '___valid_class___', ~ col) %>% 
    group_by(across(everything())) %>%
    tally %>%
    rename(`___n___` = last_col()) %>%
    mutate(!! as.symbol(col) := factor(!! as.symbol(col),
      levels = 
        c('Valid values','Valid other values','Non-valid values','Empty values')))
  
  plot_5 <- NULL
  
  if(length(unique(colset_valid[[col]])) > 1){
    
    #### plot_5 pie_values ####    
    n_obs <- nrow(colset)
    title <- paste0(' representation of validity values distribution in ',col,
                    ' (N obs. : ',n_obs,')')
    if(group_by != '') title <- paste0(title, ' - per ',group_by)
    
    group_n <- "___n___"
    aes <- aes(x = '',y = !! as.symbol(group_n), fill = !! as.symbol(col))
    
    colset_valid <-
      colset_valid %>% 
      mutate(
        prop = round((.data$`___n___`/sum(.data$`___n___`)),2),
        csum = cumsum(.data$`prop`), 
        pos = .data$`prop`/2 + lag(.data$`csum`, 1),
        pos = if_else(is.na(.data$`pos`), .data$`prop`/2, .data$`pos`)) %>%
      group_by(across(any_of(group_by))) %>%
      mutate(
        label = paste0(as.character(round((
          .data$`___n___`/sum(.data$`___n___`))*100,2)),"%"))
    
    plot_5 <-
      ggplot(colset_valid) + aes +
      geom_bar(stat='identity',width = 1,position = position_fill()) +
      geom_text(
        size = 2.5,
        aes(x = 1.8,label = !! as.symbol('label')),
        position = position_fill(vjust = 0.5)) +
      coord_polar('y', start = 0) +
      theme_void() + 
      theme(
        legend.position = "right",
        plot.title = element_text(size = 8,face = "bold")) +
      ggtitle(paste0('Pie chart', title)) +
      scale_fill_manual(values = palette_pie) +
      geom_segment(
        aes(x = 1.500, 
            y = !! as.symbol('pos'), 
            xend = 1.450, 
            yend = !! as.symbol('pos')), 
        color = "black", linewidth = 1) 
    
      if(group_by != '') {plot_5 <- plot_5 + facet_wrap(as.symbol(group_by))}

  }
  
  if(is.null(plot_1)){plot_1 <- plot_3 ; plot_3 <- NULL} 
  if(is.null(plot_2)){plot_2 <- plot_4 ; plot_4 <- NULL}
  
  # category table

  if(length(variable_summary[
    str_detect(names(variable_summary),"Categorical variable summary")]) == 1){

    `Categorical variable summary` <- variable_summary[
      str_detect(names(variable_summary), "Categorical variable summary")][[1]]
    
    if(nrow(`Categorical variable summary` %>% 
            rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
            dplyr::filter(.data$`Variable name` %in% col) %>% ungroup) > 0){
      
      summary_categories <- 
        as.data.frame(t(
          
          `Categorical variable summary` %>%
            rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
            dplyr::filter(.data$`Variable name` %in% col) %>% ungroup %>%
            select(-c(1:"Number of distinct values"))
          
        ))
      
      if(group_by != ''){
        names(summary_categories) <- 
          
          unique(pull(
            `Categorical variable summary` %>%
              rowwise() %>%                # [GF] to test. rowwise seems mandatory when using filter + %in% 
              dplyr::filter(.data$`Variable name` %in% col) %>% ungroup %>%
              select(starts_with('Grouping variable:'))
          ))
        
      } else { names(summary_categories) <- col}
      
      summary_categories <-
        summary_categories %>% 
        mutate(col = row.names(summary_categories)) %>%
        mutate(across(-c("col"), ~ str_replace_all(.,"\\n","<br>"))) %>%
        select(-'col') %>%
        mutate(across(everything(),as.character)) %>%
        remove_empty('rows')
      
      if(is.null(summary_2)){
        summary_2 <- summary_categories
        summary_categories <- NULL
        
      } else {
        summary_categories <-
          datatable(summary_categories,
                    options = 
                      list(dom = 't', 
                           scrollX = TRUE,
                           pageLength = nrow(summary_categories),
                           ordering = FALSE,
                           paging = TRUE),
                    filter = 'none' ,  
                    escape = FALSE) 
      }
    } else { summary_categories <- NULL }
  } else { summary_categories <- NULL }
  
  # assemble tables
  summary_table <- 
    bind_rows(summary_1,summary_2) %>%
    remove_empty('rows')
  
  summary_table <-
    datatable(summary_table,
              options = 
                list(dom = 't', 
                     scrollX = TRUE,
                     pageLength = nrow(summary_table),
                     ordering = FALSE,
                     paging = TRUE),
              filter = 'none' ,  
              escape = FALSE)
  
  plots <- list(
    summary_table = summary_table,
    summary_categories = summary_categories,
    main_values_1 = plot_1, 
    main_values_2 = plot_2, 
    cat_values = plot_3, 
    missing_values = plot_4, 
    pie_values = plot_5)

  plots <- plots[
    vapply(X = plots,
           FUN = function(x) !is.null(x),
           FUN.VALUE = logical(1))]
  
  return(plots)
}

#' @title
#' Generate a web-based visual report for a dataset
#'
#' @description
#' Generates a visual report of a dataset in an HTML bookdown 
#' document, with summary figures and statistics for each variable. The report 
#' outputs can be grouped by a categorical variable.
#'
#' @details
#' A dataset is a data table containing variables. A dataset object is a 
#' data frame and can be associated with a data dictionary. If no 
#' data dictionary is provided with a dataset, a minimum workable 
#' data dictionary will be generated as needed within relevant functions.
#' Identifier variable(s) for indexing can be specified by the user. 
#' The id values must be non-missing and will be used in functions that 
#' require it. If no identifier variable is specified, indexing is 
#' handled automatically by the function.
#' 
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
#' A taxonomy is a classification schema that can be defined for variable 
#' attributes. A taxonomy is usually extracted from an 
#' [Opal environment](https://www.obiba.org/pages/products/opal/), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#'
#' @seealso
#' [bookdown_open()]
#' [as_category()]
#'
#' @param dataset A dataset object.
#' @param bookdown_path A character string identifying the folder path where 
#' the bookdown report files will be saved.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#' @param group_by A character string identifying the column in the dataset
#' to use as a grouping variable. Elements will be grouped by this 
#' column.
#' @param valueType_guess Whether the output should include a more accurate 
#' valueType that could be applied to the dataset. FALSE by default.
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#' @param dataset_summary A list which identifies an existing 
#' summary produced by [dataset_summarize()] of the dataset.
#' Using this parameter can save time in generating the visual report.
#' @param dataset_name A character string specifying the name of the dataset 
#' (used internally in the function [dossier_evaluate()]).
#' @param .dataset_name `r lifecycle::badge("deprecated")`
#' @param .summary_var `r lifecycle::badge("deprecated")`
#'
#' @returns
#' A folder containing files for the bookdown site. To open the bookdown site 
#' in a browser, open 'docs/index.html', or use [bookdown_open()] with the 
#' folder path.
#'
#' @examples
#' {
#' 
#' library(fs)
#' library(dplyr)
#'  
#' # use madshapR_example provided by the package 
#' dataset <- 
#'   madshapR_example$`dataset_example` %>% 
#'   group_by(pick('gndr')) %>% 
#'   as_dataset(col_id = "part_id")
#'   
#' data_dict <- madshapR_example$`data_dict_example`
#' dataset_summary <- dataset_summarize(dataset,data_dict)
#'  
#' if(dir_exists(tempdir())) dir_delete(tempdir())
#' bookdown_path <- tempdir()
#'  
#' dataset_visualize(
#'   dataset,
#'   data_dict,
#'   dataset_summary = dataset_summary,
#'   bookdown_path = bookdown_path)
#'   
#' # To open the file in browser, open 'bookdown_path/docs/index.html'. 
#' # Or use bookdown_open(bookdown_path) function.
#' 
#' }
#'
#' @import dplyr knitr fabR
#' @import bookdown utils readr stringr fs DT ggplot2 
#' @importFrom rlang .data
#'
#' @export
dataset_visualize <- function(
    dataset = tibble(id = as.character()),
    bookdown_path,
    data_dict = NULL,
    group_by = NULL,
    valueType_guess = FALSE,
    taxonomy = NULL,
    dataset_name = .dataset_name, 
    dataset_summary = .summary_var,
    .summary_var = NULL, 
    .dataset_name = NULL){
  
  # fargs <- list()
  fargs <- as.list(match.call(expand.dots = TRUE))
  
  # future dev
  # mutate(key = paste0('<b>' , key, '</b>')),
  # @ param toc xxx xxx xxx
  # toc <- 'variables'
  
  # check input
  render <- 'html'
  
  # check on argument : taxonomy
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  
  if(!is.logical(valueType_guess))
    stop(call. = FALSE,'`valueType_guess` must be TRUE or FALSE (TRUE by default)')
  
  if(!is.character(bookdown_path))
    stop(call. = FALSE,'`bookdown_path` must be a character string.')

  bookdown_path <- str_squish(bookdown_path)
  path_to <- path_abs(bookdown_path)
  
  if(dir_exists(path_to)){stop(call. = FALSE,
"The path folder already exists. 
Please provide another name folder or delete the existing one.")}
  
  # test if enough dataset
  as_dataset(dataset, col_id(dataset))
  col_id <- col_id(dataset)
  
  # if data_dict empty
  if(is.null(data_dict)){
    data_dict <- 
      data_dict_extract(dataset,as_data_dict_mlstr = TRUE) 
  }else{
    data_dict <- as_data_dict_mlstr(data_dict)}

  if(toString(substitute(group_by)) == '') group_by <- NULL
  # attempt to catch group_by from the group_vars if the dataset is grouped
  if(length(group_vars(dataset)) == 1 & toString(substitute(group_by)) == ''){
    group_by <- group_vars(dataset)
  }
  
  dataset <- as_dataset(ungroup(dataset),col_id)
  
  dataset_name <- 
    suppressWarnings(
    ifelse(
      !is.null(dataset_name),
      dataset_name,
      make_name_list(as.character(fargs[['dataset']]),
                           list_elem = list(NULL))))
  
  # attempt to catch group_by
  if(toString(substitute(group_by)) != ''){
    group_by <- tryCatch(
      expr  = {toString(names(dataset[toString(substitute(group_by))]))},
      error = function(cond){return(toString(names(dataset[group_by])))})    
    
    if(! group_by %in% data_dict[['Categories']][['variable']]) group_by <- ''
    
  }else{ group_by <- ''}
  
  dataset <-
    suppressWarnings({
      data_dict_match_dataset(
        dataset,data_dict,
        output = 'dataset') %>%
        as_dataset(col_id)})
  
  data_dict <- 
    suppressWarnings({
      data_dict_match_dataset(
        dataset,data_dict,
        output = 'data_dict') %>%
        as_data_dict_mlstr()})
  
  # summarize initial information
  
  if(is.null(dataset_summary)){
    temp_group <- if(group_by == ''){NULL}else{group_by}
    dataset_summary <- dataset_summarize(
      dataset = dataset,
      data_dict = data_dict,
      group_by = temp_group,
      valueType_guess = valueType_guess,
      taxonomy = taxonomy,
      dataset_name = dataset_name)}

  data_dict$Variables <- 
    data_dict$Variables %>% add_index(.force = TRUE)
  
  data_dict_flat <- data_dict
  data_dict_flat[['Variables']] <- data_dict$Variables
  
  if(sum(nrow(data_dict_flat[['Categories']])) > 0){
    data_dict_flat[['Categories']] <- 
      data_dict[['Categories']] %>% 
      add_index("madshapR::index_original",.force = TRUE) %>%
      group_by(.data$`variable`) %>%
      slice(1:6) %>%
      add_index("madshapR::index_group",.force = TRUE) %>%
      mutate(across(
        -c("variable","madshapR::index_group","madshapR::index_original"), ~ 
        ifelse(.data$`madshapR::index_group` == 6,'[...]',.) )) %>%
      ungroup() %>%
      arrange(.data$`madshapR::index_original`) %>%
      select(-"madshapR::index_group",-"madshapR::index_original")
  }
  
  first_lab_var <- 
    names(data_dict_flat[['Variables']] %>%
    select(matches(c("^label$","^label:[[:alnum:]]"))))[1]
  
  if(is.na(first_lab_var)) first_lab_var <- "label"
  
  data_dict_flat <- 
    suppressWarnings(data_dict_collapse(data_dict_flat)[[1]]) %>%
    bind_rows(tibble("Categories::label" = as.character())) %>%
    select(
      "Index" = matches("index"),
      "Variable name" = "name",
      "Variable label" = any_of(first_lab_var),
      matches('valueType'),
      Categories = any_of(paste0("Categories::",first_lab_var))) %>% 
    mutate(Categories = str_replace_all(.data$`Categories`,"; \n","<br>")) %>%
    mutate(Categories = str_replace_all(
      .data$`Categories`,"\\[\\.\\.\\.\\] = \\[\\.\\.\\.\\]","[...]"))
    
  bookdown_template(path_to, overwrite = FALSE)
  if(!dir.exists(paste0(path_to,"/src"))) dir.create(paste0(path_to,"/src"))
  save(
    path_to,dataset, data_dict, group_by,data_dict_flat, dataset_summary,col_id,
    valueType_guess,
    file = paste0(path_to,"/src/r_env.RData"))
  
  ## markdown writing elements
  ##### HEADER index ##########
  
  if(render == 'html'){
  
  paste0(
    '---
title: ',dataset_name,'
date: "`r Sys.Date()`"
site: bookdown::bookdown_site
---

') %>% write_lines(file = paste0(path_to,"/index.Rmd"),append = FALSE)
  
  
  ##### _bookdown.yml ##########
  paste0(
'book_filename: "bookdownproj"
output_dir: docs
delete_merged_file: false
language:
  ui:
    chapter_name: ""

') %>% write_lines(file = paste0(path_to,"/_bookdown.yml"),append = FALSE)
  
  ##### _output.yml ##########
  paste0(
'bookdown::gitbook:
  css: style.css
  config:
    toc:
      before: |
        <li><a href="./">','DATASET : ',toupper(dataset_name),'</a></li>
    sharing:
    facebook: false
    twitter: false

') %>% write_lines(file = paste0(path_to,"/_output.yml"),append = FALSE)
  
  paste0(
'

```{r echo = FALSE, message = FALSE, warning = FALSE}

library(fabR)
library(madshapR)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)

load(file = paste0("', path_to,'/src/r_env.RData"))

```

# Overview {.unnumbered #about}

```{r echo = FALSE, message = FALSE, warning = FALSE}

Overview <- dataset_summary[str_detect(names(dataset_summary), "Overview")][[1]]

datatable(Overview, 
    colnames = rep("",ncol(Overview)),
    options = list(dom = "t", scrollX = TRUE, ordering = FALSE,paging = FALSE),
    rownames = FALSE, filter = "none" ,  escape = FALSE)

```

') %>% write_lines(file = paste0(path_to,"/index.Rmd"),append = TRUE)
  
  # if(toc == 'variables'){
  
  ##### CONTENT ##########
  
  increment <-
    paste0(rep(0,nchar(nrow(data_dict$Variables))) %>% paste(collapse = ""))
  
  for(i in seq_len(nrow(data_dict$Variables))){
    # stop()}
    
    rmd_file_name <-
      paste0(path_to,"/",
             str_sub(paste0(increment,i),-(increment %>% nchar + 1),-1),"-",
             make.names(data_dict$Variables$name[i]),".Rmd")
    file.create(rmd_file_name)
    
    paste0(
      "# ", 
      data_dict$Variables$`name`[i] %>%
      str_replace_all("(?=[^A-Za-z0-9])", "\\\\"),
      "{.unnumbered #var",i,"}\n\n") %>%
      
      
      paste0("\n<p style= \"font-size: 140%;\">**VARIABLE CHARACTERISTICS**</p>\n") %>%
      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0(
        "\n```{r ",
        str_squish(
          "echo = FALSE,message = FALSE,warning = FALSE,knitr.figure = TRUE}"),
        "\n
  
  first_lab_var <- 
    names(data_dict$Variables %>%
    select(matches(c('^label$','^label:[[:alnum:]]'))))[1]
    
  datatable(t(
     data_dict$Variables %>%
     dplyr::filter(`name` == '",data_dict$Variables$`name`[i],"') %>%
     select(
      'Variable name' = 'name', 
      'Variable label' = any_of(first_lab_var), 
      'Data dictionary valueType' = 'valueType')),
   options = list(dom = 't', scrollX = TRUE, ordering = FALSE,paging = FALSE),
   rownames = TRUE, colnames = rep('', 2),filter = 'none' ,  escape = FALSE)",
        
        "\n\n```\n") %>%
      
      paste0("\n</div>\n\n") %>%
      paste0(ifelse(
        sum(nrow(
          data_dict[['Categories']][data_dict[['Categories']][['variable']] == 
                                      data_dict$Variables$`name`[i],])) > 0,
        paste0("\n<p style= \"font-size: 140%;\"> **Categories**: </p>","\n\n") %>%
          paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
          paste0(
"\n```{r echo = FALSE, message = FALSE, warning = FALSE}",
"\n


    n_cat <- data_dict$Categories %>% 
      dplyr::filter(variable == '",data_dict$Variables$`name`[i],"')

if(nrow(n_cat) > 20){

  datatable(
    data_dict$Categories %>% 
      dplyr::filter(variable == '",data_dict$Variables$`name`[i],"') %>%
    select(
      'Variable name' = 'variable', 
      'Categorie label' = any_of(first_lab_var), 
      'Categorie code' = 'name', 
      'missing') %>%
    mutate(across(everything(), as.character)),
    options = list(scrollX = TRUE),rownames = FALSE)

}else{

  datatable(
    data_dict$Categories %>% 
      dplyr::filter(variable == '",data_dict$Variables$`name`[i],"') %>%
    select(
      'Variable name' = 'variable', 
      'Categorie label' = any_of(first_lab_var), 
      'Categorie code' = 'name', 
      'missing') %>%
    mutate(across(everything(), as.character)),
    options=list(dom = 't', scrollX = TRUE, ordering = FALSE,paging = FALSE),
    filter = 'none' ,rownames = FALSE,escape = FALSE)

  }

                            ",
                        
                        "\n\n```\n") %>%
                      paste0("\n</div>\n\n") ,"")) %>%
      paste0("
\n----------------------------------------------------------------------\n") %>%
      
      paste0(ifelse(nrow(dataset[i]) > 0, 
      "\n<p style= \"font-size: 140%;\">**SUMMARY STATISTICS**</p>\n","")) %>%
      
      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0(
        "\n```{r ",
        str_squish(
          "echo = FALSE,message = FALSE,warning = FALSE,knitr.figure = TRUE}"),
        "\n
        
 plots <- variable_visualize(
  dataset,
  col = '", names(dataset[i]),"',
  data_dict = data_dict, 
  group_by = '", group_by, "',
  valueType_guess = '", valueType_guess, "',
  variable_summary = dataset_summary)       
        
  if(!is.null(plots$summary_table))      plots$summary_table                  ",
        
        "\n\n```\n") %>%
      
      paste0("\n</div>\n\n") %>%
      
      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0(
        "\n```{r ",
        str_squish(
          "echo = FALSE,message = FALSE,warning = FALSE,knitr.figure = TRUE}"),
        "\n
        
  if(!is.null(plots$summary_categories)) plots$summary_categories             ",
        
        "\n\n```\n") %>%
      
      paste0("\n</div>\n\n") %>%
      
      paste0(
"\n---------------------------------------------------------------------\n") %>%
      
      paste0(ifelse(nrow(dataset[i]) > 0, 
      "\n<p style= \"font-size: 140%;\">**VISUAL REPRESENTATION**</p>\n","")) %>%
      
      paste0(
        "\n```{r, figures-plot12-",i,
        str_squish(
        ", fig.show='hold',fig.align = 'center',echo = FALSE,message = FALSE,
              warning = FALSE, results='hide'}"),
        "\n
        
if(!is.null(plots$main_values_1))      plots$main_values_1
if(!is.null(plots$main_values_2))      plots$main_values_2
if(!is.null(plots$cat_values))         plots$cat_values
if(!is.null(plots$missing_values))     plots$missing_values
if(!is.null(plots$pie_values))         plots$pie_values                       ",
        
        "\n\n```\n") %>%
      write_lines(file = rmd_file_name, append = FALSE)
  }
  
  silently_run(
    file.remove(
      str_subset(
        string = list.files(path_to,full.names = TRUE),
        pattern = paste0(
          path_to,"/[[:digit:]]+-",
          toString(as.character(make.names(col_id))),
          ".Rmd$")))
    )


  bookdown_render(path_to,overwrite = FALSE)
  
  return(message(
"\n\nTo edit your file, You can use the function `bookdown_open('",bookdown_path,"')`
(Compatibility tested on Chrome, Edge and Mozilla)\n\n"))
  
  }
  
  
}

