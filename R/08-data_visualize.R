#' @title
#' Generate a list of charts, figures and summary tables of a variable
#'
#' @description
#' Analyses the content of a variable and its data dictionary (if any), 
#' identifies its data type and values accordingly and generates figures and 
#' summaries (datatable format). The figures and tables are representations of
#' data distribution, statistics and valid/non valid/missing values (based on 
#' the data dictionary information if provided and the data type of the 
#' variable). This function can be used to personalize report parameters and is 
#' internally used in the function [dataset_visualize()]. Up to seven objects 
#' are generated which include : One datatable of the key elements of the 
#' data dictionary, one datatable summarizing statistics (such as mean, 
#' quartile, most seen value, most recent date, ... , depending on the 
#' data type of the variable), two graphs showing the distribution of the 
#' variable, One bar chart for categorical values (if any), One bar chart for 
#' missing values (if any), One pie chart for the proportion of valid and 
#' missing values (if any). The variable can be grouped using `group_by` 
#' parameter, which is a (categorical) column in the dataset. The user may need 
#' to use [as.factor()] in this context. To fasten the process (and allow 
#' recycling object in a workflow) the user can feed the function with a 
#' `.summary_var`, which is the output of the function [dataset_summarize()] 
#' of the column(s) `col` and  `group_by`. The summary must have the same 
#' parameters to operate. 
#'
#' @details
#' A dataset must be a data frame-like object and can be associated with a 
#' data dictionary. If no data dictionary is provided, a minimum workable 
#' data dictionary will be generated as needed by relevant functions. 
#' An identifier `id` column for sorting can be specified by the user. If 
#' specified, the `id` values must be non-missing and will be used in functions 
#' that require it. If no identifier column is specified, indexing is handled 
#' automatically by the function.
#' 
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
#' @param dataset A tibble identifying the input dataset observations 
#' associated to its data dictionary.
#' @param col A character string specifying the name of the column.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset. Automatically generated if not provided.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#' @param valueType_guess Whether the output should include a more accurate
#' valueType that could be applied to the dataset. FALSE by default.
#' @param .summary_var A summary list which is the summary of the variables.
#'
#' @seealso
#' [as.factor()]
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
#'  library(dplyr)
#'  dataset <- iris['Sepal.Width'] %>% slice(0)
#'  summary_variable <- dataset_summarize(dataset)
#'   
#'  variable_viz <-
#'    variable_visualize(
#'    dataset, col = 'Sepal.Width',
#'    .summary_var =  summary_variable)
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
    .summary_var = NULL){
  
  if(toString(col_id(dataset)) == col) {
    warning(call. = FALSE,'Your column is identifier. It will not be analysed.')
    return(ggplot())}
  
  dataset <- as_dataset(dataset)
  
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
  
  if(!is.null(data_dict)){
      tryCatch(
        expr = {
          col_dict <- 
            data_dict %>%
            data_dict_match_dataset(dataset = colset,output = 'data_dict') %>%
            as_data_dict_mlstr(name_standard = FALSE)
        },
        warning = function(cond){
          stop(cond)
        })
    
  }else{
    
    col_dict <- 
      data_dict_extract(colset,as_data_dict_mlstr = TRUE)
  }
  
  if(group_by != ''){
    
    preprocess_var <- 
      preprocess_group <- 
      dataset_preprocess(colset[c(col,group_by)], col_dict)
    
    preprocess_var <- preprocess_var[preprocess_var$name == col,] 
    preprocess_group <- preprocess_group[preprocess_group$name == group_by,] 
    
  } else {
    preprocess_var <- dataset_preprocess(colset[col], col_dict)
  }
  
  colset <- as_dataset(dataset_zap_data_dict(colset))

  if(group_by != ''){
    if(toString(unique(preprocess_group$`Categorical variable`)) %in% 
       c('mix','no'))
      stop(call. = FALSE,
           'Your grouping variable must be a categorical variable.')}
  
  preprocess_var_values <-
    preprocess_var[preprocess_var$valid_class == '3_Valid other values',]
  preprocess_var_cat_values <- 
    preprocess_var[preprocess_var$valid_class == '1_Valid values',]
  preprocess_var_cat_miss_values <- 
    preprocess_var[preprocess_var$valid_class %in% 
                     c('2_Missing values','4_NA values'),]
  
  if(is.null(.summary_var)){
    temp_group <- if(group_by == ''){NULL}else{group_by}
    .summary_var <- dataset_summarize(
      dataset = as_dataset(dataset[c(names(colset))]),
      data_dict = col_dict,
      group_by = temp_group, 
      .dataset_name = 'dataset_viz',
      valueType_guess = valueType_guess)}
  
  if(group_by != ''){
    
    cat_lab <- 
      col_dict[['Categories']] %>% 
      dplyr::filter(if_any('variable') == group_by) %>%
      select(
        !! group_by := 'name', 
        `___labels___` = matches(c("^label$","^label:[[:alnum:]]"))[1]) %>%
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
  
  colset_values <-
    colset %>% 
    mutate(temp_val = as.character(!! as.symbol(col))) %>%
    dplyr::filter(.data$`temp_val` %in% 
                    preprocess_var_values$value_var) %>%
    select(-'temp_val')
  
  colset_cat_values <-
    colset %>% 
    mutate(temp_val = as.character(!! as.symbol(col))) %>%
    dplyr::filter(.data$`temp_val` %in% 
                    preprocess_var_cat_values$value_var) %>%
    select(-'temp_val')
  
  colset_cat_miss_values <-
    colset  %>% 
    mutate(temp_val = as.character(!! as.symbol(col))) %>%
    dplyr::filter(.data$`temp_val` %in% 
                    preprocess_var_cat_miss_values$value_var) %>%
    select(-'temp_val') 
  
  # guess the generic valueType of the variable (excluding categories):

  vT_col <-   
  if(valueType_guess == TRUE){
      madshapR::valueType_list[
        madshapR::valueType_list$valueType %in% 
          valueType_guess(colset_values[[col]]),]    
  }else{

      madshapR::valueType_list[
        madshapR::valueType_list$valueType %in% 
          valueType_of(colset_values[[col]]),] 
  }
  
  n_part <- nrow(colset)
  summary_1 <- 
    as.data.frame(t(
      
      .summary_var$`Variables summary (all)` %>% 
        dplyr::filter(.data$`name` %in% col) %>%
        select(c("Total number of observations":last_col()))
    ))
  
  if(group_by != ''){
    names(summary_1) <- 
      
      unique(pull(
        .summary_var$`Variables summary (all)` %>%
          dplyr::filter(.data$`name` %in% col) %>%
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
  
  palette_NA <- "#afb1b2"
  palette_missing     <- 
    c("darkseagreen3", "lemonchiffon3","darksalmon","slategray3")
  palette_missing_fct <- colorRampPalette(palette_missing, 1)
  
  palette_missing <- 
    c(palette_NA, palette_missing,
      palette_missing[seq_len(length(unique(colset_cat_miss_values[[col]])))],
      sample(palette_mlstr_fct(
        length(unique(colset_cat_miss_values[[col]])) +
        length(palette_NA) +
        length(palette_missing)
      ))) %>%
    tolower() %>% unique
  
  palette_missing <- 
    palette_missing[(!palette_missing %in% c(palette_NA,NA))][
      seq_len(length(unique(colset_cat_miss_values[[col]])))]
  
  # a ameliorer
  # names(palette_missing) <- levels(colset_cat_miss_values[[col]])
  # palette_missing['NA'] <- palette_NA
  #FCDF5C
  palette_pie <- c()
  # palette_pie["Valid values"]       <- "#88C79A" # green
  palette_pie["Valid values"]       <- "#FCDF5C" # yellow
  palette_pie["Valid other values"] <- "#6581C0" # blue
  palette_pie["Missing values"]     <- "#EE7765" # red
  palette_pie["NA values"]          <- palette_NA # grey
  
  if(nrow(colset_values) > 0) {

    if(vT_col$`genericType` == "numeric"){

      #### summary_2 numeric ####
      summary_2 <- 
        as.data.frame(t(
          
          .summary_var$`Numerical variable summary` %>%
            dplyr::filter(.data$`name` %in% col) %>%
            select(-c(1:"% Missing categorical values (if applicable)"))
          
          ))
    
      if(group_by != ''){
        names(summary_2) <- 
          
          unique(pull(
            .summary_var$`Numerical variable summary` %>%
              dplyr::filter(.data$`name` %in% col) %>%
              select(starts_with('Grouping variable:'))
          ))
            
      } else { names(summary_2) <- col}
      
      summary_2 <-
        summary_2 %>% 
        mutate(col = row.names(summary_2)) %>%
        mutate(across(-c("col"), ~ round(.,2))) %>%
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
        scale_fill_manual(values = (palette_values))
      
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
      
      #### summary_2 character ####
      summary_2 <- 
        as.data.frame(t(
          
          .summary_var$`Text variable summary` %>%
            dplyr::filter(.data$`name` %in% col) %>%
            select(-c(1:"% Missing categorical values (if applicable)"))
          
        ))
      
      if(group_by != ''){
        names(summary_2) <- 
          
          unique(pull(
            .summary_var$`Text variable summary` %>%
              dplyr::filter(.data$`name` %in% col) %>%
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
      
      #### plot_1 character ####      
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
        theme(legend.position = "none",plot.title = 
                element_text(size = 8,face = "bold"),
              strip.background = element_rect(color = "white", fill="white")) +
        ggtitle(paste0('Most common entry', title)) +
        ylab("") +
        xlab("") +
        scale_fill_manual(values = rev(palette_values)) +
        coord_flip()
      
      if(group_by != '') {plot_1 <- plot_1 + facet_wrap(as.symbol(group_by))}
      
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
      
      plot_2 <- 
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
      
      if(group_by != '') {plot_2 <- plot_2 + facet_wrap(as.symbol(group_by))}
      
    }
    
    if(vT_col$`genericType` == "datetime"){
      
      #### summary_2 datetime ####
      summary_2 <- 
        as.data.frame(t(
          
          .summary_var$`Datetime variable summary` %>%
            dplyr::filter(.data$`name` %in% col) %>%
            select(-c(1:"% Missing categorical values (if applicable)"))
          
        ))
      
      if(group_by != ''){
        names(summary_2) <- 
          
          unique(pull(
            .summary_var$`Datetime variable summary` %>%
              dplyr::filter(.data$`name` %in% col) %>%
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
      
      #### summary_2 date ####    
      summary_2 <- 
        as.data.frame(t(
          
          .summary_var$`Date variable summary` %>%
            dplyr::filter(.data$`name` %in% col) %>%
            select(-c(1:"% Missing categorical values (if applicable)"))
          
        ))
      
      if(group_by != ''){
        names(summary_2) <- 
          
          unique(pull(
            .summary_var$`Date variable summary` %>%
              dplyr::filter(.data$`name` %in% col) %>%
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
    
    n_obs <- nrow(colset_cat_values)
    cat_lab_var <- 
      col_dict[['Categories']] %>% 
      dplyr::filter(if_any('variable') == col) %>%
      select(
        !!as.symbol(col) := 'name', 
        `___labels___` = 
          matches(c("^label$","^label:[[:alnum:]]"))[1]) %>%
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
      
      cat_lab_miss_var <- 
        col_dict[['Categories']] %>% 
        dplyr::filter(if_any('variable') == col) %>%
        select(
          !!as.symbol(col) := 'name', 
          `___labels___` = 
            matches(c("^label$","^label:[[:alnum:]]"))[1]) %>%
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
      scale_fill_manual(na.value = palette_NA, values = palette_missing) +
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
    mutate(across(col,as.character)) %>%
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
        c('Valid values','Valid other values','Missing values','NA values')))
  
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

  if(sum(nrow(.summary_var[['Categorical variable summary']])) > 0) {

    if(nrow(.summary_var$`Categorical variable summary` %>% 
            dplyr::filter(.data$`name` %in% col)) > 0){
      
      summary_categories <- 
        as.data.frame(t(
          
          .summary_var$`Categorical variable summary` %>%
            dplyr::filter(.data$`name` %in% col) %>%
            select(-c(1:"% Missing categorical values (if applicable)"))
          
        ))
      
      if(group_by != ''){
        names(summary_categories) <- 
          
          unique(pull(
            .summary_var$`Categorical variable summary` %>%
              dplyr::filter(.data$`name` %in% col) %>%
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
#' Generate a web-based bookdown visual report of a dataset
#'
#' @description
#' Generates a visual report for a dataset in an HTML bookdown document. The 
#' report provides figures and descriptive statistics for each variable to 
#' facilitate the assessment of input data. Statistics and figures are 
#' generated according to variable data type. The report can be used to help 
#' assess data structure, coherence across elements, and taxonomy or 
#' data dictionary formats. The summaries and figures provide additional 
#' information about variable distributions and descriptive statistics. 
#' The charts and tables are produced based on their data type. The variable 
#' can be grouped using `group_by` parameter, which is a (categorical) column 
#' in the dataset. The user may need to use [as.factor()] in this context. To 
#' fasten the process (and allow recycling object in a workflow) the user can 
#' feed the function with a `.summary_var`, which is the output of the function 
#' [dataset_summarize()] of the column(s) `col` and  `group_by`. The summary 
#' must have the same parameters to operate. 
#'
#' @details
#' A dataset must be a data frame-like object and can be associated with a 
#' data dictionary. If no data dictionary is provided, a minimum workable 
#' data dictionary will be generated as needed by relevant functions. 
#' An identifier `id` column for sorting can be specified by the user. If 
#' specified, the `id` values must be non-missing and will be used in functions 
#' that require it. If no identifier column is specified, indexing is handled 
#' automatically by the function.
#' 
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
#' @seealso
#' [bookdown_open()]
#'
#' @param dataset A tibble identifying the input dataset observations 
#' associated to its data dictionary.
#' @param bookdown_path A character string identifying the folder path where the 
#' bookdown report will be saved.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset. Automatically generated if not provided.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#' @param taxonomy A tibble identifying the scheme used for variables 
#' classification.
#' @param valueType_guess Whether the output should include a more accurate
#' valueType that could be applied to the dataset. FALSE by default.
#' @param render Output format of the visual report. To date, the format can 
#' only be 'html', but will be expand to other formats in the future.
#' @param .summary_var A list which is the summary of the variables.
#' @param .dataset_name A character string specifying the name of the dataset
#' (used for internal processes and programming).
#'
#' @returns
#' A bookdown folder containing files in the specified output folder. To
#' open the file in browser, open 'docs/index.html'. Or use 
#' [bookdown_open()]
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' library(fs)
#' dataset <- DEMO_files$dataset_TOKYO['height'] %>% slice(0)
#' data_dict <-
#'   data_dict_filter(
#'     DEMO_files$dd_TOKYO_format_maelstrom,
#'     filter_var = "name == 'height'")
#' 
#' .summary_var <- DEMO_files$summary_var
#' 
#' if(dir_exists(tempdir())) dir_delete(tempdir())
#' bookdown_path <- tempdir()
#'   
#' dataset_visualize(
#'   dataset, data_dict,
#'   .summary_var =.summary_var,
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
    data_dict = data_dict_extract(dataset),
    group_by = NULL,
    taxonomy = NULL,
    valueType_guess = FALSE,
    render = 'html',
    .summary_var = NULL, 
    .dataset_name = NULL){
  
  fargs <- as.list(match.call(expand.dots = TRUE))
  
  # future dev
  # mutate(key = paste0('<b>' , key, '</b>')),
  # @param toc xxx xxx xxx
  # toc <- 'variables'
  
  # check input
  render <- 'html'
  
  if(!is.logical(valueType_guess))
    stop(call. = FALSE,'`valueType_guess` must be TRUE or FALSE (TRUE by default)')
  
  if(!is.character(bookdown_path))
    stop(call. = FALSE,'`bookdown_path` must be a character string.')
  
  if(!is.character(render))
    stop(call. = FALSE,'`render` must be a character string.')
  
  bookdown_path <- str_squish(bookdown_path)
  path_to <- path_abs(bookdown_path)
  
  if(dir_exists(path_to)){stop(call. = FALSE,
"The path folder already exists. 
Please provide another name folder or delete the existing one.")}
  
  dataset <- as_dataset(dataset, col_id(dataset))
  col_id <- col_id(dataset)
  
  if(toString(substitute(group_by)) == '') group_by <- NULL
  # attempt to catch group_by from the group_vars if the dataset is grouped
  if(length(group_vars(dataset)) == 1 & toString(substitute(group_by)) == ''){
    group_by <- group_vars(dataset)
  }
  
  dataset <- as_dataset(ungroup(dataset),col_id)
  
  dataset_name <- 
    suppressWarnings(
    ifelse(
      !is.null(.dataset_name),
      .dataset_name,
      make_name_list(as.character(fargs[['dataset']]),
                           list_elem = list(NULL))))
  
  # check on argument : taxonomy
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  
  # attempt to catch group_by
  if(toString(substitute(group_by)) != ''){
    group_by <- tryCatch(
      expr  = {toString(names(dataset[toString(substitute(group_by))]))},
      error = function(cond){return(toString(names(dataset[group_by])))})    
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
        as_data_dict_mlstr(name_standard = FALSE)})
  
  # summarize initial information
  
  if(is.null(.summary_var)){
    temp_group <- if(group_by == ''){NULL}else{group_by}
    .summary_var <- dataset_summarize(
      dataset = dataset,
      data_dict = data_dict,
      group_by = temp_group,
      taxonomy = taxonomy,
      .dataset_name = dataset_name,
      valueType_guess = valueType_guess)}

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

  data_dict_flat <- 
    suppressWarnings(data_dict_collapse(data_dict_flat)[[1]]) %>%
    bind_rows(tibble("Categories::label:zzz" = as.character())) %>%
    select(
      "index in data dict." = matches("index"),
      "name",
      matches(c("^label$","^label:[[:alnum:]]"))[1],
      matches('valueType'),
      Categories = matches(c("^Categories::labels$","^Categories::label$",
                             "^Categories::label:[[:alnum:]]"))[1]) %>% 
    mutate(Categories = str_replace_all(.data$`Categories`,"; \n","<br>")) %>%
    mutate(Categories = str_replace_all(
      .data$`Categories`,"\\[\\.\\.\\.\\] = \\[\\.\\.\\.\\]","[...]"))
    

  bookdown_template(path_to, overwrite = FALSE)
  if(!dir.exists(paste0(path_to,"/src"))) dir.create(paste0(path_to,"/src"))
  save(
    path_to,dataset, data_dict, group_by,data_dict_flat, .summary_var,col_id,
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
      after: |
        <li><a href="https://maelstrom-research.org" target="blank">Generated by Maelstrom</a></li>

') %>% write_lines(file = paste0(path_to,"/_output.yml"),append = FALSE)
  
  paste0(
    '# About the dataset {.unnumbered #about}

```{r echo = FALSE, message = FALSE, warning = FALSE}

library(fabR)
library(madshapR)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)

load(file = paste0("', path_to,'/src/r_env.RData"))

```
--------------------------------------------------------------------------------


## Overview

```{r echo = FALSE, message = FALSE, warning = FALSE}

datatable(.summary_var$Overview, colnames = rep("",ncol(.summary_var$Overview)),
    options = list(pageLength = nrow(.summary_var$Overview),scrollX = TRUE),
    rownames = FALSE,escape = FALSE)

```

--------------------------------------------------------------------------------


## Variables summary


```{r echo = FALSE, message = FALSE, warning = FALSE}


# if(toc == "variables"){

  datatable(
    data_dict_flat %>%
      mutate(
        name = ifelse(name %in% col_id, name, paste0(
        "<a href=\\"./var",`index in data dict.`,".html\\" >",name,"</a>"))),
    options=list(scrollX = TRUE,pageLength=20),rownames = FALSE,escape = FALSE)

# }else{
# 
#   datatable(
#     data_dict_flat,
#     options = 
#       list(scrollX = TRUE, pageLength=20),rownames = FALSE,escape = FALSE)
# 
# }

```

--------------------------------------------------------------------------------

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
      "# ",data_dict$Variables$name[i],"{.unnumbered #var",i,"}\n\n") %>%
      
      
      paste0("\n**VARIABLE CHARACTERISTICS**\n") %>%
      
      
      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0(
        "\n```{r ",
        str_squish(
          "echo = FALSE,message = FALSE,warning = FALSE,knitr.figure = TRUE}"),
        "\n

  datatable(t(
     data_dict$Variables %>%
     dplyr::filter(name == '",data_dict$Variables$name[i],"')),
   options = list(dom = 't', scrollX = TRUE, ordering = FALSE,paging = FALSE),
   rownames = TRUE, colnames = rep('', 2),filter = 'none' ,  escape = FALSE)",
        
        "\n```\n") %>%
      
      paste0("\n</div>\n\n") %>%
      paste0(ifelse(
        sum(nrow(
          data_dict[['Categories']][data_dict[['Categories']][['variable']] == 
                                      data_dict$Variables$name[i],])) > 0,
        paste0("\n* **Categories**: ","\n\n") %>%
          paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
          paste0(
            "\n```{r echo = FALSE, message = FALSE, warning = FALSE}",
            "\n
                   
  datatable(
    data_dict$Categories %>% 
      dplyr::filter(variable == '",data_dict$Variables$name[i],"') %>%
    select(variable, name, 
    matches(c('^label$','^label:[[:alnum:]]'))[1], missing) %>%
    mutate(across(everything(), as.character)),
    options = list(scrollX = TRUE),rownames = FALSE)                          ",
                        
                        "\n```\n") %>%
                      paste0("\n</div>\n\n") ,"")) %>%
      paste0("
\n----------------------------------------------------------------------\n") %>%
      
      
      paste0("\n**SUMMARY STATISTICS**\n") %>%
      
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
  .summary_var = .summary_var)       
        
  if(!is.null(plots$summary_table))      plots$summary_table                  ",
        
        "\n```\n") %>%
      
      paste0("\n</div>\n\n") %>%
      
      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0(
        "\n```{r ",
        str_squish(
          "echo = FALSE,message = FALSE,warning = FALSE,knitr.figure = TRUE}"),
        "\n
        
  if(!is.null(plots$summary_categories)) plots$summary_categories             ",
        
        "\n```\n") %>%
      
      paste0("\n</div>\n\n") %>%
      
      paste0(
"\n---------------------------------------------------------------------\n") %>%
      
      paste0("\n**VISUAL REPRESENTATION**\n") %>%
      
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
        
        "\n```\n") %>%
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

