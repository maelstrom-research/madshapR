#' @title
#' Generate a combined plot of a variable in a dataset
#'
#' @description
#' xxx xxx xxx
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
#' @param dataset A tibble identifying the input data observations associated to
#' its data dictionary.
#' @param col A character string specifying the name of the column.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset. Automatically generated if not provided.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#' @param summary_var A summary list which provides summary of the variables (
#' If provided, the function will not do this action internally, enhancing the
#' speed of completion)
#'
#' @seealso
#' [madshapR::open_visual_report()]
#'
#' @return
#' xxx xxx xxx
#'
#' @examples
#' {
#' 
#' variable_visualize(dataset = iris, col = Petal.Length, group_by = Species)
#'
#' }
#'
#' @import dplyr fabR 
#' @import ggplot2 tidytext janitor
#' @importFrom forcats fct_rev
#' @importFrom rlang .data
#' @importFrom rlang :=
#'
#' @export
variable_visualize <- function(
    dataset = tibble(id = as.character()),
    col,
    data_dict = NULL, 
    group_by = NULL,
    summary_var = NULL){
  
  dataset <- as_dataset(dataset)
  
  if(nrow(dataset) == 0) {
    warning(call. = FALSE,'Your column has no observation.')
    return(ggplot())}
  
  if(toString(substitute(group_by)) == '') group_by = NULL
  
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
  
  if(ncol(colset)== 1){col = names(colset)[1] ; group_by <- ''}
  if(ncol(colset)== 2){col = names(colset)[1] ; group_by = names(colset)[2]}
  
  if(!is.null(data_dict)){
    col_dict <- 
      data_dict %>%
      data_dict_match_dataset(data = colset,output = 'data_dict') %>%
      as_mlstr_data_dict()
  }else{
    col_dict <- 
      data_dict_extract(colset,as_mlstr_data_dict = TRUE)
  }
  
  if(group_by != ''){
    
    resume_var <- 
      resume_group <- 
      resume_variables(colset[c(col,group_by)], col_dict)
    
    resume_var <- resume_var[resume_var$name == col,] 
    resume_group <- resume_group[resume_group$name == group_by,] 
    
  } else {
    resume_var <- resume_variables(colset[col], col_dict)
  }
  
  colset <- as_dataset(dataset_zap_data_dict(colset))
    
  if(group_by != ''){
    if(toString(unique(resume_group$`Categorical variable`)) %in% c('mix','no'))
      stop(call. = FALSE,
           'Your grouping variable must be a categorical variable.')}
  
  resume_var_values <-
    resume_var[resume_var$valid_class == '3_Valid other values',]
  resume_var_cat_values <- 
    resume_var[resume_var$valid_class == '1_Valid values',]
  resume_var_cat_miss_values <- 
    resume_var[resume_var$valid_class %in% c('2_Missing values','4_NA values'),]
  
  if(is.null(summary_var)){
    temp_group <- if(group_by == ''){NULL}else{group_by}
    summary_var <- dataset_summarize(
      dataset = as_dataset(dataset[c(names(colset))]),
      data_dict = col_dict,
      group_by = temp_group, 
      .dataset_name = 'dataset_viz',
      valueType_guess = TRUE)}
  
  if(group_by != ''){
    
    cat_lab = 
      col_dict[['Categories']] %>% 
      filter(if_any('variable') == group_by) %>%
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
    filter(!! as.symbol(col) %in% resume_var_values$value_var)
  
  colset_cat_values <-
    colset %>% 
    filter(!! as.symbol(col) %in% resume_var_cat_values$value_var) 
  
  colset_cat_miss_values <-
    colset %>% 
    filter(!! as.symbol(col) %in% resume_var_cat_miss_values$value_var) 
  
  # guess the generic valueType of the variable (excluding categories):
  vT_col <- 
    madshapR::valueType_list[
      madshapR::valueType_list$valueType %in% 
        valueType_guess(colset_values[[col]]),]
  
  n_part <- nrow(colset)
  
  summary_1 <- 
    as.data.frame(t(
      
      summary_var$`Variables summary (all)` %>%
        filter(.data$name %in% col) %>%
        select(c("Total number of observations":last_col()))
      
    ))
  
  if(group_by != ''){
    names(summary_1) <- 
      
      unique(pull(
        summary_var$`Variables summary (all)` %>%
          filter(.data$name %in% col) %>%
          select(starts_with('Grouping variable:'))
      ))
    
  } else { names(summary_1) <- col}
  
  summary_1 <-
    summary_1 %>% 
    mutate(col = row.names(summary_1)) %>%
    mutate(across(-c("col"), 
                  ~ ifelse(. == 0,NA_real_,.))) %>%
    mutate(across(-c("col"), 
                  ~ ifelse(str_detect(.data$col,'% '),round(.*100,2),round(.)))) %>%
    select(-'col') %>%
    mutate(across(everything(),as.character))
    
  if(nrow(colset_values) > 0) {
    
    if(vT_col$`genericType` == "numeric"){
      
      summary_2 <- 
        as.data.frame(t(
          
          summary_var$`Numerical variable summary` %>%
            filter(.data$name %in% col) %>%
            select(-c(1:"% Missing categorical values (if applicable)"))
          
        ))
      
      if(group_by != ''){
        names(summary_2) <- 
          
          unique(pull(
            summary_var$`Numerical variable summary` %>%
              filter(.data$name %in% col) %>%
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
      
      # Plot 1 : aes grouped or not
      
      n_obs <- nrow(colset_values)
      
      title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
      if(group_by != '') title <- paste0(title, ' - per ',group_by)
      
      aes <- 
        if(group_by == ''){
          aes(x = '',
              y = !! as.symbol(col),
              fill = '')
        }else{
          aes(x = forcats::fct_rev(!! as.symbol(group_by)), 
              y = !! as.symbol(col),
              fill =  !! as.symbol(group_by))}
        
      plot_1 <-
        ggplot(colset_values) + aes +
        geom_boxplot(outlier.color = 'red') +
        # scale_fill_viridis(discrete = TRUE, alpha=0.6) +
        # theme_ipsum() +
        coord_flip() +
        theme(legend.position="none",plot.title = element_text(size=8)) +
        ggtitle(paste0('Box plot', title)) +
        ylab("") +
        xlab("")
      
      # Plot 2 : aes grouped or not
      aes <- 
        if(group_by == ''){
          aes(x     = !! as.symbol(col),
              group = '',
              fill  = ''
          )
        }else{
          aes(x     = !! as.symbol(col),
              group = !! as.symbol(group_by),
              fill  = !! as.symbol(group_by))}
      
      plot_2 <- 
        ggplot(colset_values) + aes +
        geom_density(color="black",na.rm = FALSE) +
        # theme_ipsum() +
        ggtitle(paste0('Density plot', title)) +
        theme(legend.position="none",plot.title = element_text(size=8)) +
        ylab("") +
        xlab("")
      
      if(group_by != '') { 
        plot_2 <- plot_2 + facet_wrap(group_by)}
      
    }
    
    if(vT_col$`genericType` == "character"){
      
      summary_2 <- 
        as.data.frame(t(
          
          summary_var$`Text variable summary` %>%
            filter(.data$name %in% col) %>%
            select(-c(1:"% Missing categorical values (if applicable)"))
          
        ))
      
      if(group_by != ''){
        names(summary_2) <- 
          
          unique(pull(
            summary_var$`Text variable summary` %>%
              filter(.data$name %in% col) %>%
              select(starts_with('Grouping variable:'))
          ))
        
      } else { names(summary_2) <- col}
      
      summary_2 <-
        summary_2 %>% 
        mutate(col = row.names(summary_2)) %>%
        mutate(across(-c("col"), ~ str_trunc(.,width = 39,ellipsis = ' [...]'))) %>%
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
      
      # Plot 1 : aes grouped or not
      
      n_obs <- nrow(colset_values)
      
      title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
      if(group_by != '') title <- paste0(title, ' - per ',group_by)
      
      group_n <- "___n___"
      aes <- aes(!! as.symbol(col), !! as.symbol(group_n))
      
      plot_1 <- 
        ggplot(colset_values_main_word) + aes + 
        geom_col() +
        coord_flip() +
        theme(legend.position="none",plot.title = element_text(size=8)) +
        ggtitle(paste0('Most common entry', title)) +
        ylab("") +
        xlab("")
      
      if(group_by != '') { 
        plot_1 <- plot_1 + facet_wrap(group_by)}
      
      # Plot 2 : aes grouped or not
      n_obs <- nrow(colset_values)
      
      title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
      if(group_by != '') title <- paste0(title, ' - per ',group_by)
      
      group_n <- "___n___"
      aes <- aes(!! as.symbol(col), !! as.symbol(group_n))
      
      plot_2 <- 
        ggplot(colset_values_all_word) + aes + 
        geom_col() +
        coord_flip() +
        theme(legend.position="none",plot.title = element_text(size=8)) +
        ggtitle(paste0('Box plot', title)) +
        ylab("") +
        xlab("")
      
      if(group_by != '') { 
        plot_2 <- plot_2 + facet_wrap(group_by)}
      
    }
    
    if(vT_col$`genericType` == "date"){
      
      summary_2 <- 
        as.data.frame(t(
          
          summary_var$`Date variable summary` %>%
            filter(.data$name %in% col) %>%
            select(-c(1:"% Missing categorical values (if applicable)"))
          
        ))
      
      if(group_by != ''){
        names(summary_2) <- 
          
          unique(pull(
            summary_var$`Date variable summary` %>%
              filter(.data$name %in% col) %>%
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
      
      # convert data to wide format
      colset_span <- 
        colset_values %>%
        filter(if_any(col) == min(!! as.symbol(col)) | 
                 if_any(col) == max(!! as.symbol(col))) 
      
      
      # create dumbbell plot
      n_obs <- nrow(colset_values)
      
      title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
      if(group_by != '') title <- paste0(title, ' - per ',group_by)
      
      aes <-
        if(group_by == ''){
          aes(x = !! as.symbol(col),y = '')
        }else{
          aes(x = !! as.symbol(col),
              y = forcats::fct_rev(!! as.symbol(group_by)),
              color = !! as.symbol(group_by))}
      plot_1 <- 
        ggplot(colset_span) + aes +
        geom_line() +
        geom_point(size = 3) +
        # theme_ipsum() +
        ggtitle(paste0('Span date', title)) +
        theme(legend.position="none",plot.title = element_text(size=8)) +
        ylab("") +
        xlab("")
      
      
      # plot 2
      n_obs <- nrow(colset_values)
      
      title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
      if(group_by != '') title <- paste0(title, ' - per ',group_by)
      
      aes <-
        if(group_by == ''){
          aes(x = !! as.symbol(col),
              fill = "green")
        }else{
          aes(
            x = !! as.symbol(col),
            fill = !! as.symbol(group_by))}
      
      max_span = 
        max(ungroup(colset_span) %>%
              pull(!! as.symbol(col))) -
        min(ungroup(colset_span) %>%
              pull(!! as.symbol(col))) + 1
      
      bins = ceiling(as.integer(max_span) / 365 / 5)
      
      plot_2 <- 
        ggplot(colset_values) + aes +
        geom_histogram(bins = bins) +
        theme_minimal() +
        ggtitle(paste0('Span date ', title)) +
        theme(legend.position="none",plot.title = element_text(size=8)) +
        ylab("") +
        xlab("")
      
      if(group_by != '') { 
        plot_2 <- plot_2 + facet_wrap(group_by)}
    }
    
  }else{plot_1 <- plot_2 <- summary_2 <- NULL}

  # categorical part of variable
  plot_3 <- NULL

  if(nrow(colset_cat_values) > 0){
    
    n_obs <- nrow(colset_cat_values)
    
    cat_lab_var = 
      col_dict[['Categories']] %>% 
      filter(if_any('variable') == col) %>%
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
                 paste0(' [',str_trunc(.data$`___labels___`,width = 19,ellipsis = '...'),']'))) %>%
      unite(!! col,c(col,'___labels___'),sep = '', remove = TRUE,na.rm = TRUE) %>%
      mutate(across(all_of(col), ~ na_if(.,'')))
    
    cat_var_levels <- 
      colset_cat_values %>% 
      arrange(.data$`___category_level___`) %>%
      filter(!is.na(!!as.symbol(col))) %>%
      pull(col) %>% unique 
    
    if(length(cat_var_levels) == 0) cat_var_levels <- 0
    
    colset_cat_values <- 
      colset_cat_values %>% 
      mutate(across(!! as.symbol(col), ~ factor(.,levels=c(cat_var_levels)))) %>%
      select(-'___category_level___')
    
    title <- paste0(' representation of categorical values in ',col,' (N obs. : ',n_obs,')')
    if(group_by != '') title <- paste0(title, ' - per ',group_by)
    
    if(all(is.na(pull(colset_cat_values[col])))){
      aes <- aes(x = forcats::fct_rev(!! as.symbol(col))) 
    }else{
      aes <- aes(x = forcats::fct_rev(!! as.symbol(col)), 
                 fill =  !! as.symbol(col))
    }
    
    plot_3 <- 
      ggplot(colset_cat_values) + aes +
      geom_bar() +
      coord_flip() +
      theme(legend.position="none",plot.title = element_text(size=8)) +
      ggtitle(paste0('Box plot', title)) +
      ylab("") +
      xlab("")
    
    if(group_by != '') { 
      plot_3 <- plot_3 + facet_wrap(group_by)}
  }

  plot_4 <- NULL
  if(nrow(colset_cat_miss_values) > 0){
    n_obs <- nrow(colset_cat_miss_values)
    
    cat_lab_miss_var = 
      col_dict[['Categories']] %>% 
      filter(if_any('variable') == col) %>%
      select(
        !!as.symbol(col) := 'name', 
        `___labels___` = 
          matches(c("^label$","^label:[[:alnum:]]"))[1]) %>%
      mutate(!! as.symbol(col) := as.character(!!as.symbol(col))) %>%
      add_index('___category_level___')
    
    colset_cat_miss_values <-  
      colset_cat_miss_values %>%
      mutate(!! as.symbol(col) := as.character(!!as.symbol(col))) %>%
      left_join(cat_lab_miss_var,by = col) %>%
      mutate(
        `___labels___` = 
          ifelse(!! as.symbol(col) == .data$`___labels___`,'',
                 paste0(' [',str_trunc(.data$`___labels___`,width = 19,ellipsis = '...'),']'))) %>%
      unite(!! col,c(any_of(col),'___labels___'),sep = '', remove = TRUE,na.rm = TRUE) %>%
      mutate(across(all_of(col), ~ na_if(.,'')))
    
    cat_var_levels <- 
      colset_cat_miss_values %>% 
      arrange(.data$`___category_level___`) %>%
      filter(!is.na(!!as.symbol(col))) %>%
      pull(col) %>% unique 
    
    if(length(cat_var_levels) == 0) cat_var_levels <- 0
    
    colset_cat_miss_values <- 
      colset_cat_miss_values %>% 
      mutate(across(!! as.symbol(col), ~factor(.,levels=c(cat_var_levels)))) %>%
      select(-'___category_level___')
    
    title <- paste0(' representation of missing categorical values in ',col,' (N obs. : ',n_obs,')')
    if(group_by != '') title <- paste0(title, ' - per ',group_by)
    
    if(all(is.na(pull(colset_cat_miss_values[col])))){
      aes <- aes(x = forcats::fct_rev(!! as.symbol(col)))
    }else{
      aes <- aes(x = forcats::fct_rev(!! as.symbol(col)), fill =  !! as.symbol(col))    
    }
    
    plot_4 <- 
      ggplot(colset_cat_miss_values) + aes +
      geom_bar() +
      coord_flip() +
      theme(legend.position="none",plot.title = element_text(size=8)) +
      ggtitle(paste0('Box plot', title)) +
      ylab("") +
      xlab("")
    
    if(group_by != '') { 
      plot_4 <- plot_4 + facet_wrap(group_by)}
  }

  # categorization of variable (valid/missing/others/NA)
  resume_var <-
    resume_var %>%
    select('valid_class', 'value_var') %>% 
    rename_with(.cols = 'valid_class', ~ '___valid_class___') %>%
    rename_with(.cols = 'value_var', ~ col) %>%
    distinct
  
  colset_valid <-
    colset %>%
    mutate(across(col,as.character)) %>%
    left_join(resume_var,by = intersect(names(colset), names(resume_var))) %>%
    select(- !! col) %>%
    mutate(`___valid_class___` = str_sub(.data$`___valid_class___`,3)) %>%
    rename_with(.cols = '___valid_class___', ~ col) %>% 
    group_by(across(everything())) %>%
    tally %>%
    rename(`___n___` = last_col())
  
  plot_5 <- NULL
  
  if(length(unique(colset_valid[[col]])) > 1){
    
    n_obs <- nrow(colset)
    title <- paste0(' representation of validity values distribution in ',col,' (N obs. : ',n_obs,')')
    if(group_by != '') title <- paste0(title, ' - per ',group_by)
    
    group_n <- "___n___"
    aes <- aes(x = '',y = !! as.symbol(group_n), fill = !! as.symbol(col))
    
    plot_5 <-
      ggplot(colset_valid) + aes +
      geom_bar(stat='identity',width = 1,position = position_fill()) +
      coord_polar('y', start=0) + 
      theme_void() + 
      theme(legend.position="none",plot.title = element_text(size=8)) +
      ggtitle(paste0('Box plot', title)) +
      theme(legend.position = 'right') 
    
    if(group_by != '') {
      plot_5 <- plot_5 + facet_wrap(group_by)}
  }
  
  if(is.null(plot_1)){plot_1 <- plot_3 ; plot_3 <- NULL} 
  if(is.null(plot_2)){plot_2 <- plot_4 ; plot_4 <- NULL}
  
  # category table

  if(sum(nrow(summary_var[['Categorical variable summary']])) > 0) {

    if(nrow(summary_var$`Categorical variable summary` %>% 
            filter(.data$name %in% col)) > 0){
      
      summary_categories <- 
        as.data.frame(t(
          
          summary_var$`Categorical variable summary` %>%
            filter(.data$name %in% col) %>%
            select(-c(1:"% Missing categorical values (if applicable)"))
          
        ))
      
      if(group_by != ''){
        names(summary_categories) <- 
          
          unique(pull(
            summary_var$`Categorical variable summary` %>%
              filter(.data$name %in% col) %>%
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
#' Generate a web application (bookdown) report of list of a datasets
#'
#' @description
#' Generates a visual report for a dataset in an HTML
#' bookdown document, showing descriptive statistics for each 
#' variable to facilitate the assessment of input data. Statistics and figures
#' are generated according to their valueType.
#' This report can be used to assist the user in the assessment of the data
#' structure, fields investigation (mandatory or not), coherence across elements
#' and taxonomy or standard evaluation. The summaries and figures associated
#' provide dataset composition, with observation distribution and descriptive
#' statistics.
#'
#' @details
#' A dossier must be a named list containing at least one data frame or
#' data frame extension (e.g. a tibble), each of them being datasets.
#' The name of each tibble will be use as the reference name of the dataset.
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
#' @seealso
#' [madshapR::open_visual_report()]
#'
#' @param dataset A tibble identifying the input data observations associated to
#' its data dictionary.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset. Automatically generated if not provided.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#' @param to A character string identifying the folder path where the bookdown
#' report will be saved.
#' @param taxonomy A data frame or data frame extension (e.g. a tibble),
#' identifying the scheme used for variables classification as a tibble.
#' @param .keep_files whether to keep the R-markdown files.
#' TRUE by default.
#' @param .dataset_name A character string specifying the name of the dataset
#' (internally used in the function `madshapR::dataset_summarise()`).
#' @param summary_var A summary list which provides summary of the variables (
#' If provided, the function will not do this action internally, enhancing the
#' speed of completion)
#'
#' @return
#' A bookdown folder containing files in the specified output folder. To
#' open the file in browser, open 'index.html'. Or use
#' [madshapR::open_visual_report()]
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' library(dplyr)
#'
#' ###### Example 1: any data-frame (or tibble) can be a dataset by definition.
#' tempdir <- tempdir()
#' dataset_visualize(iris, to = tempdir,.keep_files = TRUE, group_by = Species)
#' open_visual_report(tempdir)
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
    data_dict = data_dict_extract(dataset),
    group_by = NULL,
    to,
    taxonomy = NULL,
    .dataset_name = NULL,
    .keep_files = TRUE,
    summary_var = NULL){
  
  fargs <- as.list(match.call(expand.dots = TRUE))
  
  # future dev
  # mutate(key = paste0('<b>' , key, '</b>')),
  
  # check input
  if(!is.logical(.keep_files))
    stop(call. = FALSE,'`.keep_files` must be TRUE or FALSE (TRUE by default)')
  
  to <- str_squish(to)
  dataset <- as_dataset(dataset, attributes(dataset)$`Mlstr::col_id`)
  col_id <- attributes(dataset)$`Mlstr::col_id`
  
  dataset_name <- 
    ifelse(
      !is.null(.dataset_name),
      .dataset_name,
      fabR::make_name_list(
        as.character(fargs[['dataset']]),list_elem = list(NULL)))
  
  # check on argument : taxonomy
  if(!is.null(taxonomy)) as_taxonomy(taxonomy)
  
  # attempt to catch group_by
  if(toString(substitute(group_by)) != ''){
    group_by <- tryCatch(
      expr  = {toString(names(dataset[toString(substitute(group_by))]))},
      error = function(cond){return(toString(names(dataset[group_by])))})    
  }else{ group_by = ''}
  
  dataset <-
    data_dict_match_dataset(
      dataset,data_dict,
      output = 'data') %>%
    as_dataset(attributes(dataset)$`Mlstr::col_id`)
  
  data_dict <- 
    data_dict_match_dataset(
      dataset,data_dict,
      output = 'data_dict') %>%
    as_mlstr_data_dict()
  
  # summarize initial information
  
  if(is.null(summary_var)){
    temp_group <- if(group_by == ''){NULL}else{group_by}
    summary_var <- dataset_summarize(
      dataset = dataset,
      data_dict = data_dict,
      group_by = temp_group,
      taxonomy = taxonomy,
      .dataset_name = dataset_name,
      valueType_guess = TRUE)}
  
  data_dict$Variables <- data_dict$Variables %>% add_index(.force = TRUE)
  
  data_dict_flat <-
    data_dict_flatten(data_dict)[[1]] %>%
    select(
      "index in data dict." = matches("index"),
      "name",
      matches(c("^label$","^label:[[:alnum:]]"))[1],
      matches('valueType'),
      Categories = matches(c("^Categories::label$","^Categories::label:[[:alnum:]]"))[1])
  
  path_to <- fs::path_abs(to)
  fabR::template_visual_report(path_to)
  save(path_to,dataset, data_dict, group_by,data_dict_flat, summary_var, col_id,
       file = paste0(path_to,"/temp_bookdown_report/bookdown_report.RData"))
  
  ## markdown writing elements
  
  ##### HEADER index ##########
  paste0(
    '---
title: ',dataset_name,'
date: "`r Sys.Date()`"
site: bookdown::bookdown_site
---

') %>% write_lines(
  file = paste0(path_to,
                "/temp_bookdown_report/file/bookdown-template-master/index.Rmd"),
  append = FALSE)
  
  
  ##### _bookdown.yml ##########
  paste0(
    'book_filename: "bookdownproj"
output_dir: docs
delete_merged_file: false
language:
  ui:
    chapter_name: ""

') %>% write_lines(
  file = paste0(path_to,
                "/temp_bookdown_report/file/bookdown-template-master/_bookdown.yml"),
  append = FALSE)
  
  
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

') %>% write_lines(
  file = paste0(path_to,
                "/temp_bookdown_report/file/bookdown-template-master/_output.yml"),
  append = FALSE)
  
  paste0(
    '# About the dossier dataset {.unnumbered #about}

```{r echo = FALSE, message = FALSE, warning = FALSE}

library(fabR)
library(madshapR)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)

load(file = paste0("', path_to,'/temp_bookdown_report/bookdown_report.RData"))

```
--------------------------------------------------------------------------------


## Overview

```{r echo = FALSE, message = FALSE, warning = FALSE}

datatable(summary_var$Overview,  colnames = rep("",ncol(summary_var$Overview)),
    options = list(pageLength = nrow(summary_var$Overview)),
    rownames = FALSE,escape = FALSE)

```

--------------------------------------------------------------------------------


## Variables summary


```{r echo = FALSE, message = FALSE, warning = FALSE}

datatable(
  data_dict_flat %>%
    mutate(
    name = ifelse(name %in% col_id, name, paste0(
       "<a href=\\"./var",`index in data dict.`,".html\\" >",name,"</a>")),
    Categories = str_replace_all(Categories,"; \n","<br>")),
  options = list(scrollX = TRUE, pageLength=20),rownames = FALSE,escape = FALSE)
  

```

--------------------------------------------------------------------------------

') %>% write_lines(
  file =
    paste0(path_to,
           "/temp_bookdown_report/file/bookdown-template-master/index.Rmd"),
  append = TRUE)
  
  
  ##### CONTENT ##########
  
  increment <-
    paste0(rep(0,nchar(nrow(data_dict$Variables))) %>% paste(collapse = ""))
  
  for(i in seq_len(nrow(data_dict$Variables))){
    # stop()}
    
    rmd_file_name <-
      paste0(path_to,"/temp_bookdown_report/file/bookdown-template-master/",
             str_sub(paste0(increment,i),-(increment %>% nchar + 1),-1),"-",
             data_dict$Variables$name[i],".Rmd")
    file.create(rmd_file_name)
    
    paste0(
      "# ",data_dict$Variables$name[i],"{.unnumbered #var",i,"}\n\n") %>%
      
      
      paste0("\n**VARIABLE CHARACTERISTICS**\n") %>%
      
      
      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0(
        "\n```{r ",
        str_squish("echo = FALSE,message = FALSE,warning = FALSE,knitr.figure = TRUE}"),
        "\n

  datatable(t(
     data_dict$Variables %>%
     filter(name == '",data_dict$Variables$name[i],"')),
   options = list(dom = 't', scrollX = TRUE, ordering = FALSE,paging = TRUE),
   rownames = TRUE, colnames = rep('', 2),filter = 'none' ,  escape = FALSE)",
        
        "\n```\n") %>%
      
      paste0("\n</div>\n\n") %>%
      paste0(ifelse(nrow(data_dict[['Categories']] %>%
                           filter(.data$`variable` == data_dict$Variables$name[i])) > 0,
                    paste0("\n* **Categories**: ","\n\n") %>%
                      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n")%>%
                      paste0(
                        "\n```{r echo = FALSE, message = FALSE, warning = FALSE}",
                        "\n
                   
  datatable(
    data_dict$Categories %>% 
      filter(variable == '",data_dict$Variables$name[i],"') %>%
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
        str_squish("echo = FALSE,message = FALSE,warning = FALSE,knitr.figure = TRUE}"),
        "\n
        
 plots <- variable_visualize(
  dataset,
  col = '", names(dataset[i]),"',
  data_dict = data_dict, 
  group_by = '", group_by, "',
  summary_var = summary_var)       
        
  if(!is.null(plots$summary_table))      plots$summary_table                  ",
        
        "\n```\n") %>%
      
      paste0("\n</div>\n\n") %>%
      
      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0(
        "\n```{r ",
        str_squish("echo = FALSE,message = FALSE,warning = FALSE,knitr.figure = TRUE}"),
        "\n
        
  if(!is.null(plots$summary_categories)) plots$summary_categories             ",
        
        "\n```\n") %>%
      
      paste0("\n</div>\n\n") %>%
      
      paste0(
        "\n---------------------------------------------------------------------\n") %>%
      
      paste0("\n**VISUAL REPRESENTATION**\n") %>%
      
      paste0(
        "\n```{r, figures-plot12-",i,
        str_squish(", fig.show='hold',fig.align = 'center',echo = FALSE,message = FALSE,
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
  
  fabR::silently_run(
    
    xfun::in_dir(
      dir = paste0(
        path_to,"/temp_bookdown_report/file/bookdown-template-master/"),
      exp = file.remove(
        list.files(
          paste0(
            path_to,"/temp_bookdown_report/file/bookdown-template-master/")) %>%
          str_subset(paste0(
            "^[[:digit:]]+-",
            toString(as.character(attributes(dataset)$`Mlstr::col_id`)),
            ".Rmd$"))))
  )
  
  # invisible(dev.set(dev.next()))
  # invisible(grDevices::graphics.off())
  
  xfun::in_dir(
    dir = paste0(
      path_to,"/temp_bookdown_report/file/bookdown-template-master/"),
    expr = render_book(
      input = paste0(
        path_to,
        "/temp_bookdown_report/file/bookdown-template-master/index.Rmd")))
  
  if(file.exists(paste0(path_to,"/docs"))){
    try(dir_delete(paste0(path_to,"/docs")))}
  
  fs::dir_copy(
    paste0(path_to,"/temp_bookdown_report/file/bookdown-template-master/docs"),
    paste0(path_to,"/docs"),
    overwrite = TRUE)
  
  if(.keep_files == FALSE){
    try(dir_delete(paste0(path_to,"/temp_bookdown_report/")))}
  
  return(message(
    "\n\nTo edit your file, You can use the function `open_visual_report('",to,"')`
(Compatibility tested on Chrome and Mozilla)\n\n"))
  
}

#' @title
#' Open a visual report in a browser
#'
#' @description
#' The visual report previously generated in an HTML bookdown document can be
#' opened using this short-cut function.
#' This report can be used to assist the user in the assessment of the data
#' structure, fields investigation (mandatory or not), coherence across elements
#' and taxonomy or standard evaluation. The summaries and figures associated
#' provide dataset composition, with observation distribution and descriptive
#' statistics.
#'
#' @seealso
#' [madshapR::dataset_visualize()]
#'
#' @param report_name A character string specifying the name of the report (a
#' folder in users environment) to be opened.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' library(dplyr)
#'
#' ###### Example 1: any data-frame (or tibble) can be a dataset by definition.
#' tempdir <- tempdir()
#' dataset_visualize(iris, to = tempdir,.keep_files = TRUE, group_by = Species)
#' open_visual_report(tempdir)
#' 
#' }
#'
#' @import stringr
#' @importFrom utils browseURL
#'
#' @export
#'
open_visual_report <- function(report_name){

  path_report <- str_remove(paste0(report_name,"/docs/index.html"), '^/')
  utils::browseURL(path_report)

}
