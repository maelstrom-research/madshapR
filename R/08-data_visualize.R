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
#' @param variable_summary A summary list which is the summary of the variables.
#' @param valueType_guess Whether the output should include a more accurate 
#' valueType that could be applied to the dataset. TRUE by default.
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
    dataset,
    col,
    data_dict = data_dict_extract(dataset), 
    group_by = attributes(variable_summary)[['madshapR_group::group_by']], 
    variable_summary = NULL,
    valueType_guess = TRUE){ # must be the same in dataset_summarize
  
  plot_categories <- function(x){
  
    # anchor
    
    levels <- 
      unique(x %>%                                                              # [GF] NOTE : patch, mais ici il y a une erreur. a vérifier
      select("cat_index","value_var short") %>% distinct() %>%
      arrange(pick("cat_index")) %>% pull("value_var short"))
    
    x_stacked <- 
      x %>% 
      # filter(!is.na(`value_var short`)) %>%
      group_by(pick(c("group_label_short","value_var short","value_var short","cat_index"))) %>%
      reframe(value_var_occur = sum(value_var_occur)) # %>% 
 
    aes <- 
      aes(x = fct_rev(as_category(
        x = !! as.symbol('value_var short'),
        labels = levels, 
        as_factor = TRUE)), 
        y = !! as.symbol('value_var_occur'), 
        fill = !! as.symbol('value_var short'))
    
    plot_x <- 
      ggplot(x_stacked) + aes + 
      geom_bar(stat = "identity") +  
      theme_bw() + 
      theme(legend.position="none",plot.title = 
              element_text(size = 8, face = "bold"),
            strip.background = element_rect(color = "white", fill="white")) +
      ylab(unique(x[[group_col_short]])) +
      xlab("") +
      scale_fill_manual(values = 
                          x$color_palette_valid_class %>%
                          setNames(x$`value_var short`)) +
      scale_x_discrete(drop = FALSE) + 
      coord_flip() +
      stat_summary(
        aes(label = after_stat(y)),
        cex = 3,
        fun = "sum", geom = "text", vjust = 0.33,
        hjust = -0.24,
        position = position_nudge(y = 0)) +
      scale_y_continuous(
        breaks = function(x){
          if(max(x_stacked$value_var_occur) == 0) 
            return(0) else
              br = pretty(x, n=4) 
            return(br)},
        lim = c(0,max(x_stacked$value_var_occur)*1.2)) +
      facet_wrap(~ group_label_short,ncol = 3)

    return(plot_x)
  }
  plot_pie        <- function(x){
  
    # levels <- 
    #   x %>% 
    #   select("valid_class") %>% distinct() %>%
    #   arrange(desc(pick("valid_class"))) %>% pull("valid_class") 
    # 
    # names(levels) <- levels %>% str_remove_all("1 - |2 - |3 - |4 - ")
    # 
    # levels <- factor(levels)

    x_sum <- 
      x %>% 
      select(-any_of("color_palette")) %>%
      left_join(color_palette_maelstrom,by = c("valid_class" = "values")) %>%
      group_by(pick(c("group_label_short","valid_class","color_palette"))) %>%
      mutate("sum_var_occur" = sum(value_var_occur)) %>% ungroup %>%
      select("group_label_short","valid_class","color_palette","value_var_occur") %>%
      # filter(value_var_occur > 0) %>%
      group_by(pick(c("group_label_short","valid_class","color_palette"))) %>%
      reframe(sum_var_occur = sum(value_var_occur)) %>%
      group_by(pick(c("group_label_short"))) %>%
      arrange(pick(c("group_label_short","valid_class"))) %>%
      mutate(
        prop = 10000*round(.data$`sum_var_occur`/sum(sum_var_occur),4),
        prop = ifelse(is.na(prop),0,prop))
    
    # handle total < 1
    if(nrow(x_sum) > 0){
      x_sum <- 
      x_sum %>%
      group_by(pick("group_label_short")) %>% group_split() %>%
      lapply(function(x){
        x$prop[[nrow(x)]] <- 10000 - sum(x$prop[-nrow(x)])
        return(x)})}
        
    x_pie <- 
      bind_rows(x_sum) %>%
      group_by(pick("group_label_short")) %>%
      arrange(desc('valid_class')) %>%
      mutate(valid_class = factor(valid_class %>% str_remove_all("1 - |2 - |3 - |4 - "))) %>%
      mutate(
        csum = cumsum(.data$`prop`),
        pos = .data$`prop`/2 + lag(.data$`csum`, 1),
        pos = ifelse(is.na(.data$`pos`),.data$`prop`/2,.data$`pos`),
        label = ifelse(floor(prop)/100 == 0 , "",paste0(floor(prop)/100,"%")),
        prop = prop/10000,
        pos = pos/10000,
        csum = csum/10000)

    aes <- 
      aes(
        x = "",
        y = !! as.symbol('sum_var_occur'), 
        fill = !! as.symbol("valid_class"))
      
    plot_x_pie <-
      ggplot(x_pie) + aes +
      facet_wrap(
        ~ group_label_short,ncol = 3) +
      geom_bar(stat='identity',width = 1,position = position_fill()) +
      theme_void() +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_text(size = 8,face = "bold")) +
      geom_segment(
        aes(x = ifelse(abs(!! as.symbol('pos')) == 0, NA, 1.550),
            xend = ifelse(abs(!! as.symbol('pos')) == 0, NA, 1.450),
            y = abs(!! as.symbol('pos')),
            yend = abs(!! as.symbol('pos'))),
        color = "black", linewidth = 1) +
      scale_fill_manual(
        guide = guide_legend(reverse = TRUE),
        values = x_pie$color_palette %>%
                 setNames(x_pie$`valid_class`)) + 
      geom_text(
        cex = 2.5,
        aes(x = 1.8, label = !! as.symbol('label')),
        position = position_fill(vjust = 0.5)) +
      coord_polar('y', start = 0)
    
    return(plot_x_pie)

  }
  plot_numeric    <- function(x){
    
    
    x <- x %>% 
      mutate(across(
        all_of('value_var short'), ~ as_valueType(.,vT_col$`valueType`))) %>%
      mutate(`value_var short` = ifelse(value_var_occur == 0,NA,`value_var short`))
    

    binwidth <- 
      ceiling(
      2 * IQR(x$`value_var short`, na.rm = TRUE) / 
      (length(x$`value_var short`)^(1/3)))
    
    aes <-
      aes(
        x = !! as.symbol("value_var short"),
        # y = !! as.symbol("value_var short"),
        fill =  !! as.symbol("group_label_short"))
   
    plot_histogramm <-    
      ggplot(x) + aes +
      geom_histogram(aes(y = after_stat(count)), color = "black") + 
      # geom_density(alpha = 0.8, color = "red", linewidth = 0.5) + 
      facet_wrap(~group_label_short) + # Facet by species
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_manual(values = 
                          x$color_palette_group %>%
                          setNames(x$`group_label_short`)) + 
      xlab("") + 
      ylab("")
    
    
    aes <-
      aes(
        x = !! as.symbol("value_var short"),
        y = !! as.symbol("value_var short"),
        fill =  !! as.symbol("group_label_short"))
      
      
    plot_whisker <-     
      ggplot(x) + aes +
      # facet_wrap( ~ group_label_short) + # Facet by species
      geom_boxplot(
        aes(y = fct_rev(!! as.symbol("group_label_short"))),outlier.color = 'red') +
      theme_bw() +
      # coord_flip() + 
      theme(legend.position="none",plot.title = 
              element_text(size=8, face = "bold")) +
      # ggtitle(paste0('Box plot', title)) +
      ylab("") +
      xlab("") +
      scale_fill_manual(values = 
                          x$color_palette_group %>%
                          setNames(x$`group_label_short`))
    
    return(list(plot_whisker = plot_whisker,plot_histogramm = plot_histogramm))
  }
  plot_boolean    <- function(x){
  
    # preprocess elements : valid values
    x <- 
      x %>%
      mutate(`value_var short` = ifelse(value_var_occur == 0, NA, `value_var short`)) %>%
      mutate(`value_var short` = as_any_integer(`value_var short`)) %>%
      mutate(`value_var short` = ifelse(is.na(`value_var short`), -1, `value_var short`))
    # anchor
    aes <- 
      aes(
        fill = as_factor(!! as.symbol("value_var short")),
        x = fct_rev(as_category(
          !! as.symbol("group_label_short"),
          labels = unique(c('(all)',group_cat_short)),as_factor = TRUE)))
    
    plot_x <- 
      ggplot(x) + aes + 
      geom_bar(position = 'fill', stat = "count") +
      coord_flip() +
      xlab("") +
      ylab("") + 
      scale_fill_manual(
        guide = guide_legend(reverse = TRUE),
        labels = c("", "FALSE","TRUE"),
        values = c(
          "1" = unname(color_palette_maelstrom %>% filter(.data$`values` == "cat_1") %>% pull('color_palette')),
          "0" = unname(color_palette_maelstrom %>% filter(.data$`values` == "cat_2") %>% pull('color_palette')),
          "-1" = "white")) +
      theme_bw() +
      theme(legend.position = "right",legend.title = element_blank())
    
    return(plot_x)  
  }
  plot_character  <- function(x){
    plot_x <- NULL
    return(plot_x)
  }
  plot_date       <- function(x){
    
    # 
    # # convert dataset to wide format
    # colset_span <- 
    #   x %>%
    #   dplyr::filter(
    #     if_any("value_var short") %in% min(!! as.symbol("value_var short")) | 
    #     if_any("value_var short") %in% max(!! as.symbol("value_var short"))) 
    # 

    # n_obs <- nrow(colset_values)
    # 
    # #### plot_1 date ####    
    # 
    # title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
    # if(group_by != '') title <- paste0(title, ' - per ',group_by)
    # # 
    # aes <- 
    #   aes(
    #     x = !! as.symbol("value_var short"),
    #     y = fct_rev(as_category(!! as.symbol("group_label_short"),labels = c('(all)',group_cat_long),as_factor = TRUE)))
    # 
    # 
    # plot_x <- 
    #   ggplot(colset_span) + aes +
    #   geom_line() +
    #   geom_point(size = 3) +
    #   ggtitle(paste0('Span date', 'title')) +
    #   theme_bw()+
    #   theme(legend.position="none",plot.title = 
    #           element_text(size=8,face = "bold")) +
    #   ylab("") +
    #   xlab("") +
    #   scale_color_manual(values = x$color_palette_group) 
    
    #### plot_2 date ####    
    
    # title <- paste0(' representation of ',col,' (N obs. : ',n_obs,')')
    # if(group_by != '') title <- paste0(title, ' - per ',group_by)
    # 
    # aes <-
    #   if(group_by == ''){
    #     aes(x = '',
    #         y = !! as.symbol(col),
    #         fill = '')}else{ 
    #           
    #           aes(x = fct_rev(!! as.symbol(group_by)),
    #               y = !! as.symbol(col),
    #               fill =  !! as.symbol(group_by))}
    # 
    # plot_2 <-
    #   ggplot(colset_values) + aes +
    #   geom_boxplot(outlier.color = 'red') +
    #   theme_bw() +
    #   coord_flip() + 
    #   theme(legend.position="none",plot.title = 
    #           element_text(size=8, face = "bold")) +
    #   ggtitle(paste0('Box plot', title)) +
    #   ylab("") +
    #   xlab("") +
    #   scale_fill_manual(values = (palette_values))
    
    
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
    
    plot_x <- NULL

    return(plot_x)
  } 
  plot_datetime   <- function(x){
    plot_x <- NULL
    
    return(plot_x)
  }

  #### Catch elements ####
  if(toString(group_by) == "") group_by <- group_vars(dataset)
  
  group_var    <- names(as_dataset(dataset) %>% select(all_of(group_by)))
  col_id       <- col_id(dataset)
  dataset      <- ungroup(dataset) %>% as_dataset(col_id)
  col_var      <- names(ungroup(dataset) %>% select(all_of(col)))
  if(is.null(col_id)) dataset <- as_dataset(add_index(dataset, "madshapR::index", .force = TRUE),col_id = "madshapR::index")
  col_id       <- names(dataset %>% select(all_of(col_id(dataset))))
  col_set     <- as_dataset(dataset) %>% select(all_of(c(col_id,group_var,col_var)))
  if(col_id == col_var) col_set <- as_dataset(col_set, col_id = col_id)
  col_set_pps <- dataset_preprocess(col_set %>% select(all_of(c(group_var,col_var))), data_dict, group_var)
  col_dict    <-
    data_dict_match_dataset(col_set[col_var],data_dict,output = "data_dict") %>%
    data_dict_trim_labels()
  
  if(toString(group_var) == col_var){
    col_set_pps[['(all)']] <-  col_set_pps[['madshapR::grouping_var']]}
  
  col_name_short <- unique(col_set_pps[['(all)']][["Variable name"]])
  
  group_name_short <- "(all)"
  group_col_short  <- "Grouping variable: "
  group_cat_short  <- "(all)"
  group_cat_long  <- "(all)"
  if(length(group_var) > 0 
     # & toString(group_var) != col_var
     ){
    group_name_short <- 
      unique(col_set_pps[['madshapR::grouping_var']][["Variable name"]])
    group_col_short <- paste0("Grouping variable: ",group_name_short)
    group_cat_short <- 
      unique(c(group_cat_short,
               col_set_pps[['madshapR::grouping_var']] %>%
                 select("cat_index","value_var short") %>%
                 distinct() %>% arrange(pick("cat_index")) %>% 
                 pull("value_var short")))
    group_cat_long <- 
      unique(c(group_cat_long,
               col_set_pps[['madshapR::grouping_var']] %>%
                 select("cat_index","value_var long") %>% 
                 distinct() %>% arrange(pick("cat_index")) %>% 
                 pull("value_var long")))
  }
  
  group_label_tibble <- 
    tibble(group_label_short = group_cat_short, group_label = group_cat_long)

  if(is.null(variable_summary)){
    variable_summary <- 
      dataset_summarize(
        dataset = col_set,
        data_dict = data_dict_match_dataset(col_set,data_dict,output = "data_dict") ,
        valueType_guess = valueType_guess,
        group_by = group_var, 
        dataset_name = 'dataset')}
  
  col_summary <- 
    variable_summary[str_detect(names(variable_summary),"(v|V)ariable")] %>%
    lapply(function(x) x %>% filter(.data$`Variable name` == col_name_short))
    
  col_summary <- col_summary[col_summary %>% lapply(nrow) != 0]
    
  col_set_pps <- 
    bind_rows(col_set_pps[!(names(col_set_pps) %in% 
                              "madshapR::grouping_var")])

  # guess the generic valueType of the variable (excluding categories):
  vT_list <- madshapR::valueType_list
  vT_col <-
    if(valueType_guess == TRUE){
      vT_list[vT_list$valueType %in% 
                valueType_guess(
                  col_set_pps[col_set_pps$valid_class == '1 - Valid non-categorical values',] %>% 
                    pull(`value_var short`)),c('valueType','genericType')]
    }else{
      unique(col_set_pps %>% select(c('valueType','genericType')))}
  
  col_set_pps <- 
    col_set_pps %>%
    select(-c(
      "value_var long","madshapR::group_label long",
      "valueType","genericType","Categorical variable",
      "index_value","Index","name_var"))
  
  #### summary_1 ####
  summary_1 <- 
    col_summary[
    str_detect(names(col_summary), "Variables summary \\(all\\)")][[1]]
  
  summary_1 <- 
    as.data.frame(t(
      summary_1 %>% 
        rowwise() %>%                                                           # [GF] NOTE : rowwise
        dplyr::filter(.data$`Variable name` %in% col_name_short) %>% ungroup %>%
        select(-c("Index":"Non-valid categories"))))                            # [GF] NOTE : Categories in data dictionary ) enlever
  
  summary_1 <- 
    summary_1 %>%
    mutate(col = row.names(summary_1)) %>%
    mutate(across(-c("col"), 
                  ~ ifelse(. == 0,NA_real_,.))) %>%
    select(-'col') %>%
    mutate(across(everything(),as.character)) 
  
  names(summary_1) <- 
  if(ncol(summary_1) == 1) "(all)" else
      group_cat_short[seq_len(min(length(names(summary_1)),length(group_cat_short)))+1]
  
  #### summary_2 ####
  summary_2 <- col_summary[
      str_detect(names(col_summary), "variable summary")]
  
  if(length(summary_2) > 0 
     # & toString(group_var) != col_var
     ){ 

    summary_2 <- col_summary[
      str_detect(names(col_summary), "variable summary")]

    if(length(summary_2) == 1){ summary_2 <- NULL 
    }else{
      
      summary_2 <- col_summary[
        str_detect(names(col_summary), "variable summary")][[1]]
      
      if(nrow(summary_2) > 0){
        summary_2 <- 
          as.data.frame(t(
            summary_2 %>%
              rowwise() %>%                                                       # [GF] NOTE : rowwise
              dplyr::filter(.data$`Variable name` %in% col_name_short) %>% ungroup %>%
              select(-c("Index":"Number of distinct values")))) 
        
        names(summary_2) <- 
          if(ncol(summary_2) == 1) "(all)" else
            group_cat_short[seq_len(min(length(names(summary_2)),length(group_cat_short)))+1]
        
      }else{summary_2 <- NULL}
    }
  }
  
  summary_table <- bind_rows(summary_1,summary_2)
  
  summary_table <-
    datatable(
      summary_table,
      options =
        list(
          
          # [GF] Comment : here is the place where the column names can be modified. 
          # works in the console, but overwritten in the dataset visualize (due to 
          # global parameter)
          
          dom = 't',
             scrollX = TRUE,
             pageLength = nrow(summary_table),
             ordering = FALSE,
             paging = TRUE),
      filter = 'none' ,
      escape = FALSE)

  
  #### summary_categories ####
  summary_categories <- NULL
  
  if(!is.null(col_summary[["Categorical variable summary"]])){
    
  summary_categories <- 
    col_summary[["Categorical variable summary"]]

  summary_categories <- 
    as.data.frame(t(
      summary_categories %>%
        bind_rows(tibble("Grouping" = as.character())) %>%
        rename("group_cat_long" = starts_with("Grouping")[1]) %>%
        left_join(
          tibble(
            group_cat_long = group_cat_long) %>% 
            add_index(), by = 'group_cat_long') %>% arrange(index) %>%
        select(-any_of('Grouping'),-"index",-"group_cat_long",
               -c("Index":"Number of distinct values"))))

  names(summary_categories) <- 
    if(ncol(summary_categories) == 1) "(all)" else
      group_cat_short[seq_len(min(length(names(summary_categories)),length(group_cat_short)))+1]
  
  summary_categories <-
    summary_categories %>% 
    mutate(col = row.names(summary_categories)) %>%
    mutate(across(-c("col"), ~ str_replace_all(.,"\\n","<br>"))) %>%
    select(-'col') %>%
    mutate(across(everything(),as.character))
    
  summary_categories <-
    datatable(
      summary_categories,
      options =
        list(dom = 't',
             scrollX = TRUE,
             pageLength = nrow(summary_categories),
             ordering = FALSE,
             paging = TRUE),
      filter = 'none' ,
      escape = FALSE)
    
  }
  
  #### preprocess elements ####
  preprocess_var_values <- 
    col_set_pps[col_set_pps$valid_class %in% '1 - Valid non-categorical values',] %>% 
    rename("group_label" = starts_with(group_col_short)) %>%
    left_join(group_label_tibble, by = "group_label") %>%
    group_by(pick("group_label_short"))
  
  preprocess_var_values <- 
    preprocess_var_values %>%
    group_split() %>% as.list %>%
    setNames(group_keys(preprocess_var_values)[[1]])

  # preprocess elements : categorical values
  preprocess_cat_values <- 
    col_set_pps[col_set_pps$valid_class %in%  '2 - Valid categorical values',] %>%
    rename("group_label" = starts_with(group_col_short)) %>%
    left_join(group_label_tibble, by = "group_label") %>%
    group_by(pick("group_label_short"))
  
  preprocess_cat_values <- 
    preprocess_cat_values %>%
    group_split() %>% as.list %>%
    setNames(group_keys(preprocess_cat_values)[[1]])

  # preprocess elements : missing values  
  preprocess_miss_values <- 
    col_set_pps[col_set_pps$valid_class %in% c('3 - Missing categorical value','4 - Empty values'),] %>% 
    rename("group_label" = starts_with(group_col_short)) %>%
    left_join(group_label_tibble, by = "group_label") %>%
    group_by(pick("group_label_short"))
  
  preprocess_miss_values <- 
    preprocess_miss_values %>%
    group_split() %>% as.list %>%
    setNames(group_keys(preprocess_miss_values)[[1]])
  
  # preprocess elements : valid_class  
  preprocess_valid_class <- 
    col_set_pps %>% 
    rename("group_label" = starts_with(group_col_short)) %>%
    left_join(group_label_tibble, by = "group_label") %>%
    group_by(pick("group_label_short"))
  
  preprocess_valid_class <- 
    preprocess_valid_class %>%
    group_split() %>% as.list %>%
    setNames(group_keys(preprocess_valid_class)[[1]])
  
  #### plot var values ####
  plot_var_values_1 <- NULL
  plot_var_values_2 <- NULL
  if(length(preprocess_var_values) > 0) {
    
    if(vT_col$genericType == "numeric"){
      if(vT_col$valueType == "boolean"){
      # anchor
        plot_var_values_1 <- plot_boolean(bind_rows(preprocess_var_values))
        
        }else{
          plot_var_values   <- plot_numeric(bind_rows(preprocess_var_values))
          plot_var_values_1 <- plot_var_values$plot_whisker
          plot_var_values_2 <- plot_var_values$plot_histogramm          
          }

    }
    
    if(vT_col$genericType == "character"){
      
      plot_var_values_1 <- plot_character(bind_rows(preprocess_var_values))
      
    }
    
    if(vT_col$genericType == "date"){
      
      plot_var_values_1 <- plot_date(bind_rows(preprocess_var_values))
      
    }  
    
    if(vT_col$genericType == "datetime"){
      plot_var_values_1 <- plot_datetime(bind_rows(preprocess_var_values))
    }
  }
  
  #### plot cat values ####
  plot_cat_values <- NULL  
  # categorical values
  if(length(preprocess_cat_values) > 0)
    plot_cat_values <- plot_categories(bind_rows(preprocess_cat_values))

  #### plot miss values ####
  plot_miss_values <- NULL 
  # missing values
  if(length(preprocess_miss_values) > 0)
    plot_miss_values <- plot_categories(bind_rows(preprocess_miss_values))
  
  #### plot pie values ####
  plot_pie_values <- NULL 
  # missing values
  if(length(preprocess_valid_class) > 0)
    plot_pie_values <- plot_pie(bind_rows(preprocess_valid_class))
  
  #### gather ####
  plots <- list(
    summary_table = summary_table,
    summary_categories = summary_categories,
    main_values_1 = plot_var_values_1,
    main_values_2 = plot_var_values_2,
    cat_values = plot_cat_values, 
    missing_values = plot_miss_values, 
    pie_values = plot_pie_values)
  
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
#'   group_by(gndr) %>%
#'   as_dataset(col_id = "part_id")
#'   
#' data_dict <- as_data_dict_mlstr(madshapR_example$`data_dict_example`)
#' dataset <- data_dict_apply(dataset,data_dict)
#' dataset_summary <- dataset_summarize(dataset,data_dict)
#'  
#' if(dir_exists(tempdir())) dir_delete(tempdir())
#' bookdown_path <- tempdir()
#'  
#' dataset_visualize(
#'  dataset,
#'  data_dict,
#'  dataset_summary = dataset_summary,
#'  bookdown_path = bookdown_path)
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
    group_by = attributes(dataset_summary)[['madshapR_group::group_by']],
    valueType_guess = FALSE,
    taxonomy = NULL,
    dataset_summary = NULL,
    dataset_name = NULL){
  
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

  # if(toString(substitute(group_by)) == '') group_by <- NULL
  # attempt to catch group_by from the group_vars if the dataset is grouped
  # if(length(group_vars(dataset)) == 1 & toString(substitute(group_by)) == ''){

  group_var    <- names(as_dataset(dataset) %>% select(all_of(group_by)))
  col_id       <- col_id(dataset)
  dataset <- as_dataset(ungroup(dataset),col_id)
  
  
  # }
  
  dataset_name <- 
    suppressWarnings(
    ifelse(
      !is.null(dataset_name),
      dataset_name,
      make_name_list(as.character(fargs[['dataset']]),
                           list_elem = list(NULL))))

  match_input_objects <- 
    suppressWarnings({
      data_dict_match_dataset(
        dataset,
        data_dict, 
        data_dict_apply = TRUE)})
  
  dataset <- as_dataset(match_input_objects$dataset,col_id)
  data_dict <- as_data_dict_mlstr(match_input_objects$data_dict)
  
  # summarize initial information
  if(is.null(dataset_summary)){
    dataset_summary <- dataset_summarize(
      dataset = dataset,
      data_dict = data_dict,
      group_by = group_var,
      valueType_guess = valueType_guess,
      taxonomy = taxonomy,
      dataset_name = dataset_name)}

  data_dict <- 
    data_dict_trim_labels(data_dict)
  
  data_dict$`Variables` <- 
    data_dict$`Variables` %>% 
    add_index(.force = TRUE)
  
  if(has_categories(data_dict)){
    data_dict$`Categories` <- 
      data_dict$`Categories` %>%
      left_join(
        data_dict$`Variables` %>% select("variable" = "name", "Variable name"),
        by = "variable")}
  
  bookdown_template(path_to, overwrite = FALSE)
  if(!dir.exists(paste0(path_to,"/src"))) dir.create(paste0(path_to,"/src"))
  save(
    path_to,dataset, data_dict, group_by, dataset_summary,col_id, # data_dict_flat,
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
        <li><a href="./">',dataset_name,'</a></li>
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
    paste0(rep(0,nchar(nrow(data_dict$`Variables`))) %>% paste(collapse = ""))
  
  for(i in seq_len(nrow(data_dict$`Variables`))){
    # stop()}
    
    rmd_file_name <-
      paste0(path_to,"/",
             str_sub(paste0(increment,i),-(increment %>% nchar + 1),-1),"-",
             make.names(data_dict$`Variables`$`Variable name`[i]),".Rmd")
    file.create(rmd_file_name)
    
    paste0(
      "# ", 
      data_dict$`Variables`$`Variable name`[i] %>%
      str_replace_all("(?=[^A-Za-z0-9])", "\\\\"),
      "{.unnumbered #var",i,"}\n\n") %>%
      
      
      paste0("\n<p style= \"font-size: 140%;\">**VARIABLE CHARACTERISTICS**</p>\n") %>%
      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0(
        "\n```{r ",
        str_squish(
          "echo = FALSE,message = FALSE,warning = FALSE,knitr.figure = TRUE}"),
        "\n
  
  datatable(t(
     data_dict$`Variables` %>%
     dplyr::filter(`Variable name` == '",data_dict$`Variables`$`Variable name`[i],"') %>%
     select(
      'Variable name', 
      'Variable label', 
      'Data dictionary valueType' = 'valueType')),
   options = list(dom = 't', scrollX = TRUE, ordering = FALSE,paging = FALSE),
   rownames = TRUE, colnames = rep('', 2),filter = 'none' ,  escape = FALSE)",
        
        "\n\n```\n") %>%
      
      paste0("\n</div>\n\n") %>%
      paste0(ifelse(
        sum(nrow(
          data_dict[['Categories']][data_dict[['Categories']][['Variable name']] == 
                                      data_dict$`Variables`$`Variable name`[i],])) > 0,
        paste0("\n<p style= \"font-size: 140%;\"> **Categories** </p>","\n\n") %>%
          paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
          paste0(
"\n```{r echo = FALSE, message = FALSE, warning = FALSE}",
"\n


    n_cat <- data_dict$`Categories` %>% 
      dplyr::filter(`Variable name` == '",data_dict$`Variables`$`Variable name`[i],"')

if(nrow(n_cat) > 20){

  datatable(
    data_dict$`Categories` %>% 
      dplyr::filter(`Variable name` == '",data_dict$`Variables`$`Variable name`[i],"') %>%
    select(
      'Variable name',
      'Categories in data dictionary' = 'Categories in data dictionary long', 
      'Non-valid categories' = 'Non-valid categories long') %>%
    mutate(across(everything(), as.character)),
    options = list(scrollX = TRUE),rownames = FALSE)

}else{

  datatable(
    data_dict$`Categories` %>% 
      dplyr::filter(`Variable name` == '",data_dict$`Variables`$`Variable name`[i],"') %>%
    select(
      'Variable name',
      'Categories in data dictionary' = 'Categories in data dictionary long', 
      'Non-valid categories' = 'Non-valid categories long') %>%
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
  valueType_guess = ", valueType_guess, ",
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

