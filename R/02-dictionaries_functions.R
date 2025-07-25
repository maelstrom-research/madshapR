#' @title
#' Transform single-row category information to multiple rows as element 
#'
#' @description
#' Expands data dictionary column(s) in a element (the parameter 'from'),
#' into another element (the parameter 'to').
#' If the element `from` contains any column starting with 'prefix', (xx,yy),
#' these columns will be added as 'xx' and 'yy' in the element identified by
#' `to`. This data frame will be created if necessary, and columns will be 
#' added, from left to right. (unique names will be generated if necessary).
#' Separator of each element is the following structure :
#' 'name = xx1 ; name = xx2'.
#' This function is mainly used to expand the column(s) 'Categories::xx' in 
#' "Variables" to "Categories" element with column(s) xx.
#' This function is the reversed operation of [data_dict_collapse()]
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
#' @seealso
#' [data_dict_collapse()]
#'
#' @param data_dict A list of data frame(s) representing metadata to be
#' transformed. 
#' @param from A symbol identifying the name of the element (data frame) to take
#' column(s) from. Default is 'Variables'.
#' @param to A symbol identifying the name of the element (data frame) to create
#' column(s) to. Default is 'Categories'.
#' @param name_prefix Character string of the prefix of columns of interest.
#' This prefix will be used to select columns, and to rename them in the 'to'
#' element. Default is 'Categories::'.
#'
#' @returns
#' A list of data frame(s) identifying a data dictionary.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' # use madshapR_examples provided by the package
#' data_dict_collapsed <- madshapR_examples$`data_dictionary_example - collapsed`
#' 
#' data_dict_expanded <- data_dict_expand(data_dict_collapsed)
#' glimpse(data_dict_expand(data_dict_expanded))
#' 
#' }
#'
#' @import dplyr tidyr stringr fabR
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
data_dict_expand <- function(
    data_dict,
    from = 'Variables',
    name_prefix = 'Categories::',
    to = 'Categories'){
  
  # test
  as_data_dict_shape(data_dict)
  
  from <- substitute(from)
  if(typeof(from) == "character") from <- as.symbol(from)
  if(typeof(from) == "symbol")    from <- substitute(from)
  if(typeof(from) == "language")  from <- as.symbol(from)
  
  to <- substitute(to)
  if(typeof(to) == "character") to <- as.symbol(to)
  if(typeof(to) == "symbol")    to <- substitute(to)
  if(typeof(to) == "language")  to <- as.symbol(to)
  
  if(is.null(data_dict[[from]])){
    stop(call. = FALSE,
         "Your data dictionary contains no '",from,"' element.")}
  
  names_col <-
    data_dict[[from]] %>%
    select(starts_with(name_prefix), - any_of(name_prefix)) %>% names
  
  if(length(names_col) == 0){
    warning(
      "Your data dictionary contains no column starting with '",
      name_prefix,"' in ",from)
    return(data_dict)}
  
  rename_col <- make.unique(str_remove(names_col,name_prefix))
  
  data_dict[[to]] <-
    tibble(variable = as.character(),name = as.character()) %>%
    bind_rows(data_dict[[to]])
  
  for(i in names_col){
    # stop()}
    
    tryCatch(
      {to_temp <-
        data_dict[[from]] %>%
        select(variable = "name", col_to = !! i ) %>%
        dplyr::filter(!is.na(.data$`col_to`)) %>%
        mutate(
          col_to = ifelse(str_detect(.data$`col_to`, "_="),
                          str_replace_all(.data$`col_to`, "_=", "__SEP_IN__"),
                          .data$`col_to`),
          col_to = ifelse(str_detect(.data$`col_to`, "_;"),
                          str_replace_all(.data$`col_to`, "_;", "__SEP_OUT__"),
                          .data$`col_to`),
          col_to = ifelse(str_detect(.data$`col_to`, "__SEP_IN__"),
                          .data$`col_to`,
                          str_replace_all(.data$`col_to`, "=", "__SEP_IN__")),
          col_to = ifelse(str_detect(.data$`col_to`, "__SEP_OUT__"),
                          .data$`col_to`,
                          str_replace_all(.data$`col_to`, ";", "__SEP_OUT__"))
        ) %>%
        separate_rows("col_to", sep="__SEP_OUT__") %>%
        separate(.data$`col_to`, into = c("name",i), sep = "__SEP_IN__") %>%
        mutate_all(~ str_squish(.)) %>%
        rename_with(.cols = !! i ,.fn =  ~ rename_col[which(names_col == i)])
      
      data_dict[[to]] <-
        data_dict[[to]] %>%
        full_join(to_temp,by = c("name","variable"))}
      ,
      warning=function(w) {
        # Choose a return value in case of warning
        error_vars <-
          silently_run({(
            data_dict[[from]] %>%
              select(variable = "name", col_to = !! i ) %>%
              dplyr::filter(!is.na(.data$`col_to`)) %>%
              mutate(
                col_to = ifelse(str_detect(.data$`col_to`, "_="),
                                str_replace_all(
                                  .data$`col_to`, "_=", "__SEP_IN__"),
                                .data$`col_to`),
                col_to = ifelse(str_detect(.data$`col_to`, "_;"),
                                str_replace_all(
                                  .data$`col_to`, "_;", "__SEP_OUT__"),
                                .data$`col_to`),
                col_to = ifelse(str_detect(.data$`col_to`, "__SEP_IN__"),
                                .data$`col_to`,
                                str_replace_all(
                                  .data$`col_to`, "=", "__SEP_IN__")),
                col_to = ifelse(str_detect(.data$`col_to`, "__SEP_OUT__"),
                                .data$`col_to`,
                                str_replace_all(
                                  .data$`col_to`, ";", "__SEP_OUT__"))) %>%
              separate_rows("col_to", sep="__SEP_OUT__") %>%
              separate(.data$`col_to`,
                       into = c("name", i),
                       sep = "__SEP_IN__") %>%
              dplyr::filter(is.na(!! i)) %>% 
              pull("variable") %>% toString)
            
          })
        
        stop(call. = FALSE,
             "\n\nParsing elements failures in your data dictionary.",
             "\nVariables affected:\n",
             error_vars,"\n",
             "Column affected:  ",i,"\n",
             bold("\n\nUseful tip:"),
             " If your colums contains ',' or '=' in
its labels, replace the separators by '_;' and '_=' and reprocess.
Example:
  > wrong: '0 = No alcohol  ; 1 = Alcohol(red ; white)'
  > good : '0 = No alcohol ",bold("_;")," 1 = Alcohol(red ; white)'\n")
      })
    
  }
  
  data_dict[[from]] <- data_dict[[from]] %>% select(- !! names_col)
  
  data_dict <- as_data_dict_shape(data_dict)
  
  return(data_dict)
}

#' @title
#' Transform multi-row category column(s) to single rows and join to "Variables"
#'
#' @description
#' Collapses a data dictionary element (the parameter 'from'),
#' into column(s) in another element (the parameter 'to')
#' If the element 'to' exists, and contains any column 'xx' or 'yy', these
#' columns will be added to the element 'from' under the names 'to:xx'
#' and 'to:yy'. (unique names will be generated if necessary). Each element
#' of these column will gather all information to process the reverse operation.
#' Separator of each element is the following structure :
#' 'name = xx1 ; name = xx2'.
#' This function is mainly used to collapse the 'Categories' element into 
#' columns in 'Variables'.
#' This function is the reversed operation of [data_dict_expand()]
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
#' @seealso
#' [data_dict_expand()]
#'
#' @param data_dict A list of data frame(s) representing metadata to be
#' transformed. 
#' @param from A symbol identifying the name of the element (data frame) to take
#' column(s) from. Default is 'Categories'.
#' @param to A symbol identifying the name of the element (data frame) to create
#' column(s) to. Default is 'Variables'.
#' @param name_prefix A character string of the prefix of columns of interest.
#' This prefix will be used to select columns, and to rename them in the 'to'
#' element. Default is 'Categories::'.
#'
#' @returns
#' A list of data frame(s) identifying a data dictionary.
#'
#' @examples
#' {
#'
#' # use madshapR_examples provided by the package
#' data_dict <- madshapR_examples$`data_dictionary_example`
#' data_dict_collapsed <- data_dict_collapse(data_dict)
#' head(data_dict_collapse(data_dict_collapsed))
#'
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom rlang .data
#'
#' @export
data_dict_collapse <- function(
    data_dict,
    from = 'Categories',
    to = 'Variables',
    name_prefix = 'Categories::'){
  
  # test
  as_data_dict_shape(data_dict)
  
  from <- substitute(from)
  if(typeof(from) == "character") from <- as.symbol(from)
  if(typeof(from) == "symbol")    from <- substitute(from)
  if(typeof(from) == "language")  from <- as.symbol(from)
  
  to <- substitute(to)
  if(typeof(to) == "character") to <- as.symbol(to)
  if(typeof(to) == "symbol")    to <- substitute(to)
  if(typeof(to) == "language")  to <- as.symbol(to)
  
  if(is.null(data_dict[[from]])){
    warning("Your data dictionary contains no '",from,"' element.")
    return(data_dict)}
  
  if(is.null(data_dict[[to]])){
    warning("Your data dictionary contains no '",to,"' element.")
    return(data_dict)}
  
  # add categories content
  if(sum(nrow(data_dict[[from]])) > 0){
    
    col_from <- tibble(name = as.character())
    
    for(i in names(data_dict[[from]] %>% select(-c("name","variable")))){
      # stop()}
      
      cat_temp <-
        data_dict[[from]] %>%
        select("variable","name",!! i) %>%
        unite("from", .data$`name`, !! i, sep = " __SEP_IN__ ") %>%
        group_by(.data$`variable`) %>%
        reframe(from = paste0(.data$`from`,collapse = " __SEP_OUT__ \n")) %>%
        mutate(
          from = ifelse(str_detect(.data$`from`, ";"),
                        str_replace_all(.data$`from`, "__SEP_OUT__", "_;"),
                        .data$`from`),
          from = ifelse(str_detect(.data$`from`, "="),
                        str_replace_all(.data$`from`, "__SEP_IN__", "_="),
                        .data$`from`),
          from =   str_replace_all(.data$`from`, "__SEP_OUT__", ";"),
          from =   str_replace_all(.data$`from`, "__SEP_IN__", "="))
      
      names(cat_temp) <- c("name",paste0(name_prefix,i))
      
      col_from <- full_join(col_from,cat_temp, by = "name")
      
    }
    
    data_dict[[to]] <-
      data_dict[[to]] %>%
      full_join(col_from, by = c("name"))
    
    data_dict[[from]] <- NULL
  }
  
  data_dict <- as_data_dict_shape(data_dict)
  
  return(data_dict)
}

#' @title
#' Transform column(s) of a data dictionary from long format to wide format
#'
#' @description
#' Transforms column(s) of a data dictionary from long format to wide format. 
#' If a taxonomy is provided, the corresponding columns in the data
#' dictionary will be converted to a format with the taxonomy expanded.
#' This operation is equivalent to performing a [tidyr::pivot_wider()] on these 
#' columns following the taxonomy structure provided. Variable names in the  
#' data dictionary must be unique.
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
#' A taxonomy is a classification schema that can be defined for variable 
#' attributes. A taxonomy is usually extracted from an 
#' [Opal environment](https://www.obiba.org/pages/products/opal/), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#'
#' @seealso
#' [tidyr::pivot_wider()], [as_data_dict()]
#'
#' @param data_dict A list of data frame(s) representing metadata to be
#' transformed.
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#'
#' @returns
#' A list of data frame(s) identifying a data dictionary.
#'
#' @examples
#' {
#'
#' library(dplyr)
#' 
#' # use madshapR_examples provided by the package
#' data_dict <- madshapR_examples$`data_dictionary_example`
#' taxonomy  <- madshapR_examples$`taxonomy_example`
#' data_dict_longer <- data_dict_pivot_longer(data_dict, taxonomy)
#' data_dict_wider <- data_dict_pivot_wider(data_dict_longer, taxonomy)
#' 
#' glimpse(data_dict_wider)
#'
#' }
#'
#' @import dplyr tidyr stringr fabR
#' @importFrom rlang .data
#'
#' @export
data_dict_pivot_wider <- function(data_dict, taxonomy = NULL){
  
  # test
  as_data_dict_shape(data_dict)
  taxonomy <- as_taxonomy(taxonomy)
  
  if(is.null(taxonomy)) return(data_dict)
  
  data_dict_init <- data_dict
  data_dict_unique_name <-
    make.unique(replace_na(data_dict[['Variables']]$`name`,"NA"))
  
  data_dict[['Variables']]$`name` <- data_dict_unique_name
  
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    mutate(across(everything(),as.character))
  
  taxonomy <- as_taxonomy(taxonomy)
  taxonomy_id <-
    taxonomy  %>%
    unite(
      col = "taxonomy_id",
      c("taxonomy", "vocabulary"),
      na.rm = TRUE,
      sep = "::",
      remove = FALSE) %>%
    arrange("index_taxonomy", "index_vocabulary", "index_term")
  
  taxonomy_id <-
    taxonomy_id[,
                c('index_vocabulary','taxonomy_id','taxonomy','vocabulary')] %>%
    distinct() %>%
    mutate(
      name_col = str_replace(
        .data$`taxonomy_id`,
        .data$`vocabulary`,
        as.character(.data$`index_vocabulary`)),
      name_term = paste0(.data$`name_col`,".term"))
  
  taxonomy_id <- taxonomy_id[,c('name_col','name_term','taxonomy')]
  
  taxonomy_id <-
    taxonomy_id[
      taxonomy_id$`name_col`%in% names(data_dict[["Variables"]]) &
        taxonomy_id$`name_term` %in% names(data_dict[["Variables"]]),]
  
  if(nrow(taxonomy_id) > 0){
    
    for(i in seq_len(nrow(taxonomy_id))){
      # stop()}
      
      name_col   <- taxonomy_id$`name_col`[i]
      name_term  <- taxonomy_id$`name_term`[i]
      col_final  <- taxonomy_id$`taxonomy`[i]
      
      data_dict_colnames <-
        intersect(
          names(data_dict[['Variables']]),
          c('name', name_col, name_term))
      
      data_dict_temp <-
        data_dict[['Variables']][,data_dict_colnames] %>%
        pivot_wider(
          names_from = all_of(name_col),
          values_from = all_of(name_term),
          names_prefix = paste0("__temp__.",col_final,"::"))
      
      data_dict_temp <-
        data_dict_temp[vapply(X = data_dict_temp,
                              FUN = function(x) !all(is.na(x)),
                              FUN.VALUE = logical(1))]
      
      col_temp  <- names(data_dict_temp)[-1]
      col_final <- str_remove(col_temp,"^__temp__\\.")
      
      if(length(col_temp)){
        
        for(j in seq_len(length(col_temp))){
          # stop()}
          
          col_temp_j  <- col_temp[j]
          col_final_j <- col_final[j]
          
          if(col_temp_j %in% names(data_dict[['Variables']])){
            stop(call. = FALSE,
                 "Column name ",col_temp_j, " already exists in your data dictionary")}
          
          data_dict[['Variables']] <-
            data_dict[['Variables']] %>%
            full_join(
              data_dict_temp[,c('name',col_temp_j)] ,
              by = c("name"))
          
          if(sum(names(data_dict[['Variables']]) %in% col_final_j) == 1){
            
            data_dict[['Variables']] <-
              data_dict[['Variables']] %>%
              unite(!! col_final_j,
                    !! col_final_j,
                    !! col_temp_j, sep = "|", na.rm = TRUE) %>%
              mutate(across(!! col_final_j, ~ na_if(.,"")))
            
          }else{
            data_dict[['Variables']] <-
              data_dict[['Variables']] %>%
              rename_with(.cols = any_of(col_temp_j), .fn = ~ col_final_j)
          }
        }
      }
      
      data_dict[['Variables']][,c(name_col,name_term)] <- NULL
    }
    
    if(paste0(attributes(taxonomy)$`madshapR::class`,"") == "taxonomy_mlstr"){
      
      keys <-
        taxonomy[!is.na(taxonomy$`vocabulary_short`),
                 c('vocabulary','vocabulary_short')] %>%
        distinct %>%
        mutate(
          vocabulary_short =
            paste0("Mlstr_area::",.data$`vocabulary_short`)) %>%
        mutate(vocabulary = paste0("Mlstr_area::",.data$`vocabulary`))
      
      col_area <-
        names(data_dict[['Variables']][,intersect(
          keys$`vocabulary_short`,
          names(data_dict[['Variables']]))])
      
      for(i in col_area){
        # stop()}
        
        data_dict[['Variables']] <-
          data_dict[['Variables']] %>%
          rename_with(
            .cols = any_of(i),
            .fn =  ~ keys[keys$`vocabulary_short` == i,][['vocabulary']])
      }
      
      taxo_scales <-
        taxonomy %>%
        unite(
          col = "area_scale_id",
          c("taxonomy_scale", "vocabulary_scale"),
          na.rm = TRUE,
          sep = "::",
          remove = FALSE) %>%
        mutate(area_scale_id = na_if(.data$`area_scale_id`, ""))
      
      taxo_scales <-
        taxo_scales[
          !is.na(taxo_scales$`area_scale_id`),
          c('area_scale_id','term_scale')] %>%
        distinct %>%
        rename("___area_scale_id___" = "area_scale_id") %>%
        rename("Mlstr_area::1.scale" = "term_scale")
      
      if(!is.null(data_dict[['Variables']][['Mlstr_area::1.scale']]) &
         all(is.na(data_dict[['Variables']][['Mlstr_area::1.scale']]))){
        data_dict[['Variables']][['Mlstr_area::1.scale']] <- NULL}
      
      if(!is.null(data_dict[['Variables']][['Mlstr_area::1.scale']]) &
         !all(is.na(data_dict[['Variables']][['Mlstr_area::1.scale']]))){
        
        if(!is.null(data_dict[['Variables']][['___area_scale_id___']])){
          stop(call. = FALSE,
               "Column name '___area_scale_id___' already exists in your data dictionary")}
        
        silently_run({
          data_dict[['Variables']] <-
            data_dict[['Variables']] %>%
            left_join(
              
              data_dict[['Variables']][
                !is.na(data_dict[['Variables']][['Mlstr_area::1.scale']]),
                c('name','Mlstr_area::1.scale')] %>%
                left_join(taxo_scales, by = "Mlstr_area::1.scale") %>%
                pivot_wider(
                  names_from = "___area_scale_id___",
                  values_from = "Mlstr_area::1.scale") ,
              
              by = c("name") )
          
          data_dict[['Variables']]['Mlstr_area::1.scale'] <- NULL
        })
        
        if(!is.null(data_dict[['Variables']][['NA']])){
          warning(toString(unique(
            data_dict[['Variables']][['NA']][!is.na(data_dict[['Variables']][['NA']])])),
            " scale(s) not in your taxonomy but present in your data dictionary")}
      }
    }
  }
  
  
  # verification of the taxonomy, terms and vocabularies
  new_names <- 
    names(data_dict[['Variables']])[
      !names(data_dict[['Variables']]) %in% 
        names(data_dict_init[['Variables']])]
  
  authorized_names <-
    taxonomy  %>%
    select("taxonomy", "vocabulary") %>% distinct %>%
    unite(col = "taxonomy_id", c("taxonomy", "vocabulary"),
          na.rm = TRUE, sep = "::", remove = FALSE) %>%
    pull("taxonomy_id")
  
  if(paste0(attributes(taxonomy)$`madshapR::class`,"") == "taxonomy_mlstr"){
    
    authorized_names <- 
      c(authorized_names ,
        taxonomy  %>%
          select("taxonomy_scale", "vocabulary_scale") %>% distinct %>%
          unite(col = "area_scale_id", 
                c("taxonomy_scale", "vocabulary_scale"),
                na.rm = TRUE, sep = "::", remove = FALSE) %>%
          dplyr::filter(.data$`area_scale_id` != "") %>%
          pull("area_scale_id"))
  }
  
  wrong_names <- new_names[! new_names %in% authorized_names]
  
  if(length(wrong_names) > 0){
    warning(wrong_names %>% toString,
            " column name(s) not in your taxonomy but present in your data dictionary")}
  
  data_dict[['Variables']]$`name` <- data_dict_init[['Variables']]$`name`
  
  return(data_dict)
}

#' @title
#' Transform column(s) of a data dictionary from wide format to long format
#'
#' @description
#' Transforms column(s) of a data dictionary from wide format to long format. 
#' If a taxonomy is provided, the corresponding columns in the data
#' dictionary will be converted to a standardized format with fewer columns.
#' This operation is equivalent to performing a [tidyr::pivot_longer()] on 
#' these columns following the taxonomy structure provided. Variable names in 
#' the data dictionary must be unique.
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
#' A taxonomy is a classification schema that can be defined for variable 
#' attributes. A taxonomy is usually extracted from an 
#' [Opal environment](https://www.obiba.org/pages/products/opal/), and a 
#' taxonomy object is a data frame that must contain at least the columns 
#' `taxonomy`, `vocabulary`, and `terms`. Additional details about Opal 
#' taxonomies are 
#' [available online](https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html).
#'
#' @seealso
#' [tidyr::pivot_longer()], [as_data_dict()]
#'
#' @param data_dict A list of data frame(s) representing metadata to be
#' transformed.
#' @param taxonomy An optional data frame identifying a variable classification 
#' schema.
#'
#' @returns
#' A list of data frame(s) identifying a data dictionary.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#'
#' # use madshapR_examples provided by the package
#' data_dict <- madshapR_examples$`data_dictionary_example`
#' taxonomy  <- madshapR_examples$`taxonomy_example`
#' data_dict_longer <- data_dict_pivot_longer(data_dict, taxonomy)
#' 
#' glimpse(data_dict_longer)
#'
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
data_dict_pivot_longer <- function(data_dict, taxonomy = NULL){
  
  # test
  as_data_dict_shape(data_dict)
  
  if(is.null(taxonomy)) { return(data_dict) }else{
    as_taxonomy(taxonomy) 
  }
  
  # make unique names for names in data dictionary 
  data_dict_init <- data_dict
  
  data_dict[['Variables']] <- 
    data_dict[['Variables']] %>%
    add_index("madshapR::index", .force = TRUE)
  
  data_dict[['Variables']]$`name` <-
    make.unique(replace_na(data_dict[['Variables']]$`name`,"NA"))
  
  order_taxonomy <-
    taxonomy %>%
    select('taxonomy') %>%
    distinct() %>% pull('taxonomy')
  
  taxonomy_id <-
    taxonomy  %>%
    unite(
      col = "taxonomy_id", c('taxonomy', 'vocabulary'),
      na.rm = TRUE, sep = "::", remove = FALSE) %>%
    unite(
      col = "voc_term", c('vocabulary', 'term'),
      na.rm = TRUE, sep = "::", remove = FALSE) %>%
    arrange(.data$`index_taxonomy`,
            .data$`index_vocabulary`,
            .data$`index_term`) %>%
    group_by(.data$`taxonomy`) %>%
    group_split()
  
  names(taxonomy_id) <- sort(order_taxonomy)
  taxonomy_id <- taxonomy_id[order_taxonomy]
  
  for(i in names(taxonomy_id)){
    # stop()}
    
    taxonomy_i <-
      taxonomy_id[[i]] %>%
      rowwise() %>%                # [GF] NOTE : rowwise 
      dplyr::filter(.data$`taxonomy_id` %in% 
                      names(data_dict[['Variables']])) %>%
      select('voc_term','taxonomy_id','index_vocabulary', 
             'index_term','vocabulary') %>%
      distinct
    
    if(taxonomy_i %>% nrow > 0){
      
      try({
        data_dict_temp <-
          data_dict[['Variables']] %>%
          select('name',matches(paste0('^',taxonomy_i$`taxonomy_id`,'$'))) %>%
          # pivoting area of information
          pivot_longer(
            cols = starts_with(i),
            names_to = i,
            names_prefix = paste0(i,"::"),
            values_to = "term",
            values_drop_na = TRUE) %>%
          unite(
            col = "voc_term", c(!! i, 'term'),
            na.rm = TRUE, sep = "::", remove = FALSE) %>%
          left_join(taxonomy_i
                    ,by = 'voc_term') %>%
          arrange(!! i, .data$`index_vocabulary`, .data$`index_term`) %>% 
          mutate(
            across("vocabulary", ~ ifelse(is.na(.data$`taxonomy_id`),!!as.symbol(i),.))) %>%
          mutate(
            # across("term", ~ ifelse(is.na(.data$`taxonomy_id`),paste0("[ERROR] - ", .data$term),.))) %>%
            across("term", ~ ifelse(is.na(.data$`taxonomy_id`),.data$term,.))) %>%
          select(-matches('index_vocabulary'),
                 -matches('index_term'),
                 -matches('index_term'),
                 -matches('voc_term'),
                 -matches('taxonomy_id')) %>%
          group_by(.data$`name`) %>%
          distinct()
      }, silent = TRUE)
      
      group_max_size <- data_dict_temp %>% group_size() %>% max()
      arrange_taxonomy <-
        paste0(i,"::",rep(1:group_max_size,2) %>% sort(),c("",".term"))
      
      try({
        
        silently_run({
          data_dict_temp <-
            data_dict_temp %>%
            reframe(
              across(c(any_of(i), .data$`term`),
                     ~ paste0(.,collapse = "|"))) %>%
            separate(
              col = i,
              into = arrange_taxonomy[seq_len(length(arrange_taxonomy))%% 2==1],
              sep = "\\|") %>%
            separate(
              col = .data$`term`,
              into = arrange_taxonomy[seq_len(length(arrange_taxonomy))%% 2==0],
              sep = "\\|") %>%
            ungroup() %>%
            select(
              everything(),
              -any_of(arrange_taxonomy),
              any_of(arrange_taxonomy))
        })
        
        silently_run({
          data_dict[['Variables']] <-
            data_dict[['Variables']] %>%
            select(-matches(paste0("^",i,"::",taxonomy_i$`vocabulary`,"$"))) %>%
            full_join(data_dict_temp, by = c("name"))
        })
        
      }, silent = TRUE)}
  }
  
  
  if(paste0(attributes(taxonomy)$`madshapR::class`,"") == "taxonomy_mlstr"){
    
    keys <-
      taxonomy %>%
      select(.data$`vocabulary`, .data$`vocabulary_short`) %>%
      dplyr::filter(!is.na(.data$`vocabulary_short`)) %>% distinct
    
    col_area <-
      data_dict[['Variables']] %>%
      select(matches("Mlstr_area::[0-9]+$")) %>% names
    
    for(i in col_area){
      
      key <-
        keys %>%
        rename_with(
          .cols = .data$`vocabulary`,
          .fn =  ~ paste0(i)) %>%
        rename_with(
          .cols = .data$`vocabulary_short`,
          .fn =  ~ paste0(i,".vocabulary_short"))
      
      # re-arrange things (can do better)
      if(is.null(data_dict[['Variables']][['___Mlstr_temp___']]) |
         is.null(data_dict[['Variables']][['___Mlstr_temp___vocabulary']])){
        
        silently_run({
          data_dict[['Variables']] <-
            data_dict[['Variables']] %>% left_join(key) %>%
            rename_with(
              .cols = all_of(i),
              .fn =  ~ "___Mlstr_temp___") %>%
            rename_with(
              .cols = paste0(i,".vocabulary_short"),
              .fn =  ~ "___Mlstr_temp___vocabulary") %>%
            mutate(`___Mlstr_temp___` = .data$`___Mlstr_temp___vocabulary`) %>%
            rename_with(.cols = .data$`___Mlstr_temp___`, .fn =  ~ i) %>%
            select(-.data$`___Mlstr_temp___vocabulary`)
        })
        
      }else{
        stop(call. = FALSE,
             "Your data dictionary cannot be processed into Maelstrom format.
(presence of `___Mlstr_temp___` column")
      }
    }
    
    cols_scales <-
      taxonomy %>%
      unite("area_scale_id", .data$`taxonomy_scale`, .data$`vocabulary_scale`,
            na.rm = TRUE, sep = "::", remove = FALSE) %>%
      select(.data$`area_scale_id`) %>%
      dplyr::filter(!is.na(.data$`area_scale_id`)) %>% distinct %>%
      pull("area_scale_id") %>%
      intersect(names(data_dict[['Variables']]))
    
    if(length(cols_scales) > 0){
      data_dict[['Variables']] <-
        data_dict[['Variables']] %>%
        unite(
          "Mlstr_area::1.scale",all_of(cols_scales),sep=" | ",na.rm = TRUE) %>%
        mutate(`Mlstr_area::1.scale` = na_if(.data$`Mlstr_area::1.scale`,""))}
    
    arrange_taxonomy <-
      paste0("Mlstr_area","::",
             rep(seq_len(length(col_area)),2) %>% sort(),
             c("",".term"))
    
    # re-arrange things (can do better)
    data_dict[['Variables']] <-
      data_dict[['Variables']] %>%
      select(
        everything(),
        -starts_with("Mlstr_additional"),
        -starts_with("Mlstr_area"),
        starts_with("Mlstr_additional"),
        matches("^Mlstr_area::1$"),
        matches("^Mlstr_area::1.term$"),
        matches("^Mlstr_area::1.scale$"),
        matches("^Mlstr_area::2$"),
        matches("^Mlstr_area::2.term$"),
        matches("^Mlstr_area::3$"),
        matches("^Mlstr_area::3.term$"),
        everything()) %>%
      rename_with(
        ~ case_when(
          . == "Mlstr_additional::1.term" ~ "Mlstr_additional::Source",
          . == "Mlstr_additional::2.term" ~ "Mlstr_additional::Target",
          TRUE ~ .)) %>%
      select(
        -matches("^Mlstr_additional::1$"),-matches("^Mlstr_additional::2$"))
    
  }
  
  if(sum(data_dict[["Variables"]][['madshapR::index']]) != 
     nrow(data_dict[["Variables"]]) * (nrow(data_dict[["Variables"]]) + 1)/2){
    stop(call. = FALSE,
         "An error occurred in data_dict_pivot_longer(). Please contact us.")
  }
  
  data_dict[["Variables"]] <- 
    data_dict[["Variables"]] %>% arrange(.data$`madshapR::index`) %>%
    select(-"madshapR::index")
  
  data_dict[["Variables"]]$`name` <- data_dict_init[["Variables"]]$`name`
  
  return(data_dict)
}

#' @title
#' Subset data dictionary by row values
#'
#' @description
#' Subsets either or both the 'Variables' and 'Categories' elements of a data
#' dictionary. Rows are conserved if their values satisfy the condition.
#' This is a wrapper function analogous to [dplyr::filter()].
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
#' @seealso
#' [dplyr::filter()]
#'
#' @param data_dict A list of data frame(s) representing metadata to be
#' filtered.
#' @param filter_var Expressions that are defined in the element 'Variables' in
#' the data dictionary.
#' @param filter_cat Expressions that are defined in the element 'Categories' in
#' the data dictionary.
#' @param filter_all Expressions that are defined both in the 'Categories' and
#' 'Variables' in the data dictionary.
#'
#' @returns
#' A list of data frame(s) identifying a workable data dictionary structure.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' # use madshapR_examples provided by the package
#' # Data dictionary where the column 'table' is added to 
#' # refer to the associated dataset.
#' 
#' data_dict <- 
#'   madshapR_examples$`data_dictionary_example` %>% 
#'   lapply(function(x) mutate(x,table = "dataset"))
#' 
#' ###### Example 1 search and filter through a column in 'Variables' element
#' data_dict_f1 <- data_dict_filter(data_dict,filter_var = "name == 'gndr'")
#' glimpse(data_dict_f1)
#' 
#' ###### Example 2 search and filter through a column in 'Categories' element
#' data_dict_f2 <- data_dict_filter(data_dict,filter_cat = "missing == TRUE")
#' glimpse(data_dict_f2)
#' 
#' ###### Example 3 search and filter through a column across all elements.
#' # The column must exist in both 'Variables' and 'Categories' and have the
#' # same meaning
#' data_dict_f3 <- data_dict_filter(data_dict,filter_all = "table == 'dataset'")
#' glimpse(data_dict_f3)
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
data_dict_filter <- function(
    data_dict,
    filter_var = NULL,
    filter_cat = NULL,
    filter_all = NULL){
  
  # test if enough data_dict
  as_data_dict_shape(data_dict)
  
  if(!is.null(filter_all) & (!is.null(filter_var) | !is.null(filter_cat)))
    stop(call. = FALSE,"Too many argments entered")
  
  if( is.null(filter_all) &   is.null(filter_var) &  is.null(filter_cat))
    return(data_dict)
  
  if(!is.null(filter_all))
    filter_var <- filter_cat <- filter_all
  
  data_dict[['Variables']] <-
    eval(parse(
      text = paste(
        "data_dict[['Variables']] %>% dplyr::filter(",filter_var,")")))
  
  if(has_categories(data_dict)){
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      rowwise() %>%                # [GF] NOTE : rowwise
      dplyr::filter(.data$`variable` %in% data_dict[['Variables']]$`name`) %>% ungroup
    
    if(!is.null(filter_cat)){
      data_dict[['Categories']] <-
        eval(parse(
          text=paste(
            "data_dict[['Categories']] %>% dplyr::filter(",filter_cat,")")))}
  }
  
  if(!has_categories(data_dict))
    data_dict[['Categories']] <- NULL
  
  return(data_dict)
  
}

#' @title
#' Split grouped data dictionaries into a named list
#'
#' @description
#' Divides data dictionary element(s) into the groups defined by the query.
#' This function divides both the 'Variables' and 'Categories' elements (if 
#' the group exists under the same definition in in both) into a list of 
#' data dictionaries, each with the rows of the associated group and all the 
#' original columns, including grouping variables. This function is analogous 
#' to running [dplyr::group_by()] and [dplyr::group_split()]. Each element is 
#' named using the group values. [data_dict_list_nest()] reverses the effect.
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
#' @seealso
#' [dplyr::group_by()], [dplyr::group_split()] ,
#' [data_dict_group_by()], [data_dict_list_nest()]
#'
#' @param data_dict A list of data frame(s) representing metadata to be
#' transformed.
#' @param ... Column in the data dictionary to split it by. If not provided, the
#' splitting will be done on the grouping element of a grouped data dictionary.
#'
#' @returns
#' A list of data frame(s) identifying a list of workable data dictionary structure.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#'
#' # use madshapR_examples provided by the package
#' # Create a list of data dictionaries where the column 'table' is added to 
#' # refer to the associated dataset. The object created is not a 
#' # data dictionary per say, but can be used as a structure which can be 
#' # shaped into a data dictionary.
#' 
#' data_dict_list <- list(
#'   data_dict_1 = madshapR_examples$`data_dictionary_example - collapsed`,
#'   data_dict_2 = madshapR_examples$`data_dictionary_example` )
#' 
#' data_dict_ns <-
#'   data_dict_list_nest(data_dict_list, name_group = "table") %>%
#'   data_dict_group_by(col = "table")
#'   
#' data_dict_sp <- data_dict_group_split(data_dict_ns,col = "table")
#' 
#' glimpse(data_dict_sp)
#'  
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
data_dict_group_split <- function(data_dict, ...){
  
  # test if enough data_dict
  as_data_dict_shape(data_dict)
  
  if(!is_grouped_df(data_dict[['Variables']]))
    data_dict <- data_dict_group_by(data_dict, ...)
  
  if(!is_grouped_df(data_dict[['Variables']]))
    stop(call. = FALSE,
         "\n\nThe data dictionary list must be grouped to be split. Please group them
using data_dict_group_by(data_dict, col)")
  
  col <- as.symbol(names(group_keys(data_dict[['Variables']])))
  
  group_names_var <- pull(group_keys(data_dict[['Variables']]))
  
  if(!has_categories(data_dict)){
    data_dict[['Categories']] <-
      tibble(col = as.character()) %>%
      rename_with(.cols = "col", ~ deparse(col)) %>%
      group_by(!! col)
  }
  
  names_var <- names(group_keys(data_dict[['Variables']]))[1]
  names_cat <- names(group_keys(data_dict[['Categories']]))[1]
  
  if(names_var != names_cat){
    stop(call. = FALSE,
         "Grouping column must be the same in 'Variables' and 'Categories'.")}
  
  group_names_cat <- pull(group_keys(data_dict[['Categories']]))
  
  if(!all(group_names_cat %in% group_names_var)) stop(call. = FALSE,
                                                      "\nThese data dictionaries contain group of variables in 'Categories' which
cannot be found accross the variables declared in 'Variables'.")
  
  # if(length(group_names_var) == 1) return(data_dict)
  
  data_dicts_var <-
    data_dict[['Variables']] %>%
    group_split() %>% as.list()
  names(data_dicts_var) <- group_names_var
  
  data_dicts_cat <-
    data_dict[['Categories']] %>%
    group_split() %>% as.list()
  names(data_dicts_cat) <- group_names_cat
  
  if(length(data_dicts_cat) == 0) data_dicts_cat <- NULL
  
  data_dict_list <-
    vector(mode = "list", length = length(group_names_var))
  
  names(data_dict_list) <- group_names_var
  
  
  for(i in group_names_var){
    # stop()}
    
    data_dict_list[[i]] <- list(Variables  = NULL, Categories = NULL)
    data_dict_list[[i]] <-
      list(
        Variables  =
          bind_rows(data_dict_list[[i]][['Variables']], data_dicts_var[[i]]),
        Categories =
          bind_rows(data_dict_list[[i]][['Categories']], data_dicts_cat[[i]]))
    
    if(sum(nrow(data_dict_list[[i]][['Categories']])) == 0){
      data_dict_list[[i]][['Categories']] <- NULL }
  }
  
  return(data_dict_list)
}

#' @title
#' Bind listed data dictionaries
#'
#' @description
#' Binds a list of data dictionaries into one data dictionary.
#' This is a wrapper function analogous to [dplyr::bind_rows()].
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
#' @seealso
#' [dplyr::bind_rows()]
#'
#' @param data_dict_list A list of data frame(s) representing metadata to be
#' transformed.
#' @param name_group A character string of one column in the dataset that can be
#' taken as a grouping column.
#'
#' @returns
#' A list of data frame(s) identifying a workable data dictionary structure.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' # use madshapR_examples provided by the package
#' # Create a list of data dictionaries where the column 'table' is added to 
#' # refer to the associated dataset. The object created is not a 
#' # data dictionary per say, but can be used as a structure which can be 
#' # shaped into a data dictionary.
#' 
#' data_dict_list <- list(
#'   data_dict_1 = madshapR_examples$`data_dictionary_example` ,
#'   data_dict_2 = madshapR_examples$`data_dictionary_example - collapsed`)
#' 
#' data_dict_ns <-
#'   data_dict_list_nest(data_dict_list, name_group = "table") %>%
#'   data_dict_group_by(col = "table")
#' 
#' glimpse(data_dict_ns)
#' 
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
data_dict_list_nest <- function(data_dict_list, name_group = NULL){
  
  # test if enough data_dict
  data_dict_list %>%
    lapply(as_data_dict_shape)
  
  data_dict <- list(Variables = tibble(), Categories = tibble())
  
  for(i in seq_len(length(data_dict_list))){
    # stop()}
    
    data_dict[['Variables']] <-
      bind_rows(
        data_dict[['Variables']],
        data_dict_list[[i]][['Variables']] %>%
          mutate(across(everything(), as.character)))
    
    if(has_categories(data_dict_list[[i]])){
      data_dict[['Categories']] <-
        bind_rows(
          data_dict[['Categories']],
          data_dict_list[[i]][['Categories']] %>%
            mutate(across(everything(), as.character)))
    }
  }
  
  if(!is.null(name_group)){
    if(name_group %in%
       c(names(data_dict[['Variables']]),names(data_dict[['Categories']]))){
      warning(
        "The column '",name_group,
        "' already exists in data dictionary and will not be added.")
    }else{
      
      name_group_col_var <- tibble()
      name_group_col_cat <- tibble()
      
      for(i in names(data_dict_list)){
        # stop()}
        
        name_group_col_var <-
          bind_rows(
            name_group_col_var,
            data_dict_list[[i]][['Variables']][1] %>%
              mutate(name_list_group = names(data_dict_list[i])) %>%
              select(.data$`name_list_group`) %>%
              rename_with(.cols = .data$`name_list_group`,~ name_group))
        
        
        if(has_categories(data_dict_list[[i]])){
          name_group_col_cat <-
            bind_rows(
              name_group_col_cat,
              data_dict_list[[i]][['Categories']][1] %>%
                mutate(name_list_group = names(data_dict_list[i])) %>%
                select(.data$`name_list_group`) %>%
                rename_with(.cols = .data$`name_list_group`,~ name_group))
        }
        
      }
      
      data_dict[['Variables']]  <-
        name_group_col_var %>% bind_cols(data_dict[['Variables']])
      data_dict[['Categories']] <-
        name_group_col_cat %>% bind_cols(data_dict[['Categories']])
    }
  }
  
  if(!has_categories(data_dict)){
    data_dict[['Categories']] <- NULL
  }
  
  return(data_dict)
}


#' @title
#' Group listed data dictionaries by specified column names
#'
#' @description
#' Groups the data dictionary element(s) by the groups defined by the query.
#' This function groups both the 'Variables' and 'Categories' elements (if 
#' the group exists under the same definition in in both). This function is 
#' analogous to running [dplyr::group_by()]. Each element is named using the 
#' group values. [data_dict_ungroup()] reverses the effect.
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
#' @seealso
#' [dplyr::group_by()], [data_dict_ungroup()]
#'
#' @param data_dict A list of data frame(s) representing metadata to be
#' transformed.
#' @param col variable to group by.
#'
#' @returns
#' A list of data frame(s) identifying a workable data dictionary structure.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' # use madshapR_examples provided by the package
#' # Create a list of data dictionaries where the column 'table' is added to 
#' # refer to the associated dataset. The object created is not a 
#' # data dictionary per say, but can be used as a structure which can be 
#' # shaped into a data dictionary.
#' 
#' data_dict_list <- list(
#'   data_dict_1 = madshapR_examples$`data_dictionary_example` ,
#'   data_dict_2 = madshapR_examples$`data_dictionary_example - collapsed`)
#' 
#' data_dict_ns <- 
#'   data_dict_list_nest(data_dict_list, name_group = "table")
#' 
#' data_dict_gp <- data_dict_group_by(data_dict_ns, col = "table")
#' glimpse(data_dict_gp)
#' 
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
data_dict_group_by <- function(data_dict, col){
  
  # test if enough data_dict
  as_data_dict_shape(data_dict)
  
  col <- substitute(col)
  if(typeof(col) == "character") col <- as.symbol(col)
  if(typeof(col) == "symbol")    col <- substitute(col)
  if(typeof(col) == "language")  col <- as.symbol(col)
  
  if(is.null(col)) return(data_dict)
  
  group_names_var <-
    c(sort(unique(data_dict[['Variables']][[col]]),na.last = TRUE))
  
  categories <- has_categories(data_dict)
  
  if(!has_categories(data_dict)){
    data_dict[['Categories']] <-
      tibble(col = as.character()) %>%
      rename_with(.cols = "col", ~ deparse(col))}
  
  group_names_cat <-
    c(sort(unique(data_dict[['Categories']][[col]]),na.last = TRUE))
  
  if(is.null(group_names_cat)) 
    stop(call. = FALSE,
         paste0("Column '",col,"' not found in Categories."))
  
  if(!all(group_names_cat %in% group_names_var)) 
    stop(call. = FALSE,
         "\n\nThese data dictionaries contain group of variables in 'Categories' which
cannot be found accross the variables declared in 'Variables'.")
  
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    arrange(!! col) %>%
    group_by(!! col)
  
  data_dict[['Categories']] <-
    data_dict[['Categories']] %>%
    arrange(!! col) %>%
    group_by(!! col)
  
  if(categories == FALSE) data_dict[['Categories']] <- NULL
  
  return(data_dict)
}

#' @title
#' Ungroup data dictionary
#'
#' @description
#' Ungroups the data dictionary element(s). This function ungroups both the
#' 'Variables' and 'Categories' elements (if both are grouped data frames). 
#' This function is analogous to running [dplyr::ungroup()].
#' [data_dict_group_by()] allows to group a data dictionary and this function 
#' reverses the effect.
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
#' @seealso
#' [dplyr::ungroup()]
#' [data_dict_group_by()]
#'
#' @param data_dict A list of data frame(s) representing metadata to be
#' transformed.
#'
#' @returns
#' A list of data frame(s) identifying a workable data dictionary structure.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' # use madshapR_examples provided by the package
#' # Create a list of data dictionaries where the column 'table' is added to 
#' # refer to the associated dataset. The object created is not a 
#' # data dictionary per say, but can be used as a structure which can be 
#' # shaped into a data dictionary.
#' 
#' data_dict_list <- list(
#'   data_dict_1 = madshapR_examples$`data_dictionary_example` ,
#'   data_dict_2 = madshapR_examples$`data_dictionary_example`)
#' 
#' data_dict_nest <-
#'   data_dict_list_nest(data_dict_list, name_group = "table") %>%
#'   data_dict_group_by(col = "table")
#' 
#' glimpse(data_dict_ungroup(data_dict_nest))
#' 
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
data_dict_ungroup <- function(data_dict){
  
  # test if enough data_dict
  as_data_dict_shape(data_dict)
  
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    ungroup()
  
  if(has_categories(data_dict))
    data_dict[['Categories']] <-
    data_dict[['Categories']] %>%
    ungroup()
  
  return(data_dict)
}

#' @title
#' Apply a data dictionary to a dataset
#'
#' @description
#' Applies a data dictionary to a dataset, creating a labelled dataset with 
#' variable attributes. Any previous attributes will be preserved. For 
#' variables that are factors, variables will be transformed into 
#' haven-labelled variables. The data dictionary will be added as an attribute
#' (attributes(dataset)$`madshapR::Data dictionary`) and can be extracted using
#' the function [data_dict_extract()]
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
#' @seealso
#' [attributes()], [haven::labelled()],[data_dict_extract()]
#'
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided. 
#'
#' @returns
#' A labelled data frame with metadata as attributes, specified for each 
#' variable from the input data dictionary.
#'
#' @examples
#' {
#' 
#' # use madshapR_examples provided by the package
#' dataset   <- madshapR_examples$`dataset_example`
#' data_dict <- as_data_dict_mlstr(madshapR_examples$`data_dictionary_example`)
#' dataset   <- data_dict_apply(dataset, data_dict)
#' 
#' head(dataset)
#' 
#' }
#'
#' @import dplyr tidyr stringr haven
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
data_dict_apply <- function(dataset, data_dict = NULL){
  
  # if data_dict empty
  if(is.null(data_dict)) data_dict <- data_dict_extract(dataset)
  
  vT_list <- madshapR::valueType_list
  
  # preserve dataset
  as_dataset(dataset, col_id(dataset))
  preserve_attributes <- col_id(dataset)
  preserve_group <- group_vars(dataset)
  dataset <- as_dataset(ungroup(dataset))
  
  if(toString(attributes(data_dict)$`madshapR::class`) == 'data_dict_mlstr'){
    data_dict_init <- data_dict
    data_dict <- 
      as_data_dict_mlstr(data_dict) %>%
      as_data_dict
  }else{
    data_dict <- as_data_dict(data_dict)
    data_dict_init <- data_dict}
  
  # names must exist both in dataset and data dictionary
  # data dictionary is not applied to dataset, since it may come from an
  # automated datadict (text by default).
  if(suppressWarnings(check_dataset_valueType(dataset,data_dict)) %>% 
     dplyr::filter(str_detect(.data$`condition`,"\\[ERROR\\]")) %>% nrow > 0){
    stop(call. = FALSE,
"Valuetype in the data dictionary is not compatible with data.",
bold("\n\nUseful tip:"),
" Use dataset_evaluate(dataset, data_dict) to get a full assessment of
your dataset")}
  
  if(suppressWarnings(check_dataset_variables(dataset, data_dict)) %>% 
     dplyr::filter(str_detect(.data$`condition`,"\\[ERROR\\]")) %>% nrow > 0){
    stop(call. = FALSE,
"Names across your data dictionary differ from names across the dataset.",
bold("\n\nUseful tip:"),
" Use dataset_evaluate(dataset, data_dict) to get a full assessment of
your dataset")}
  
  # set cleaning prefix of Variables element
  # (addition of Variables:: before all variables except name)
  # names(data_dict[['Variables']])  <-
  #   make.unique(str_remove(names(data_dict[['Variables']]),"^Variables::"))
  # 
  # names(data_dict[['Variables']])[-
  #   which(names(data_dict[['Variables']])=='name')] <-
  #   paste0("Variables::",
  #          names(data_dict[['Variables']]))[-
  #             which(names(data_dict[['Variables']])=='name')]
  
  # set cleaning prefix of Categories element
  # (addition of Categories:: before all variables
  # except variable, name, labels and na_values)
  # if(has_categories(data_dict)){
  #   names(data_dict[['Categories']]) <-
  #     make.unique(str_remove(names(data_dict[['Categories']]),"^Categories::"))
  #   names(data_dict[['Categories']])[-
  #                                      which(names(data_dict[['Categories']]) %in%
  #                                              c('variable','name','labels', 'na_values'))] <-
  #     paste0("Categories::",
  #            names(data_dict[['Categories']])[-
  #                                               which(names(data_dict[['Categories']]) %in%
  #                                                       c('variable','name','labels','na_values'))])}
  
  names_data <- names(dataset)
  names_data_dict <- data_dict[['Variables']]$`name`
  
  for(i in names_data){
      # stop()}

    data_dict_vT <-
      data_dict[['Variables']][which(data_dict[['Variables']]$`name` == i),] %>%
      select(any_of(c('typeof','class'))) %>% as.vector() %>% unlist
    
    data_dict_vT <- typeof_convert_to_valueType(data_dict_vT[1],data_dict_vT[2])

    # apply the valueType of the data dictionary to the dataset  
    dataset[[i]] <- as_valueType(x = dataset[[i]],valueType = data_dict_vT)
  
    # preserve initial attributes and class
    attrs_init <- attributes(dataset[[i]])
    attrs_class <- list(
      class = unlist(str_split(attrs_init[['class']],pattern = " _; ")))
    attrs_init[['class']] <- NULL
       
    # add Variables attributes from the data dictionary
    attrs_var <-
      data_dict[['Variables']][which(data_dict[['Variables']]$`name` == i),]
    attrs_var <- attrs_var[vapply(X = attrs_var,
                                  FUN = function(x) !all(is.na(x)),
                                  FUN.VALUE = logical(1))]
    
    attrs_var <- c(attrs_var) %>% unlist %>% as.list()
    attrs_var[['name']]        <- NULL
    attrs_var[['typeof']]      <- NULL
    attrs_var[['class']]       <- NULL
    attrs_cat                  <- list()
    
    hasCat <- isTRUE(nrow(data_dict[['Categories']][
      which(data_dict[['Categories']][['variable']] == i),]) > 0)
    
    if(hasCat){
      
      attrs_cat_lab   <- list()
      attrs_cat_oth   <- list()
      
      # collect categorical attributes. add na_values to ensure its presence.
      cat_i <-
        data_dict[['Categories']][
          which(data_dict[['Categories']][['variable']] == i),]
      
      cat_i <- 
        cat_i[vapply(X = cat_i,
                     FUN = function(x) !all(is.na(x)),
                     FUN.VALUE = logical(1))]
      

        
      # create the vector for naming the attribute labels
      attrs_cat_lab <- as_valueType(x = cat_i$`name`, valueType = data_dict_vT)
      names(attrs_cat_lab) <- cat_i$`labels`
      attrs_cat_lab <- list(labels = attrs_cat_lab)
        

      # not used
      cat_i[['name']]      <- NULL
      cat_i[['labels']]    <- NULL
      cat_i[['variable']]  <- NULL
        
        
      if(ncol(cat_i) > 0) {
        for(j in seq_len(ncol(cat_i))){
          # stop()}
          vec_attr <- attrs_cat_lab$labels
          names(vec_attr) <-  cat_i[[j]]
          vec_attr <- vec_attr[which(!is.na(cat_i[[j]]))]
          
          attrs_cat_oth[[names(cat_i[j])]] <- vec_attr
        }}
      
      # assemble
      attrs_cat <- c(attrs_cat_lab,attrs_cat_oth)
      
        # change the class to apply the labelled class
      attrs_class <- 
        list(class = c("haven_labelled","vctrs_vctr",
                       vT_list[[which(vT_list$`valueType` == data_dict_vT),"explicit_class"]]))
  
    }
    
    attrs_total <- c(attrs_init,attrs_class,attrs_var, attrs_cat)
    attributes(dataset[[i]]) <- attrs_total
    
  }
  
  attributes(dataset)$`madshapR::Data dictionary` <- data_dict_init
  
  dataset <- 
    dataset %>%
    group_by(pick(any_of(preserve_group))) %>% 
    as_dataset(col_id = preserve_attributes)
  
  return(dataset)
}

#' @title
#' Generate a data dictionary from a dataset
#'
#' @description
#' Generates a data dictionary from a dataset. If the dataset variables have no 
#' associated metadata, a minimum data dictionary is created by using variable 
#' attributes.
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
#' The object may be specifically formatted to be compatible with additional 
#' [Maelstrom Research software](https://maelstrom-research.org/page/software), 
#' in particular [Opal environments](https://www.obiba.org/pages/products/opal/).
#'
#' @param dataset A dataset object.
#' @param as_data_dict_mlstr Whether the input data dictionary should be coerced 
#' with specific format restrictions for compatibility with other 
#' Maelstrom Research software. TRUE by default.
#'
#' @returns
#' A list of data frame(s) representing metadata of the dataset variables.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' ###### Example 1: use madshapR_examples provided by the package
#' # download a dataset and its data dictionary
#' # apply the data dictionary to its dataset
#' dataset   <- madshapR_examples$`dataset_example` 
#' data_dict <- as_data_dict_mlstr(madshapR_examples$`data_dictionary_example`)
#' dataset   <- data_dict_apply(dataset,data_dict)
#' 
#' # extract the data dictionary from the dataset
#' data_dict <- data_dict_extract(dataset)
#' 
#' glimpse(data_dict)
#' 
#' ###### Example 2: extract data dictionary from any dataset (the 
#' # data dictionary will be created upon attributes of the dataset. Factors 
#' # will be considered as categorical variables)
#' 
#' glimpse(data_dict)
#' 
#' }
#'
#' @import dplyr tidyr stringr fabR haven
#' @importFrom rlang .data
#'
#' @export
data_dict_extract <- function(dataset, as_data_dict_mlstr = TRUE){
  
  # test
  as_dataset(dataset) # no col_id
  if(!is.logical(as_data_dict_mlstr))
    stop(call. = FALSE,
         '`as_data_dict_mlstr` must be TRUE or FALSE (TRUE by default)')
  
  if(!is.null(attributes(dataset)$`madshapR::Data dictionary`)){
    return(attributes(dataset)$`madshapR::Data dictionary`)
  }
  
  grouping_vars  <- group_vars(dataset)
  dataset <- 
    dataset %>% ungroup %>%
    mutate(across(any_of(grouping_vars), as_category))
  
  data_dict <-
    list(
      Variables = tibble(name = as.character()),
      Categories = tibble(variable = as.character(), name = as.character()))
  
  for(i in names(dataset)){
    # stop()}
    
    attrs_i <- attributes(dataset[[i]])
    # a corriger plus tard pour la tzone
    # attrs_i[['tzone']] <- NULL
    attrs_i[['class']] <- NULL
    
    if(is.factor(dataset[[i]])){
      attrs_i$`labels` <- attrs_i$`levels`
      attrs_i$`levels` <- NULL
      names(attrs_i$`labels`) <- make.unique(attrs_i$`labels`)
    }
    
    vT_list <- madshapR::valueType_list
    
    # if(is.labelled(dataset[[i]])){
    #   attrs_i[['class']] <- 
    #     vT_list[[which(vT_list$`valueType` == valueType_of(dataset[[i]])),"class"]]
    #   if(is.na(attrs_i[['class']])) attrs_i[['class']] <- NULL  
    # }
    
    attrs_i[['class']] <-
      vT_list[[which(vT_list$`valueType` == valueType_of(dataset[[i]])),"class"]]
    if(is.na(attrs_i[['class']])) attrs_i[['class']] <- NULL

    attrs_i[['typeof']] <-
      vT_list[[which(vT_list$`valueType` == valueType_of(dataset[[i]])),"typeof"]]
      
    data_dict_var <- tibble(name = i)
    data_dict_cat <- tibble(variable = as.character())
    
    if(length(attrs_i) > 0){
      
      for(j in seq_len(length(attrs_i))){
        # stop()}
        
        attr_col_name     <- attrs_i[j] %>% names
        attr_content_col  <- attrs_i[[j]] %>% names
        attr_name_cat     <- attrs_i[[j]] %>% as.character()
        
        if(is.null(attr_content_col)) {
          data_dict_var[attr_col_name] <-
            paste0(attr_name_cat,collapse = " _; ")
        }else{
          cat_attr <- tibble(variable = i, name = attr_name_cat)
          cat_attr[[attr_col_name]] <- attr_content_col
          data_dict_cat <- 
            data_dict_cat %>%
            full_join(cat_attr,
                      by = intersect(names(data_dict_cat),names(cat_attr)))
        }
      }
    }
    
    
    data_dict[['Variables']]  <-
      data_dict[['Variables']]  %>% bind_rows(data_dict_var)
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>% bind_rows(data_dict_cat)
    
  }

  # names(data_dict[['Variables']])  <-
  #   make.unique(str_remove(names(data_dict[['Variables']]),"^Variables::"))
  # names(data_dict[['Categories']]) <-
  #   make.unique(str_remove(names(data_dict[['Categories']]),"^Categories::"))
  # 
  
  if(!has_categories(data_dict)) data_dict[['Categories']] <- NULL
  
  # if(is.null(data_dict$`Variables`[['valueType']]) &
  #    is.null(data_dict$`Variables`[['typeof']])){
  #   
  #   data_dict <-  silently_run(valueType_adjust(from = dataset, to = data_dict))
  # 
  # }
  
  
  # if(is.null(data_dict$`Variables`[['valueType']])){
  #   data_dict$`Variables`[['valueType']] <- NULLNA
  #   data_dict <- valueType_self_adjust(data_dict)
  # }
  
  if(!as_data_dict_mlstr) data_dict <- as_data_dict(data_dict)
  if( as_data_dict_mlstr) data_dict <- as_data_dict_mlstr(data_dict)
  
  return(data_dict)
}

#' @title
#' Inner join between a dataset and its associated data dictionary
#'
#' @description
#' Performs an inner join between a dataset and its associated data dictionary, 
#' keeping only variables present in both. This function returns the matched
#' dataset rows, the matched data dictionary rows, or both, in a list.
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
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset.
#' @param data_dict_apply Whether data dictionary(ies) should be applied to 
#' associated dataset(s), creating labelled dataset(s) with variable attributes. 
#' Any previous attributes will be preserved. FALSE by default.
#' @param output A vector of character string which indicates if the function
#' returns a dataset ('dataset'), data dictionary ('data_dict') of both.
#' Default is c('dataset','data_dict').
#'
#' @returns
#' Either a data frame, identifying the dataset, or a list of data frame(s)
#' identifying a data dictionary. Returns both in a list by default.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' # use madshapR_examples provided by the package
#' dataset <- madshapR_examples$`dataset_example`
#' data_dict <- madshapR_examples$`data_dictionary_example - errors`
#' 
#' match_data_dict <- data_dict_match_dataset(dataset,data_dict,output = 'data_dict')
#' match_dataset <- data_dict_match_dataset(dataset,data_dict,output = 'dataset')
#' 
#' head(match_data_dict)
#' glimpse(match_dataset)
#' 
#' }
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
data_dict_match_dataset <- function(
    dataset,
    data_dict,
    data_dict_apply = FALSE,
    output = c("dataset","data_dict")){
  
  # test
  as_data_dict_shape(data_dict)
  if(!is.logical(data_dict_apply))
    stop(call. = FALSE,
         '`data_dict_apply` must be TRUE of FALSE (FALSE by default)')

  # preserve dataset
  as_dataset(dataset) # no col_id
  preserve_attributes <- col_id(dataset)
  preserve_group <- group_vars(dataset)
  dataset <- as_dataset(ungroup(dataset))
  
  names_data <-
    paste0("name %in% c('",paste0(names(dataset),collapse = "','"),"')")
  data_dict <- data_dict_filter(data_dict, filter_var = names_data)
  dataset <- dataset %>% select(data_dict[['Variables']]$`name`)
  
  if(length(dataset) == 0)
    warning("No match found between dataset and data dictionary")
  
  if(data_dict_apply == TRUE){
    
    if(toString(attributes(data_dict)$`madshapR::class`) == "data_dict_mlstr"){
      data_dict <- 
        as_data_dict_mlstr(data_dict)
    }else{
      data_dict <- as_data_dict(data_dict)}
    
    dataset <- data_dict_apply(dataset, data_dict)
  }

  dataset <- 
    dataset %>%
    group_by(pick(any_of(preserve_group))) %>% 
    as_dataset(col_id = preserve_attributes)
  
  if(all(output[2:1] %in% c("dataset","data_dict")))
    return(list(dataset = dataset, data_dict = data_dict))
  
  if(output[1] == c("dataset"))
    return(dataset)
  
  if(output[1] == c("data_dict"))
    return(data_dict)
  
  # else
  stop(call. = FALSE,
       "`output` parameter must be either 'dataset' or 'data_dict'.
Leave blank to get both in a list.")
}

#' @title
#' Validate and coerce any object as a workable data dictionary structure
#'
#' @description
#' Validates the input object as a workable data dictionary structure and 
#' returns it with the appropriate `madshapR::class` attribute. This function 
#' mainly helps validate input within other functions of the package but could 
#' be used to check if a data dictionary is valid for use in a function.
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
#' @seealso
#' For a better assessment, please use [data_dict_evaluate()].
#'
#' @param object A potential valid data dictionary to be coerced.
#'
#' @returns
#' A list of data frame(s) with `madshapR::class` 'data_dict_shape'.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' # use madshapR_examples provided by the package
#' data_dict <- madshapR_examples$`data_dictionary_example`
#' data_dict <- as_data_dict_shape(data_dict)
#' glimpse(data_dict)
#'
#'}
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
as_data_dict_shape <- function(object){
  
  # if the Variables sheet is input in parameter
  if(sum(names(object) %in% c('Variables')) == 1 & !is.data.frame(object)){
    
    # name column must exist
    if(is.null(object[['Variables']][['name']])){
      stop(call. = FALSE,
           "Column 'name' in 'Variables' is missing in your data dictionary.")}
    
    # if Categories exists
    if(!is.null(object[['Categories']])){
      
      #, variable column must exist
      if(is.null(object[['Categories']][['variable']])){
        stop(call. = FALSE,
             "Column 'variable' in 'Categories' is missing in your data dictionary.")}
      
      #, name column must exist
      if(is.null(object[['Categories']][['name']])){
        stop(call. = FALSE,
             "Column 'name' in 'Categories' is missing in your data dictionary.")}
    }
    
    # if all test pass:
    attributes(object)$`madshapR::class` <- "data_dict_structure"
    return(object)
    
  }
  
  # else
  stop(call. = FALSE,
       "\n\nThis object is not a data dictionary as defined by the package, 
which must be a list containing at least 'Variables' list of elements.
Please refer to documentation.")
  
}

#' @title
#' Validate and coerce any object as a data dictionary
#'
#' @description
#' Checks if an object is a valid data dictionary and returns it with the 
#' appropriate `madshapR::class` attribute. This function mainly helps validate 
#' inputs within other functions of the package but could be used to check if 
#' an object is valid for use in a function. If either the columns 'typeof' or
#' 'class' already exists in 'Variables', or 'na_values', 'labels' in 
#' 'Categories', the function will return the same data dictionary. Otherwise, 
#' These columns will be added, using 'valueType' in 'Variables', and, 'label'  
#' and 'missing' in 'Categories.
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
#' @seealso
#' For a better assessment, please use [data_dict_evaluate()].
#'
#' @param object A potential data dictionary object to be coerced.
#'
#' @returns
#' A list of data frame(s) with `madshapR::class` 'data_dict'.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' # use madshapR_examples provided by the package
#' ###### Example 1 : use the function to apply the attribute "data_dict" to the 
#' # object. 
#' data_dict <- 
#'   as_data_dict(madshapR_examples$`data_dictionary_example - as_data_dict`)
#' 
#' glimpse(data_dict)
#' 
#' ###### Example 2 : use the function to shape the data dictionary formatted as
#' # data_dict_mlstr to data_dict object. The function mainly converts valueType 
#' # column into corresponding typeof/class columns in 'Variables', and converts
#' # missing column into "na_values" column. 
#' data_dict <- as_data_dict_mlstr(madshapR_examples$`data_dictionary_example`)
#' data_dict <- as_data_dict(data_dict)
#' 
#' glimpse(data_dict)
#'
#'}
#'
#' @import dplyr tidyr stringr fabR
#' @importFrom crayon bold
#' @importFrom rlang .data
#' 
#' @export
as_data_dict <- function(object){
  
  is_data_dict_mlstr <- FALSE
  if(toString(attributes(object)$`madshapR::class`) == 'data_dict_mlstr'){
    
    is_data_dict_mlstr <- all(
      silently_run(check_data_dict_categories(object)) %>% 
        dplyr::filter(str_detect(.data$`condition`,"\\[ERROR\\]")) %>% nrow == 0,
      silently_run(check_data_dict_missing_categories(object)) %>% 
        dplyr::filter(str_detect(.data$`condition`,"\\[ERROR\\]")) %>% nrow == 0,
      silently_run(check_data_dict_valueType(object)) %>% 
        dplyr::filter(str_detect(.data$`condition`,"\\[ERROR\\]")) %>% nrow == 0)
  }
  
  # test if element is enough data_dict
  data_dict <- as_data_dict_shape(object)
  
  # variable names must be unique and non-null
  if(check_data_dict_variables(data_dict) %>% 
     dplyr::filter(str_detect(.data$`condition`,"\\[ERROR\\]")) %>% nrow > 0){
    stop(call. = FALSE,
"Variable names must exist and be unique in your data dictionary.",
bold("\n\nUseful tip:"),
" Use data_dict_evaluate(data_dict) to get a full assessment of your
data dictionary")}
  
  # variable names must exist in categories
  if(has_categories(data_dict)){
    if(check_data_dict_categories(data_dict) %>% 
       dplyr::filter(str_detect(.data$`condition`,"\\[ERROR\\]")) %>% nrow > 0){
      stop(call. = FALSE,
"Variable names in categories must exist and be unique in the data dictionary.",
bold("\n\nUseful tip:"),
" Use data_dict_evaluate(data_dict) to get a full assessment of your
data dictionary")}}
  
  if(nrow(data_dict[['Variables']]) == 0){
    data_dict <-
      list(Variables = tibble(name = as.character(),typeof = as.character()))
    attributes(data_dict)$`madshapR::class` <- "data_dict"
    return(data_dict) }
  
  # data_dict shaping
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    ungroup() %>%
    mutate(across(everything() ,~str_squish(.))) %>%
    mutate(across(everything() ,~na_if(.,"")))

  # add label(:xx) if not present
  first_lab_var <- first_label_get(data_dict)[['Variables']]
  
  # add label if does not exists
  if(first_lab_var == ""){
    data_dict[['Variables']] <-
      data_dict[['Variables']] %>%
      mutate(`label` = .data$`name`) %>%
      select(1:"name","label",everything())
    
    first_lab_var <- "label"
  }
  
  # fill labels if some are empty
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>% 
    mutate(across(any_of(first_lab_var), ~ ifelse(is.na(.),.data$`name`,.)))
  
  if(has_categories(data_dict)){
    
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      ungroup #%>%
      # mutate(across(everything(),~str_squish(.))) %>%
      # mutate(across(everything(),~na_if(.,"")))
    
    if(is_data_dict_mlstr){
      
      # add label(:xx) if not present
      first_lab_var <- first_label_get(data_dict)[['Variables']]
      
      # add label if does not exists in categories
      if(!(first_lab_var %in% names(data_dict[['Categories']]))){
        data_dict[['Categories']] <-
          data_dict[['Categories']] %>% 
          mutate(`madshapR::label` = .data$`name`) %>%
          rename_with(.cols = "madshapR::label",~ first_lab_var) %>%
          select(1:"name",any_of(first_lab_var),everything())}
      
      # fill labels if some are empty
      data_dict[['Categories']] <-
        data_dict[['Categories']] %>% 
        mutate(across(any_of(first_lab_var), ~ ifelse(is.na(.),.data$`name`,.)))
      
      # rename the column as 'labels'
      data_dict[['Categories']] <- 
        data_dict[['Categories']] %>%
        select(-any_of("labels")) %>%
        rename_with(.cols = !! first_lab_var,.fn = ~ "labels")

    }
    
    # in the case of not mlstr, add labels if not exists, and fill the blanks
    # in categories
    if(!("labels" %in% names(data_dict[['Categories']]))){
      data_dict[['Categories']] <-
        data_dict[['Categories']] %>%
        mutate(`labels` = .data$`name`) %>%
        select(1:"name","labels",everything())}
      
    # fill labels if some are empty
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>% 
      mutate(across(any_of("labels"), ~ ifelse(is.na(.),.data$`name`,.)))
  
  }else{ 
    data_dict[['Categories']] <- NULL }

  ## VARIABLES ##
  
  # index: if exists, check if distinct integers and reorder.
  test_index <- data_dict[['Variables']][['index']]
  valid_index_0 <- !is.null(test_index) 
  valid_index_1 <- class(silently_run(as_any_integer(test_index)))[[1]] == "integer"
  valid_index_2 <- isTRUE(all(unique(test_index) %in% test_index))
  
  if(all(valid_index_0, valid_index_1,valid_index_2)){
    data_dict[['Variables']] <- 
      data_dict[['Variables']] %>%
      mutate(index = as_any_integer(.data$`index`)) %>%
      arrange(.data$`index`)
  }

  # if the data_dict is Maelstrom, then use the valueType (if exists), and discard it
  # if not exists, addition of typeof for categorical variables, text else.
  if(is_data_dict_mlstr){
    
      data_dict[['Variables']] <-
        data_dict[['Variables']] %>%
        bind_rows(tibble(valueType = as.character())) %>%
        rowwise() %>%
        mutate(typeof = valueType_convert_to_typeof(.data$`valueType`)[["typeof"]]) %>%
        mutate(class  = valueType_convert_to_typeof(.data$`valueType`)[["class"]]) %>%
        ungroup %>%
        select(1:"valueType","typeof","class",everything(),-"valueType")

    }

  # if not mlstr, transform typeof and class into valueType
  # if no typeof, look through categories to create the according typeof/class
  # columns
  
  if(has_categories(data_dict) & 
     length(data_dict[['Variables']][['typeof']]) == 0){
    
    category_outcomes <-
      data_dict[['Categories']] %>%
      select('name') %>% distinct %>%
      rowwise() %>%
      mutate('madshapR::valueType' = valueType_guess(.data$`name`))
    
    category_outcomes <-
      data_dict[['Categories']] %>%
      select('variable','name') %>%
      left_join(category_outcomes, by = "name") %>%
      select('variable','madshapR::valueType') %>%
      distinct %>%
      group_by(.data$`variable`) %>%
      reframe('madshapR::valueType' = 
              paste0(.data$`madshapR::valueType`,collapse = "|"))
    
    category_outcomes <-
      data_dict[['Categories']] %>%
      select('variable','name') %>%
      left_join(category_outcomes, by = "variable") %>%
      group_by(.data$`variable`) %>% group_split() %>% as.list() %>%
      lapply(function(x){
        test_vT <- str_detect(x$`madshapR::valueType`[1], "\\|")
        if(test_vT) x <- x %>% mutate(
          'madshapR::valueType' = valueType_guess(unique(x$`name`)))
        return(x) }) %>%
      bind_rows() %>%
      select(name = 'variable','madshapR::valueType') %>%
      distinct
    
    data_dict[['Variables']] <-
      left_join(data_dict[['Variables']],category_outcomes,by = "name") %>%
      rowwise %>%
      mutate(typeof = valueType_convert_to_typeof(.data$`madshapR::valueType`)[['typeof']]) %>%
      mutate(class  = paste0(valueType_convert_to_typeof(.data$`madshapR::valueType`)[['class']],collapse = " _; ")) %>%
      mutate(across(c('class'), ~ na_if(.,'NA'))) %>%
      mutate(across(c('typeof'), ~ replace_na(.,'character'))) %>%
      ungroup %>%
      select(-"madshapR::valueType")
    
  }
  
  # if no information provided to create typeof, then character
  if(length(data_dict[['Variables']][['typeof']]) == 0){
    data_dict[['Variables']][['typeof']] <- 'character'
    data_dict[['Variables']][['class']]  <- NULL}
  
  data_dict[['Variables']] <- 
    data_dict[['Variables']] %>%
    select(1:"typeof",any_of("class"),everything())
  
  if(all(is.na(data_dict[['Variables']][['class']])))
    data_dict[['Variables']][['class']]  <- NULL
  
  # the Variables sheet is done at this step.
  # If no categories, return the data dict as it is.
  if(!has_categories(data_dict)){
    attributes(data_dict)$`madshapR::class` <- "data_dict"
    return(data_dict)}
  
  ## CATEGORIES ##
  # create a temporary valueType for sorting elements in Categories
  
  corres_vT <-
    data_dict[['Variables']] %>%
    select('name', 'typeof',any_of("class")) %>%
    bind_rows(tibble(class = as.character())) %>%
    rowwise() %>%
    mutate(`madshapR::valueType` = typeof_convert_to_valueType(.data$`typeof`,.data$`class`)) %>%
    ungroup %>%
    select("variable" = 'name',"madshapR::valueType") %>%
    rowwise() %>%                # [GF] NOTE : rowwise
    dplyr::filter(.data$`variable` %in% data_dict[['Categories']][["variable"]]) %>%
    ungroup
  
  # if mlstr, for missing, turn into boolean, or create if not exists
  if(is_data_dict_mlstr){
    
    if(length(data_dict[['Categories']][['missing']]) > 0){
      
      data_dict[['Categories']][['missing']] <- 
        as_any_boolean(data_dict[['Categories']][['missing']])
      
      data_dict[['Categories']] <-
        data_dict[['Categories']] %>%
        mutate(`missing` = replace_na(.data$`missing`,FALSE))
      
    } else data_dict[['Categories']][['missing']] <- FALSE
  }
  
  # for categories, reoder elements according list of variables declared
  data_dict[['Categories']] <-
    data_dict[['Categories']] %>%
    left_join(corres_vT %>% add_index("madshapR::index", .force = TRUE),by = "variable") %>%
    group_by(.data$`madshapR::valueType`) %>% group_split() %>% as.list %>%
    lapply(function(x) {
      test_name <- as_valueType(x$`name`, unique(x$`madshapR::valueType`))
      
      if(all(as.character(test_name) %in% as.character(x$`name`))){
        x$`name` <- test_name}
      
      if(is_data_dict_mlstr){
        x <- x %>% arrange( .data$`missing`, .data$`variable`, .data$`name`) 
      } else x <- x %>% arrange(.data$`variable`, .data$`name`)
      
      x <- x %>% mutate(name = as.character(.data$`name`))
      
      return(x)}) %>% 
    bind_rows() %>%
    arrange(.data$`madshapR::index`) %>%
    select(-c('madshapR::valueType',"madshapR::index"))
  
  if(is_data_dict_mlstr){
    
    data_dict[["Categories"]] <-
      data_dict[["Categories"]] %>%
      rowwise() %>%
      mutate(
        na_values = ifelse(isTRUE(.data$`missing`),.data$`labels`,NA_character_)) %>%
      ungroup %>%
      select(1:'missing',"na_values",everything(),-"missing")
    
    if(all(is.na(data_dict[["Categories"]][["na_values"]]))){
      data_dict[["Categories"]][["na_values"]] <- NULL
    }}
  
  # if all test pass:
  if(all(is.na(data_dict[['Variables']][['class']])))
    data_dict[['Variables']][['class']]  <- NULL

  if(all(is.na(data_dict[['Categories']][['na_values']])))
    data_dict[['Categories']][['na_values']]  <- NULL

  attributes(data_dict)$`madshapR::class` <- "data_dict"
  return(data_dict)
}

#' @title
#' Validate and coerce any object as an Opal data dictionary format
#'
#' @description
#' Validates the input object as a valid data dictionary compliant with formats 
#' used in Maelstrom Research ecosystem, including Opal, and returns it with 
#' the appropriate `madshapR::class` attribute. This function mainly helps 
#' validate input within other functions of the package but could be used to 
#' check if an object is valid for use in a function.
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
#' The object may be specifically formatted to be compatible with additional 
#' [Maelstrom Research software](https://maelstrom-research.org/page/software), 
#' in particular [Opal environments](https://www.obiba.org/pages/products/opal/).
#'
#' @seealso
#' For a better assessment, please use [data_dict_evaluate()].
#'
#' @param object A potential valid data dictionary to be coerced.
#' @param name_standard Whether the input data dictionary has variable names
#' compatible with Maelstrom Research ecosystem, including Opal)or not. 
#' FALSE by default.
#'
#' @returns
#' A list of data frame(s) with `madshapR::class` 'data_dict_mlstr'.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' ###### Example 1 : use the function to apply the attribute "data_dict" to the 
#' # object. 
#' data_dict <- 
#'   as_data_dict_mlstr(madshapR_examples$`data_dictionary_example`)
#' 
#' glimpse(data_dict)
#' 
#' ###### Example 2 : use the function to shape the data dictionary formatted as
#' # data_dict_mlstr to data_dict object. The function mainly converts valueType 
#' # column into corresponding typeof/class columns in 'Variables', and converts
#' # missing column into "na_values" column. 
#' data_dict <- 
#'   as_data_dict_mlstr(madshapR_examples$`data_dictionary_example - as_data_dict`)
#'   
#' glimpse(data_dict)
#'
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
as_data_dict_mlstr <- function(object, name_standard = FALSE){

  # test if data_dict is enougth a data dictionary shape
  data_dict <- as_data_dict_shape(object)
  
  # if valueType exists, vT must be valid
  if(suppressWarnings(check_data_dict_valueType(data_dict))  %>%
     dplyr::filter(str_detect(.data$`condition`,"\\[ERROR\\]")) %>% nrow > 0){
    stop(call. = FALSE,
"ValueType is not an accepted type (see ??valueType_list for complete list).",
bold("\n\nUseful tip:"),
" Use data_dict_evaluate(data_dict) to get a full assessment of your
data dictionary")}
  
  # check missing validity
  if(suppressWarnings(check_data_dict_missing_categories(data_dict)) %>% 
     dplyr::filter(str_detect(.data$`condition`,"\\[ERROR\\]")) %>% nrow > 0){
    stop(call. = FALSE,
"\n
Values in 'missing' column (in Categories) are non-Boolean",
bold("\n\nUseful tip:"),
" Use data_dict_evaluate(data_dict) to get a full assessment of your
data dictionary")}
  
  # Check standard for names
  if(name_standard == TRUE){
    if(nrow(check_name_standards(data_dict[['Variables']][['name']])) > 0){
      stop(call. = FALSE,
"Variable names contain special characters, contain spaces, or begin with a number.",
bold("\n\nUseful tip:"),
" Use data_dict_evaluate(data_dict) to get a full assessment of your
data dictionary")}
  }
  
  # test if data_dict is enougth a data dictionary
  data_dict <- as_data_dict(object = data_dict)
  
  # # if valueType already exists, return as it is.
  # if(length(data_dict[['Variables']][['valueType']]) > 0){
  #   attributes(data_dict)$`madshapR::class` <- "data_dict_mlstr"
  #   return(data_dict) }
  # 
  # # if no information provided to create typeof, then character
  # if(!(all(c("class","typeof") %in% names(data_dict[['Variables']])))){
  #   data_dict[['Variables']][['typeof']] <- 'character'
  #   data_dict[['Variables']][['class']]  <- 'character'}
  # 
  # # if no typeof, this step is identical to as_data_dict (init)
  # if(has_categories(data_dict) & 
  #    !(all(c("class","typeof") %in% names(data_dict[['Variables']])))){
  #   
  #   category_outcomes <-
  #     data_dict[['Categories']] %>%
  #     select('name') %>% distinct %>%
  #     rowwise() %>%
  #     mutate('madshapR::valueType' = valueType_guess(.data$`name`))
  #   
  #   category_outcomes <-
  #     data_dict[['Categories']] %>%
  #     select('variable','name') %>%
  #     left_join(category_outcomes, by = "name") %>%
  #     select('variable','madshapR::valueType') %>%
  #     distinct %>%
  #     group_by(.data$`variable`) %>%
  #     reframe('madshapR::valueType' = 
  #               paste0(.data$`madshapR::valueType`,collapse = "|"))
  #   
  #   category_outcomes <-
  #     data_dict[['Categories']] %>%
  #     select('variable','name') %>%
  #     left_join(category_outcomes, by = "variable") %>%
  #     group_by(.data$`variable`) %>% group_split() %>% as.list() %>%
  #     lapply(function(x){
  #       test_vT <- str_detect(x$`madshapR::valueType`[1], "\\|")
  #       if(test_vT) x <- x %>% mutate(
  #         valueType = valueType_guess(unique(x$`name`)))
  #       return(x) }) %>%
  #     bind_rows() %>%
  #     select(name = 'variable','madshapR::valueType') %>%
  #     distinct
  #   
  #   data_dict[['Variables']] <-
  #     left_join(data_dict[['Variables']],category_outcomes,by = "name") %>%
  #     rowwise() %>%
  #     mutate(typeof = valueType_convert_to_typeof(.data$`madshapR::valueType`)[[1]]) %>%
  #     mutate(class  = valueType_convert_to_typeof(.data$`madshapR::valueType`)[[2]]) %>%
  #     ungroup %>%
  #     select(-"madshapR::valueType") %>%
  #     mutate(across(c('typeof','class'), ~ replace_na(.,'character')))
  #   
  # }
  # 
  # # if no information provided to create typeof, then character
  # if(!(all(c("class","typeof") %in% names(data_dict[['Variables']])))){
  #   data_dict[['Variables']][['typeof']] <- 'character'
  #   data_dict[['Variables']][['class']]  <- 'character'}
  # 
  # # (end)


  # test if the typeof is usable to generate valueType

  test_vT <- tibble(name = as.character(),`madshapR::valueType` = as.character())
  for(i in seq_along(data_dict[['Variables']][['name']])){
    
    data_dict[['Variables']] <- 
      data_dict[['Variables']] %>%
      bind_rows(tibble(class = as.character()))
    
    test <- silently_run(
      typeof_convert_to_valueType(
        data_dict[['Variables']][['typeof']][i],
        data_dict[['Variables']][['class']][i]))
    
    test_vT <-
      test_vT %>%
      bind_rows(
        tibble(
          name = data_dict[['Variables']][['name']][i],
          `madshapR::valueType` = ifelse(class(test)[[1]] == "try-error",NA_character_,test)))
  }

  # On rajoute vT au dd
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    bind_rows(tibble(valueType = as.character(),typeof = as.character())) %>%
    left_join(
      test_vT %>% select('name','madshapR::valueType'), 
      by = 'name') %>%
    mutate(valueType = if_else(
      is.na(.data$`valueType`),.data$`madshapR::valueType`,.data$`valueType`)) %>%
    # mutate(across(c('valueType'), ~ replace_na(.,'character'))) %>%
    select(1:"typeof","valueType",everything(),-"madshapR::valueType")
  
  test_vT = is.na(data_dict[['Variables']][['valueType']])
  if(!all(!test_vT)){
    warning(
      "The column 'typeof' in your data dictionary contains values that were
impossible to coerce in valueType. This column has been kept for further
investigations.",
      "\n\nVariable(s) name : ",
      toString(data_dict[['Variables']][['name']][test_vT]))
  }else{
    data_dict[['Variables']][['typeof']] <- NULL
    data_dict[['Variables']][['class']] <- NULL
  }

  if(has_categories(data_dict)){
    
    # addition of label(:xx) if not present
    first_lab_var <- first_label_get(data_dict)[['Variables']]
# 
#     # if label(:xx) already exists, return as it is.
#     if(length(data_dict[['Categories']][[first_lab_var]]) > 0){
#       attributes(data_dict)$`madshapR::class` <- "data_dict_mlstr"
#       return(data_dict)}
     
    # rename the column as 'labels
    
    if(length(data_dict[['Categories']][[first_lab_var]]) > 0){
      data_dict[['Categories']] <-
        data_dict[['Categories']] %>%
        mutate(labels = !! as.name(first_lab_var)) %>%
        select(1:any_of(first_lab_var),"labels",everything(),-any_of(first_lab_var))
    }
    
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      rename_with(.cols = "labels", ~ first_lab_var) %>%
      mutate(across(any_of(first_lab_var), ~ ifelse(is.na(.),.data$`name`,.)))
    
    # addition of missing if not present
    # if missing already exists, return as it is.
    # check presence of na_values, if identical to label, NULL, rename else
    
    if(length(data_dict[['Categories']][['missing']]) == 0){

      if(length(data_dict[['Categories']][['na_values']]) == 0){
        
        data_dict[['Categories']]$`missing` <- FALSE
        
      }else if(all(data_dict[['Categories']][['na_values']] ==
                   data_dict[['Categories']][[first_lab_var]] ,na.rm = TRUE)){
        
        data_dict[['Categories']]$`missing` <- 
          !is.na(data_dict[['Categories']]$`na_values`)
        
        data_dict[['Categories']] <- 
          data_dict[['Categories']] %>%
          select(1:'na_values','missing',everything(),-"na_values")
        
      }else{
        
        warning(
"The column 'na_values' in your data dictionary contains values that were
impossible to coerce in missing. This column has been kept for further
investigations.")
        
        data_dict[['Categories']]$`missing` <- FALSE
        data_dict[['Categories']] <- 
          data_dict[['Categories']] %>%
          select(1:'na_values','missing',everything())
      }
    }
    
    data_dict[['Categories']][['missing']] <- 
      as_any_boolean(data_dict[['Categories']][['missing']])
  
#   if(as_data_dict == TRUE){
#     
#     # # check if label and name are duplicated
#     # if(length(data_dict[['Variables']][['label']]) > 0){
#     #   if(all(data_dict[['Variables']][['name']] ==
#     #          data_dict[['Variables']][['label']])){
#     #     data_dict[['Variables']]$`label` <- NULL}}
#     
#     # valueType as typeof
#     if(length(data_dict[['Variables']][['typeof']]) == 0){
#       
#       data_dict[['Variables']] <-
#         data_dict[['Variables']] %>%
#         left_join(
#           madshapR::valueType_list %>%
#             select(
#               valueType = "valueType",
#               typeof = "typeof") %>%
#             distinct,
#           by = "valueType") %>%
#         select(-"valueType")}
#     
#     if(has_categories(data_dict)){
#       
#       # protection of labels if already exists
#       if(length(data_dict[['Categories']][['labels']]) > 0){
#         new_name <-
#           setdiff(
#             make.unique(c('labels',names(data_dict[['Categories']])))[-1],
#             names(data_dict[['Categories']]))
#         
#         warning(
#           "The data dictionary contains 'labels' column, which usage is protected in R.
# new name: ",new_name)
#         
#         names(data_dict[['Categories']]) <-
#           make.unique(c('labels',names(data_dict[['Categories']])))[-1]
#       }
#       
#       data_dict[['Categories']] <-
#         data_dict[['Categories']] %>%
#         rename_with(.cols = starts_with("label")[1], ~ 'labels')
#       
#       # protection of na_values if already exists
#       if(length(data_dict[['Categories']][['na_values']]) > 0){
#         new_name <-
#           setdiff(
#             make.unique(c('na_values',names(data_dict[['Categories']])))[-1],
#             names(data_dict[['Categories']]))
#         
#         warning(
#           "The data dictionary contains 'na_values' column, which usage is protected in R.
# New name: ",new_name)
#         
#         names(data_dict[['Categories']]) <-
#           make.unique(c('na_values',names(data_dict[['Categories']])))[-1]
#       }
#       
#       data_dict[['Categories']] <-
#         data_dict[['Categories']] %>%
#         rename_with(.cols = "missing", ~ 'na_values') %>%
#         mutate(
#           na_values =
#             ifelse(.data$`na_values` == TRUE,.data$`labels`, NA_character_))
#       data_dict[['Categories']] <-
#         data_dict[['Categories']] %>%
#         select(
#           'variable',
#           'name',
#           'labels',
#           'na_values',
#           everything())
#     }
#     
#     data_dict[['Variables']] <-
#       data_dict[['Variables']] %>%
#       select('name','typeof',everything())
#     
#   }
    
    # identical to as_data_dict (init)
    # create a temporary valueType for sorting elements in Categories
    corres_vT <-
      data_dict[['Variables']] %>%
      select("variable" = 'name', 'madshapR::valueType' = "valueType") %>%
      rowwise() %>%                # [GF] NOTE : rowwise
      dplyr::filter(.data$`variable` %in% data_dict[['Categories']][["variable"]]) %>%
      ungroup
  
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      left_join(corres_vT %>% add_index("madshapR::index", .force = TRUE),by = "variable") %>%
      group_by(.data$`madshapR::valueType`) %>% group_split() %>% as.list %>%
      lapply(function(x) {
        test_name <- as_valueType(x$`name`, unique(x$`madshapR::valueType`))
        
        if(all(as.character(test_name) == as.character(x$`name`))){
          x$`name` <- test_name}
        
        x <- x %>% arrange( .data$`missing`, .data$`variable`, .data$`name`) 
        x <- x %>% mutate(name = as.character(.data$`name`))
        
        return(x)}) %>% 
      bind_rows() %>%
      arrange(.data$`madshapR::index`) %>%
      select(-c('madshapR::valueType',"madshapR::index"))
    
    # identical to as_data_dict (end)
    
  }
    
  # if all tests pass
  attributes(data_dict)$`madshapR::class` <- "data_dict_mlstr"
  
  return(data_dict)
}

#' @title
#' Test if an object is a workable data dictionary structure
#'
#' @description
#' Tests if the input object has adequate structure to work with functions 
#' involving data dictionary shaping. This function mainly helps validate input 
#' within other functions of the package but could be used to check if an 
#' object is valid for use in a function.
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
#' @seealso
#' For a better assessment, please use [data_dict_evaluate()].
#'
#' @param object A potential data dictionary structure to be evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#' 
#' # use madshapR_examples provided by the package
#' is_data_dict_shape(madshapR_examples$`data_dictionary_example - errors`)
#' is_data_dict_shape(madshapR_examples$`data_dictionary_example - errors with data`)
#' is_data_dict_shape(madshapR_examples$`data_dictionary_example`)
#' is_data_dict_shape(iris)
#'
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
is_data_dict_shape <- function(object){
  
  # if only the data dictionary shape is given in parameter
  test <- silently_run(try(as_data_dict_shape(object),silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)
}

#' @title
#' Test if an object is a valid data dictionary
#'
#' @description
#' Tests if the input object is a valid data dictionary. This function mainly 
#' helps validate input within other functions of the package but could be used 
#' to check if an object is valid for use in a function.
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
#' @seealso
#' For a better assessment, please use [data_dict_evaluate()].
#'
#' @param object A potential data dictionary to be evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#' 
#' # use madshapR_examples provided by the package
#' is_data_dict(madshapR_examples$`data_dictionary_example - errors`)
#' is_data_dict(madshapR_examples$`data_dictionary_example - errors with data`)
#' is_data_dict(madshapR_examples$`data_dictionary_example`)
#' is_data_dict(iris)
#'
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
is_data_dict <- function(object){
  
  object <- object
  # if only the data frame is given in parameter
  test <- silently_run(try(as_data_dict(object),silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)
  
}

#' @title
#' Test if an object is a valid Maelstrom data dictionary
#'
#' @description
#' Tests if the input object is a valid data dictionary compliant with formats 
#' used in Maelstrom Research ecosystem, including Opal. This function mainly 
#' helps validate input within other functions of the package but could be used 
#' to check if an object is valid for use in a function.
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
#' @seealso
#' For a better assessment, please use [data_dict_evaluate()].
#'
#' @param object A potential Maelstrom formatted data dictionary to be
#' evaluated.
#'
#' @returns
#' A logical.
#'
#' @examples
#' {
#' 
#' # use madshapR_examples provided by the package
#' is_data_dict_mlstr(madshapR_examples$`data_dictionary_example - errors`)
#' is_data_dict_mlstr(madshapR_examples$`data_dictionary_example - errors with data`)
#' is_data_dict_mlstr(madshapR_examples$`data_dictionary_example`)
#' is_data_dict_mlstr(iris)
#'
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom rlang .data
#'
#' @export
is_data_dict_mlstr <- function(object){
  
  object <- object
  # if only the data frame is given in parameter
  test <- silently_run(try(as_data_dict_mlstr(object),silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)
}
