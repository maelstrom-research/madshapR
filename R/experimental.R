#' @title
#' Validate and coerce any object as a categorical variable.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Converts a vector object to a categorical object, typically a column in a 
#' data frame.
#'
#' @param x A vector object to be coerced to categorical.
#' @param labels An optional vector of the unique values (as character strings) 
#' that x might have taken. The default is the unique set of values taken by 
#' as.character(x), sorted into increasing order of x. Note that this set can be 
#' specified as smaller than sort(unique(x)).
#' @param na_values An optional vector of the unique values (as character strings) 
#' among labels, for which the value is considered as missing. The default 
#' is NULL. Note that this set can be specified as smaller than labels.
#' @param as_factor Whether the output is a categorical variable (haven labelled
#' object) or is a factor (labels and na_values will be lost, but the order of
#' the levels will be preserved). FALSE by default.
#'
#' @seealso
#' [haven::labelled()]
#'
#' @return
#' A vector with class haven_labelled.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' ##### Example 1: use madshapR_example provided by the package
#' dataset <-
#'   madshapR_example$`dataset_example` %>%
#'   mutate(prg_ever = as_category(prg_ever))
#'   
#' head(dataset$prg_ever)
#' 
#' ###### Example 2: any data frame can be a dataset
#' cat_cyl <- as_category(mtcars[['cyl']])
#' 
#' head(cat_cyl)
#' 
#'}
#'
#' @import dplyr haven
#' @importFrom rlang .data
#'
#' @export
as_category <- function(
    x, 
    labels = as.vector(c(na.omit(unique(x)))), 
    na_values = NULL,
    as_factor = FALSE){
  
  # # x = c('A','B','A','D','C','C')
  # # labels = c("premier" = "B","second" = "A", "troisieme" = "C", "dernier" = "D")
  # # na_values = c("second" = "A", "dernier" = "D")
  # 
  # attributes(x)[["coucou"]] <- "coup"
    
  if(all(is.null(labels))) return(drop_category(x))
  
  # check if x is a column 
  if(is.list(x) & nrow(x) %>% sum <= 1)
    return(as_category(x = x[[1]],labels,na_values))
  
  # check if the col is a vector
  if(is.list(x)) stop("'list' object cannot be coerced to a category")
  
  att <- attributes(x)
  vT_x <- valueType_of(x)
  
  if(is.factor(x)){
    x <- as_valueType(x,vT_x)
    att$labels <- att$levels
    names(att$labels) <- att$levels
    att['levels'] <- NULL
    att['class'] <- NULL}
  
  att_names_old <-
    tibble(
      labels = as.character(att$labels), 
      labels_old = as.character(att$labels), 
      names_old = names(att$labels)) %>%
    bind_rows(tibble(names_old = as.character()))
  
  att_na_old <-
    tibble(
      labels = as.character(att$na_values), 
      na_names_old = as.character(att$na_values)) %>%
    bind_rows(tibble(na_names_old = as.character()))
  
  att_names_new <- 
    tibble(
      labels = as.character(!!labels), 
      labels_new = as.character(!!labels),
      names_new = names(!!labels)) %>%
    bind_rows(tibble(names_new = as.character())) 
  
  if(!is.null(na_values)){
    if(!all(unname(na_values) %in% c(unname(att$labels),unname(labels))))
      stop(call. = FALSE,
           "`na_values` must be taken from labels.")}

  na_values <- na_values[unname(na_values) %in% c(unname(att$labels),unname(labels))]
  att_na_new <-
    tibble(
      labels = as.character(!!na_values), 
      na_names_new = as.character(!! na_values)) %>%
    bind_rows(tibble(na_names_new = as.character())) 
  
  att_new <-
    att_names_old %>%
    full_join(att_na_old, by = "labels") %>%
    full_join(att_names_new, by = "labels") %>%
    full_join(att_na_new, by = "labels") %>%
    mutate(
      names_new = ifelse(is.na(.data$names_new),.data$names_old,.data$names_new),
      labels_new = ifelse(is.na(.data$labels_new),.data$labels_old,.data$labels_new),
      na_names_new = ifelse(is.na(.data$na_names_new),.data$na_names_old,.data$na_names_new)) %>%
    mutate(names_new = ifelse(is.na(.data$names_new),.data$labels_new,.data$names_new)) %>%
    mutate(across(everything(),as.character)) %>%
    dplyr::filter(.data$names_new != "NULL")
  
  new_labels <- as_valueType(att_new$labels_new,vT_x)
  names(new_labels) <- att_new$names_new
  new_na_values <- as_valueType(att_new[!is.na(att_new$na_names_new),]$na_names_new,vT_x)
  names(new_na_values) <- att_new[!is.na(att_new$na_names_new),]$names_new
  
  att$labels <- new_labels
  att$na_values <- new_na_values
  
  if(length(att[['labels']] ) == 0) att[['labels']] <- NULL 
  if(length(att[['na_values']]) == 0) att[['na_values']] <- NULL 
  
  vT_list <- madshapR::valueType_list
  att$`class` <-
    c("haven_labelled","vctrs_vctr",
      vT_list[[which(vT_list$`valueType` == valueType_of(x)),"explicit_class"]])
  
  attributes(x) <-  
    att[unique(c(na.omit(names(c(att['labels'],att["na_values"],att["class"],att[names(att)])))))]
  
  
  

  if(as_factor == TRUE){
    
    
    labels    <- attributes(x)[['labels']]
    na_values <- attributes(x)[['na_values']]
    
    att_x = attributes(drop_category(x))
    att_x['class'] <- NULL
    
    x_txt <- as.character(x)
    x_fct <- factor(x_txt, levels = unname(labels))
    att_x_fct <- attributes(x_fct)
    
    attributes(x_fct) <- 
      c(att_x_fct, att_x)
    x <- x_fct
    
    }

  return(x)
  
}

#' @title
#' Validate and coerce any object as a non-categorical variable.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Converts a vector object to a non-categorical object, typically a column in a 
#' data frame. The categories come from valid values present in the 
#' object and are suppressed from an associated data dictionary (when present).
#'
#' @param x object to be coerced.
#'
#' @return
#' A R object.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' ###### Example 1: use madshapR_example provided by the package
#' dataset <-
#'   madshapR_example$`dataset_example` %>%
#'   mutate(prg_ever_cat = as_category(prg_ever)) %>%
#'   mutate(prg_ever_no_cat = drop_category(prg_ever))
#'   
#' head(dataset[c("prg_ever_cat","prg_ever_no_cat")])
#' 
#' ###### Example 2: any data frame can be a dataset
#' iris_no_cat <- 
#'   tibble(iris) %>% mutate(Species = drop_category(Species))
#' 
#' head(iris_no_cat)
#' 
#'}
#'
#' @import dplyr haven
#' @importFrom rlang .data
#'
#' @export
drop_category <- function(x){
  
  # check if the col is a vector
  if(is.list(x)) stop("'list' object cannot be coerced to a category")
  
  # check if the col is already a category
  if(!is_category(x)) return(x)
  
  fct_att <- attributes(x)
  vT_x <- valueType_of(x)
  
  x <- as_valueType(as.character(x),valueType = vT_x)
  vec_att <- attributes(x)
  
  fct_att <- fct_att[!str_detect(names(fct_att),'Categories::')]
  fct_att['class']      <- NULL
  fct_att['labels']     <- NULL
  fct_att['levels']     <- NULL
  fct_att['na_values']  <- NULL
  
  attributes(x) <- c(vec_att, fct_att)
  
  return(x)
  
}

#' @title
#' Test and validate if an object is a categorical variable.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Tests if the input object is a categorical variable. This function mainly helps 
#' validate input within other functions of the package but could be used
#' to check if a column is categorical.
#' 
#' @param x object to be coerced.
#' @param threshold Optional. The function returns TRUE if the number of unique 
#' values in the input vector is lower. 
#'
#' @return
#' A logical.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' ###### Example 1: use madshapR_example provided by the package
#' dataset <-
#'   madshapR_example$`dataset_example` %>%
#'   mutate(prg_ever_cat = as_category(prg_ever)) %>%
#'   mutate(prg_ever_no_cat = drop_category(prg_ever))
#'   
#' is_category(dataset[['prg_ever_cat']])
#' is_category(dataset[['prg_ever_no_cat']])
#' 
#' ###### Example 2: any data frame can be a dataset
#' iris %>% reframe(across(everything(), is_category))
#' 
#'}
#'
#' @import dplyr haven
#' @importFrom rlang .data
#'
#' @export
is_category <- function(x, threshold = NULL) {
  
  unique_column <- unique(x)
  if(is.factor(unique_column))   return(TRUE)
  if(is.labelled(unique_column)) return(TRUE)
  if(all(is.na(unique_column)))  return(FALSE)
  if(is.null(threshold))         return(FALSE)
  
  return(length(unique_column) <= threshold)
}


#' @title
#' Test if an object has categorical variables.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Test if the object has categorical variables, typically a data frame or 
#' categorical entries in the data dictionary. This function mainly helps 
#' validate input within other functions of the package but could be used to 
#' check if a dataset or a data dictionary has categorical variables.
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
#' @param ... Object that can be either a dataset or a data dictionary.
#'
#' @return
#' A logical.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' ###### Example 1: use madshapR_example provided by the package
#' dataset_with_cat <- madshapR_example$`dataset_example` %>%
#'   mutate(prg_ever_cat = as_category(prg_ever))
#'   
#' has_categories(madshapR_example$`dataset_example`)
#' has_categories(dataset_with_cat)
#' has_categories(madshapR_example$`data_dict_example`)
#' 
#' ###### Example 2: any data frame can be a dataset
#' has_categories(iris)
#' has_categories(mtcars)
#' 
#'}
#'
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
has_categories <- function(...){
  
  if(is_dataset(...)){
    
    dataset <- as_dataset(...,col_id = col_id(...))
    has_cat <-  
      dataset %>%
      reframe(across(everything(),is_category)) %>%
      pivot_longer(cols = everything()) %>% pull("value") %>% 
      sum
    
    if(has_cat > 0 ) return(TRUE) 
    if(has_cat == 0) return(FALSE)}
  
  if(is_data_dict_shape(...)){
    
    data_dict <- as_data_dict_shape(...)
    
    has_cat <- sum(nrow(data_dict[["Categories"]]))
    
    if(has_cat > 0 ) return(TRUE)
    if(has_cat == 0) return(FALSE)}
  
  stop(call. = FALSE, "The argument is neither a dataset nor a data dictionary.")
  
}

#' @title 
#' Get First Label from Data Dictionary
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function retrieves the first variable and category labels from a data dictionary.
#' It checks if the labels are present, and if not, returns empty strings. The function
#' also determines the class of the data dictionary based on its attributes and structure.
#' The function first validates the input using `as_data_dict_shape`. It then attempts to 
#' extract the first variable label from the 'Variables' section of the data dictionary.
#' If categories are present, it will also extract the first relevant category label. 
#' The class of the data dictionary is determined based on its attributes and structure.
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
#' @param data_dict A list of data frame(s) representing metadata.
#' 
#' @return A named character vector with the following elements:
#'   - `Variables`: The first variable label found, or an empty string if none are found.
#'   - `Categories`: The first category label found, or `NULL` if no categories are present.
#'   - `madshapR::class`: A string indicating the class of the data dictionary.
#'
#'
#' @examples
#' {
#'  # use madshapR_example provided by the package
#'  data_dict <- madshapR_example$`data_dict_example`
#'  first_label_get(data_dict)
#' 
#' }
#' 
#' @import dplyr tidyr
#' @importFrom rlang .data
#'
#' @export
first_label_get <- function(data_dict){
  
  # test input
  as_data_dict_shape(data_dict)
  
  # add label(:xx) if not present
  first_lab_var <- 
    names(data_dict[['Variables']] %>%
            select(matches(c("^label$","^label:[[:alnum:]]"))))[1] %>%
    replace_na("")
  
  # add label(:xx) if not present
  
  first_lab_cat <- 
    if(has_categories(data_dict)){
      names(data_dict[['Categories']] %>%
              select(matches(c(
                !! paste0("^",first_lab_var,"$"),
                "^labels$",
                "^label$","^label:[[:alnum:]]"))))[1] %>%
        replace_na("")
    }else{ NULL }
  
  `madshapR::class` <-
    if(!is.null(attributes(data_dict)$`madshapR::class`))
      toString(attributes(data_dict)$`madshapR::class`) else 
        if(is_data_dict_mlstr(data_dict)) "data_dict_mlstr" else 
          if(is_data_dict(data_dict)) "data_dict" else
            "data_dict_structure"
  
  `madshapR::class` <-
    if(`madshapR::class` == "data_dict_mlstr" & toString(first_lab_cat) == "labels")
      "data_dict" else `madshapR::class`
  
  labels <- c(
    "Variables" = first_lab_var,
    "Categories" = first_lab_cat,
    "madshapR::class" = `madshapR::class`)
  
  return(labels)
  
}


#' @title
#' Add Shortened Labels to Data Dictionary
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function modifies a data dictionary by adding shortened labels for both 
#' variables and categories. The shortened labels are created based on specified 
#' maximum lengths for the variable and category names and labels.
#' The function first validates the input using `as_data_dict_shape` and extracts the 
#' first variable and category labels using `first_label_get`. It then calculates the 
#' lengths of names and labels, ensuring that they do not exceed the specified maximum 
#' lengths. The function handles both variables and categories, creating short labels 
#' while replacing any missing values with "Empty".
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
#' @param data_dict A data dictionary, typically a list containing 'Variables' 
#' and 'Categories' data frames.
#' @param max_length_name_var An integer specifying the maximum length for 
#' variable names (default is 10).
#' @param max_length_total_var An integer specifying the maximum total length 
#' for variable names and labels combined (default is 25).
#' @param max_length_name_cat An integer specifying the maximum length for 
#' category names (default is 10).
#' @param max_length_total_cat An integer specifying the maximum total length 
#' for category names and labels combined (default is 15).
#' @param .keep_columns An boolean specifying if the output preserves the other 
#' columns of the dataset.
#'
#' @return A modified data dictionary with additional columns for shortened labels:
#'   - `madshapR::label_var_short`: Shortened variable labels.
#'   - `madshapR::label_cat_long`: Shortened category labels (if categories are present).
#'
#' @examples{
#' 
#'  # use madshapR_example provided by the package
#'  data_dict <- madshapR_example$`data_dict_example - errors`
#'  data_dict_with_short_labels <- data_dict_trim_labels(data_dict)
#'  
#'  attributes(data_dict_with_short_labels)
#' 
#' }
#' 
#' @import dplyr stringr fabR
#' @importFrom rlang .data
#'
#' @export
data_dict_trim_labels <- function(
    data_dict, 
    max_length_var_name = 31,
    max_length_var_label = 255, 
    max_length_cat_name = 15,
    max_length_cat_label_short = 63,
    max_length_cat_label_long = 31,
    .keep_columns = TRUE){
  
  # test input
  as_data_dict_shape(data_dict)
  
  # extract labels
  labs <- first_label_get(data_dict)
  
  # labels for Variables
  data_dict$`Variables` <- 
    data_dict$`Variables` %>%
    select(-any_of(c('Variable name','Variable label'))) %>%
    mutate('var_name' =  .data$`name`,
           'var_lab' =  !! as.symbol(labs[['Variables']])) %>%
    mutate(across(c('var_name', 'var_lab'), ~ as.character(.))) %>%
    mutate(across(c('var_name'),  ~ replace_na(.,"[Unnamed variable]"))) %>%
    mutate(across(c('var_lab'),  ~ replace_na(.,"[unlabelled variable]"))) %>%
    mutate(across(c('var_lab'),  ~ ifelse(
      .data$`var_name`    == "[Unnamed variable]" & 
      .data$`var_lab` == "[Unlabelled variable]","[Unnamed variable]",.data$`var_lab`))) %>% 
    rowwise() %>%
    mutate(
      length_name = nchar(.data$`var_name`),
      length_label = nchar(.data$`var_lab`),
      max_length_name = min(max_length_var_name,.data$`length_name`),
      max_length_label = min(max_length_var_label,.data$`length_label`),
      "madshapR::Variable name" = 
        str_trunc(.data$`var_name`,
                  width = max(.data$`max_length_name`,3), ellipsis = '...'),
      "madshapR::Variable label" = 
        str_trunc(.data$`var_lab`,
                  width = max(.data$`max_length_label`,3), ellipsis = '...')) %>%
    
    group_by(.data$`madshapR::Variable name`) %>% 
    add_index("count_short_name",.force = TRUE) %>%
    mutate(
      "count_short_name"= ifelse(.data$`count_short_name` == 1,"",paste0(".",.data$`count_short_name`)),
      'madshapR::Variable name' = paste0(.data$`madshapR::Variable name`,.data$`count_short_name`)) %>%
    ungroup %>%
    group_by(.data$`madshapR::Variable label`) %>% 
    add_index("count_short_lab",.force = TRUE) %>%
    mutate(
      "count_short_lab"= ifelse(.data$`count_short_lab` == 1,"",paste0(".",.data$`count_short_lab`)),
      'madshapR::Variable label' = paste0(.data$`madshapR::Variable label`,.data$`count_short_lab`)) %>%
    ungroup %>%
    rename(
      'Variable name' = 'madshapR::Variable name',
      'Variable label' = 'madshapR::Variable label')
        
  # labels for Cariables
  if(has_categories(data_dict)){
    
    
    if(is.null(data_dict[['Categories']][['missing']])){
      
      data_dict[['Categories']][['missing']] <- FALSE
      
    }
    
    data_dict$`Categories` <- 
      data_dict$`Categories` %>%
      select(-any_of(
        c('Category codes and labels short',
          'Category codes and labels long',
          'Category missing codes short',
          'Category missing codes long'))) %>%
      mutate('var_name' =  .data$`variable`,
             'cat_name' =  .data$`name`, 
             'cat_lab' = !! as.symbol(labs[['Categories']]),
             'cat_miss'  = .data$`missing`) %>%
      mutate(across(c('var_name', 'cat_name', 'cat_lab','cat_miss') , ~ as.character(.))) %>%
      mutate(across(c('var_name'),  ~ replace_na(.,"[Unnamed variable]"))) %>%
      mutate(across(c('cat_name'),  ~ replace_na(.,"[Unnamed category]"))) %>%
      mutate(across(c('cat_lab') ,  ~ replace_na(.,"[unlabelled category]"))) %>%
      mutate(across(c('cat_lab'),   ~ ifelse(
        .data$`var_name` == "[Unnamed category]" & 
        .data$`cat_lab` == "[unlabelled category]", "[Unnamed category]",.data$`cat_lab`))) %>% 
      rowwise() %>%
      mutate(
        length_name = nchar(.data$`cat_name`),
        length_label = nchar(.data$`cat_lab`),
        max_length_name = min(max_length_cat_name,.data$`length_name`),
        max_length_label_short = min(max_length_cat_label_short,.data$`length_label`),
        max_length_label_long = min(max_length_cat_label_long,.data$`length_label`),
        "name_short" = 
          str_trunc(.data$`cat_name`,
                    width = max(.data$`max_length_name`,3), ellipsis = '...'),
        "label_short" = 
          str_trunc(.data$`cat_lab`,
                    width = max(.data$`max_length_label_short`,3), ellipsis = '...'),
        
        "label_long" = 
          str_trunc(.data$`cat_lab`,
                    width = max(.data$`max_length_label_long`,3), ellipsis = '...'),
        
        'code_lab_short' = 
          ifelse(.data$`cat_name` == .data$`cat_lab`,
                 paste0('[',.data$`name_short`,']'), 
                 paste0('[',.data$`name_short`,'] ',.data$`label_short`)),
        
        'code_lab_long' = 
          ifelse(.data$`cat_name` == .data$`cat_lab`,
                 paste0('[',.data$`name_short`,']'), 
                 paste0('[',.data$`name_short`,'] ',.data$`label_long`)))  %>%
      group_by(.data$`var_name`,.data$`code_lab_short`) %>% 
      add_index("count_short_lab",.force = TRUE) %>%
      mutate(
        "count_short_lab"= ifelse(.data$`count_short_lab` == 1,"",paste0(".",.data$`count_short_lab`)),
        'code_lab_short' = paste0(.data$`code_lab_short`,.data$`count_short_lab`)) %>%
      ungroup %>%
      group_by(.data$`var_name`,.data$`code_lab_long`) %>% 
      add_index("count_long_lab",.force = TRUE) %>%
      mutate(
        "count_long_lab"= ifelse(.data$`count_long_lab` == 1,"",paste0(".",.data$`count_long_lab`)),
        'code_lab_long' = paste0(.data$`code_lab_long`,.data$`count_long_lab`)) %>%
      ungroup %>%
      rowwise() %>%
      mutate(
        'code_missing_short' = 
          ifelse(isTRUE(silently_run(as_any_boolean(.data$cat_miss))) & 
                   class(silently_run(as_any_boolean(data_dict[['Categories']][['missing']]))) != 'try-error',
                 .data$`code_lab_short`,NA_character_),
        'code_missing_long' = 
          ifelse(isTRUE(silently_run(as_any_boolean(.data$cat_miss))) & 
                   class(silently_run(as_any_boolean(data_dict[['Categories']][['missing']]))) != 'try-error',
                 .data$`code_lab_long`,NA_character_)) %>%
      rename('Category codes and labels short' = 'code_lab_short',
             'Category codes and labels long' = 'code_lab_long',
             'Category missing codes short' = 'code_missing_short',
             'Category missing codes long' = 'code_missing_long') %>%
      ungroup
     
    
  }else{
    
    
    if(!is.null(data_dict[['Categories']])){

      data_dict[['Categories']] <-     
        data_dict[['Categories']] %>%
        bind_rows(
          tibble(
          "variable" = as.character(),
          "name" = as.character(),
          "Category codes and labels short" = as.character(),
          "Category codes and labels long" = as.character(),
          "Category missing codes short" = as.character(),
          "Category missing codes long" = as.character()))
    }
  }
  
  data_dict <- 
    data_dict %>% lapply(function(x) 
      select(x,-any_of(c(
        'var_name','var_lab','cat_name','cat_lab',
        'length_name','length_label','max_length_name','max_length_label',
        'max_length_label_short','max_length_label_long',
        'name_short','label_short','label_long',"cat_miss",
        'count_short_name','count_short_lab','count_long_lab'))))
  
  if(has_categories(data_dict)){
    
    data_dict$Variables <-
      data_dict$Variables %>%
      left_join(
        data_dict$Categories %>%
          select(-name) %>%
          dplyr::filter(!is.na(.data$`variable`)) %>%
          group_by(name = .data$`variable`) %>%
          mutate(across(
            c('Category codes and labels short',
              'Category codes and labels long',
              'Category missing codes short',
              'Category missing codes long'), ~ replace_na(.,""))) %>%
          reframe(across(
            c('Category codes and labels short',
              'Category codes and labels long',
              'Category missing codes short',
              'Category missing codes long'), ~ paste0(.,collapse = '\n'))) %>%
          mutate(
            'Category missing codes short' = 
              ifelse(str_squish(.data$`Category missing codes short`) == "",
                     NA_character_,.data$`Category missing codes short`)) %>%
          mutate(
            'Category missing codes long' = 
              ifelse(str_squish(.data$`Category missing codes long`) == "",
                     NA_character_,.data$`Category missing codes long`)),
        by = 'name') 
    
  }else{
    
    data_dict$Variables <-
      data_dict$Variables %>%
      bind_rows(
        tibble(
          "Category codes and labels short" = as.character(),
          "Category codes and labels long" = as.character(),
          "Category missing codes short" = as.character(),
          "Category missing codes long" = as.character()))
  }
    
  
  if(.keep_columns == FALSE){
    
    data_dict$`Variables` <- 
      data_dict$`Variables` %>%
      select("name", 
             "Variable name", 
             "Variable label",
             "Category codes and labels short",
             "Category codes and labels short",
             "Category missing codes short",
             "Category missing codes long")
    
    if(has_categories(data_dict)){
      data_dict$`Categories` <- 
        data_dict$`Categories` %>%
        select("variable", "name",
               "Category codes and labels short",
               "Category codes and labels long",
               "Category missing codes short",
               "Category missing codes long")
      
    }
  }
  
  return(data_dict)
}



#' @title
#' Update a data dictionary from a dataset
#'
#' @description
#' Updates a data dictionary from a dataset, creating a new data dictionary with
#' updated content, from variables selected in the dataset. Any previous other
#' meta data will be preserved. The new data dictionary can be applied to the 
#' dataset using [data_dict_apply()].
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
#' [data_dict_apply(), data_dict_extract()]
#'
#' @param dataset A dataset object.
#' @param data_dict A list of data frame(s) representing metadata of the input 
#' dataset. Automatically generated if not provided.
#' @param cols An optional character string specifying the name(s) or 
#' position(s) of the column(s) for which meta data will be updated. All by 
#' default.
#'
#' @returns
#' A list of data frame(s) identifying a data dictionary.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' 
#' # use madshapR_example provided by the package
#' dataset   <- madshapR_example$`dataset_example`
#' data_dict <- as_data_dict_mlstr(madshapR_example$`data_dict_example`)
#' dataset <- data_dict_apply(dataset,data_dict)
#' 
#' # the data dictionary contains no categorical variable.
#' 
#' # create a category in the dataset
#' dataset   <- dataset %>% mutate(gndr = as_category(gndr, labels = c("coucou" = 1),na_values = 2))
#' new_data_dict <- data_dict_update(data_dict, dataset, "gndr")
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
data_dict_update <- function(
    data_dict = NULL,
    dataset, 
    cols = names(dataset)){

  attributes(dataset)[["madshapR::Data dictionary"]]   <- NULL
  
  if(is.null(data_dict)){
    data_dict <- data_dict_extract(dataset)
    return(data_dict)
  }

  dataset <- 
    as_dataset(dataset) %>% 
    select(all_of(cols)) %>% 
    ungroup

  data_dict_to_udpate <- 
    dataset %>% 
    data_dict_extract() %>% 
    as_data_dict_mlstr()
  
  data_dict[['Variables']] <- 
    data_dict[['Variables']] %>%
    add_index("madshapR::index")
  
  data_dict_to_udpate[['Variables']] <- 
    data_dict_to_udpate[['Variables']] %>%
    left_join(data_dict[['Variables']][c('name','madshapR::index')], 
              by = 'name')

  data_dict[['Variables']] <-   
    data_dict[['Variables']] %>%
    dplyr::filter(!.data$`name` %in% cols) %>% 
    bind_rows(data_dict_to_udpate[['Variables']]) %>%
    arrange("madshapR::index") %>% select(-"madshapR::index")
    
  if(has_categories(data_dict_to_udpate)){
    
    data_dict[['Categories']] <- 
      data_dict[['Categories']] %>%
      bind_rows(tibble(
        'variable' = as.character(),
        'name' = as.character())) %>%
      dplyr::filter(!.data$`variable` %in% cols) %>% 
      bind_rows(data_dict_to_udpate[['Categories']])
  }
  
  data_dict <- as_data_dict_mlstr(data_dict)
  return(data_dict)
  
}
  
