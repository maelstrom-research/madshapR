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
as_category <- function(x, labels = c(na.omit(unique(x))), na_values = NULL){
  
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
    att['class'] <- NULL
  }
  
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
    if(!all(unname(na_values) %in% unname(labels)))
      stop(call. = FALSE,
           "`na_values` must be taken from labels.")}

  na_values <- labels[na_values == labels]
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
    dplyr::filter(names_new != "NULL")
  
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
                "^label$","^label:[[:alnum:]]"))))[1] 
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
#'   - `madshapR::label_short_var`: Shortened variable labels.
#'   - `madshapR::label_short_cat`: Shortened category labels (if categories are present).
#'
#' @examples{
#' 
#'  # use madshapR_example provided by the package
#'  data_dict <- madshapR_example$`data_dict_example`
#'  data_dict_with_short_labels <- data_dict_add_labels_short(data_dict)
#'  
#'  attributes(data_dict_with_short_labels)
#' 
#' }
#' 
#' @import dplyr stringr fabR
#' @importFrom rlang .data
#'
#' @export
data_dict_add_labels_short <- function(
    data_dict, 
    max_length_name_var = 10,
    max_length_total_var = 25,
    max_length_name_cat = 10,
    max_length_total_cat = 15,
    .keep_columns = TRUE){
  
  # test input
  as_data_dict_shape(data_dict)
  
  # extract labels
  labs <- first_label_get(data_dict)  
  
  # labels for Variables
  data_dict$`Variables` <- 
    data_dict$`Variables` %>%
    mutate(across(c('name', !! labs[['Variables']]), ~ as.character(.))) %>%
    mutate(across(c('name', !! labs[['Variables']]),  ~ replace_na(.,"{Empty}"))) %>%
    rowwise() %>%
    mutate(
      length_name = nchar(.data$`name`),
      length_label = nchar(!!as.name(labs[['Variables']])),
      max_length_name = min(max_length_name_var,.data$`length_name`),
      remain_length_lab = min(
        max_length_total_var - .data$`max_length_name`,
        .data$`length_label`),
      name_short = ifelse(
        .data$`name` == '{Empty}', "Empty",
        str_trunc(.data$`name`,width = max(.data$`max_length_name`,3), ellipsis = '...')),
      label_short = ifelse(
        !! as.name(labs[['Variables']]) == '{Empty}', "Empty", 
        str_trunc(!!as.symbol(labs[['Variables']]),
                  width = max(.data$`remain_length_lab`,3), ellipsis = '...')),
      'madshapR::label_short_var' = 
        ifelse(.data$`name` == !!as.symbol(labs[['Variables']]),
               .data$`name_short`, 
               paste0(.data$`name_short`,' (',.data$`label_short`,')')))  %>%
    group_by(.data$`madshapR::label_short_var`) %>% 
    add_index("count_short_lab",.force = TRUE) %>%
    mutate(
      "count_short_lab"= ifelse(.data$`count_short_lab` == 1,"",paste0(".",.data$`count_short_lab`)),
      'madshapR::label_short_var' = paste0(.data$`madshapR::label_short_var`,.data$`count_short_lab`)) %>%
    mutate(across(c('name', !! labs[['Variables']]), ~na_if(.,"{Empty}"))) %>%
    select(1:!!labs[['Variables']],'madshapR::label_short_var',everything()) %>%
    ungroup 
  
  # labels for Cariables
  if(has_categories(data_dict)){
    data_dict$`Categories` <- 
      data_dict$`Categories` %>%
      mutate(across(c('name', !! labs[['Categories']]), ~as.character(.))) %>%
      mutate(across(c('name', !! labs[['Categories']]), ~replace_na(.,"{Empty}"))) %>%
      rowwise() %>%
      mutate(
        length_name = nchar(.data$`name`),
        length_label = nchar(!!as.name(labs[['Categories']])),
        max_length_name = min(max_length_name_cat,.data$`length_name`),
        remain_length_lab = min(max_length_total_cat - .data$`max_length_name`,
                                .data$`length_label`),
        name_short = ifelse(
          .data$`name` == '{Empty}', "Empty", 
          str_trunc(.data$`name`,width = max(.data$`max_length_name`,3), ellipsis = '...')),
        label_short = ifelse(
          !! as.name(labs[['Categories']]) == '{Empty}', "Empty", 
          str_trunc(!!as.symbol(labs[['Categories']]),
                    width = max(.data$`remain_length_lab`,3), ellipsis = '...')),
        'madshapR::label_short_cat' = 
          ifelse(.data$`name` == !!as.symbol(labs[['Categories']]),
                 paste0('[',.data$`name_short`,']'), 
                 paste0('[',.data$`name_short`,'] ',.data$`label_short`))) %>%
      group_by(.data$`variable`,.data$`madshapR::label_short_cat`) %>% 
      add_index("count_short_lab",.force = TRUE) %>%
      mutate(
        "count_short_lab"= ifelse(.data$`count_short_lab` == 1,"",paste0(".",.data$`count_short_lab`)),
        'madshapR::label_short_cat' = paste0(.data$`madshapR::label_short_cat`,.data$`count_short_lab`)) %>%
      mutate(across(c('name', !! labs[['Categories']]), ~na_if(.,"{Empty}"))) %>%
      select(1:!!labs[['Categories']],'madshapR::label_short_cat',everything()) %>%
      ungroup
    
  }
  
  data_dict <- 
    data_dict %>% lapply(function(x) 
      select(x,-c('length_name','length_label','max_length_name',
                  'remain_length_lab','name_short','label_short',
                  'count_short_lab')))
  
  if(.keep_columns == FALSE){
    
    data_dict$`Variables` <- 
      data_dict$`Variables` %>%
      select("name","madshapR::label_short_var") 
    
    if(has_categories(data_dict)){
      
      data_dict$`Categories` <- 
        data_dict$`Categories` %>%
        select("variable", "name","madshapR::label_short_cat")
      
    }
  }
  
  return(data_dict)
}
