#' @title
#' Validate and coerce any object as a categorical variable.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Converts a vector object to a categorical object, typically a column in a 
#' data frame. The categories come from non-missing values present in the 
#' object and are added to an associated data dictionary (when present).
#'
#' @param x A vector object to be coerced to categorical.
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
#' mtcars <- tibble(mtcars)
#' as_category(mtcars[['cyl']])
#' 
#' head(mtcars %>% mutate(cyl = as_category(cyl)))
#' 
#' 
#'}
#'
#' @import dplyr haven
#' @importFrom rlang .data
#'
#' @export
as_category <- function(x){
  
  # check if the col is a vector
  if(is.list(x)) stop("'list' object cannot be coerced to a category")
  
  # check if the col is already a category
  if(is_category(x)) return(x)
  
  att <- attributes(x)
  
  # if column has a specific class, it first will be cast as a character
  if(!is.null(attributes(x)[['class']][1])) {
    att[['class']] <- NULL
    x <- as.character(x)}
  
  x_init <- x
  x <- as.factor(x)
  fct_att <- attributes(x)
  
  fct_att$labels <- fct_att$levels
  names(fct_att$labels) <-  fct_att$labels
  fct_att$levels <- NULL
  
  
  vT_list <- madshapR::valueType_list
  fct_att$`class` <-
    c("haven_labelled","vctrs_vctr",
      vT_list[[which(vT_list$`valueType` == valueType_of(x)),"explicit_class"]])
  
  attributes(x_init) <-  c(fct_att['labels'],fct_att['class'],att)
  
  return(x_init)
  
}


#' @title
#' Validate and coerce any object as a non-categorical variable.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Converts a vector object to a non-categorical object, typically a column in a 
#' data frame. The categories come from non-missing values present in the 
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
#' head(iris[['Species']])
#' head(drop_category(iris[['Species']]))
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
#' Test if an object is a valid dataset
#'
#' @description
#' Tests if the input object is a valid dataset. This function mainly helps 
#' validate input within other functions of the package but could be used
#' to check if a dataset is valid.
#' 
#' @title
#' Validate if an object is a categorical variable.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Test if vector object is a categorical variable, typically a column in a 
#' data frame. This function mainly helps validate input within other functions 
#' of the package.
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
#' iris %>% reframe(across(everything(), is_category))
#' is_category(iris[['Species']])
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
