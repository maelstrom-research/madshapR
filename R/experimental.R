#' @title
#' Convert input to a categorical variable.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function is used to convert a vector object to a categorical object. It
#' Is analoguous to [as.factor()] function when the vector (variable) has no
#' data dictionary associated. When it has, the characteristics of the category
#' is added to the data dictionary.
#'
#' @param x object to be coerced.
#'
#' @seealso
#' [as.factor()]
#'
#' @return
#' A R Object of class haven_labelled.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' iris %>% mutate(Species = as_category(Species))
#' as_category(iris[['Species']])
#' 
#'}
#'
#' @import dplyr haven
#' @importFrom rlang .data
#'
#' @noRd
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
      vT_list[[which(vT_list$`valueType` == valueType_of(x)),"class"]])
  
  attributes(x_init) <-  c(fct_att['labels'],fct_att['class'],att)
  
  return(x_init)
  
}


#' @title
#' Convert categorical input to a non-categorical variable.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function is used to convert a vector categorical object to a 
#' non-categorical object. When it has, the 
#' characteristics of the category is added to the data dictionary.
#'
#' @param x object to be coerced.
#'
#' @return
#' A R object.
#'
#' @examples
#' {
#' 
#' drop_category(iris[['Species']])
#' 
#'}
#'
#' @import dplyr haven
#' @importFrom rlang .data
#'
#' @noRd
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
#' Test if a column in a dataset is a categorical variable.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Test if a column in a dataset is a categorical variable. This function mainly 
#' helps validate input within other functions of the package.
#'
#' @param x object to be coerced.
#'
#' @return
#' A logical.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' iris %>% summarise(across(everything(), is_category))
#' is_category(iris[['Species']])
#' 
#'}
#'
#' @import dplyr haven
#' @importFrom rlang .data
#'
#' @noRd
is_category <- function(column, threshold = NULL) {
  
  unique_column <- unique(column)
  if(is.factor(unique_column))   return(TRUE)
  if(is.labelled(unique_column)) return(TRUE)
  if(all(is.na(unique_column)))  return(FALSE)
  if(is.null(threshold))         return(FALSE)
  
  return(length(unique_column) <= threshold)
}
