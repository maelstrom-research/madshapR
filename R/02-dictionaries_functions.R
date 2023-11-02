#' @title
#' Transform single-row category information to multiple rows as element 
#'
#' @description
#' Expands data dictionary column(s) in a element (the parameter 'from'),
#' into another element (the parameter 'to').
#' If the element 'from' contains any column starting with 'prefix', (xx,yy),
#' these columns will be added as 'xx' and 'yy' in the element identified by
#' 'to'. This tibble will be created if necessary, and columns will be added,
#' from left to right. (unique names will be generated if necessary).
#' Separator of each element is the following structure :
#' 'name = xx1 ; name = xx2'.
#' This function is mainly used to expand the column(s) 'Categories::xx' in 
#' "Variables" to "Categories" element with column(s) xx.
#' This function is the reversed operation of [data_dict_collapse()]
#'
#' @details
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
#' @seealso
#' [data_dict_collapse()]
#'
#' @param data_dict A list of tibble(s) representing meta data to be
#' transformed. Automatically generated if not provided.
#' @param from Symbol identifying the name of the element (tibble) to take
#' column(s) from. Default is 'Variables'.
#' @param to Symbol identifying the name of the element (tibble) to create
#' column(s) to. Default is 'Categories'.
#' @param name_prefix Character string of the prefix of columns of interest.
#' This prefix will be used to select columns, and to rename them in the 'to'
#' element. Default is 'Categories::'.
#'
#' @returns
#' A list of tibble(s) identifying a data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' 
#' data_dict <- DEMO_files$`dd_PARIS_format_flatten`
#' data_dict_expand(data_dict)
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
    tibble(name = as.character(),variable = as.character()) %>%
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
              pull(.data$`variable`) %>% toString)

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
#' @seealso
#' [data_dict_expand()]
#'
#' @param data_dict A list of tibble(s) representing meta data to be
#' transformed. Automatically generated if not provided.
#' @param from Symbol identifying the name of the element (tibble) to take
#' column(s) from. Default is 'Categories'.
#' @param to Symbol identifying the name of the element (tibble) to create
#' column(s) to. Default is 'Variables'.
#' @param name_prefix Character string of the prefix of columns of interest.
#' This prefix will be used to select columns, and to rename them in the 'to'
#' element. Default is 'Categories::'.
#'
#' @returns
#' A list of tibble(s) identifying a data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$dd_MELBOURNE_1_format_maelstrom
#' data_dict_collapse(data_dict)
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
        summarise(from = paste0(.data$`from`,collapse = " __SEP_OUT__ \n"),
                  .groups = "drop") %>%
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
#' [tidyr::pivot_wider()], [as_data_dict()]
#'
#' @param data_dict A list of tibble(s) representing meta data to be
#' transformed.
#' @param taxonomy A tibble identifying the scheme used for variables 
#' classification.
#'
#' @returns
#' A list of tibble(s) identifying a data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' taxonomy <- DEMO_files$taxonomy_opal_mlstr
#' data_dict <- DEMO_files$dd_TOKYO_format_maelstrom_tagged
#' data_dict_pivot_wider(data_dict, taxonomy)
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
    pull(.data$`taxonomy_id`)
  
  if(paste0(attributes(taxonomy)$`madshapR::class`,"") == "taxonomy_mlstr"){
    
    authorized_names <- 
      c(authorized_names ,
        taxonomy  %>%
          select("taxonomy_scale", "vocabulary_scale") %>% distinct %>%
          unite(col = "area_scale_id", 
                c("taxonomy_scale", "vocabulary_scale"),
                na.rm = TRUE, sep = "::", remove = FALSE) %>%
          dplyr::filter(.data$`area_scale_id` != "") %>%
          pull(.data$`area_scale_id`))
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
#' [tidyr::pivot_longer()], [as_data_dict()]
#'
#' @param data_dict A list of tibble(s) representing meta data to be
#' transformed.
#' @param taxonomy A tibble identifying the scheme used for variables 
#' classification.
#'
#' @returns
#' A list of tibble(s) identifying a data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' taxonomy <- DEMO_files$taxonomy_opal_mlstr
#' data_dict <- DEMO_files$dd_TOKYO_format_opal_tagged
#' data_dict_pivot_longer(data_dict,taxonomy)
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
      dplyr::filter(.data$`taxonomy_id` %in% 
                      (names(data_dict[['Variables']]))) %>%
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
            across(!! i,
                   ~ ifelse(is.na(.data$`taxonomy_id`),NA_character_,.))) %>%
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
            summarise(
              across(c(any_of(i), .data$`term`),
                     ~ paste0(.,collapse = "|")),
              .groups = "drop") %>%
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
      pull(.data$`area_scale_id`) %>%
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
        .cols = any_of("Mlstr_additional::1.term"),
        .fn = ~ "Mlstr_additional::Source") %>%
      rename_with(
        .cols = any_of("Mlstr_additional::2.term"),
        .fn = ~ "Mlstr_additional::Target") %>%
      select(
        -matches("^Mlstr_additional::1$"),-matches("^Mlstr_additional::2$"))
    
  }
  
  if(sum(data_dict[["Variables"]][['madshapR::index']]) != 
     nrow(data_dict[["Variables"]]) * (nrow(data_dict[["Variables"]]) + 1)/2){
    stop(call. = FALSE,
         "An error occured in data_dict_pivot_longer(). Please contact us.")
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
#' @seealso
#' [dplyr::filter()]
#'
#' @param data_dict A list of tibble(s) representing meta data to be
#' transformed.
#' @param filter_var Expressions that are defined in the element 'Variables' in
#' the data dictionary.
#' @param filter_cat Expressions that are defined in the element 'Categories' in
#' the data dictionary.
#' @param filter_all Expressions that are defined both in the 'Categories' and
#' 'Variables' in the data dictionary.
#'
#' @returns
#' A list of tibble(s) identifying a workable data dictionary structure.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' 
#' # Create a list of data dictionaries where the column 'table' is added to 
#' # refer to the associated dataset. The object created is not a 
#' # data dictionary per say, but can be used as a structure which can be 
#' # shaped into a data dictionary.
#' library(dplyr)
#' 
#' data_dict_list <- list()
#' data_dict_1 <-
#'   DEMO_files$dd_MELBOURNE_1_format_maelstrom %>%
#'   lapply(function(x){x %>% mutate(table = "MELBOURNE_1")})
#' data_dict_2 <- DEMO_files$dd_MELBOURNE_2_format_maelstrom %>%
#'   lapply(function(x){x %>% mutate(table = "MELBOURNE_2")})
#' 
#' data_dict_list <-
#'   list(Variables = bind_rows(data_dict_1$Variables,data_dict_2$Variables),
#'        Categories = bind_rows(data_dict_1$Categories,data_dict_2$Categories))
#' 
#' ###### Example 1 search and filter through a column in 'Variables' element
#' data_dict_filter(data_dict_list,filter_var = "valueType == 'integer'")
#' 
#' ###### Example 2 search and filter through a column in 'Categories' element
#' data_dict_filter(data_dict_list,filter_cat = "missing == TRUE")
#' 
#' ###### Example 3 search and filter through a column in 'Variables' element.
#' # The column must exist in both 'Variables' and 'Categories' and have the 
#' # same meaning
#' data_dict_filter(data_dict_list,filter_all = "table == 'MELBOURNE_1'")
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
  
  if(!is.null(data_dict[['Categories']])){
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      dplyr::filter(.data$`variable` %in% data_dict[['Variables']]$`name`)
    
    if(!is.null(filter_cat)){
      data_dict[['Categories']] <-
        eval(parse(
          text=paste(
            "data_dict[['Categories']] %>% dplyr::filter(",filter_cat,")")))}
  }
  
  if(sum(nrow(data_dict[['Categories']])) == 0)
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
#' @seealso
#' [dplyr::group_by()], [dplyr::group_split()] ,
#' [data_dict_group_by()], [data_dict_list_nest()]
#'
#' @param data_dict A list of tibble(s) representing meta data to be
#' transformed.
#' @param ... Column in the data dictionary to split it by. If not provided, the
#' splitting will be done on the grouping element of a grouped data dictionary.
#'
#' @returns
#' A list of tibble(s) identifying a list of workable data dictionary structure.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' library(dplyr)
#'
#' # Create a list of data dictionaries where the column 'table' is added to 
#' # refer to the associated dataset. The object created is not a 
#' # data dictionary per say, but can be used as a structure which can be 
#' # shaped into a data dictionary.
#' 
#' data_dict_list <- DEMO_files[
#'     c('dd_MELBOURNE_1_format_maelstrom',
#'       'dd_MELBOURNE_2_format_maelstrom')] %>% 
#'     data_dict_list_nest(name_group = 'table')
#'  
#'  data_dict_group_split(data_dict_list,col = "table")
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
  
  if(sum(nrow(data_dict[['Categories']])) == 0){
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
#' @seealso
#' [dplyr::bind_rows()]
#'
#' @param data_dict_list A list of tibble(s) representing meta data to be
#' transformed.
#' @param name_group A character string of one column in the dataset that can be
#' taken as a grouping column.
#'
#' @returns
#' A list of tibble(s) identifying a workable data dictionary structure.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' # Create a list of data dictionaries where the column 'table' is added to 
#' # refer to the associated dataset. The object created is not a 
#' # data dictionary per say, but can be used as a structure which can be 
#' # shaped into a data dictionary.
#' 
#' data_dict_list <- DEMO_files[
#'     c('dd_MELBOURNE_1_format_maelstrom',
#'       'dd_MELBOURNE_2_format_maelstrom')]
#'  
#' data_dict_list_nest(data_dict_list,name_group = "table")
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
    
    if(sum(nrow(data_dict_list[[i]][['Categories']])) > 0){
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
        
        if(!is.null(data_dict_list[[i]][['Categories']][1])){
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
#' @seealso
#' [dplyr::group_by()], [data_dict_ungroup()]
#'
#' @param data_dict A list of tibble(s) representing meta data to be
#' transformed.
#' @param col variable to group by.
#'
#' @returns
#' A list of tibble(s) identifying a workable data dictionary structure.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' # Create a list of data dictionaries where the column 'table' is added to 
#' # refer to the associated dataset. The object created is not a 
#' # data dictionary per say, but can be used as a structure which can be 
#' # shaped into a data dictionary.
#' 
#' library(dplyr)
#' 
#' data_dict_list <- 
#'   DEMO_files[c('dd_MELBOURNE_1_format_maelstrom',
#'                'dd_MELBOURNE_2_format_maelstrom')] %>%
#' data_dict_list_nest(name_group = 'table')
#'  
#' data_dict_group_by(data_dict_list,col = "table")
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
  
  categories <- TRUE
  if(is.null(data_dict[['Categories']])) categories <- FALSE
  
  if(sum(nrow(data_dict[['Categories']])) == 0){
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
#' 'Variables' and 'Categories' elements (if both are grouped tibbles). 
#' This function is analogous to running [dplyr::ungroup()].
#' [data_dict_group_by()] allows to group a data dictionary and this function 
#' reverses the effect.
#'
#' @details
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
#' @seealso
#' [dplyr::ungroup()]
#' [data_dict_group_by()]
#'
#' @param data_dict A list of tibble(s) representing meta data to be
#' transformed.
#'
#' @returns
#' A list of tibble(s) identifying a workable data dictionary structure.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' # Create a list of data dictionaries where the column 'table' is added to 
#' # refer to the associated dataset. The object created is not a 
#' # data dictionary per say, but can be used as a structure which can be 
#' # shaped into a data dictionary.
#' 
#' library(dplyr)
#' 
#' data_dict_list <- DEMO_files[
#'     c('dd_MELBOURNE_1_format_maelstrom',
#'       'dd_MELBOURNE_2_format_maelstrom')] %>%
#'   data_dict_list_nest(name_group = 'table') %>%
#'   data_dict_group_by(col = "table")
#'     
#'  data_dict_ungroup(data_dict_list)
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
  
  if(!is.null(data_dict[['Categories']]))
    data_dict[['Categories']] <-
    data_dict[['Categories']] %>%
    ungroup()
  
  return(data_dict)
}

#' @title
#' Apply a data dictionary to a dataset
#'
#' @description
#' Applies a data dictionary to a data structure, creating a labelled dataset.
#' All previous attributes will be preserved. For factors, the attribute 
#' 'levels' will be transformed into attribute 'labels' and values will be 
#' recast into appropriate datatypes.
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
#' @seealso
#' [attributes()]
#'
#' @param dataset A tibble identifying the input dataset observations 
#' associated to its data dictionary.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset.
#' Automatically generated if not provided.
#'
#' @returns
#' A tibble identifying the dataset with the data dictionary applied to each
#' variable as attributes.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' dataset <- DEMO_files$dataset_MELBOURNE_1
#' data_dict <- as_data_dict_mlstr(DEMO_files$dd_MELBOURNE_1_format_maelstrom)
#' data_dict_apply(dataset, data_dict)
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
  
  # test
  as_dataset(dataset, attributes(dataset)$`madshapR::col_id`)
  preserve_attributes <- attributes(dataset)$`madshapR::col_id`
  if(toString(attributes(data_dict)$`madshapR::class`) == 'data_dict_mlstr'){
    data_dict <- 
      as_data_dict_mlstr(data_dict,as_data_dict = TRUE, name_standard = FALSE)
  }else{data_dict <- as_data_dict(data_dict)}
  
  # names must exist both in dataset and data dictionary
  # data dictionary is not applied to dataset, since it may come from an
  # automated datadict (text by default).
  if(suppressWarnings(check_dataset_variables(dataset, data_dict)) %>% 
     dplyr::filter(str_detect(.data$`condition`,"\\[ERR\\]")) %>% nrow > 0){
    stop(call. = FALSE,
"Names across your data dictionary differ from names across the dataset.",
         bold("\n\nUseful tip:"),
" Use dataset_evaluate(dataset, data_dict) to get a full assessment of
your dataset")}
  
  # set cleaning prefix of Variables element
  # (addition of Variables:: before all variables except name)
  names(data_dict[['Variables']])  <-
    make.unique(str_remove(names(data_dict[['Variables']]),"^Variables::"))
  names(data_dict[['Variables']])[-
          which(names(data_dict[['Variables']])=='name')] <-
    paste0("Variables::",
  names(data_dict[['Variables']]))[-
          which(names(data_dict[['Variables']])=='name')]
  
  # set cleaning prefix of Categories element
  # (addition of Categories:: before all variables
  # except variable, name, labels and na_values)
  if(!is.null(data_dict[['Categories']])){
    names(data_dict[['Categories']]) <-
      make.unique(str_remove(names(data_dict[['Categories']]),"^Categories::"))
    names(data_dict[['Categories']])[-
          which(names(data_dict[['Categories']]) %in%
          c('variable','name','labels', 'na_values'))] <-
      paste0("Categories::",
          names(data_dict[['Categories']])[-
          which(names(data_dict[['Categories']]) %in%
          c('variable','name','labels','na_values'))])}
  
  names_data <- names(dataset)
  names_data_dict <- data_dict[['Variables']]$`name`
  
  for (i in names_data) {
    # stop()}
    
    vT_list <- madshapR::valueType_list
    vT <- valueType_of(x = dataset[[i]])
    dataset[[i]] <- as_valueType(x = (dataset[[i]]),valueType = vT)
    
    attrs_init <- attributes(dataset[[i]])
    
    attrs_var <-
      data_dict[['Variables']][which(data_dict[['Variables']]$`name` == i),]
    attrs_var <- attrs_var[vapply(X = attrs_var,
                                  FUN = function(x) !all(is.na(x)),
                                  FUN.VALUE = logical(1))]
    attrs_var <- c(attrs_var) %>% unlist %>% as.list()
    attrs_var <- attrs_var[names(attrs_var) != 'name']
    
    attrs_cat <- list()
    attrs_fct <- list()
    attrs_na <- list(na_values = c())
    
    if (!is.null(data_dict[['Categories']])) {
      cat_i <-
        data_dict[['Categories']][
          which(data_dict[['Categories']]$`variable` == i),]
      cat_i <- cat_i[vapply(X = cat_i,
                            FUN = function(x) !all(is.na(x)),
                            FUN.VALUE = logical(1))]
      
      if(is.null(cat_i[['na_values']])) cat_i[['na_values']] <- NA
      
      if(nrow(cat_i) > 0) {
        # create vector of dataset
        attributes(dataset[[i]])$`class` <- NULL
        
        vec_data <-
          try({as_valueType(x = cat_i$`name`, valueType = vT)},silent = TRUE)
        if(class(vec_data)[1] == 'try-error') {
          vT_cat <- valueType_guess(x = cat_i$`name`)
          vec_data <- as_valueType(x = cat_i$`name`, valueType = vT_cat)}
        
        names(vec_data) <- cat_i$`labels`
        attrs_na <- list(na_values = vec_data[which(!is.na(cat_i$`na_values`))])
        cat_i$`name`        <- NULL
        cat_i$`labels`      <- NULL
        cat_i$`variable`  <- NULL
        cat_i$`na_values` <- NULL
        
        if(ncol(cat_i) > 0) {
          for(j in seq_len(length(cat_i))){
            # stop()}
            vec_attr <- vec_data
            names(vec_attr) <-  cat_i[[j]]
            vec_attr <- vec_attr[which(!is.na(cat_i[[j]]))]
            attrs_cat[[names(cat_i[j])]] <- vec_attr
          }}
        
        # val_labels(dataset[[i]]) <- vec_data
        attributes(dataset[[i]])$`labels` <- vec_data
        attributes(dataset[[i]])$`class` <-
          c("haven_labelled","vctrs_vctr",
            vT_list[[which(vT_list$`valueType` == vT),"class"]])
        attrs_fct <- attributes(dataset[[i]])
        
      }}
    
    attrs_total <- c(attrs_fct,attrs_na, attrs_init, attrs_var,attrs_cat)
    attributes(dataset[[i]]) <- attrs_total
    
    # suppression of na_values if empty
    if(length(attrs_na[[1]]) == 0) attributes(dataset[[i]])$`na_values` <- NULL
    
  }
  
  dataset <-
    dataset[names_data_dict] %>%
    as_tibble() %>%
    as_dataset(col_id = preserve_attributes)
  
  return(dataset)
}

#' @title
#' Create a data dictionary from a dataset
#'
#' @description
#' Creates a data dictionary in a format compliant with formats used in 
#' Maelstrom Research ecosystem, including Opal (with 'Variables' and 
#' 'Categories' in separate tibbles and standard columns in each) from any 
#' dataset in tibble format. If the input dataset has no associated metadata, a 
#' data dictionary with minimal required information is created from the column 
#' (variable) names to create the data dictionary structure required for 
#' 'madshapR'. All columns except variable names will be blank.
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
#' @param dataset A tibble identifying the input dataset observations which 
#' contains meta data as attributes.
#' @param as_data_dict_mlstr Whether the output data dictionary has a simple
#' data dictionary structure or not (meaning has a Maelstrom data dictionary
#' structure, compatible with Maelstrom Research ecosystem, including Opal). 
#' TRUE by default.
#'
#' @returns
#' A list of tibble(s) identifying a data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' 
#' ###### Example 2: extract data dictionary from any dataset (the 
#' # data dictionary will be created upon attributes of the dataset. Factors 
#' # will be considered as categorical variables)
#' data_dict_extract(iris)
#' 
#' }
#'
#' @import dplyr tidyr stringr fabR
#' @importFrom rlang .data
#'
#' @export
data_dict_extract <- function(dataset, as_data_dict_mlstr = TRUE){
  
  # test
  as_dataset(dataset) # no col_id
  if(!is.logical(as_data_dict_mlstr))
    stop(call. = FALSE,
         '`as_data_dict_mlstr` must be TRUE or FALSE (TRUE by default)')
  
  dataset <- ungroup(dataset)
  
  data_dict <-
    list(
      Variables = tibble(name = as.character()),
      Categories = tibble(variable = as.character(), name = as.character()))
  
  for(i in names(dataset)){
    # stop()}
    
    attrs_i <- attributes(dataset[[i]])
    attrs_i$`tzone` <- NULL
    attrs_i$`class` <- NULL
    if(is.factor(dataset[[i]])){
      names(attrs_i$`levels`) <- make.unique(attrs_i$`levels`)
    }
    
    data_dict_var <- tibble(name = i)
    data_dict_cat <- tibble(variable = as.character())
    
    if(length(attrs_i) > 0){
      
      # if(length(attrs_i) > 0){
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
          data_dict_cat <- data_dict_cat %>%
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
  
  names(data_dict[['Variables']])  <-
    make.unique(str_remove(names(data_dict[['Variables']]),"^Variables::"))
  names(data_dict[['Categories']]) <-
    make.unique(str_remove(names(data_dict[['Categories']]),"^Categories::"))
  
  
  if(sum(nrow(data_dict[['Categories']])) == 0)data_dict[['Categories']] <- NULL

  data_dict <-  
    silently_run({
      valueType_adjust(from = dataset, to = data_dict) %>%
        valueType_adjust() %>%
        as_data_dict_mlstr(
          as_data_dict = !as_data_dict_mlstr,
          name_standard = FALSE) 
    })
  
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
#' @param dataset A tibble identifying the input dataset observations.
#' @param data_dict A list of tibble(s) representing meta data.
#' @param data_dict_apply whether to apply the data dictionary to its dataset.
#' The resulting tibble will have for each column its associated meta data as
#' attributes. The factors will be preserved. FALSE by default.
#' @param output A vector of character string which indicates if the function
#' returns a dataset ('dataset'), data dictionary ('data_dict') of both.
#' Default is c('dataset','data_dict').
#'
#' @returns
#' Either a tibble, identifying the dataset, or a list of tibble(s)
#' identifying a data dictionary. Returns both in a list by default.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#' library(dplyr)
#' dataset <- DEMO_files$dataset_MELBOURNE_1 %>% select(-1)
#' data_dict <- DEMO_files$dd_MELBOURNE_1_format_maelstrom
#' data_dict_match_dataset(dataset, data_dict)
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
  as_dataset(dataset) # no col_id
  if(!is.logical(data_dict_apply))
    stop(call. = FALSE,
         '`data_dict_apply` must be TRUE of FALSE (FALSE by default)')
  
  names_data <-
    paste0("name %in% c('",paste0(names(dataset),collapse = "','"),"')")
  data_dict <- data_dict_filter(data_dict, filter_var = names_data)
  
  dataset <- dataset %>% select(data_dict[['Variables']]$`name`)
  
  if(length(dataset) == 0)
    warning("No match found between dataset and data dictionary")
  
  if(data_dict_apply == TRUE)
    return(data_dict_apply(dataset, data_dict))
  
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
#' Validate and coerce an object to a workable data dictionary structure
#'
#' @description
#' Validates the input object as a workable data dictionary structure and 
#' returns it with the appropriate 'madshapR::class' attribute. This function 
#' mainly helps validate input within other functions of the package but could 
#' be used to check if a data dictionary is valid for use in a function.
#'
#' @details
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
#' @seealso
#' For a better assessment, please use [data_dict_evaluate()].
#'
#' @param object A potential valid data dictionary to be coerced.
#'
#' @returns
#' A list of tibble(s) identifying a data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$dd_PARIS_format_maelstrom
#' as_data_dict_shape(data_dict)
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
"\n\nThis object is not a data dictionary as defined by Maelstrom standards, 
which must be a list containing at least 'Variables' list of elements.
Please refer to documentation.")
  
}

#' @title
#' Validate and coerce any object as data dictionary
#'
#' @description
#' Validates the input object as a valid data dictionary and coerces it with 
#' the appropriate 'madshapR::class' attribute. This function mainly helps 
#' validate input within other functions of the package but could be used to 
#' check if an object is valid for use in a function.
#' 
#' @details
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
#' @seealso
#' For a better assessment, please use [data_dict_evaluate()].
#'
#' @param object A potential valid data dictionary to be coerced.
#'
#' @returns
#' A list of tibble(s) identifying a data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$dd_PARIS_format_maelstrom
#' as_data_dict(data_dict)
#'
#'}
#'
#' @import dplyr tidyr stringr fabR
#' @importFrom crayon bold
#' @importFrom rlang .data
#' 
#' @export
as_data_dict <- function(object){
  
  data_dict <- as_data_dict_shape(object)
  
  # variable names must be unique and non-null
  if(check_data_dict_variables(data_dict) %>% 
     dplyr::filter(str_detect(.data$`condition`,"\\[ERR\\]")) %>% nrow > 0){
    stop(call. = FALSE,
"Variable names must exist and be unique in your data dictionary.",
         bold("\n\nUseful tip:"),
" Use data_dict_evaluate(data_dict) to get a full assessment of your
data dictionary")}
  
  # variable names must be unique and non-null
  if(sum(nrow(data_dict[['Categories']])) > 0){
    if(check_data_dict_categories(data_dict) %>% 
       dplyr::filter(str_detect(.data$`condition`,"\\[ERR\\]")) %>% nrow > 0){
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
  
  # dataset shaping
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    ungroup() %>%
    mutate(across(everything() ,~str_squish(.))) %>%
    mutate(across(everything() ,~na_if(.,"")))
  
  if(sum(nrow(data_dict[['Categories']])) > 0){
    
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      ungroup() %>%
      mutate(across(everything(),~str_squish(.))) %>%
      mutate(across(everything(),~na_if(.,"")))
  }
  
  # if not exists, addition of typeof for categorical variables, text else
  if(length(data_dict[['Variables']][['typeof']]) == 0){
    
    # test if vT exists and is good
    if(length(data_dict[['Variables']][['valueType']]) > 0){
      
      test_vT <-
        data_dict[['Variables']] %>%
        select('name', 'valueType') %>%
        mutate(`valueType` = replace_na(.data$`valueType`,"character")) %>%
        left_join(
          madshapR::valueType_list %>%
            select(
              valueType = 'valueType',
              typeof = 'typeof') %>%
            distinct,
          by = "valueType")
      
      if(all(!is.na(test_vT))){
        
        data_dict[['Variables']] <-
          data_dict[['Variables']] %>%
          left_join(test_vT %>% select('name','typeof'), by = 'name')
        
      }else if(sum(nrow(data_dict[['Categories']])) > 0){
        
        category_outcomes <-
          data_dict[['Categories']] %>%
          select('name') %>% distinct %>%
          rowwise() %>%
          mutate(valueType = valueType_guess(.data$`name`))
        
        category_outcomes <-
          data_dict[['Categories']] %>%
          select('variable','name') %>%
          left_join(category_outcomes, by = "name") %>%
          select('variable','valueType') %>%
          distinct %>%
          group_by(.data$`variable`) %>%
          summarise(valueType = paste0(.data$`valueType`,collapse = "|"))
        
        category_outcomes <-
          data_dict[['Categories']] %>%
          select('variable','name') %>%
          left_join(category_outcomes, by = "variable") %>%
          group_by(.data$`variable`) %>% group_split() %>% as.list() %>%
          lapply(function(x){
            test_vT <- str_detect(x$valueType[1], "\\|")
            if(test_vT) x <- x %>% mutate(
              valueType = valueType_guess(unique(x$name)))
            return(x)
          }) %>%
          bind_rows() %>%
          select('variable','valueType') %>% distinct %>%
          left_join(madshapR::valueType_list, by = "valueType") %>%
          select(name = 'variable','typeof')
        
        data_dict[['Variables']] <-
          left_join(data_dict[['Variables']],category_outcomes,by = "name") %>%
          mutate(`typeof` = replace_na(.data$`typeof`,'character'))
        
      }else{data_dict[['Variables']][['typeof']] <- 'character'}
    }else{data_dict[['Variables']][['typeof']] <- 'character'}
  }
  
  if(sum(nrow(data_dict[['Categories']])) > 0){
    
    # addition of valueType for sorting elements

    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      select('variable','name') %>% 
      add_index('madshapR::index', .force = TRUE) %>%
      left_join(data_dict[['Variables']] %>%
                  select(variable = 'name', 'typeof'), by = "variable") %>%
      group_by(typeof) %>% group_split() %>% as.list %>%
      lapply(function(x) {
        x$name <- as_valueType(x$`name`, valueType_guess(unique(x$`name`)))
        x <- x %>% arrange(.data$`variable`, .data$`name`) %>%
          mutate(name = as.character(.data$`name`))
        return(x)
      }) %>% bind_rows() %>%
      select(-'typeof') %>%
      left_join(
        data_dict[['Categories']] %>%
          add_index('madshapR::index',.force = TRUE) %>%
          select(-'name'),by = c('madshapR::index', 'variable')) %>%
      select(-'madshapR::index')
    
    # add labels if not exists
    if(length(data_dict[['Categories']][['labels']]) == 0){
      if(length(data_dict[['Categories']][['levels']]) > 0){
        
        # check if levels equals name (that means the levels are factors)
        if(all(data_dict[['Categories']][['levels']] ==
               data_dict[['Categories']][['name']],na.rm = TRUE)){
          data_dict[['Categories']]['levels'] <- NULL}}
      
      # name label as names
      data_dict[['Categories']]['labels'] <- data_dict[['Categories']]['name']
    } # else do nothing
    
    # add na_values (as NA, will be removed anyway) if not exists
    if(length(data_dict[['Categories']][['na_values']]) == 0){
      
      # name label as names
      data_dict[['Categories']]['na_values'] <- NA_character_
    } # else do nothing
    
    # gather haven and factors if levels remain
    if(length(data_dict[['Categories']][['levels']]) > 0){
      
      # check if levels isnt NA when labels is (recip.) and
      # check if na_values is NA when levels is (recip.)
      # (that means labels and levels are factors)
      
      if(all(!is.na(data_dict[['Categories']][['levels']]) ==
             is.na(data_dict[['Categories']][['labels']]))){
        data_dict[['Categories']] <-
          data_dict[['Categories']] %>%
          mutate(
            labels =
              ifelse(
                !is.na(.data$`levels`) & is.na(.data$`na_values`),
                .data$`levels`, .data$`labels`),
            levels =
              ifelse(
                !is.na(.data$`levels`) & (.data$`levels` == .data$`labels`),
                NA_character_ , .data$`levels`))}
    }
    
    # rearrange elements by missingness, then name, then variable
    new_name <-
      setdiff(
        make.unique(c('missing',names(data_dict[['Categories']])))[-1],
        names(data_dict[['Categories']]))
    
    names(data_dict[['Categories']]) <-
      make.unique(c("missing",names(data_dict[['Categories']])))[-1]
    
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      mutate(
        missing = !is.na(.data$`na_values`),
        missing = ifelse(is.na(.data$`missing`),FALSE,.data$`missing`)) %>%
      mutate(
        missing =
          ifelse(
            (.data$`name` < 0 & .data$`missing` == TRUE),
            2, .data$`missing`)) %>%
      group_by(.data$`variable`) %>%
      arrange(.data$`variable`,.data$`missing`) %>%
      ungroup() %>%
      mutate(missing = ifelse(.data$`missing` == 2, 1, .data$`missing`)) %>%
      mutate(missing = as.logical(.data$`missing`)) %>%
      select(-'missing')
    
    if(length(new_name) > 0)
      data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      rename_with(.cols = any_of(new_name) , .fn = ~ paste0('missing'))
    
  }
  
  # reorder things
  # dataset shaping
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>% select('name','typeof',everything())
  data_dict[['Variables']] <-
    data_dict[['Variables']][
      vapply(X = data_dict[['Variables']],
             FUN = function(x) !all(is.na(x)),
             FUN.VALUE = logical(1))]
  
  if(sum(nrow(data_dict[['Categories']])) > 0){
    
    data_dict[['Categories']] <-
      inner_join(
        data_dict[['Variables']] %>%
          select(variable = 'name'), data_dict[['Categories']],
        by = "variable",multiple = "all") %>%
      select('variable','name','labels',matches("^na_values$"), everything())
    
    data_dict[['Categories']] <-
      data_dict[['Categories']][vapply(
        X = data_dict[['Categories']],
        FUN = function(x) !all(is.na(x)),
        FUN.VALUE = logical(1))]
    
  }
  
  # if all test pass:
  attributes(data_dict)$`madshapR::class` <- "data_dict"
  return(data_dict)
}

#' @title
#' Validate and coerce an object to an Opal data dictionary format
#'
#' @description
#' Validates the input object as a valid data dictionary compliant with formats 
#' used in Maelstrom Research ecosystem, including Opal, and returns it with 
#' the appropriate 'madshapR::class' attribute. This function mainly helps 
#' validate input within other functions of the package but could be used to 
#' check if an object is valid for use in a function.
#'
#' @details
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
#' @seealso
#' For a better assessment, please use [data_dict_evaluate()].
#'
#' @param object A potential valid data dictionary to be coerced.
#' @param as_data_dict Whether the output data dictionary has a simple
#' data dictionary structure or not (meaning has a Maelstrom data dictionary
#' structure, compatible with Maelstrom Research ecosystem, including Opal). 
#' FALSE by default.
#' @param name_standard Whether the input data dictionary has variable names
#' compatible with Maelstrom Research ecosystem, including Opal)or not. 
#' TRUE by default.
#'
#' @returns
#' A list of tibble(s) identifying a data dictionary.
#'
#' @examples
#' {
#' 
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$dd_MELBOURNE_1_format_maelstrom
#' as_data_dict_mlstr(DEMO_files$dd_MELBOURNE_1_format_maelstrom)
#'
#' }
#'
#' @import dplyr tidyr fabR
#' @importFrom crayon bold
#' @importFrom rlang .data
#'
#' @export
as_data_dict_mlstr <- function(
    object, 
    as_data_dict = FALSE, 
    name_standard = TRUE){
  
  # test if data_dict is already data dictionary
  data_dict <- as_data_dict(object)
  
  if(!is.logical(as_data_dict))
    stop(call. = FALSE,
         '`as_data_dict` must be TRUE of FALSE (FALSE by default)')
  
  # if valueType exists, vT must be valid
  if(suppressWarnings(check_data_dict_valueType(data_dict))  %>%
     dplyr::filter(str_detect(.data$`condition`,"\\[ERR\\]")) %>% nrow > 0){
    stop(call. = FALSE,
         "valueType are incompatible with Maelstrom standards.",
         bold("\n\nUseful tip:"),
         " Use data_dict_evaluate(data_dict) to get a full assessment of your
data dictionary")}
  
  # check missing validity
  if(suppressWarnings(check_data_dict_missing_categories(data_dict)) %>% 
     dplyr::filter(str_detect(.data$`condition`,"\\[ERR\\]")) %>% nrow > 0){
    stop(call. = FALSE,
         "\n
Incompatible missing value in the missing columns with Maelstrom standards",
         bold(
"\n\nUseful tip:"),
" Use data_dict_evaluate(data_dict) to get a full assessment of your
data dictionary")}
  
  # Check standard for names
  if(name_standard == TRUE){
    if(nrow(check_name_standards(data_dict[['Variables']][['name']])) > 0){
      stop(call. = FALSE,
"names are incompatible with Maelstrom standards.",
bold("\n\nUseful tip:"),
" Use data_dict_evaluate(data_dict) to get a full assessment of your
data dictionary")}
  }
  
  # assess if tO is good
  test_vT <-
    data_dict[['Variables']] %>%
    select('name', 'typeof') %>%
    mutate(`typeof` = replace_na(.data$`typeof`,"character")) %>%
    left_join(
      madshapR::valueType_list %>%
        select(
          valueType = 'toValueType',
          typeof = 'toTypeof') %>%
        distinct,
      by = "typeof")
  
  # si pas bon de base, mettre un message et garder tO
  if(!all(!is.na(test_vT))){
    warning(
      "The column 'typeof' in your data dictionary contains values that were
impossible to coerce in valueType. This column can be kept for further
investigations.",
      "\n\nVariable(s) name : ",
      toString(pull(test_vT[which(is.na(test_vT[['valueType']])),'name'])))
  }else{data_dict[['Variables']][['typeof']] <- NULL}
  
  # si vT existe
  # add valueType if not exists
  if(length(data_dict[['Variables']][['valueType']]) == 0){
    
    # sinon on rajoute vT au dd
    data_dict[['Variables']] <-
      data_dict[['Variables']] %>%
      left_join(test_vT %>% select('name','valueType'), by = 'name')
  } # else do nothing
  
  # add label(:xx) if not present
  lab_name_var <-
    names(data_dict[['Variables']] %>%
            select(matches(c("^label$","^label:[[:alnum:]]"))))
  
  # add label if does not exists
  if(length(lab_name_var) == 0){
    data_dict[['Variables']] <-
      data_dict[['Variables']] %>% mutate(label = .data$`name`)}
  
  
  if(sum(nrow(data_dict[['Categories']])) > 0){
    
    # addition of label(:xx) if not present
    
    lab_name_var <-
      names(data_dict[['Variables']] %>%
              select(matches(c("^label$","^label:[[:alnum:]]"))))[1]
    
    lab_name_cat <-
      intersect(lab_name_var,
        names(data_dict[['Categories']] %>%
                select(matches(c("^label$","^label:[[:alnum:]]")))))
    
    if(length(lab_name_cat) == 0){
      
      # check presence of labels, if identical to name, NULL, rename else

      data_dict[['Categories']] <-
        data_dict[['Categories']] %>%
        rename_with(.cols = "labels", ~ lab_name_var)
      
    }else if(all(data_dict[['Categories']][['labels']] ==
                 data_dict[['Categories']][['name']])) {
      data_dict[['Categories']][['labels']] <- NULL}
    
    # addition of missing if not present
    missing_name <-
      names(data_dict[['Categories']] %>% select(matches(c("^missing$"))))
    
    if(length(missing_name) == 0){
      
      # check presence of na_values, if identical to label, NULL, rename else
      if(length(data_dict[['Categories']][['na_values']]) == 0){
        
        data_dict[['Categories']]$`missing` <- FALSE
        
      }else if(all(data_dict[['Categories']][['na_values']] ==
                   data_dict[['Categories']][['label']],na.rm = TRUE)){
        data_dict[['Categories']]$`missing` <-
          !is.na(data_dict[['Categories']]$`na_values`)
        data_dict[['Categories']]$`na_values` <- NULL
      }else{data_dict[['Categories']]$`missing` <- FALSE}
      
      # check if missings and na_values are duplicated
    }else if(all(!is.na(data_dict[['Categories']][['na_values']]) ==
                 data_dict[['Categories']][['missing']])){
      data_dict[['Categories']]$`na_values` <- NULL
    }
    
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      mutate(
        missing = as_any_boolean(.data$`missing`),
        missing = ifelse(is.na(.data$`missing`),FALSE,.data$`missing`)) %>%
      mutate(
        missing =
          ifelse((
            .data$`name` < 0 & .data$`missing` == TRUE),
            2, .data$`missing`)) %>%
      group_by(.data$`variable`) %>%
      arrange(.data$`variable`,.data$`missing`) %>%
      ungroup() %>%
      mutate(missing = ifelse(.data$`missing` == 2, 1, .data$`missing`)) %>%
      mutate(missing = as.logical(.data$`missing`))
    
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      select(
        'variable','name',
        matches(c("^label$","^label:[[:alnum:]]")),matches("^missing$"),
        everything())
  }
  
  if(sum(nrow(data_dict[['Categories']])) == 0)
    data_dict[['Categories']] <- NULL
  
  if(as_data_dict == TRUE){
    
    # # check if label and name are duplicated
    # if(length(data_dict[['Variables']][['label']]) > 0){
    #   if(all(data_dict[['Variables']][['name']] ==
    #          data_dict[['Variables']][['label']])){
    #     data_dict[['Variables']]$`label` <- NULL}}
    
    # valueType as typeof
    if(length(data_dict[['Variables']][['typeof']]) == 0){
      
      data_dict[['Variables']] <-
        data_dict[['Variables']] %>%
        left_join(
          madshapR::valueType_list %>%
            select(
              valueType = "valueType",
              typeof = "toTypeof") %>%
            distinct,
          by = "valueType") %>%
        select(-"valueType")}
    
    if(sum(nrow(data_dict[['Categories']])) > 0){
      
      # protection of labels if already exists
      if(length(data_dict[['Categories']][['labels']]) > 0){
        new_name <-
          setdiff(
            make.unique(c('labels',names(data_dict[['Categories']])))[-1],
            names(data_dict[['Categories']]))
        
        warning(
"The data dictionary contains 'labels' column, which usage is protected in R.
new name: ",new_name)
        
        names(data_dict[['Categories']]) <-
          make.unique(c('labels',names(data_dict[['Categories']])))[-1]
      }
      
      data_dict[['Categories']] <-
        data_dict[['Categories']] %>%
        rename_with(.cols = starts_with("label")[1], ~ 'labels')
      
      # protection of na_values if already exists
      if(length(data_dict[['Categories']][['na_values']]) > 0){
        new_name <-
          setdiff(
            make.unique(c('na_values',names(data_dict[['Categories']])))[-1],
            names(data_dict[['Categories']]))
        
        warning(
"The data dictionary contains 'na_values' column, which usage is protected in R.
New name: ",new_name)
        
        names(data_dict[['Categories']]) <-
          make.unique(c('na_values',names(data_dict[['Categories']])))[-1]
      }
      
      data_dict[['Categories']] <-
        data_dict[['Categories']] %>%
        rename_with(.cols = "missing", ~ 'na_values') %>%
        mutate(
          na_values =
            ifelse(.data$`na_values` == TRUE,.data$`labels`, NA_character_))
      data_dict[['Categories']] <-
        data_dict[['Categories']] %>%
        select(
          'variable',
          'name',
          'labels',
          'na_values',
          everything())
    }
    
    data_dict[['Variables']] <-
      data_dict[['Variables']] %>%
      select('name','typeof',everything())
    
  }
  
  # reorder things
  data_dict[['Variables']] <-
    suppressMessages({left_join(
      data_dict[['Variables']] %>%
        select(
          'name',
          matches(c("^label$","^label:[[:alnum:]]")),
          matches('^valueType$')),
      data_dict[['Variables']][vapply(
        X = data_dict[['Variables']],
        FUN = function(x) !all(is.na(x)),
        FUN.VALUE = logical(1))] %>% 
        bind_rows(tibble(name = as.character())))})
    
  if(sum(nrow(data_dict[['Categories']])) > 0){
    
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      left_join(data_dict[['Variables']] %>%
                  select(variable =  'name') %>%
                  add_index('madshapR::index'),
                by = join_by('variable')) %>%
      arrange(.data$`madshapR::index`) %>%
      select(-'madshapR::index')
      
    data_dict[['Categories']] <-
      suppressMessages({left_join(
        data_dict[['Categories']] %>%
          select(
            'variable','name',
            matches(c("^labels$","^label$","^label:[[:alnum:]]"))),
        data_dict[['Categories']][vapply(
          X = data_dict[['Categories']],
          FUN = function(x) !all(is.na(x)),
          FUN.VALUE = logical(1))] %>% 
          bind_rows(tibble(variable = as.character())))})
    
  }
  
  if(as_data_dict == TRUE) {
    attributes(data_dict)$`madshapR::class` <- "data_dict"
  }else{
    attributes(data_dict)$`madshapR::class` <- "data_dict_mlstr"}
  
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
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$dd_MELBOURNE_1_format_maelstrom
#' is_data_dict_shape(data_dict)
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
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$dd_MELBOURNE_1_format_maelstrom
#' is_data_dict(data_dict)
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
  # if only the tibble is given in parameter
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
#' # use DEMO_files provided by the package
#'
#' data_dict <- DEMO_files$dd_MELBOURNE_1_format_maelstrom
#' is_data_dict_mlstr(data_dict)
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
  # if only the tibble is given in parameter
  test <- silently_run(try(as_data_dict_mlstr(object),silent = TRUE))
  if(class(test)[1] == 'try-error')    return(FALSE)
  return(TRUE)
}
