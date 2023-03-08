
#' @title
#' Generate a web application (bookdown) report of list of a datasets
#'
#' @description
#' Generates a visual report for a study-specific dataset in an HTML
#' bookdown document, showing descriptive statistics for each study-specific
#' variable to facilitate the assessment of input data. Statistics and figures
#' are generated according to their valueTypes.
#' This report can be used to assist the user in the assessment of the data
#' structure, fields investigation (mandatory or not), coherence across elements
#' and taxonomy or standard evaluation. The summaries and figures associated
#' provide dataset composition, with observation repartition and descriptive
#' statistics.
#'
#' @details
#' A study must be a named list containing at least one data frame or
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
#' In addition, the data dictionary may follow Maelstrom research stardards,
#' and its content can be evaluated accordingly, such as naming convention
#' restriction, columns like 'valueType', 'missing' and 'label(:xx)',
#' and/or any taxonomy provided.
#'
#' A dataset must be a data frame or data frame extension (e.g. a tibble) and
#' can be associated to a data dictionary. If not, a minimum workable data
#' dictionary can always be generated, when any column will be reported, and
#' any factor column will be analysed as categorical variable (the column
#' 'levels' will be created for that. In addition, the dataset may follow
#' Maelstrom research stardards, and its content can be evaluated accordingly,
#' such as naming convention restriction, or id columns declaration (which
#' full completeness is mandatory.
#'
#' @seealso
#' [datashapR::open_visual_report()]
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
#' @param out parameter that specifies the graphical outputs expected in the
#' report. Can be 'ggplot2'.
#' @param .keep_files whether to keep the R-markdown files.
#' FALSE by default.
#'
#' @return
#' A bookdown folder containing files in the specified output folder. To
#' open the file in browser, open 'index.html'. Or use
#' [datashapR::open_visual_report()]
#'
#' @examples
#' \dontrun{
#' # Create index of files in one folder and read the files
#' index_DEMO <- file_index_create(folder = ""DEMO"")
#' file_index_read(index = index_DEMO, file_name = ""study_TOKYO"")
#' file_index_read(index = index_DEMO, file_name = ""dd_TOKYO"")
#' # use case 1: report of demo dataset TOKYO
#' study_visual_report(
#' dataset = study_TOKYO,
#' data_dict = dd_TOKYO_format_maelstrom_tagged,
#' to = ""DEMO/reports/TOKYO"")
#' # use case 2: report of demo dataset TOKYO, grouped by gndr
#' study_visual_report(
#' dataset = study_TOKYO,
#' data_dict = dd_TOKYO_format_maelstrom_tagged,
#' to = ""DEMO/reports/TOKYO_gndr"",group_by = ""gndr"",out = ""ggplot2"")
#'# re-index files to include new files created
#' index_DEMO <- file_index_create(folder = ""DEMO"")
#' # read the book down
#' file_index_read(
#'  index_DEMO,file_path = ""DEMO/reports/TOKYO_gndr/docs/index.html"")
#' }
#'
#' @import dplyr knitr fabR
#' @import bookdown utils readr stringr grDevices fs DT
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
dataset_visualize <- function(
  dataset = NULL,
  data_dict = NULL,
  group_by = NULL,
  to,
  out = "ggplot2",
  .keep_files = TRUE){

  # check input
  if(!is.null(dataset)) as_dataset(dataset)
  if(!is.null(data_dict)) as_data_dict(data_dict)
  if(!is.logical(.keep_files))
    stop(call. = FALSE,'`.keep_files` must be TRUE of FALSE (TRUE by default)')

  if(is.null(data_dict)){data_dict <- data_dict_extract(dataset)}

  if(!"summary_1" %in% (data_dict$Variables %>% names)){

    data_dict$Variables <- fabR::add_index(data_dict$Variables, .force = TRUE)
    data_dict <-
      data_dict %>%
      identify_visual_type(dataset = dataset) %>%
      identify_plot_type(dataset = dataset, group_by = group_by, out = out)
  }

#   if(nrow(data_dict$Variables) == 0){
#
#     return(message(
# "[Error]: the name of your dataset has not been found in your data dictionary.
#  Please verify the name of your dataset in your datadictionary (column 'name'
#  in 'Variables' sheet)
# and reprocess."))
#   }

  # global data
  ## dataset must have ID in first column

  # count_tag <- count_tag(data_dict)
  # all_na_column <- fabR::get_all_na_cols(dataset)
  # nb_unique_participants <- dataset %>% select(1) %>% unique %>% nrow()

  fabR::template_visual_report(to)
  save(to,data_dict,group_by,
       file = paste0(to,"/temp_bookdown_report/bookdown_report.RData"))

  ## markdown writing elements

  ##### HEADER ##########

  paste0(
    '# About the study dataset {.unnumbered #about}

```{r echo = FALSE, message = FALSE, warning = FALSE}

library(datashapR)
load(file = paste0(file = paste0("',
getwd(),"/",to,'/temp_bookdown_report/bookdown_report.RData")))

```
--------------------------------------------------------------------------------




**Number of variables (including id column)**: ',dataset %>% ncol,'



## Variables

```{r echo = FALSE, message = FALSE, warning = FALSE}


data_dict$Variables <-
  data_dict$Variables[vapply(
    X = data_dict$Variables,
    FUN = function(x) !all(is.na(x)),
    FUN.VALUE = logical(1))]

datatable(
  data_dict$Variables %>%
    select(-viz_type,-code_dd,-plot_1,-plot_2,-plot_3,-plot_4,-summary_1) %>%
    filter(name != "',dataset[1] %>% names,'") %>%
    mutate(name = paste0("<a href=\\"./var",index,".html\\" >",name,"</a>")),
  options = list(scrollX = TRUE),rownames = FALSE,escape = FALSE)

```
--------------------------------------------------------------------------------

## Categories

```{r echo = FALSE, message = FALSE, warning = FALSE}

if(sum(nrow(data_dict$Categorie)) > 0){
  data_dict$Categories <-
    data_dict$Categories[vapply(
      X = data_dict$Categories,
      FUN = function(x) !all(is.na(x)),
      FUN.VALUE = logical(1))]

  datatable(data_dict$Categories,
  options = list(scrollX = TRUE),rownames = FALSE)
}

```
--------------------------------------------------------------------------------

') %>% write_lines(
  file =
    paste0(to,"/temp_bookdown_report/file/bookdown-template-master/index.Rmd"),
  append = TRUE)


  # ## Areas of information
  #
  # ```{r echo = FALSE, message = FALSE, warning = FALSE}
  #
  # # fabR::plot_bar("data_dict$Variables", "mlstr_area_1", out = "plotly-code")
  # # fabR::plot_pie("data_dict$Variables", "mlstr_area_1", out = "plotly-code")
  #
  # ```
  #

  ##### CONTENT ##########

  increment <-
    paste0(rep(0,nchar(nrow(data_dict$Variables))) %>% paste(collapse = ""))

  for(i in seq_len(nrow(data_dict$Variables))){

    rmd_file_name <-
      paste0(to,"/temp_bookdown_report/file/bookdown-template-master/",
             str_sub(paste0(increment,i),-(increment %>% nchar + 1),-1),"-",
             data_dict$Variables$name[i],".Rmd")
    file.create(rmd_file_name)

    paste0(
      "# ",data_dict$Variables$name[i],"{.unnumbered #var",i,"}\n\n") %>%

      paste0("\n**VARIABLE CHARACTERISTICS**\n") %>%

      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0(
        "\n```{r ",
        str_squish("
echo = FALSE,
message = FALSE,
warning = FALSE,
knitr.figure = TRUE}"),"\n",
"datatable(
   data_dict$Variables %>%
     filter(name == '",data_dict$Variables$name[i],"') %>%
     select(-viz_type,-code_dd,-plot_1,-plot_2,-plot_3,-plot_4,-summary_1) %>%
     gather %>% filter(!is.na(value)) %>%
     mutate(key = paste0('<b>' , key, '</b>')),
   options = list(dom = 't', scrollX = TRUE, ordering = FALSE,paging = TRUE),
   rownames = FALSE, colnames = rep('', 2),filter = 'none' ,  escape = FALSE)",
        "\n```\n") %>% cat

      paste0("\n</div>\n\n") %>%
      paste0(
        ifelse(nrow(data_dict$Categories %>%
                 filter(.data$`variable` == data_dict$Variables$name[i])) > 0,
               paste0("\n* **Categories**: ","\n\n") %>%
                 paste0("\n<div style= \"display:flex; margin:auto\" > \n\n")%>%
                 paste0(
                   "\n```{r echo = FALSE, message = FALSE, warning = FALSE}\n",
                   "datatable(
   data_dict$Categories %>% filter(variable == '",
   data_dict$Variables$name[i],"'),
   options = list(scrollX = TRUE),rownames = FALSE)",
                   "\n```\n") %>%
                 paste0("\n</div>\n\n")
               ,"")) %>%
      paste0("
\n----------------------------------------------------------------------\n") %>%
      paste0("\n**SUMMARY STATISTICS**\n") %>%
      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0(
"\n```{r ",
str_squish("
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  knitr.figure = TRUE}"),"\n\n",
data_dict$Variables$summary_1[i],"\n\n```\n") %>%
      paste0("\n</div>\n\n") %>%
      paste0(
"\n---------------------------------------------------------------------\n") %>%
      paste0("\n**VISUAL REPRESENTATION**\n") %>%

      paste0(
"\n```{r, figures-plot12-",i,
str_squish(", fig.show='hold',
 fig.align = 'center',
 echo = FALSE,
 message = FALSE,
 warning = FALSE}"),"\n",
             "\n","try({",data_dict$Variables$plot_1[i],"}, silent = TRUE)\n",
             "\n","try({",data_dict$Variables$plot_2[i],"}, silent = TRUE)\n",
             "\n","try({",data_dict$Variables$plot_3[i],"}, silent = TRUE)\n",
             "\n","try({",data_dict$Variables$plot_4[i],"}, silent = TRUE)\n",
             "\n```\n") %>%
      paste0("\n") %>%

      write_lines(file = rmd_file_name, append = FALSE)
  }

  wd <- getwd()
  graphics.off()

  setwd(
    paste0(wd,"/",to,"/temp_bookdown_report/file/bookdown-template-master/"))
  fabR::silently_run(file.remove(list.files()%>%str_subset(names(dataset[1]))))

  try({
    render_book(paste0(
      wd,"/",to,
      "/temp_bookdown_report/file/bookdown-template-master/index.Rmd"))},
    silent = FALSE)
  setwd(wd)

  if(dir.exists(paste0(to,"/docs"))) dir_delete(paste0(to,"/docs"))
  dir_copy(
    paste0(to,"/temp_bookdown_report/file/bookdown-template-master/docs"),
    paste0(to,"/docs"))

  if(.keep_files == FALSE){dir_delete(paste0(to,"/temp_bookdown_report/"))}

  # browseURL(paste0(to,"/docs/index.html"))

  return(message(
"\n\nTo edit your file, You can use the function `open_visual_report('",to,"')`
(Compatibility tested on Chrome and Mozilla)\n\n"))

}


#' @title
#' Identify visual type of a variable based on valueType
#'
#' @description
#' This helper function analyses the content of a dataset and its data
#' dictionary to extract the type of visualization to generate in a report.
#' This function can be used to manually personalize the report parameters.
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
#' In addition, the data dictionary may follow Maelstrom research stardards,
#' and its content can be evaluated accordingly, such as naming convention
#' restriction, columns like 'valueType', 'missing' and 'label(:xx)',
#' and/or any taxonomy provided.
#'
#' A dataset must be a data frame or data frame extension (e.g. a tibble) and
#' can be associated to a data dictionary. If not, a minimum workable data
#' dictionary can always be generated, when any column will be reported, and
#' any factor column will be analysed as categorical variable (the column
#' 'levels' will be created for that. In addition, the dataset may follow
#' Maelstrom research stardards, and its content can be evaluated accordingly,
#' such as naming convention restriction, or id columns declaration (which
#' full completeness is mandatory.
#'
#' @param dataset A tibble identifying the input data observations associated
#' to its data dictionary.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset. Automatically generated if not provided.
#'
#' @return
#' A list of two tibbles which makes up the data dictionary in
#' Maelstrom Research format where a column 'viz_type' has been added to the
#' data dictionary provided as an input.
#'
#' @examples
#' \dontrun{
#' # Example 1: viz type of iris dataset
#' library(tidyverse)
#' identify_visual_type(dataset = iris) %>% .$Variables %>%
#'  select(name,viz_type)
#' }
#'
#' @import dplyr stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
identify_visual_type <- function(dataset, data_dict){

  try({

    if(sum(nrow(data_dict[['Categories']])) > 0 ){
      var_names_cat_dd <-
        data_dict[['Categories']] %>%
        select(.data$`variable`, code = .data$`name`) %>% unique %>%
        group_by(.data$`variable`) %>%
        summarise(code_dd = paste(.data$`code`,collapse = "','")) %>%
        mutate(code_dd = paste0("c('NA','",.data$`code_dd`,"')")) %>%
        filter(!is.na(.data$`variable`))
    }else{
      var_names_cat_dd <-
        tibble(
          variable = as.character(),
          code = as.character(),
          code_dd = as.character())
    }

    if(nrow(var_names_cat_dd) == 0){
      data_dict$Variables <-
        data_dict$Variables %>%
        rename(variable = .data$`name`) %>%
        filter(.data$`variable` %in% (dataset %>% names)) %>%
        mutate(viz_type = .data$`valueType`) %>%
        rename(name = .data$`variable`)
    }else{

      name_var <-
        names(dataset[names(dataset) %in% (var_names_cat_dd$variable)])
      var_name_cat_dataset <-
        tibble(variable = as.character(),code_dataset = as.character())
      for(i in name_var){
        var_name_cat_dataset <-
          add_row(
            var_name_cat_dataset,
            paste0("c('",
                   pull(unique(dataset[i])) %>%
                     toString %>%
                     str_replace_all(", ","','"),"')") %>%
              as_tibble() %>% mutate(variable = i) %>%
              select(.data$`variable`, code_dataset = .data$`value`))}

      to_eval <-
        var_name_cat_dataset %>%
        inner_join(var_names_cat_dd, by = "variable") %>%
        mutate(
          to_eval =
            paste0("all(",.data$`code_dataset`," %in% ",.data$`code_dd`,")"))

      to_eval <-
        to_eval %>% rowwise %>%
        mutate(
          to_eval = eval(parse(text = str_squish(.data$`to_eval`) %>%
                                 str_remove_all("\\\r")))) %>%
        mutate(
          viz_type = ifelse(.data$`to_eval` == TRUE,"categorical","dual")) %>%
        select(.data$`variable`,.data$`viz_type`) %>%
        ungroup()

      data_dict$Variables <-
        data_dict$Variables %>% rename(variable = .data$`name`) %>%
        left_join(to_eval, by = "variable") %>%
        filter(.data$`variable` %in% (dataset %>% names)) %>%
        mutate(
          viz_type =
            ifelse(is.na(.data$`viz_type`),.data$`valueType`,.data$`viz_type`)
          ) %>%
        rename(name = .data$`variable`)
    }

  },silent = FALSE)

  return(data_dict)

}

#' @title
#' Generate R script for plots based on the 'viz_type' of the variable
#' coucou
#'
#' @description
#' This helper function uses the visual type attributed to a variable in a data
#' dictionary to generate an R script that generates plots in a report. This
#' function can be used to manually personalize the report parameters. The plots
#' can use an additional variable to group each variable shown by the grouping
#' variable.
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
#' In addition, the data dictionary may follow Maelstrom research stardards,
#' and its content can be evaluated accordingly, such as naming convention
#' restriction, columns like 'valueType', 'missing' and 'label(:xx)',
#' and/or any taxonomy provided.
#'
#' A dataset must be a data frame or data frame extension (e.g. a tibble) and
#' can be associated to a data dictionary. If not, a minimum workable data
#' dictionary can always be generated, when any column will be reported, and
#' any factor column will be analysed as categorical variable (the column
#' 'levels' will be created for that. In addition, the dataset may follow
#' Maelstrom research stardards, and its content can be evaluated accordingly,
#' such as naming convention restriction, or id columns declaration (which
#' full completeness is mandatory.
#'
#' @param dataset A tibble identifying the input data observations associated to
#' its data dictionary.
#' @param data_dict A list of tibble(s) representing meta data of an
#' associated dataset. Automatically generated if not provided.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#' @param out parameter that specifies the graphical outputs expected in the
#' report: can be either 'ggplot2' or 'ggplot2-code'.
#'
#' @return
#' A list of two tibbles which makes up the data dictionary in
#' Maelstrom Research format where columns plots and summary have been added to
#' the data dictionary provided as an input.
#'
#' @examples
#' \dontrun{
#' # Example 1: plot R stripts for iris variables.
#' data_dict_extract(iris, categories = ""Species"") %>%
#' identify_visual_type(data_dict = ., dataset = iris) %>%
#' identify_plot_type(data_dict = ., dataset = iris) %>% .$Variables %>%
#' select(name,viz_type, contains(""plot""),contains(""summary""))
#' }
#'
#' @import dplyr stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
identify_plot_type <- function(
    dataset = NULL,
    data_dict,
    group_by = NULL,
    out = "plotly"){

  if(! "viz_type" %in% colnames(data_dict$Variables)){
    data_dict$Variables <-
      data_dict$Variables %>% mutate(viz_type = .data$`valueType`)
  }

  if(sum(nrow(data_dict[['Categories']])) == 0 ){
    data_dict[['Categories']] <-
      tibble(
        variable = as.character(),
        name = as.character(),
        missing = as.logical(),
        code_dd = as.character())}

  data_dict$Variables <-
    data_dict$Variables %>%
    left_join(
      data_dict$Categories %>%
        select(
          name = .data$`variable`,
          code = .data$`name`,
          .data$`missing`) %>%
        filter(.data$`missing` == 1) %>%
        group_by(.data$`name`) %>%
        summarise(code_dd = paste(.data$`code`,collapse = "','")) %>%
        mutate(code_dd = paste0("c('",.data$`code_dd`,"')")) %>%
        filter(!is.na(.data$`name`)), by = "name") %>%
    mutate(code_dd = replace_na(.data$`code_dd`,"c()"))

  group_by <- ifelse(is.null(group_by),'NULL', paste0("'",group_by,"'"))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(
      plot_1 = case_when(
        .data$`viz_type` == "text"                                             ~
          paste0(
            'fabR::plot_main_word(tbl = dataset,col = "',
            .data$`name`,'" , missing_values = "',
            .data$`code_dd`,'", out = "',
            out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "decimal"                                          ~
          paste0(
            'fabR::plot_box(tbl = dataset,col = "',
            .data$`name`,'" , missing_values = "',
            .data$`code_dd`,'", out = "',
            out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "integer"                                          ~
          paste0(
            'fabR::plot_box(tbl = dataset,col = "',
            .data$`name`,'" , missing_values = "',
            .data$`code_dd`,'", out = "',
            out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "date"                                             ~
          paste0(
            'fabR::plot_date(tbl = dataset,col = "',
            .data$`name`,'" , missing_values = "',
            .data$`code_dd`,'", out = "',
            out,'-code", group_by = ',group_by,' , time = "year")'),

        .data$`viz_type` == "dual"  & .data$`valueType` == "text"              ~
          paste0(
            'fabR::plot_main_word(tbl = dataset,col = "',
            .data$`name`,'" , missing_values = "',
            .data$`code_dd`,'", out = "',
            out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "dual"  & .data$`valueType` == "decimal"           ~
          paste0(
            'fabR::plot_density(tbl = dataset,col = "',
            .data$`name`,'" , missing_values = "',
            .data$`code_dd`,'", out = "',
            out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "dual"  & .data$`valueType` == "integer"           ~
          paste0(
            'fabR::plot_box(tbl = dataset,col = "',
            .data$`name`,'" , missing_values = "',
            .data$`code_dd`,'", out = "',
            out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "dual"  & .data$`valueType` == "date"              ~
          paste0(
            'fabR::plot_date(tbl = dataset,col = "',
            .data$`name`,'" , missing_values = "',
            .data$`code_dd`,'", out = "',
            out,'-code", group_by = ',group_by,' , time = "year")') ,

        TRUE                                                                   ~
          "'message(\"\")'"))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(
      plot_2 = case_when(
        .data$`viz_type` == "decimal"                                          ~
          paste0(
            'fabR::plot_density(  tbl = dataset,col = "',
            .data$`name`,'" , missing_values = "',
            .data$`code_dd`,'", out = "',
            out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "integer"                                          ~
          paste0(
            'fabR::plot_histogram(tbl = dataset,col = "',
            .data$`name`,'" , missing_values = "',
            .data$`code_dd`,'", out = "',
            out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "categorical"                                      ~
          paste0(
            'fabR::plot_bar(      tbl = dataset,col = "',
            .data$`name`,
            '"               , out = "',
            out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "dual"  & .data$`valueType` == "decimal"           ~
          paste0(
            'fabR::plot_box(      tbl = dataset,col = "',
            .data$`name`,'" , missing_values = "',
            .data$`code_dd`,'", out = "',
            out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "dual"  & .data$`valueType` == "integer"           ~
          paste0(
            'fabR::plot_histogram(tbl = dataset,col = "',
            .data$`name`,'" , missing_values = "',
            .data$`code_dd`,'", out = "',
            out,'-code", group_by = ',group_by,')') ,
        TRUE                                                                   ~
          "'message(\"\")'"))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(
      plot_3 = case_when(
        .data$`viz_type` == "categorical"                                      ~
          paste0(
            'fabR::plot_pie(      tbl = dataset,col = "',
            .data$`name`,'" ,         out = "',
            out,'-code", group_by = ',group_by,')'),
        TRUE                                                                   ~
          "'message(\"\")'"))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(plot_4 =
             paste0(
               'fabR::fabR::plot_pie_valid_value(   tbl = dataset,col = "',
               .data$`name`,'" , missing_values = "',
               .data$`code_dd`,'", out = "',
               out,'-code", group_by = ',group_by,')'))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(
      summary_1 = case_when(
        .data$`viz_type` == "text"                                             ~
          paste0(
            'fabR::summary_text(     tbl = dataset,col = "',
            .data$`name`,'", missing_values = "',
            .data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "decimal"                                          ~
          paste0(
            'fabR::summary_numerical(tbl = dataset,col = "',
            .data$`name`,'", missing_values = "',
            .data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "integer"                                          ~
          paste0(
            'fabR::summary_numerical(tbl = dataset,col = "',
            .data$`name`,'", missing_values = "',
            .data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "date"                                             ~
          paste0(
            'fabR::summary_text(     tbl = dataset,col = "',
            .data$`name`,'", missing_values = "',
            .data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "categorical"                                      ~
          paste0(
            'fabR::summary_category( tbl = dataset,col = "',
            .data$`name`,'", missing_values = "',
            .data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),

        .data$`viz_type` == "dual"  & .data$`valueType` == "text"              ~
          paste0(
            'fabR::summary_text(     tbl = dataset,col = "',
            .data$`name`,'", missing_values = "',
            .data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "dual"  & .data$`valueType` == "decimal"           ~
          paste0(
            'fabR::summary_numerical(tbl = dataset,col = "',
            .data$`name`,'", missing_values = "',
            .data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "dual"  & .data$`valueType` == "integer"           ~
          paste0(
            'fabR::summary_numerical(tbl = dataset,col = "',
            .data$`name`,'", missing_values = "',
            .data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "dual"  & .data$`valueType` == "date"              ~
          paste0(
            'fabR::summary_text(     tbl = dataset,col = "',
            .data$`name`,'", missing_values = "',
            .data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        TRUE                                                                   ~
          NA_character_))
  # this_dd <<- data_dict

  for (i in seq_len(length(data_dict$Variables$index))) {
    data_dict$Variables$plot_1[i]    <-
      eval(parse(text = str_squish(data_dict$Variables$plot_1[i])))
    data_dict$Variables$plot_2[i]    <-
      eval(parse(text = str_squish(data_dict$Variables$plot_2[i])))
    data_dict$Variables$plot_3[i]    <-
      eval(parse(text = str_squish(data_dict$Variables$plot_3[i])))
    data_dict$Variables$plot_4[i]    <-
      eval(parse(text = str_squish(data_dict$Variables$plot_4[i])))
    data_dict$Variables$summary_1[i] <-
      eval(parse(text = str_squish(data_dict$Variables$summary_1[i])))
  }

  return(data_dict)

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
#' provide dataset composition, with observation repartition and descriptive
#' statistics.
#'
#' @seealso
#' [datashapR::dataset_visualize()]
#'
#' @param report_name A character string specifying the name of the report (a
#' folder in users environment) to be opened.
#'
#' @examples
#' \dontrun{
#' # Example 1: yyy yyy yyy.
#' }
#'
#' @importFrom utils browseURL
#'
#' @export
#'
open_visual_report <- function(report_name){

  utils::browseURL(paste0(report_name,"/docs/index.html"))

}
