
# madshapR (current development version 1.0.2.1000)

# madshapR 1.0.2

## Creation of NEWS feed !!

Addition of `NEWS.md` for the development version use “(development
version)”.

## Bug fixes and improvements

- Some improvements in the documentation of the package has been made.

- internal call of libraries (using ‘::’) has been replaced by proper
  import in the declaration function.

- get functions in
  [fabR](https://guifabre.github.io/fabR-documentation/) have been
  changed in its last release. the functions using them as dependancies
  ( ‘check_xxx()’) have been updated accordingly.

- DEMO files no longer include harmonization files that are now in the
  package
  [harmonizR](https://maelstrom-research.github.io/harmonizR-documentation/)

## Dependency changes

**New Imports:** haven, lifecycle

**No longer in Imports:** xfun

## New functions

These functions are imported from
[fabR](https://guifabre.github.io/fabR-documentation/)

- `bookdown_template()` replaces the deprecated function
  `bookdown_template()`.

- `bookdown_render()` which renders a Rmd collection of files into a
  docs/index.html website.

- `bookdown_open()` Which allows to open a docs/index.html document when
  the bookdown is rendered

This separation into 3 functions will allow future developments, such as
render as a ppt or pdf.

## deprecated functions

Due to another package development (see
[fabR](https://guifabre.github.io/fabR-documentation/)), The function
`open_visual_report()` has been deprecated in favor of `bookdown_open()`
imported from fabR package.

# madshapR 1.0.0

This package is a collection of wrapper functions used in data
pipelines.

This is still a work in progress, so please let me know if you used a
function before and is not working any longer.

## functions to generate, shape and format meta data.

These functions allows to create, extract transform and apply meta data
to a dataset.

- Transform and shape:

`data_dict_collapse()`,`data_dict_expand()`,`data_dict_filter()`,
`data_dict_group_by()`,`data_dict_group_split()`,`data_dict_list_nest()`,
`data_dict_pivot_longer()`,`data_dict_pivot_wider()`,`data_dict_ungroup()`

- extract/apply meta data:

`data_dict_match_dataset()`,`data_dict_apply()`, `data_dict_extract()`

- evaluate and apply attributes:

`as_data_dict()`, `as_data_dict_mlstr()`,`as_data_dict_shape()`,
`is_data_dict()`, `is_data_dict_mlstr()`, `is_data_dict_shape()`
`as_taxonomy()`, `is_taxonomy()`

## functions to generate, shape and format data.

These functions allows to create, extract transform data/meta data from
a dataset. A dossier is a list of datasets.

- evaluate and apply attributes:

`as_dataset()`, `as_dossier()` `is_dataset()`, `is_dossier()`

- Extract/transform meta data: `data_extract()`, `dossier_create()`,
  `dataset_zap_data_dict()`, `dataset_cat_as_labels()`

## Functions to work with data types

These functions allow user to work with, extract or assign data type
(valueType) to values and/or dataset.

`as_valueType()`, `is_valueType()`, `valueType_adjust()`,
`valueType_guess()`, `valueType_self_adjust()`, `valueType_of()`

## Unit tests and QA for datasets and data dictionaries

These helper functions evaluate content of a dataset and/or data
dictionary to extract from them irregularities or potential errors.
These informations are stored in a tibble that can be use to assess
inputs.

`check_data_dict_categories()`, `check_data_dict_missing_categories()`,
`check_data_dict_taxonomy()`, `check_data_dict_variables()`,
`check_data_dict_valueType()`, `check_dataset_categories()`,
`check_dataset_valueType()`, `check_dataset_variables()`,
`check_name_standards()`

## Summarise information in dataset and data dictionaries

These helper functions evaluate content of a dataset and/or data
dictionary to extract from them summary statistics and elements such as
missing values, NA, category names, etc. These informations are stored
in a tibble that can be use to summary inputs.

`dataset_preprocess()`, `summary_variables()`,
`summary_variables_categorical()`,`summary_variables_date()`,
`summary_variables_numeric()`,`summary_variables_text()`

## Write and read excel and csv

- `read_csv_any_formats()` The csv file is read twice to detect the
  number of lines to use in attributing the column type (‘guess_max’
  parameter of read_csv). This avoids common errors when reading csv
  files.

- `read_excel_allsheets()` The Excel file is read and the values are
  placed in a list of tibbles, with each sheet in a separate element in
  the list. If the Excel file has only one sheet, the output is a single
  tibble.

- `write_excel_allsheets()` Write all Excel sheets using
  `xlsx::write.xlsx()` recursively.

## Plot and summary functions used in a visual report

`plot_bar()`, `plot_box()`, `plot_date()`, `plot_density()`,
`plot_histogram()`, `plot_main_word()`, `plot_pie_valid_value()`,
`summary_category()`, `summary_numerical()`,`summary_text()`

## aggregate information and generate reports

- assess data

`data_dict_evaluate()` `dataset_evaluate()` `dossier_evaluate()`

- summarize data

`dataset_summarize()` `dossier_summarize()`

- visualize data

`dataset_visualize()` `variable_visualize()` `open_visual_report()`
