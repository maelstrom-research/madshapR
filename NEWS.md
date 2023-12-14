
# madshapR 1.0.3

## Bug fixes and improvements

Some of the tests were made with another package (Rmonize) which as
“madshapR” as a dependence.

### Improvement in handling pooled data

The functions `harmo_process()`, `pool_harmonized_dataset_create()`,
`harmonized_dossier_create()`, `harmonized_dossier_evaluate()`,
`harmonized_dossier_summarize()`, `harmonized_dossier_visualize()` share
the same parameter “harmonized_col_dataset” which is (if exists) the
name of the column referring the input dataset names. If this column
exists and is declared by the user, this will be used across the
pipeline as a grouping/separating variable. By default, the name of each
dataset will be used instead.

### Enhance reports

- in visual reports, void confusing changes in color scheme in visual
  reports.

- Histograms for date variables display valid ranges.

- in reports, change % NA as proportion in reports.

- `harmonized_dossier_visualize()` report shows variable labels in the
  same lang.

- in visual reports, the bar plot only appears when there are multiple
  missing value types, otherwise only the pie chart is shown.

- in reports, all of the percentages are now included under “Other
  values (non categorical)”, which gives a single value.

- <https://github.com/maelstrom-research/madshapR/issues/51>

suppress overwrite parameter in `dataset_visualize()`.

- <https://github.com/maelstrom-research/madshapR/issues/42>

in `dataset_summary()` minor issue (consistency in column names and
content).

- correction of the function `variable_visualize()` when valueType_guess
  = TRUE

### Correct Data dictionary functions

- <https://github.com/maelstrom-research/madshapR/issues/50>

enhance the function `check_data_dict_valueType()`, which was too slow.

- <https://github.com/maelstrom-research/madshapR/issues/49>

`valueType_adjust()` now works with empty column (all NAs)

- allow the format date to be transformed into text in
  dataset_zap_data_dict() when the format is unclear.

## New functions

- `col_id()` function which is a short cut for calling the attribute
  `madshapR::col_id` of a dataset.

- `as_category()`,`is_category()`,`drop_category()` function which
  coerces a vector as a categorical object. Typically a column in a
  dataset that needs to be coerced into a categorical variable (The data
  dictionary is updated accordingly).

- creation of `col_id()` function which is a short cut for calling the
  attribute `madshapR::col_id` of a dataset.

## Deprecated functions

- Rename and update example rda Object (in data) of `DEMO_files` into
  `madshapR_DEMO` for consistency across our other packages.

# madshapR 1.0.2

## Creation of NEWS feed !!

Addition of `NEWS.md` for the development version use “(development
version)”.

## Bug fixes and improvements

- Some improvements in the documentation of the package has been made.

- internal call of libraries (using `::`) has been replaced by proper
  import in the declaration function.

- get functions in
  [fabR](https://guifabre.github.io/fabR-documentation/) have been
  changed in its last release. the functions using them as dependencies
  ( `check_xxx()`) have been updated accordingly.

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

This is still a work in progress, so please let us know if you used a
function before and is not working any longer.

## Helper functions

- `madshapR_help()` Call the help center for full documentation

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

## Summarize information in dataset and data dictionaries

These helper functions evaluate content of a dataset and/or data
dictionary to extract from them summary statistics and elements such as
missing values, NA, category names, etc. These informations are stored
in a tibble that can be use to summary inputs.

`dataset_preprocess()`, `summary_variables()`,
`summary_variables_categorical()`,`summary_variables_date()`,
`summary_variables_numeric()`,`summary_variables_text()`

## Write and read excel and csv

- `read_csv_any_formats()` The csv file is read twice to detect the
  number of lines to use in attributing the column type (`guess_max`
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
