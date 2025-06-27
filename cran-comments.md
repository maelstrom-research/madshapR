## R CMD check results

0 errors | 0 warnings | 0 note

# Submission for CRAN : madshapR 2.0.0

> side note 1: dear people of CRAN, please note that fabR, madshapR,
Rmonize, mlstrOpalr, and BanffIT belong to a suite that is going to be 
updated. fabR (2.1.1) has already been updated . 
madshapR, Rmonize, mlstrOpalr and BanffIT will not be retrocompatible, 
as we discussed with all our community, update all the codes and tested 
for 3 months, along with git monitoring and .onAttach() message. 
Ultimately we are thrilled to see the next release published in CRAN. 
Best Regards, Guillaume FABRE.

> side note 2: dear people of CRAN, please note that some examples of the 
madshapR package take some time to be processed. The examples generate 
complexe summaries and visual reports that cannot be shorten. 
These functions have been put in \donttest{ 
Best Regards, Guillaume FABRE.


## Bug fixes and improvements

* The package now handles the valueType __datetime__, which formerly was considered 
either as a __text__ or __date__. 
https://github.com/maelstrom-research/madshapR/issues/123
https://github.com/maelstrom-research/madshapR/issues/112
https://github.com/maelstrom-research/madshapR/issues/75

* The valueType object (present in columns in a data dictionary or as an attribute
of a variable) had some errors and bugs that have been corrected.
https://github.com/maelstrom-research/madshapR/issues/87
https://github.com/maelstrom-research/madshapR/issues/82
https://github.com/maelstrom-research/madshapR/issues/81
https://github.com/maelstrom-research/madshapR/issues/76

* When a column in a dataset is all NA (empty), the previous version had some 
issues that have been has been corrected.
https://github.com/maelstrom-research/madshapR/issues/116
https://github.com/maelstrom-research/madshapR/issues/115
https://github.com/maelstrom-research/madshapR/issues/109

* A bug in `data_dict_pivot_longer()` when 'Source' or 'Target' column was not 
present has been corrected.
https://github.com/maelstrom-research/madshapR/issues/86

* The SPSS format, which `haven` package uses to produce labelled variables, define
integers different form madshapR, which ultimately . That has been taken in account and
corrected.

* The SPSS format in the `haven` package used to produce labelled variables defines
integers differently from madshapR, which was causing errors. 
The difference has been taken into account.
https://github.com/maelstrom-research/madshapR/issues/83


## The group_by parameter has been redesigned.

* `dataset_preprocess()` now handles grouped dataset, using parameter "group_by".

* Users can now define groups in summaries and visual reports using a variable 
that is not categorical or has empty values.
https://github.com/maelstrom-research/madshapR/issues/47

* Previously, the "group_by" argument had some flaws, resulting in bugs that have
been corrected.
https://github.com/maelstrom-research/madshapR/issues/114
https://github.com/maelstrom-research/madshapR/issues/113
https://github.com/maelstrom-research/madshapR/issues/110
https://github.com/maelstrom-research/madshapR/issues/105

## Enhancements in the assessment and summary reports!

* The assessment and summary reports had some updates, such as renamed columns 
and bug corrections.
https://github.com/maelstrom-research/madshapR/issues/126
https://github.com/maelstrom-research/madshapR/issues/104
https://github.com/maelstrom-research/madshapR/issues/98
https://github.com/maelstrom-research/madshapR/issues/97
https://github.com/maelstrom-research/madshapR/issues/96
https://github.com/maelstrom-research/madshapR/issues/95
https://github.com/maelstrom-research/madshapR/issues/94
https://github.com/maelstrom-research/madshapR/issues/93
https://github.com/maelstrom-research/madshapR/issues/92
https://github.com/maelstrom-research/madshapR/issues/91
https://github.com/maelstrom-research/madshapR/issues/90
https://github.com/maelstrom-research/madshapR/issues/89
https://github.com/maelstrom-research/madshapR/issues/88
https://github.com/maelstrom-research/madshapR/issues/85
https://github.com/maelstrom-research/madshapR/issues/80
https://github.com/maelstrom-research/madshapR/issues/79

## Enhancements in the visual reports!

* The visual reports have been improved, including better visual outputs and
color palettes, and new features such as total number of rows next to the bar charts.

https://github.com/maelstrom-research/madshapR/issues/108
https://github.com/maelstrom-research/madshapR/issues/107
https://github.com/maelstrom-research/madshapR/issues/106
https://github.com/maelstrom-research/madshapR/issues/100
https://github.com/maelstrom-research/madshapR/issues/84
https://github.com/maelstrom-research/madshapR/issues/64


## New functions

* `typeof_convert_to_valueType()` converts typeof (and class if any) into its corresponding valueType.

* `valueType_convert_to_typeof()` converts valueType into its corresponding typeof and class in R representation.

* `data_dict_update()` updates a data dictionary from a dataset.

* `data_dict_trim_labels()` adds shortened labels to data dictionary.

* `first_label_get()` gets the first label from a data dictionary.

* `has_categories()` tests if a dataset has categorical variables.

