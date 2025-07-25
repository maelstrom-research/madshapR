% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/08-data_visualize.R
\name{dataset_visualize}
\alias{dataset_visualize}
\title{Generate a web-based visual report for a dataset}
\usage{
dataset_visualize(
  dataset = tibble(id = as.character()),
  bookdown_path,
  data_dict = data_dict_extract(dataset),
  group_by = attributes(dataset_summary)[["madshapR_group::group_by"]],
  valueType_guess = FALSE,
  taxonomy = NULL,
  dataset_summary = NULL,
  dataset_name = NULL
)
}
\arguments{
\item{dataset}{A dataset object.}

\item{bookdown_path}{A character string identifying the folder path where
the bookdown report files will be saved.}

\item{data_dict}{A list of data frame(s) representing metadata of the input
dataset. Automatically generated if not provided.}

\item{group_by}{A character string identifying the column in the dataset
to use as a grouping variable. Elements will be grouped by this
column.}

\item{valueType_guess}{Whether the output should include a more accurate
valueType that could be applied to the dataset. FALSE by default.}

\item{taxonomy}{An optional data frame identifying a variable classification
schema.}

\item{dataset_summary}{A list which identifies an existing
summary produced by \code{\link[=dataset_summarize]{dataset_summarize()}} of the dataset.
Using this parameter can save time in generating the visual report.}

\item{dataset_name}{A character string specifying the name of the dataset
(used internally in the function \code{\link[=dossier_evaluate]{dossier_evaluate()}}).}
}
\value{
A folder containing files for the bookdown site. To open the bookdown site
in a browser, open 'docs/index.html', or use \code{\link[=bookdown_open]{bookdown_open()}} with the
folder path.
}
\description{
Generates a visual report of a dataset in an HTML bookdown
document, with summary figures and statistics for each variable. The report
outputs can be grouped by a categorical variable.
}
\details{
A dataset is a data table containing variables. A dataset object is a
data frame and can be associated with a data dictionary. If no
data dictionary is provided with a dataset, a minimum workable
data dictionary will be generated as needed within relevant functions.
Identifier variable(s) for indexing can be specified by the user.
The id values must be non-missing and will be used in functions that
require it. If no identifier variable is specified, indexing is
handled automatically by the function.

A data dictionary contains the list of variables in a dataset and metadata
about the variables and can be associated with a dataset. A data dictionary
object is a list of data frame(s) named 'Variables' (required) and
'Categories' (if any). To be usable in any function, the data frame
'Variables' must contain at least the \code{name} column, with all unique and
non-missing entries, and the data frame 'Categories' must contain at least
the \code{variable} and \code{name} columns, with unique combination of
\code{variable} and \code{name}.

The valueType is a declared property of a variable that is required in
certain functions to determine handling of the variables. Specifically,
valueType refers to the
\href{https://opaldoc.obiba.org/en/dev/variables-data.html#value-types}{OBiBa data type of a variable}.
The valueType is specified in a data dictionary in a column 'valueType' and
can be associated with variables as attributes. Acceptable valueTypes
include 'text', 'integer', 'decimal', 'boolean', datetime', 'date'. The full
list of OBiBa valueType possibilities and their correspondence with R data
types are available using \link{valueType_list}. The valueType can be used to
coerce the variable to the corresponding data type.

A taxonomy is a classification schema that can be defined for variable
attributes. A taxonomy is usually extracted from an
\href{https://www.obiba.org/pages/products/opal/}{Opal environment}, and a
taxonomy object is a data frame that must contain at least the columns
\code{taxonomy}, \code{vocabulary}, and \code{terms}. Additional details about Opal
taxonomies are
\href{https://opaldoc.obiba.org/en/latest/web-user-guide/administration/taxonomies.html}{available online}.
}
\examples{
\donttest{

library(fs)
library(dplyr)
 
# use madshapR_examples provided by the package 
dataset <-
  madshapR_examples$`dataset_example` \%>\%
  group_by(gndr) \%>\%
  as_dataset(col_id = "part_id")
  
data_dict <- as_data_dict_mlstr(madshapR_examples$`data_dictionary_example`)
dataset <- data_dict_apply(dataset,data_dict)
dataset_summary <- dataset_summarize(dataset,data_dict)
 
if(dir_exists(tempdir())) dir_delete(tempdir())
bookdown_path <- tempdir()
 
dataset_visualize(
 dataset,
 data_dict,
 dataset_summary = dataset_summary,
 bookdown_path = bookdown_path)
  
# To open the file in browser, open 'bookdown_path/docs/index.html'. 
# Or use bookdown_open(bookdown_path) function.

}

}
\seealso{
\code{\link[=bookdown_open]{bookdown_open()}}
\code{\link[=as_category]{as_category()}}
}
