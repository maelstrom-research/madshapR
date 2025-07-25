% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05-unit_checks.R
\name{check_data_dict_valueType}
\alias{check_data_dict_valueType}
\title{Assess a data dictionary for non-valid valueType values}
\usage{
check_data_dict_valueType(data_dict)
}
\arguments{
\item{data_dict}{A list of data frame(s) representing metadata to be evaluated.}
}
\value{
A data frame providing non-standard valueType declared in a data dictionary.
}
\description{
Generates a data frame report of any variable with a valueType that is not in
the list of allowed valueType values. This function also assesses if the
valueType is compatible with any associated categorical values declared.
This report can be used to help assess data structure, presence of fields,
coherence across elements, and taxonomy or data dictionary formats.
}
\details{
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
}
\examples{
{

# use madshapR_examples provided by the package
data_dict <- madshapR_examples$`data_dictionary_example - errors with data`
check_data_dict_valueType(data_dict)

}

}
