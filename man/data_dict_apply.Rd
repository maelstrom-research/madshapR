% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-dictionaries_functions.R
\name{data_dict_apply}
\alias{data_dict_apply}
\title{Apply a data dictionary to a dataset}
\usage{
data_dict_apply(dataset, data_dict = NULL)
}
\arguments{
\item{dataset}{A dataset object.}

\item{data_dict}{A list of data frame(s) representing metadata of the input
dataset. Automatically generated if not provided.}
}
\value{
A labelled data frame with metadata as attributes, specified for each
variable from the input data dictionary.
}
\description{
Applies a data dictionary to a dataset, creating a labelled dataset with
variable attributes. Any previous attributes will be preserved. For
variables that are factors, variables will be transformed into
haven-labelled variables. The data dictionary will be added as an attribute
(attributes(dataset)$\verb{madshapR::Data dictionary}) and can be extracted using
the function \code{\link[=data_dict_extract]{data_dict_extract()}}
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
}
\examples{
{

# use madshapR_examples provided by the package
dataset   <- madshapR_examples$`dataset_example`
data_dict <- as_data_dict_mlstr(madshapR_examples$`data_dictionary_example`)
dataset   <- data_dict_apply(dataset, data_dict)

head(dataset)

}

}
\seealso{
\code{\link[=attributes]{attributes()}}, \code{\link[haven:labelled]{haven::labelled()}},\code{\link[=data_dict_extract]{data_dict_extract()}}
}
