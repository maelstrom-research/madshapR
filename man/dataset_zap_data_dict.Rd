% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-dataset_functions.R
\name{dataset_zap_data_dict}
\alias{dataset_zap_data_dict}
\title{Remove labels (attributes) from a data frame, leaving its unlabelled columns}
\usage{
dataset_zap_data_dict(dataset, zap_factor = FALSE)
}
\arguments{
\item{dataset}{A dataset object.}

\item{zap_factor}{Whether the factor column should be coerced with its
corresponding valueType. FALSE by default.}
}
\value{
A data frame identifying a dataset.
}
\description{
Removes any attributes attached to a data frame. Any value in columns will be
preserved. Any 'Date' (typeof) column will be recast as character to
preserve information.
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
}
\examples{
{

# use madshapR_examples provided by the package

dataset <- madshapR_examples$`dataset_example`
data_dict <- as_data_dict_mlstr(madshapR_examples$`data_dictionary_example`)
dataset <- data_dict_apply(dataset,data_dict)
unlabbeled_dataset <- dataset_zap_data_dict(dataset)

head(unlabbeled_dataset)

}

}
\seealso{
\code{\link[haven:zap_labels]{haven::zap_labels()}}.
}
