% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-dataset_functions.R
\name{is_dossier}
\alias{is_dossier}
\title{Test if an object is a valid dossier (list of dataset(s))}
\usage{
is_dossier(object)
}
\arguments{
\item{object}{A potential dossier to be evaluated.}
}
\value{
A logical.
}
\description{
Tests if the input object is a valid dossier. This function mainly helps
validate input within other functions of the package but could be used to
check if a dossier is valid.
}
\details{
A dossier is a named list containing at least one data frame or more,
each of them being datasets. The name of each tibble will be use as the
reference name of the dataset.
}
\examples{
{

# use madshapR_examples provided by the package
# Any list of data frame can be a dossier by definition.
library(stringr)

dossier <- madshapR_examples[str_detect(names(madshapR_examples),"^dataset_example")]
is_dossier(dossier)
is_dossier(list(dataset_1 = iris, dataset_2 = mtcars))
is_dossier(iris)

}

}
