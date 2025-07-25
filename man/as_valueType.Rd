% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04-catalogue_functions.R
\name{as_valueType}
\alias{as_valueType}
\title{Validate and coerce any object according to a given valueType}
\usage{
as_valueType(x, valueType = "text")
}
\arguments{
\item{x}{Object to be coerced. Can be a vector.}

\item{valueType}{A character string of the valueType used to coerce x.}
}
\value{
The object coerced accordingly to the input valueType.
}
\description{
Attributes a valueType to an object, that can be a vector, or in a data frame
using \link[dplyr:mutate]{dplyr::mutate}.
}
\details{
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
dataset <- madshapR_examples$`dataset_example`
as_valueType(head(dataset$dob),'date')

# as_valueType is compatible with tidyverse syntax
library(dplyr)
dataset <- 
  tibble(mtcars) \%>\%
  mutate(cyl = as_valueType(cyl,'integer'))

head(dataset)

}

}
\seealso{
\href{https://opaldoc.obiba.org/en/dev/magma-user-guide/value/type.html}{Opal documentation}
}
