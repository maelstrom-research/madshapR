% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04-catalogue_functions.R
\name{is_valueType}
\alias{is_valueType}
\title{Test if a character object is one of the valid valueType values}
\usage{
is_valueType(object)
}
\arguments{
\item{object}{A potential valueType name to be evaluated.}
}
\value{
A logical.
}
\description{
Confirms whether the input object is a valid valueType. This function mainly
helps validate input within other functions of the package but could be used
to check if a valueType is valid.
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

is_valueType('integer')
is_valueType('intereg')

}

}
\seealso{
\href{https://opaldoc.obiba.org/en/dev/magma-user-guide/value/type.html}{Opal documentation}
}
