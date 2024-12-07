% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify.R
\name{identify_predictors_categorical}
\alias{identify_predictors_categorical}
\title{Identify Valid Categorical Predictors}
\usage{
identify_predictors_categorical(df = NULL, predictors = NULL)
}
\arguments{
\item{df}{(required; data frame, tibble, or sf) A data frame with responses and predictors. Default: NULL.}

\item{predictors}{(optional; character vector) Names of the predictors to select from \code{df}. If omitted, all numeric columns in \code{df} are used instead. If argument \code{response} is not provided, non-numeric variables are ignored. Default: NULL}
}
\value{
character vector: categorical predictors names
}
\description{
Returns the names of character or factor predictors, if any. Removes categorical predictors with constant values, or with as many unique values as rows are in the input data frame.
}
\examples{

data(
  vi,
  vi_predictors
)

non.numeric.predictors <- identify_predictors_categorical(
  df = vi,
  predictors = vi_predictors
)

non.numeric.predictors

}
\seealso{
Other data_types: 
\code{\link{identify_predictors}()},
\code{\link{identify_predictors_numeric}()},
\code{\link{identify_predictors_type}()},
\code{\link{identify_predictors_zero_variance}()},
\code{\link{identify_response_type}()}
}
\author{
Blas M. Benito, PhD
}
\concept{data_types}
