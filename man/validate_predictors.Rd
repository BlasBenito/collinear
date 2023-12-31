% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{validate_predictors}
\alias{validate_predictors}
\title{Validate the 'predictors' argument for analysis}
\usage{
validate_predictors(
  df = NULL,
  response = NULL,
  predictors = NULL,
  min_numerics = 0,
  decimals = 4
)
}
\arguments{
\item{df}{(required; data frame) A validated data frame with numeric and/or character predictors, and optionally, a response variable. Default: NULL.}

\item{response}{(optional, character string) Name of a numeric response variable. Used to remove the response from the predictors when predictors is NULL. Character response variables are ignored. Default: NULL.}

\item{predictors}{(optional; character vector) character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:NULL}

\item{min_numerics}{(required, integer) Minimum number of numeric predictors required. Default: 1}

\item{decimals}{(required, integer) Number of decimal places for the zero variance test. Smaller numbers will increase the number of variables detected as near-zero variance. Recommended values will depend on the range of the numeric variables in 'df'. Default: 4}
}
\value{
A character vector of validated predictor names
}
\description{
Requires the argument 'df' to be validated with \code{\link[=validate_df]{validate_df()}}.

Validates the 'predictors' argument to ensure it complies with the requirements of the package functions. It performs the following actions:
\itemize{
\item Stops if 'df' is NULL.
\item Stops if 'df' is not validated.
\item If 'predictors' is NULL, uses column names of 'df' as 'predictors' in the 'df' data frame.
\item Raise a warning if there are names in 'predictors' not in the column names of 'df', and returns only the ones in 'df'.
\item Stop if the number of numeric columns in 'predictors' is smaller than 'min_numerics'.
\item Raise a warning if there are zero-variance columns in 'predictors' and returns a new 'predictors' argument without them.
\item Tags the vector with the attribute \code{validated = TRUE} to let the package functions skip the data validation.
}
}
\examples{

data(
  vi,
  vi_predictors
  )

#validating example data frame
vi <- validate_df(
  df = vi
)

#validating example predictors
vi_predictors <- validate_predictors(
  df = vi,
  predictors = vi_predictors
)

#tagged as validated
attributes(vi_predictors)$validated

}
\author{
Blas M. Benito
}
