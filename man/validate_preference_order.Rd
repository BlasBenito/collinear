% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{validate_preference_order}
\alias{validate_preference_order}
\title{Validate Argument preference_order}
\usage{
validate_preference_order(
  predictors = NULL,
  preference_order = NULL,
  preference_order_auto = NULL,
  function_name = "collinear::validate_preference_order()",
  quiet = FALSE
)
}
\arguments{
\item{predictors}{(optional; character vector) Names of the predictors to select from \code{df}. If omitted, all numeric columns in \code{df} are used instead. If argument \code{response} is not provided, non-numeric variables are ignored. Default: NULL}

\item{preference_order}{(optional; string, character vector, output of \code{\link[=preference_order]{preference_order()}}). Defines a priority order, from first to last, to preserve predictors during the selection process. Accepted inputs are:
\itemize{
\item \strong{"auto"} (default): if \code{response} is not NULL, calls \code{\link[=preference_order]{preference_order()}} for internal computation.
\item \strong{character vector}: predictor names in a custom preference order.
\item \strong{data frame}: output of \code{\link[=preference_order]{preference_order()}} from \code{response} of length one.
\item \strong{named list}: output of \code{\link[=preference_order]{preference_order()}} from \code{response} of length two or more.
\item \strong{NULL}: disabled.
}. Default: "auto"}

\item{preference_order_auto}{(required, character vector) names of the predictors in the automated preference order returned by \code{\link[=vif_select]{vif_select()}} or \code{\link[=cor_select]{cor_select()}}}

\item{function_name}{(optional, character string) Name of the function performing the check. Default: "collinear::validate_preference_order()"}

\item{quiet}{(optional; logical) If FALSE, messages generated during the execution of the function are printed to the console Default: FALSE}
}
\value{
character vector: ranked variable names
}
\description{
Internal function to validate the argument \code{preference_order}.
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

#validate preference order
my_order <- c(
  "swi_max",
  "swi_min",
  "swi_deviance" #wrong one
)

my_order <- validate_preference_order(
  predictors = vi_predictors,
  preference_order = my_order,
  preference_order_auto = vi_predictors
)

#has my_order first
#excludes wrong names
#all other variables ordered according to preference_order_auto
my_order
}
\seealso{
Other data_validation: 
\code{\link{validate_data_cor}()},
\code{\link{validate_data_vif}()},
\code{\link{validate_df}()},
\code{\link{validate_encoding_arguments}()},
\code{\link{validate_predictors}()},
\code{\link{validate_response}()}
}
\concept{data_validation}
