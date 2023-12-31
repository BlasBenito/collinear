% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vif_select.R
\name{vif_select}
\alias{vif_select}
\title{Automated multicollinearity reduction via Variance Inflation Factor}
\usage{
vif_select(
  df = NULL,
  response = NULL,
  predictors = NULL,
  preference_order = NULL,
  max_vif = 5,
  encoding_method = "mean"
)
}
\arguments{
\item{df}{(required; data frame) A data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.}

\item{response}{(recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.}

\item{predictors}{(optional; character vector) character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'}

\item{preference_order}{(optional; character vector) vector with column names in 'predictors' in the desired preference order, or result of the function \code{\link[=preference_order]{preference_order()}}. Allows defining a priority order for selecting predictors, which can be particularly useful when some predictors are more critical for the analysis than others. Predictors not included in this argument are ranked by their Variance Inflation Factor. Default: NULL.}

\item{max_vif}{(optional, numeric) Numeric with recommended values between 2.5 and 10 defining the maximum VIF allowed for any given predictor in the output dataset. Higher VIF thresholds should result in a higher number of selected variables. Default: 5.}

\item{encoding_method}{(optional; character string). Name of the target encoding method to convert character and factor predictors to numeric. One of "mean", "rank", "loo", "rnorm" (see \code{\link[=target_encoding_lab]{target_encoding_lab()}} for further details). Default: "mean"}
}
\value{
Character vector with the names of the selected predictors.
}
\description{
Automates multicollinearity management by selecting variables based on their Variance Inflation Factor (VIF).

Warning: predictors with perfect correlation might cause errors, please use \code{\link[=cor_select]{cor_select()}} to remove perfect correlations first.

The \code{\link[=vif_select]{vif_select()}} function is designed to automate the reduction of multicollinearity in a set of predictors by using Variance Inflation Factors.

If the 'response' argument is provided, categorical predictors are converted to numeric via target encoding (see \code{\link[=target_encoding_lab]{target_encoding_lab()}}). If the 'response' argument is not provided, categorical variables are ignored.

The Variance Inflation Factor for a given variable \code{y} is computed as \code{1/(1-R2)}, where \code{R2} is the multiple R-squared of a multiple regression model fitted using \code{y} as response and all other predictors in the input data frame as predictors. The VIF equation can be interpreted as the "rate of perfect model's R-squared to the unexplained variance of this model".

The possible range of VIF values is (1, Inf]. A VIF lower than 10 suggest that removing \code{y} from the data set would reduce overall multicollinearity. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5, 5, and 10.

The function \code{\link[=vif_select]{vif_select()}} applies a recursive algorithm to remove variables with a VIF higher than a given threshold (defined by the argument \code{max_vif}).

If the argument \code{response} is provided, all non-numeric variables in \code{predictors} are transformed into numeric using target encoding (see \code{\link[=target_encoding_lab]{target_encoding_lab()}}). Otherwise, non-numeric variables are ignored.

The argument \code{preference_order} allows defining a preference selection order to preserve (when possible) variables that might be interesting or even required for a given analysis.

For example, if \code{predictors} is \code{c("a", "b", "c")} and \code{preference_order} is \code{c("a", "b")}, there are two possibilities:
\itemize{
\item If the VIF of \code{"a"} is higher than the VIF of \code{"b"}, and both VIF values are above \code{max_vif}, then \code{"a"} is selected and \code{"b"} is removed.
\item If their correlation is equal or above \code{max_cor}, then \code{"a"} is selected, no matter its correlation with \code{"c"},
}

If \code{preference_order} is not provided, then the predictors are ranked by their variance inflation factor as computed by \code{\link[=vif_df]{vif_df()}}.
}
\examples{

data(
  vi,
  vi_predictors
)

#subset to limit example run time
vi <- vi[1:1000, ]
vi_predictors <- vi_predictors[1:10]

#reduce correlation in predictors with cor_select()
vi_predictors <- cor_select(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  max_cor = 0.75
)

#without response
#without preference_order
#permissive max_vif
#only numeric predictors are processed
selected.predictors <- vif_select(
  df = vi,
  predictors = vi_predictors,
  max_vif = 10
)

selected.predictors

#without response
#without preference_order
#restrictive max_vif
#only numeric predictors are processed
selected.predictors <- vif_select(
  df = vi,
  predictors = vi_predictors,
  max_vif = 2.5
)

selected.predictors

#with response
#without preference_order
#restrictive max_cor
#slightly different solution than previous one
#because categorical variables are target-enccoded
selected.predictors <- vif_select(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  max_vif = 2.5
)

selected.predictors

#with response
#with user-defined preference_order
#restrictive max_cor
#numerics and categorical variables in output
selected.predictors <- vif_select(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  preference_order = c(
    "soil_type", #categorical variable
    "soil_temperature_mean",
    "swi_mean",
    "rainfall_mean",
    "evapotranspiration_mean"
  ),
  max_vif = 2.5
)

selected.predictors


#with response
#with automated preference_order
#restrictive max_cor and max_vif
#numerics and categorical variables in output
preference.order <- preference_order(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  f = f_rsquared #cor(response, predictor)
)

head(preference.order)

selected.predictors <- vif_select(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  preference_order = preference.order,
  max_vif = 2.5
)

selected.predictors

}
\author{
Blas M. Benito
\itemize{
\item David A. Belsley, D.A., Kuh, E., Welsch, R.E. (1980). Regression Diagnostics: Identifying Influential Data and Sources of Collinearity. John Wiley & Sons. \doi{10.1002/0471725153}.
}
}
