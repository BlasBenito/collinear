% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preference_order_methods.R
\name{f_r2_counts}
\alias{f_r2_counts}
\alias{f_r2_glm_poisson}
\alias{f_r2_glm_poisson_poly2}
\alias{f_r2_gam_poisson}
\title{Association Between a Count Response and a Continuous Predictor}
\usage{
f_r2_glm_poisson(df)

f_r2_glm_poisson_poly2(df)

f_r2_gam_poisson(df)
}
\arguments{
\item{df}{(required, data frame) with columns:
\itemize{
\item "x": (numeric) continuous predictor.
\item "y" (integer) counts response.
}}
}
\description{
These functions take a data frame with a integer counts response "y", and a continuous predictor "x", fit a univariate model, and return the R-squared of observations versus predictions:
\itemize{

\item \code{f_r2_glm_poisson()} Pearson's R-squared between a count response and the predictions of a GLM model with formula \code{y ~ x} and family \code{stats::poisson(link = "log")}.

\item \code{f_r2_glm_poisson_poly2()} Pearson's R-squared between a count response and the predictions of a GLM model with formula \code{y ~ stats::poly(x, degree = 2, raw = TRUE)} and family \code{stats::poisson(link = "log")}.

\item \code{f_r2_gam_poisson()} Pearson's R-squared between a count response and the predictions of a \code{\link[mgcv:gam]{mgcv::gam()}} model with formula \code{y ~ s(x)} and family \code{stats::poisson(link = "log")}.

\item \code{f_r2_rpart()}: Pearson's R-squared of a Recursive Partition Tree fitted with \code{\link[rpart:rpart]{rpart::rpart()}} with formula \code{y ~ x}.

\item \code{f_r2_rf()}: Pearson's R-squared of a 100 trees Random Forest model fitted with \code{\link[ranger:ranger]{ranger::ranger()}} and formula \code{y ~ x}.
}
}
\examples{

#load example data
data(vi)

#reduce size to speed-up example
vi <- vi[1:1000, ]

#integer counts response and continuous predictor
#to data frame without NAs
df <- data.frame(
  y = vi[["vi_counts"]],
  x = vi[["swi_max"]]
) |>
  na.omit()

#GLM model with Poisson family
f_r2_glm_poisson(df = df)

#GLM model with second degree polynomials and Poisson family
f_r2_glm_poisson_poly2(df = df)

#GAM model with Poisson family
f_r2_gam_poisson(df = df)
}
\seealso{
Other preference_order_functions: 
\code{\link{f_auc}},
\code{\link{f_r2}},
\code{\link{f_v}()},
\code{\link{f_v_rf_categorical}()}

Other preference_order_functions: 
\code{\link{f_auc}},
\code{\link{f_r2}},
\code{\link{f_v}()},
\code{\link{f_v_rf_categorical}()}

Other preference_order_functions: 
\code{\link{f_auc}},
\code{\link{f_r2}},
\code{\link{f_v}()},
\code{\link{f_v_rf_categorical}()}

Other preference_order_functions: 
\code{\link{f_auc}},
\code{\link{f_r2}},
\code{\link{f_v}()},
\code{\link{f_v_rf_categorical}()}
}
\concept{preference_order_functions}
