% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performance_score_r2.R
\name{performance_score_r2}
\alias{performance_score_r2}
\title{Pearson's R-squared of Observations vs Predictions}
\usage{
performance_score_r2(o = NULL, p = NULL)
}
\arguments{
\item{o}{(required, numeric vector) Response values. Default: NULL}

\item{p}{(required, numeric vector) Model predictions. Default: NULL}
}
\value{
numeric: Pearson R-squared
}
\description{
Internal function to compute the R-squared of observations versus model predictions.
}
\seealso{
Other modelling_tools: 
\code{\link{case_weights}()},
\code{\link{model_formula}()},
\code{\link{performance_score_auc}()},
\code{\link{performance_score_v}()}
}
\concept{modelling_tools}
