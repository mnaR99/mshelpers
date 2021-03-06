% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ml.R
\name{fit_mlm}
\alias{fit_mlm}
\title{Fit multiple models}
\usage{
fit_mlm(x, dists, quiet = FALSE)
}
\arguments{
\item{x}{numeric vector}

\item{dists}{a character vector containing the distribution models to fit; see
help(mshelpers::models)}

\item{quiet}{hide errors (TRUE, the default), or display them as they occur?}
}
\value{
Returns a tibble with the fitted models and their corresponding log-likelihood,
AIC, BIC and p-value of the Kolmogorov-Smirnov test.
}
\description{
Estimate the parameters of specified models
}
