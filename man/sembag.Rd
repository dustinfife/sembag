% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_sembag.R
\name{sembag}
\alias{sembag}
\title{Use sembag}
\usage{
sembag(
  data,
  formula,
  iterations = 500,
  fit_function = NULL,
  variable_sampler = NULL,
  data_sampler = NULL,
  validation_function = NULL,
  mtry = NULL,
  ...
)
}
\arguments{
\item{formula}{A formula of the form ~ a + b + c. Note, there is no need to
put an outcome variable in the formula}

\item{...}{other arguments passed to other functions inside sembag}
}
\description{
Use sembag
}
