% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm.R
\name{extract_model}
\alias{extract_model}
\title{Extract linear model diagnostics}
\usage{
extract_model(x, ..., data = stats::model.frame(x))
}
\arguments{
\item{x}{linear model object of class 'lm'}

\item{...}{items passed to update_response}

\item{data}{data to be modeled}
}
\value{
object of class 'ModelResponse'
}
\description{
Extract all information possible with broom::tidy, broom::glance, and broom::augment.
}
\examples{
radon_model <- lm(Uppm ~ typebldg + basement + dupflag, data = radon_mn)
result <- extract_model(radon_model)
result$as_json()
}
