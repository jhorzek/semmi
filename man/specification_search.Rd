% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specification_search.R
\name{specification_search}
\alias{specification_search}
\title{specification_search}
\usage{
specification_search(
  model,
  data,
  alpha = 0.05,
  operators = c("=~", "~", "~~"),
  previous_model = NULL,
  added = data.frame(label = character(), mi = numeric()),
  ...
)
}
\arguments{
\item{model}{syntax for a lavaan model}

\item{data}{data used by the lavaan model}

\item{alpha}{significance level for the modification indices}

\item{operators}{which parameters should be considered to be added to the model?
Default: all types of parameters. If set to "=~", only loadings are added. If
set to "~" only regressions are added. If set to "~~" only covariances are added.}

\item{previous_model}{INTERNAL: Do not use this manually! lavaan syntax for
a previously estimated model}

\item{added}{INTERNAL: Do not use this manually. Vector with modifications already
added to the model}

\item{...}{option to pass arguments to the lavaan sem-function}
}
\value{
object of class Spec_Search with (1) a lavaan model including all
modifications and (2) a data.frame with labels of added modifications and the
modification index value
}
\description{
Provides specification search for lavaan models. Specification search is an exploratory
strategy, where parameters are added to the model if the modification indices
are significant. This practice has been criticized for capitalization on chance
(see \href{https://psycnet.apa.org/doi/10.1037/0033-2909.111.3.490}{MacCallum et al., 1992}).
That is, the model may adapt too closely to the data and start fitting noise.
Thus, specification search should be used very carefully; e.g., by combining it
with cross-validation.
}
\examples{
# The following example is adapted from ?lavaan::sem
library(semmi)
model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
  # none specified -> we want to use specification
  # search to check for residual correlations
'

fit <- sem(model, data = PoliticalDemocracy)
spec_searched <- specification_search(model = model,
                                      data = PoliticalDemocracy,
                                      operators = "~~")
spec_searched
}
