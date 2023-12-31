% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subsample_specification_search.R
\name{resample_specification_search}
\alias{resample_specification_search}
\title{resample_specification_search}
\usage{
resample_specification_search(
  model,
  data,
  alpha = 0.05,
  operators = c("=~", "~", "~~"),
  N_subsets,
  number_of_resamples,
  max_iter = 100 * number_of_resamples,
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

\item{N_subsets}{sample size in subsets}

\item{number_of_resamples}{number of resamples to use. The more the merrier!}

\item{max_iter}{maximal number of tries: Subsmaples samples may fail to fit.
Therefore, we set an upper bound on the number of draws.}

\item{...}{option to pass arguments to the lavaan sem-function}
}
\value{
object of class resample_Spec_Search with modifications in each
bootstrap sample.
}
\description{
Provides resample specification search for lavaan models. Specification search is an exploratory
strategy, where parameters are added to the model if the modification indices
are significant. This practice has been criticized for capitalization on chance
(see \href{https://psycnet.apa.org/doi/10.1037/0033-2909.111.3.490}{MacCallum et al., 1992}).
That is, the model may adapt too closely to the data and start fitting noise.
In resampled specification search, random samples are drawn from the original
data set. These samples have a smaller size than the original data and each subject
may be present once or not at all in the subset. resample_specification_search will apply specification
search to each of the samples and return the number of times a specific modification
has been added to the model. The objective is to make specification search less
dependent on the sample, that is, to improve the stability. Importantly, this is
still an exploratory method and the SEM is no longer confirmatory.
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
spec_searched <- resample_specification_search(model = model,
                                      data = PoliticalDemocracy,
                                      operators = "~~",
                                      N_subsets = 70,
                                      number_of_resamples = 5) # should be much higher!
spec_searched
}
