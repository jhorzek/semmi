
<!-- README.md is generated from README.Rmd. Please edit that file -->

# semmi

Fitting structural equation models is hard. Most models do not fit well
and post-hoc respecification with modification indices is a common
practice. This practice has been criticized for capitalization on chance
(see [MacCallum et al.,
1992](https://psycnet.apa.org/doi/10.1037/0033-2909.111.3.490)). That
is, the model may adapt too closely to the data and start fitting noise.

**semmi** is an R package building on [**lavaan**]() that provides
bootstrapping and resampling methods for model re-specification with
modification indices. The objective is to make specification search less
dependent on the sample, that is, to improve the stability. Importantly,
this is still an exploratory method and the SEM is no longer
confirmatory.

In bootstrapped specification search, random samples are drawn from the
original data set. These samples have the same size as the original
data, but may contain subjects multiple times.
bootstrap_specification_search will apply specification search to each
of the samples and return the number of times a specific modification
has been added to the model. The objective is to make specification
search less dependent on the sample, that is, to improve the stability.
Importantly, this is still an exploratory method and the SEM is no
longer confirmatory.

In resampled specification search, random samples are drawn from the
original data set. These samples have a smaller size than the original
data and each subject may be present once or not at all in the subset.
semmi will apply specification search to each of the samples and return
the number of times a specific modification has been added to the model.

## Installation

You can install the development version of **semmi** as follows:

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("jhorzek/semmi")
```

## Example

``` r
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
plot(spec_searched)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
