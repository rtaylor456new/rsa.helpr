
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rsa.helpr

<!-- badges: start -->

[![R-CMD-check](https://github.com/rtaylor456new/rsa.helpr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rtaylor456new/rsa.helpr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of rsa.helpr is to enhance efficiency, accessibility, and
reproducibility in vocational rehabilitation (VR) analysis.
Specifically, the focus is to streamline the cleaning and analysis of
RSA-911 and Transition Readiness Toolkit (TRT) data, a new measure of
program effectiveness.

## Installation

You can install the development version of rsa.helpr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rtaylor456new/rsa.helpr")
```

## Quick Start

Load the package and explore function documentations.

``` r
library(rsa.helpr)
# ?load_data
# ?clean_utah
# ?clean_scores
# ?create_metadata
# ?visualize_densities
# ?clean_provider
# ?visualize_metadata
# ?handle
# etc.
```

## Cleaning Process Example

To clean and merged RSA-911 data and TRT scores data and condense into
metadata for seamless analysis, simply apply four functions in
`rsa.helpr`. This following is an example of the simple cleaning steps
using defaults (see functions’ documentions for information on optional
arguments):

``` r
cleaned_rsa <- clean_utah(rsa_simulated)
cleaned_scores <- clean_scores(scores_simulated, state_filter = "Utah")
merged_data <- merge_scores(cleaned_rsa, cleaned_scores)
metadata <- create_metadata(merged_data)
```

These larger functions standardize and streamline cleaning steps, but
`rsa.helpr` allows for more flexible and independent data preparation
with the use of smaller helper functions. For example, `handle_splits`
is used to separate variables with values including special characters.
It allows for differing lengths of values (e.g. 1, 1;2, 3;3;4, NA). It
separates by the identified special character and creates new variables,
based on the original name, with one value per column.

``` r
var_clean <- handle_splits(rsa_simulated$E395_App_Medical_911, 
                           var_name = "E395_App_Medical_911")
```

## More Information

### Vignette

For more examples and a more detailed demonstration of `rsa.helpr` in
action, see the vignette, `rsa.helpr.Rmd`, within the repository or
[here](https://rtaylor456new.github.io/rsa.helpr/articles/rsa_helpr.html).

### Associated `shiny` App Dashboard

`rsa.helpr` is the backbone of the associated `shiny` app, hosted at
<https://rsa-data-dashboard.shinyapps.io/rsa_dashboard_app/>. This app
allows for automated and coding-free data preparation and simple
analyses.
