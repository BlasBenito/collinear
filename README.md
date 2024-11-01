
<!-- README.md is generated from README.Rmd. Please edit that file -->

# collinear: R Package for Seamless Multicollinearity Management

<!-- Development badges 
&#10;[![R-CMD-check](https://github.com/BlasBenito/collinear/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BlasBenito/collinear/actions/workflows/R-CMD-check.yaml)
[![Devel-version](https://img.shields.io/badge/devel%20version-1.0.1-blue.svg)](https://github.com/blasbenito/collinear)
&#10;<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10039489.svg)](https://doi.org/10.5281/zenodo.10039489)
[![CRAN
status](https://www.r-pkg.org/badges/version/collinear)](https://cran.r-project.org/package=collinear)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/grand-total/collinear)](https://CRAN.R-project.org/package=collinear)
[![R-CMD-check](https://github.com/BlasBenito/collinear/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BlasBenito/collinear/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Warning

Version 2.0.0 of `collinear` includes changes that may disrupt existing
workflows, and results from previous versions may not be reproducible
due to enhancements in the automated selection algorithms. Please refer
to the Changelog for details.

## Summary

The `collinear` package combines four methods for easy management of
multicollinearity in modelling data frames with numeric and categorical
variables:

- **Target Encoding**: Transforms categorical predictors to numeric
  using a numeric response as reference.
- **Preference Order**: Ranks predictors by their association with a
  response variable to preserve important ones in multicollinearity
  filtering.
- **Pairwise Correlation Filtering**: Automated multicollinearity
  filtering of numeric and categorical predictors based on pairwise
  correlations.
- **Variance Inflation Factor Filtering**: Automated multicollinearity
  filtering of numeric predictors based on Variance Inflation Factors.

These methods are combined in the function `collinear()`, and as
individual functions in `target_encoding_lab()`, `preference_order()`,
`cor_select()`, and `vif_select()`.

## Citation

If you find this package useful, please cite:

*Blas M. Benito (2024). collinear: R Package for Seamless
Multicollinearity Management. Version 1.2.0. doi:
10.5281/zenodo.10039489*

## Main Improvements in Version 2.0.0

1.  **Expanded Functionality**: Functions `collinear()` and
    `preference_order()` support both categorical and numeric responses
    and predictors, and can handle several responses at once.
2.  **Robust Selection Algorithms**: Enhanced selection in
    `vif_select()` and `cor_select()`.
3.  **Refined `preference_order()`**: New functions to compute
    association between response and predictors covering most use-cases,
    and automated function selection depending on data features.
4.  **Simplified Target Encoding**: Streamlined and parallelized for
    better efficiency, and new default is “loo” (leave-one-out).
5.  **Parallelization and Progress Bars**: Utilizes `future` and
    `progressr` for enhanced performance and user experience.

## Install

The package `collinear` can be installed from CRAN.

``` r
install.packages("collinear")
```

The development version can be installed from GitHub.

``` r
remotes::install_github(
  repo = "blasbenito/collinear", 
  ref = "development"
  )
```

Previous versions are in the “archive_xxx” branches of the GitHub
repository.

``` r
remotes::install_github(
  repo = "blasbenito/collinear", 
  ref = "archive_v1.1.1"
  )
```

## Required Packages

The packages required for this tutorial are:

## Parallelization Setup and Progress Bars

Most functions in the package now accept a parallelization setup via
`future::plan()` and progress bars via `progressr::handlers()`. However,
progress bars are ignored in this tutorial because they don’t work in
Rmarkdown.

``` r
future::plan(
  future::multisession,
  workers = parallelly::availableCores() - 1
  )

#progressr::handlers(global = TRUE)
```

## Example Data

The data frame `vi` consists of rows, columns, and 0 NA values. It
contains several numeric and categorical responses and predictors.

The response columns, all derived from the same data, have descriptive
names: `vi_numeric`, `vi_counts` (integers), `vi_binomial` (1s and 0s),
`vi_categorical` (five categories), and `vi_factor` (factor version of
the previous one).

Predictor names are grouped in character vectors:
`vi_predictors_numeric` (49 numeric and integer predictors),
`vi_predictors_categorical` (12 character and factor predictors), and
`vi_predictors` containing them all.

The code below makes it smaller to accelerate this tutorial.

``` r
set.seed(1)
df <- dplyr::slice_sample(
  vi, 
  n = 5000
  )
```

## Using `collinear()`

The function `collinear()` provides access to the key functionalities of
this package. The code below shows a call to `collinear()` with two
responses and mixed predictor types.

``` r
selection <- collinear::collinear(
  df = df,
  response = c(
    "vi_numeric",
    "vi_categorical"
    ),
  predictors = vi_predictors,
  quiet = FALSE
)
```

The function returns a named list of vectors with predictor names when
more than one response is provided, and a character vector otherwise.

``` r
selection
```

The selections for both responses are different because `collinear()`
follows different paths for numeric and categorical responses and
predictors.

| **Functionality**                  | **numeric `response`**                                          | **categorical `response`**                                                                                                       |
|------------------------------------|-----------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------|
| **Target-encoding**                | executed: 12 categorical `predictors` transformed to numeric    | skipped: all categorical `predictors` go to next steps                                                                           |
| **Preference order**               | `f_r2_pearson()`: R-squared between `response` and `predictors` | `f_v_rf_categorical()`: Cramer’s V of `response` against univariate random forest predictions                                    |
| **Pairwise correlation filtering** | \- *numeric vs num.*: Pearson correlation                       | \- *num. vs num.*: Pearson correlation <br> - *num. vs categorical*: target-encoding + Pearson <br> - *cat. vs cat.*: Cramer’s V |
| **VIF filtering**                  | Applied to all remaining `predictors`                           | Applied to numeric remaining `predictors`                                                                                        |

The following sections offer a detailed explanation of these alternative
paths and the reasons behind them.

### How It Works

The table below shows the main functions called by `collinear()`, their
functionality, data requirements, and how to disable them.

| **Function**            | **Functionality**                                | **Requirements**                                      | **Disabled**                                          |
|-------------------------|--------------------------------------------------|-------------------------------------------------------|-------------------------------------------------------|
| `target_encoding_lab()` | transform categorical predictors <br> to numeric | \- numeric `response` <br> - categorical `predictors` | \- `response = NULL` <br> - `encoding_method = NULL`  |
| `preference_order()`    | rank and preserve <br> important predictors      | any `response`                                        | \- `response = NULL` <br> - `preference_order = NULL` |
| `cor_select()`          | reduce <br> pairwise correlation                 | any `predictors`                                      | `cor_max = NULL`                                      |
| `vif_select()`          | reduce <br> variance inflation                   | numeric `predictors`                                  | `vif_max = NULL`                                      |

#### `target_encoding_lab()`

Target-encoding requires a numeric `response` to transform categorical
`predictors` to numeric. This transformation enables the application of
the same multicollinearity filtering (and modelling) methods to
categorical and numeric predictors.

In `collinear()`, this functionality is controlled by the argument
`encoding_method`, which disables the functionality entirely when
`NULL`, or defines the encoding method as “loo” (leave-one-out, default
method), “mean”, or “rank”.

There is a lengthier article about target-encoding
[here](https://www.blasbenito.com/post/target-encoding/), let me show
you a brief example of how it works. The example data frame below has
two levels of the categorical predictor “koppen_zone” and the response
“vi_numeric”.

When introducing this data frame into `target_encoding_lab()` with the
method “loo”, it is first grouped by “koppen_zone” levels, and then each
case is encoded as the average of `response` across all other cases
within the same level.

The result shows “koppen_zone” encoded as numeric and ready for a
multicollinearity analysis.

Due to the requirement for a numeric `response`, in the example call to
`collinear()` target encoding is only applied for the `response`
“vi_numeric” as follows:

``` r
df_vi_numeric <- collinear::target_encoding_lab(
  df = df,
  response = "vi_numeric",
  predictors = vi_predictors,
  method = "loo",
  overwrite = TRUE,
  quiet = TRUE
)
```

This operation results in zero categorical predictors in the data frame:

``` r
collinear::identify_predictors_categorical(
  df = df_vi_numeric,
  predictors = vi_predictors
)
```

On the other hand, target encoding is skipped for the categorical
`response` “vi_categorical”, resulting in 12 categorical predictors.

``` r
df_vi_categorical <- df

collinear::identify_predictors_categorical(
  df = df_vi_categorical,
  predictors = vi_predictors
)
```

#### `preference_order()`

The functions `collinear()`, `cor_select()`, and `vif_select()` are
designed to preserve as many “relevant” predictors as possible during
the multicollinearity filtering. This functionality is controlled by the
argument `preference_order`. This argument accepts two different inputs,
a custom character vector or a data frame.

**Custom character vector**

Vector with `predictors` names in order of interest for the analyst. The
example below shows a hypothetical case focused on preserving soil
temperature variables over all others.

``` r
selection_from_vector <- collinear::collinear(
  df = df,
  response = "vi_numeric",
  predictors = vi_predictors_numeric,
  preference_order = c(
    "soil_temperature_mean",
    "soil_temperature_range",
    "soil_temperature_min",
    "soil_temperature_max"
  ),
  quiet = TRUE
)

selection_from_vector
```

**Data frame**

Must have the column “predictor”, and should be ordered from higher to
lower quantitative preference.

The function `preference_order()` generates this data frame by computing
the univariate association between each predictor and the `response`
using a given `f` function:

``` r
preference_df <- collinear::preference_order(
  df = df,
  response = "vi_numeric",
  predictors = vi_predictors_numeric,
  f = f_r2_pearson,
  quiet = TRUE
)
```

The resulting data frame can be plugged into the `preference_order`
argument of `collinear()`:

``` r
selection_from_df <- collinear::collinear(
  df = df,
  response = "vi_numeric",
  predictors = vi_predictors_numeric,
  preference_order = preference_df,
  quiet = TRUE
)

selection_from_df
```

But `collinear()` can also compute preference order on its own to obtain
the same result:

``` r
selection_auto <- collinear::collinear(
  df = df,
  response = "vi_numeric",
  predictors = vi_predictors_numeric,
  preference_order = "auto",
  f = "auto",
  quiet = TRUE
)

selection_auto
```

The argument `f` of `preference_order()` (named `f` in
`collinear()`) either receives a function name, or the string “auto”.

**Function name**

The function `f_functions()` returns a data frame with the details of
all `f` functions.

``` r
collinear::f_functions()
```

All these functions take a data frame named `df` with the columns “x”
and “y” as input, so preparing a custom one is a simple task. For
example, the one below returns the R-squared of a linear model between
the predictor and the response. One just have to take in mind that
`preference_order()` will rank the results of such function from higher
to lower values.

``` r
f_lm <- function(df){
  summary(lm(y ~ x, data = df))$r.squared
}

preference_lm <- collinear::preference_order(
  df = df,
  response = "vi_numeric",
  predictors = vi_predictors_numeric,
  f = f_lm,
  quiet = TRUE
)
```

, and it does not require a numeric response or predictors to do so:

``` r
selection_categorical <- collinear::collinear(
  df = df,
  response = "vi_categorical",
  predictors = vi_predictors_categorical,
  preference_order = "auto",
  f = "auto",
  quiet = TRUE
)

selection_categorical
```

#### `cor_select()`

#### `vif_select()`

If you got here, thank you for your interest in `collinear`. I hope you
can find it useful!

Blas M. Benito, PhD
