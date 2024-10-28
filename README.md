
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

The data frame `vi` consists of 30,000 rows and 67 columns, featuring
various response types and a mix of numeric and categorical predictors.
The code below makes it smaller to accelerate this tutorial.

``` r
set.seed(1)
df <- dplyr::slice_sample(
  vi, 
  n = 5000
  )
```

The response columns, all derived from the same data, have descriptive
names: `vi_numeric`, `vi_counts` (integers), `vi_binomial` (1s and 0s),
`vi_categorical` (five categories), and `vi_factor` (factor version of
the previous one).

Predictor names are grouped in character vectors:
`vi_predictors_numeric` (49 numeric and integer predictors),
`vi_predictors_categorical` (12 character and factor predictors), and
`vi_predictors` containing them all.

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

The predictor names are returned in the same order as the preference
order. If `response` is provided, `collinear()` returns non-collinear
variables ordered by their association with `response`. Otherwise, the
predictors are ordered from lower to higher multicollinearity.

### How It Works

The table below shows the functions called directly by `collinear()`,
their functionality, data requirements, and how to disable them.

| **Function**            | **Functionality**                           | **Requirements**                                      | **Disabled**                                          |
|-------------------------|---------------------------------------------|-------------------------------------------------------|-------------------------------------------------------|
| `target_encoding_lab()` | categorical predictors <br> to numeric      | \- numeric `response` <br> - categorical `predictors` | \- `response = NULL` <br> - `encoding_method = NULL`  |
| `preference_order()`    | rank and preserve <br> important predictors | any `response`                                        | \- `response = NULL` <br> - `preference_order = NULL` |
| `cor_select()`          | reduce <br> pairwise correlation            | any `predictors`                                      | `cor_max = NULL`                                      |
| `vif_select()`          | reduce <br> variance inflation              | numeric `predictors`                                  | `vif_max = NULL`                                      |

The next sections follow the processing of the responses “vi_numeric”
and “vi_character” through these functions.

#### `target_encoding_lab()`

This function requires a numeric `response` to transform categorical
`predictors` to numeric. This transformation allows applying the same
multicollinearity filtering methods to all predictors.

In `collinear()`, this functionality is controlled by the argument
`encoding_method`, which defines the encoding method, or disables the
functionality entirely when `NULL`.

The code chunk below creates an example data frame with two levels of
the categorical predictor “koppen_zone” and the response “vi_numeric”.

``` r
df_toy <- df |> 
  dplyr::select(vi_numeric, koppen_zone) |> 
  dplyr::filter(koppen_zone %in% c("Af", "BSh")) |> 
  dplyr::group_by(koppen_zone) |> 
  dplyr::slice_head(n = 5) |> 
  dplyr::ungroup()
```

When introducing this data frame into `target_encoding_lab()` with the
method “loo” (leave-one-out), the data frame is grouped by the levels of
“koppen_zone”, and each value in a group is encoded as the average of
`response` across all other group cases.

The methods “mean” and “rank” are also available through the argument
`encoding_method` of `collinear()`. Please check the documentation of
`target_encoding_lab()` for further details.

``` r
df_toy <- collinear::target_encoding_lab(
  df = df_toy,
  response = "vi_numeric",
  predictors = "koppen_zone",
  method = "loo",
  overwrite = TRUE,
  quiet = TRUE
)
```

The function output shows “koppen_zone” encoded as numeric, and ready
for a multicollinearity analysis along with other numeric predictors.

In the example call to `collinear()` shown above, target encoding is
executed for “vi_numeric”, resulting in zero categorical predictors.

``` r
df_vi_numeric <- collinear::target_encoding_lab(
  df = df,
  response = "vi_numeric",
  predictors = vi_predictors,
  method = "loo",
  overwrite = TRUE,
  quiet = TRUE
)

#find categorical predictors
collinear::identify_predictors_categorical(
  df = df_vi_numeric,
  predictors = vi_predictors
)
```

On the other hand, target encoding is ignored for the categorical
`response` “vi_categorical”, resulting in 12 categorical predictors to
be handled by the multicollinearity analysis.

``` r
df_vi_categorical <- df

collinear::identify_predictors_categorical(
  df = df_vi_categorical,
  predictors = vi_predictors
)
```

#### `preference_order()`

This function ranks predictors by the strength of their association to
the response to help preserve a relevant subset during multicollinearity
filtering.

Three arguments control this feature in `collinear()`:

- `preference_order`: Either a pre-computed or custom ranking of
  predictors, or “auto” to let `collinear()` handle it internally.
- `preference_f`: Function to compute the association between `response`
  and `predictors` (check `help(preference_order)`, which lists the
  available options). If “auto” (default), then `f_auto()` selects one
  based on the data features.
- `preference_warn_limit`: If not `NULL`, this feature helps identifying
  predictors that are suspiciously similar to the response.

Preference order is computed as follows for the `response` “vi_numeric”:

``` r
preference_vi_numeric <- collinear::preference_order(
  df = df_vi_numeric,
  response = "vi_numeric",
  predictors = vi_predictors,
  f = "auto",
  warn_limit = 0.95,
  quiet = FALSE
)
```

When all variables are numeric, `f_auto()` selects `f_r2_pearson()` to
compute the Pearson correlation between `response` and `predictors`.

The function returns the data frame shown below, ordered by the value of
the column “preference”.

The order of the column “predictor” matches the output of `collinear()`
for the response “vi_numeric”.

``` r
selection$vi_numeric
```

This data frame resulting from `preference_order()` can be used as input
for the `preference_order` argument of `collinear()`.

If the choice made by `f_auto()` is deemed inappropriate, there are
other options.

``` r
preference_gam <- collinear::preference_order(
  df = df_vi_numeric,
  response = "vi_numeric",
  predictors = vi_predictors,
  f = f_r2_gam_gaussian,
  quiet = TRUE
)
```

The argument `preference_order` of `collinear()` also accepts a
user-defined vector of preference. The example below shifts the focus of
the multicollinearity analysis towards several soil-related predictors.

``` r
collinear::collinear(
  df = df,
  response = "vi_numeric",
  predictors = vi_predictors,
  preference_order = c(
    "soil_ph",
    "soil_clay",
    "soil_silt",
    "soil_sand",
    "soil_nitrogen",
    "soil_soc",
    "soil_temperature_mean"
  ),
  quiet = TRUE,
  cor_max = 0.5,
  vif_max = 2.5
)
```

In this case, `collinear()` passes all predictors not in the custom
vector to `preference_order()`, and appends the resulting ranking to the
custom vector.

#### `cor_select()`

#### `vif_select()`

If you got here, thank you for your interest in `collinear`. I hope you
can find it useful!

Blas M. Benito, PhD
