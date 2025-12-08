# Dual Filtering Strategy

## Summary

The function
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md)
applies two complementary methods to assess different aspects of
multicollinearity:

- **Pairwise correlation (Pearson and/or Cramer’s V)**: identifies pairs
  of highly redundant variables.

- **Variance Inflation Factors**: measures how much the variance of a
  regression coefficient is inflated due to multicollinearity with *all
  other* predictors.

By combining a pairwise method (correlation) and a multivariate one
(VIF),
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md)
is able to capture multicollinearity that either method alone might
miss. At the same time, it respects predictor rankings to protect
relevant variables during filtering.

This article explains how this dual filtering algorithm works,
demonstrates its behavior with examples, and provides guidance on when
to use each filtering method.

## Setup

The examples herein require the following setup.

## The Multicollinearity Filtering Algorithm

The filtering algorithm in
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md)
works as follows:

1.  Predictors are ranked according to the argument `preference_order`,
    or from lower to higher overall multicollinearity otherwise.

2.  The correlation matrix between all predictors is computed with
    [`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md).

3.  The first predictor in the ranking is *selected*, while all others
    are *candidates*.

4.  The candidates are evaluated sequentially:

4a. If `max_cor` is set, and the maximum correlation between the
candidate and the selected is higher than `max_cor`, the candidate is
discarded and the algorithm moves to the next one. Otherwise, the
candidate goes to the next step.

4b. If `max_vif` is set, and the maximum VIF of the candidate against
all selected predictors is higher than `max_vif`, the candidate is
discarded. Otherwise, the candidate is selected, and the algorithm moves
into the next candidate.

To finalize, the selected predictors are returned.

Let’s take a look at how that works in code with the multicollinearity
thresholds and the predictors and below.

``` r
max_vif = 5
max_cor = 0.7

predictors <- c(
  "swi_mean",
  "soil_temperature_mean", 
  "rainfall_mean",
  "growing_season_length"
)
```

To simplify the code below a bit, the **predictors are already ordered
by preference**, and we omit the cases `max_vif = NULL` or
`max_cor = NULL`.

The correlation matrix is computed with
[`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md)
and ordered after the argument `preference_order`.

``` r
m <- collinear::cor_matrix(
  df = vi_smol,
  predictors = predictors,
  quiet = TRUE
)

m <- m[
  predictors, 
  predictors
  ]

round(m, 2)
#>                       swi_mean soil_temperature_mean rainfall_mean
#> swi_mean                  1.00                 -0.19          0.66
#> soil_temperature_mean    -0.19                  1.00          0.05
#> rainfall_mean             0.66                  0.05          1.00
#> growing_season_length     0.88                 -0.07          0.78
#>                       growing_season_length
#> swi_mean                               0.88
#> soil_temperature_mean                 -0.07
#> rainfall_mean                          0.78
#> growing_season_length                  1.00
```

The first predictor in `preference_order` is added to `selected` while
the remaining ones go to `candidates`.

``` r
selected <- predictors[1]
candidates <- predictors[-1]
```

The first `candidate` gets ready for evaluation.

``` r
candidate <- candidates[1]
```

The maximum correlation between `selected` and `candidate` is assessed
by extracting the `selected` rows and the `candidate` column of the
absolute correlation matrix, computing its maximum, and comparing it
with `max_cor`.

``` r
max(
  abs(
    m[
      selected, 
      candidate
      ]
    )
  ) > max_cor
#> [1] FALSE
```

If the expression above evaluates to `TRUE`, the candidate is removed
from `candidates`, as in `candidates <- candidates[-1]`, and a new
candidate is selected for evaluation.

Otherwise the algorithm moves to the VIF evaluation.

The subset of `m` with all rows and columns of `selected` and
`candidate` is used as input for
[`vif()`](https://blasbenito.github.io/collinear/reference/vif.md).

``` r
max(
  collinear::vif(
    m = m[
      c(selected, candidate),
      c(selected, candidate)
    ]
  )
) > max_vif
#> [1] FALSE
```

If it evaluates to `TRUE`, `candidate` is rejected and the algorithm
goes to the next candidate.

Otherwise, `candidate` is added to `selected` and removed from
`candidates`, and a new `candidate` is selected for evaluation.

``` r
selected <- c(selected, candidate)
candidates <- candidates[-1]
candidate <- candidates[1]
```

Let’s take a look at the whole loop:

``` r
#compute and reorder correlation matrix
m <- collinear::cor_matrix(
  df = vi_smol,
  predictors = predictors,
  quiet = TRUE
)

m <- m[
  predictors,
  predictors
]

#generate selected and candidates
selected <- predictors[1]
candidates <- predictors[-1]

#iterate over candidates
for (candidate in candidates) {
  
  #correlation
  if (
    max(
      abs(
        m[
          selected,
          candidate
        ]
      )
    ) > max_cor
  ) {
    #if TRUE move to next candidate
    next
  }

  #vif evaluation
  if (
    max(
      vif(
        m[
          c(selected, candidate),
          c(selected, candidate)
        ]
      )
    ) > max_vif
  ) {
    #if TRUE, move to next candidate
    next
  }

  selected <- c(selected, candidate)
  
}

selected
#> [1] "swi_mean"              "soil_temperature_mean" "rainfall_mean"
```

Let’s see if
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md)
returns the same selection.

``` r
collinear::collinear_select(
  df = vi_smol,
  predictors = predictors,
  preference_order = predictors,
  max_cor = 0.7,
  max_vif = 5
)
#> [1] "swi_mean"              "soil_temperature_mean" "rainfall_mean"        
#> attr(,"validated")
#> [1] TRUE
```

And that is all about how multicollinearity filtering works within
`collinear`!
