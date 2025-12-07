# Target Encoding

## Summary

[**Target encoding**](https://www.blasbenito.com/post/target-encoding/)
maps categorical predictors to a statistic of a reference numeric
variable across categories.

This functionality, implemented in \[target_encoding_lab()\], is used in
two different ways:

- \[cor_df()\] applies it to compute pairwise correlations between
  numeric and categorical predictors, if any.
- \[collinear()\] applies it optionally if the user requests it and the
  response is numeric to transform all categorical predictors to numeric
  and speed-up the multicollinearity analysis.

## Setup

The code chunk below prepares the session for this article.

## How It Works

To explain how target encoding works, let’s first create a little silly
dataframe:

``` r
df <- data.frame(
  num = 1:6,
  cat = c(rep("a", 3), rep("b", 3))
)

df
#>   num cat
#> 1   1   a
#> 2   2   a
#> 3   3   a
#> 4   4   b
#> 5   5   b
#> 6   6   b
```

### Mean Target Encoding

To transform `cat` to numeric we can remap it to the average of `num`
across each category in `cat`, as shown in the code below using the
`dplyr` syntax.

``` r
df <- df |> 
  dplyr::group_by(cat) |> 
  dplyr::mutate(cat_mean = mean(num)) |> 
  dplyr::ungroup()

df
#> # A tibble: 6 × 3
#>     num cat   cat_mean
#>   <int> <chr>    <dbl>
#> 1     1 a            2
#> 2     2 a            2
#> 3     3 a            2
#> 4     4 b            5
#> 5     5 b            5
#> 6     6 b            5
```

This is equivalent to the “mean” encoding method implemented in
[`target_encoding_lab()`](https://blasbenito.github.io/collinear/reference/target_encoding_lab.md).

``` r
df <- collinear::target_encoding_lab(
  df = df,
  response = "num",
  predictors = "cat",
  methods = "mean",
  quiet = TRUE
)

df
#>   num cat cat__encoded_loo
#> 1   1   a              2.5
#> 2   2   a              2.0
#> 3   3   a              1.5
#> 4   4   b              5.5
#> 5   5   b              5.0
#> 6   6   b              4.5
```

### Leave-One-Out Target Encoding

Another encoding method is the leave-one-out (`"loo"`), which computes
the group mean while excluding the current observation.

For each given case, it sums the numeric values in the same category,
subtracts the current row’s value, and divides by the count of
observations in that category minus one. The code below replicates this
behavior.

``` r
df <- df |> 
  dplyr::group_by(cat) |> 
  dplyr::mutate(
    cat_loo = (sum(num) - num) / (dplyr::n() - 1)
    ) |> 
  dplyr::ungroup()

df
#> # A tibble: 6 × 4
#>     num cat   cat__encoded_loo cat_loo
#>   <int> <fct>            <dbl>   <dbl>
#> 1     1 a                  2.5     2.5
#> 2     2 a                  2       2  
#> 3     3 a                  1.5     1.5
#> 4     4 b                  5.5     5.5
#> 5     5 b                  5       5  
#> 6     6 b                  4.5     4.5
```

Notice how the encoded values now vary within each category. For
category “a”: rows get values based on the other two observations in
“a”, and the same happens with “b”.

The same method can be applied in
[`target_encoding_lab()`](https://blasbenito.github.io/collinear/reference/target_encoding_lab.md)
when setting `method` to `"loo"`.

``` r
df <- collinear::target_encoding_lab(
  df = df,
  response = "num",
  predictors = "cat",
  methods = "loo",
  quiet = TRUE
)

df
#>   num cat cat__encoded_loo
#> 1   1   a              2.5
#> 2   2   a              2.0
#> 3   3   a              1.5
#> 4   4   b              5.5
#> 5   5   b              5.0
#> 6   6   b              4.5
```

### Rank Target Encoding

The rank method orders categories by their mean values and assigns them
integer ranks. It is useful to capture the ordinal relationship between
categories without being sensitive to outliers or extreme values.

The `dplyr` version of this algorithm is a bit more verbose, but I hope
it does the trick:

``` r
df <- df |> 
  dplyr::group_by(cat) |> 
  dplyr::mutate(cat_mean = mean(num)) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(cat_mean) |> 
  dplyr::group_by(cat) |> 
  dplyr::mutate(cat_rank = dplyr::cur_group_id()) |> 
  dplyr::ungroup()

df
#> # A tibble: 6 × 4
#>     num cat   cat_mean cat_rank
#>   <int> <fct>    <dbl>    <int>
#> 1     1 a            2        1
#> 2     2 a            2        1
#> 3     3 a            2        1
#> 4     4 b            5        2
#> 5     5 b            5        2
#> 6     6 b            5        2
```

The same method can be applied via
[`target_encoding_lab()`](https://blasbenito.github.io/collinear/reference/target_encoding_lab.md)
with \`encoding_method = “rank”.

``` r
df <- collinear::target_encoding_lab(
  df = df,
  response = "num",
  predictors = "cat",
  methods = "rank",
  quiet = TRUE
)

df
#>   num cat cat__encoded_loo
#> 1   1   a              2.5
#> 2   2   a              2.0
#> 3   3   a              1.5
#> 4   4   b              5.5
#> 5   5   b              5.0
#> 6   6   b              4.5
```

## Target Encoding in Practice

### Computing Correlations with Mixed Variable Types

When computing pairwise correlations between numeric and categorical
predictors, \[cor_df()\] automatically applies leave-one-out target
encoding behind the scenes.

``` r
x <- collinear::cor_df(
  df = collinear::vi_smol,
  predictors = c(
    "soil_temperature_mean", # numeric
    "koppen_zone"            # categorical
  ),
  quiet = TRUE
)

x
#>                       x           y correlation  metric
#> 1 soil_temperature_mean koppen_zone   0.9195774 Pearson
```

This is the same as applying
[`target_encoding_lab()`](https://blasbenito.github.io/collinear/reference/target_encoding_lab.md)
with method `"loo"` and then compute the pairwise correlation with
[`stats::cor()`](https://rdrr.io/r/stats/cor.html).

``` r
df <- collinear::target_encoding_lab(
  df = vi_smol,
  response = "soil_temperature_mean",
  predictor = "koppen_zone",
  encoding_method = "loo",
  overwrite = TRUE,
  quiet = TRUE
)

stats::cor(
  x = df$soil_temperature_mean,
  y = df$koppen_zone
)
#> [1] 0.9195774
```

### Speeding Up Multicollinearity Analysis

Enabling target encoding when working with numeric responses and
categorical predictors in \[collinear()\] provides two key advantages:
it speeds up computation considerably, and more importantly, it replaces
Cramer’s V associations between pairs of categorical variables with
Pearson correlations between pairs of numeric variables, providing a
more statistically sound multicollinearity assessment.

``` r
# without target encoding
time_without <- system.time({
  result_without_encoding <- collinear::collinear(
    df = vi_smol,
    responses = "vi_numeric",
    predictors = vi_predictors_categorical,
    encoding_method = NULL,
    quiet = TRUE
  )
})

# with target encoding
time_with <- system.time({
  result_with_encoding <- collinear::collinear(
    df = vi_smol,
    responses = "vi_numeric",
    predictors = vi_predictors_categorical,
    encoding_method = "loo",
    quiet = TRUE
  )
})

data.frame(
  encoding = c("No", "Yes"),
  seconds = c(time_without["elapsed"], time_with["elapsed"])
)
#>   encoding seconds
#> 1       No  10.507
#> 2      Yes   2.562
```

The speed-up is considerable!

But that’s not the only consequence of applying target encoding. Let’s
take a look at the variable selections on each run.

``` r
result_without_encoding$vi_numeric$selection
#> [1] "koppen_zone"      "soil_type"        "biogeo_ecoregion" "country_name"    
#> attr(,"validated")
#> [1] TRUE
```

``` r
result_with_encoding$vi_numeric$selection
#> [1] "koppen_zone"  "subregion"    "biogeo_realm" "continent"   
#> attr(,"validated")
#> [1] TRUE
```

The selection resulting from the run with target encoding is a bit
shorter. Why? Because without target encoding, pairwise associations
between categorical predictors are computed with Cramer’s V, which tends
to underestimate association under high cardinality (number of
categories).

Hence, when target encoding is applied,
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
captures true multicollinearity and ensures a consistent filtering
across all predictors, no matter their type.

## Important Considerations

**Overfitting Risk**: Mean encoding can leak information from the
response into the predictor. The leave-one-out method mitigates this
risk and is generally recommended for modeling workflows.

**Category Frequency**: Target encoding works best with categories that
have sufficient observations. Categories with very few observations may
produce unstable encoded values.

**When to Use Target Encoding**: Enable `encoding_method = "loo"` in
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
when you have a numeric response and categorical predictors, and you
want the most accurate multicollinearity assessment. The improved
statistical soundness and computational speed make it the preferred
approach for mixed data types.
