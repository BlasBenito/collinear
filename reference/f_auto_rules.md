# Decision rules for `f_auto()`

Dataframe with rules used by
[`f_auto()`](https://blasbenito.github.io/collinear/reference/f_auto.md)
to select the function `f` in
[`f_functions()`](https://blasbenito.github.io/collinear/reference/f_functions.md)
to compute preference order in
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md).
In most cases, random forest is selected as base model to provide
homogeneous results across case types.

## Usage

``` r
f_auto_rules()
```

## Value

dataframe

## See also

Other preference_order_tools:
[`f_auto()`](https://blasbenito.github.io/collinear/reference/f_auto.md),
[`f_functions()`](https://blasbenito.github.io/collinear/reference/f_functions.md)

## Examples

``` r
f_auto_rules()
#>                name     response_type predictors_type
#> 1     f_numeric_glm continuous-binary         numeric
#> 2      f_numeric_rf continuous-binary     categorical
#> 3      f_numeric_rf continuous-binary           mixed
#> 4     f_numeric_glm    continuous-low         numeric
#> 5      f_numeric_rf    continuous-low     categorical
#> 6      f_numeric_rf    continuous-low           mixed
#> 7     f_numeric_glm   continuous-high         numeric
#> 8      f_numeric_rf   continuous-high     categorical
#> 9      f_numeric_rf   continuous-high           mixed
#> 10   f_binomial_glm  integer-binomial         numeric
#> 11    f_binomial_rf  integer-binomial     categorical
#> 12    f_binomial_rf  integer-binomial           mixed
#> 13      f_count_glm    integer-binary         numeric
#> 14       f_count_rf    integer-binary     categorical
#> 15       f_count_rf    integer-binary           mixed
#> 16      f_count_glm       integer-low         numeric
#> 17       f_count_rf       integer-low     categorical
#> 18       f_count_rf       integer-low           mixed
#> 19      f_count_glm      integer-high         numeric
#> 20       f_count_rf      integer-high     categorical
#> 21       f_count_rf      integer-high           mixed
#> 22 f_categorical_rf       categorical         numeric
#> 23 f_categorical_rf       categorical           mixed
#> 24 f_categorical_rf       categorical     categorical
```
