# collinear 2.0.0

**Warning**: This version includes several breaking changes.

## Main Changes

### Function `preference_order()`

- Now works with any combination of categorical and numeric responses and predictors. Previously, only numeric responses were considered valid.

- Accepts a character vector with multiple response variables and returns a named list of data frames in such cases.

- All functions used as input for the argument `f` have been rewritten, with extended coverage of cases. These functions have also been consistently renamed following these rules:
  - A code indicating the metric: `r2` for R-squared, `auc` for area under the curve (for binomial responses), and `v` for Cramer's V (for categorical responses).
  - A code indicating the model: `spearman`, `pearson`, and `v` for direct association; `glm` for GLMs; `gam` for GAMs; `rf` for Random Forest models; and `rpart` for Recursive Partition Trees.
  - The model family for GLMs or GAMs: `gaussian` for numeric responses, `binomial` for binomial responses, and `poisson` for integer counts.
  - The term `poly2` for GLMs with second-degree polynomials.
  
- When `f = NULL`, the function `f_auto()` determines an appropriate default adapted to the types of the response and predictors.

- Now issues a warning if predictors show a suspiciously high association with the response. The sensitivity of this test is controlled by the new argument `warn_limit`.

- Parallelization setup is now managed via `future::plan()`, and a progress bar is available through `progressr::handlers()`.

### Function `collinear()`

- Now works with any combination of categorical and numeric responses and predictors. Previously, only numeric responses were valid. Categorical predictors are excluded from VIF analysis but are returned in the output if they pass the pairwise correlation test.

- Accepts a character vector with multiple response variables and returns a named list of data frames in such cases.

- The preference order is now computed internally if `preference_order = NULL` (default). Therefore, all relevant arguments of the function `preference_order()` have been added to `collinear()` with the prefix "preference_".

- Parallelization setup is now managed via `future::plan()`, with a progress bar provided by `progressr::handlers()`. This setup is leveraged by `preference_order()` and `cor_select()`.

- Target encoding can be disabled by setting the `encoding_method` argument to `NULL`.

- VIF filtering can be disabled by setting `max_vif` to `NULL`.

- Pairwise correlation filtering can be disabled by setting `max_cor` to `NULL`.

### Function `cor_select()`

- A new robust forward selection algorithm ensures that the most important predictors are retained after multicollinearity filtering when `preference_order` is used.

- Target encoding, along with the `response` and `encoding_method` arguments, has been removed from this function. This change also applies to `cor_df()`.

- The function now calls `validate_data_cor()` to ensure that the data is suitable for pairwise correlation multicollinearity filtering.

- Parallelization setup is now managed via `future::plan()`, with a progress bar provided by `progressr::handlers()`. This setup is used by `cor_numeric_vs_categorical()` and `cor_categorical_vs_categorical()` to speed up pairwise correlation computation.

### Function `cor_df()`

- Fixed a bug that prevented `cor_numeric_vs_categorical()` and `cor_categorical_vs_categorical()` from triggering properly.

### Function `vif_select()`

- A new robust forward selection algorithm better preserves predictors with higher preference when `preference_order` is used.

- Target encoding, along with the `response` and `encoding_method` arguments, has been removed. As a result, this function now only works with numeric predictors. This change also applies to `vif_df()`.

- The new function `validate_data_vif()` is called to ensure the data is suitable for VIF-based multicollinearity filtering. Attempting a VIF analysis in a data frame with more columns than rows now returns an error.

### Function `target_encoding_lab()` and Companion Functions

- Completely rewritten for parallelization using `future::plan()` and a progress bar via `progressr::handlers()`.

- The default encoding method is now "loo" (leave-one-out), as it provides more useful results in most cases.

- The functions `target_encoding_mean()`, `target_encoding_rank()`, and `target_encoding_loo()` have been simplified to the bare minimum, with all redundant logic moved to `target_encoding_lab()`.

- NA cases in the predictor to encode are now grouped under "NA".

- The "rnorm" method has been deprecated, and the function `target_encoding_rnorm()` has been removed from the package.

## Other Changes

- Added the function `cor_clusters()` to group predictors using `stats::hclust()` based on their pairwise correlation matrix.

- Streamlined the package documentation using roxygen methods to inherit sections and parameters.

- Removed `dplyr` as a dependency.

- Added `mgcv`, `rpart`, and `ranger` to Imports to support all `f_xxx()` functions from the start.

- All warnings in data validation functions have been converted to messages. These messages now indicate the function that generated them, aiding in debugging and ensuring that messages and warnings are printed in the correct order.



# collinear 1.1.1

Hotfix of issue with solve(tol = 0) in systems with no large double support (noLD). This one wasn't fun.

# collinear 1.1.0

Added argument "smoothing" to `target_encoding_mean()` function to implement original target encoding method.

Added alias `f_rf_rsquared()` to the function `f_rf_deviance()`.

Added column "vi_binary" to `vi` as a binary version of "vi_mean".

Added function `auc_score()` to compute the area under the curve of predictions from binary models. 

Added function `case_weights()` to compute case weights when binary responses are unbalanced.

Added function `f_rf_auc_balanced()` to be used as input for the `f` argument of `preference_order()` when the response is binary and balanced.

Added function `f_rf_auc_unbalanced()` to be used as input for the `f` argument of `preference_order()` when the response is binary and unbalanced.

Added function `f_gam_auc_balanced()` to be used as input for the `f` argument of `preference_order()` when the response is binary and balanced.

Added function `f_gam_auc_unbalanced()` to be used as input for the `f` argument of `preference_order()` when the response is binary and unbalanced.

Added function `f_logistic_auc_balanced()` to be used as input for the `f` argument of `preference_order()` when the response is binary and balanced.

Added function `f_logistic_auc_unbalanced()` to be used as input for the `f` argument of `preference_order()` when the response is binary and unbalanced.

Fixed issue with perfect correlations in `vif_df()`. Now perfect correlations are replaced with 0.99 (for correlation == 1) and -0.99 (for correlation == -1) in the correlation matrix to avoid errors in `solve()`.

Added the example dataset `toy`, derived from `vi`, but with known relationships between all variables.

Fixed issue in function cor_df() where many cases would be lost because the logic to remove diagonals was flawed, as all pairs with correlation == 1 were being removed.

Fixed issue in functions cor_select() and vif_select() where ignoring `predictors` and using only `df` would lead to empty selections.

# collinear 1.0.2

This version fixes bugs in two functions: `cor_select()` and `cor_df()`

**`cor_select()`**

  + When only one variable was left in the correlation matrix, the one column matrix became a vector with no colnames, which yielded an error. Now, to avoid this issue, drop = FALSE is used in the matrix subsetting.

  + The previous version started removing predictors on a backwards fashion, from the last predictor in preference order, moving up one by one to the top. Under the wrong circumstances (low number of predictors, low max_cor, and high correlation between first and second predictors in preference order) this configuration would lead to keep only the first predictor, even when having others comply with the max_cor restriction lower down in the preference order. The new version produces smaller subsets of predictors with a higher diversity.

**`cor_df()`**

  + The data frame returned pairs of the same variable when cor_method was "spearman". Fixed with a dplyr::filter(x != y).


# collinear 1.0.1

Re-submission after minor CRAN comments.

Changes:

- version number bumped up to 1.0.1

- removed if(interactive()){} from all @examples.

- removed plot() call from the @examples of the function target_encoding_lab() because it was messing up pkgdown's build. This piece of code triggered the comment "Please always make sure to reset to user's options()" by the reviewer, so this should solve the issue.

- made sure that all examples run in less than 5 seconds.

- fixed a bug in which all functions would include the response as a predictor when 'predictors = NULL' and 'response' was a valid column of the input data frame.

# collinear 1.0.0

First functional version of the package submitted to CRAN.
