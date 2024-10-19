# collinear 1.2.0

**Warning**: This version includes several breaking changes.

## Main Changes

### Function `preference_order()`

- Now accepts **categorical responses** (class character or factor) as input to the `response` argument, in addition to numeric, binomial (1s and 0s), and integer counts.
- Parallelization setup is now managed via `future::plan()` and a progress bar via `progressr::handlers()`. As a result, the `workers` argument has been removed.
- All functions used as input for the `f` argument of `preference_order()` have been rewritten and renamed for clarity:
  - The first part of the name after the `f_` prefix indicates the metric used to compute association: `r2` for R-squared, `auc` for area under the curve in binomial responses, and `v` for Cramer's V in categorical responses.
  - The second part of the name indicates the model used to compute association: `spearman`, `pearson`, and `v` for direct association; `glm` for GLMs; `gam` for GAMs; `rf` for Random Forest models; and `rpart` for Recursive Partition Trees.
  - The third part of the name indicates the family used in GLMs or GAMs: `gaussian` for numeric responses, `binomial` for binomial responses, and `poisson` for integer counts.
  - If `poly2` appears in the name, it indicates that second-degree polynomials are used to build the model.
  
- A new function `f_default()` determines an appropriate default for `f` when `f = NULL`, based on the response and predictors. It uses the decision rules in the data frame returned by `f_default_rules()`. In any case, `f_default()` prints a message with the selected function name.

### Function `collinear()`

- Parallelization setup is now managed via `future::plan()` and a progress bar via `progressr::handlers()`. This setup is leveraged by `preference_order()` and `cor_select()`.
- Now accepts **categorical responses** (class character or factor) as input to the `response` argument, in addition to numeric, binomial (1s and 0s), and integer counts. However, target encoding only runs for numeric, binomial, or integer responses.
- Categorical predictors in `predictors` are excluded from the VIF analysis but are still returned in the output if they pass the pairwise correlation analysis.
- `preference_order()` is now run internally before multicollinearity filtering. As a result, the `f` argument has been added to the function's signature. If `f` is `NULL`, it is selected via `f_default()` (see details above).
- The new functions `validate_data_cor()` and `validate_data_vif()` are called to ensure that the data is suitable for pairwise correlation or VIF-based multicollinearity filtering.
- Target encoding can be disabled by setting the `encoding_method` argument to `NULL`.
- VIF filtering can be disabled by setting `vif_max` to `NULL`.
- Pairwise correlation filtering can be disabled by setting `cor_max` to `NULL`.

### Function `cor_select()`

- A new forward selection algorithm ensures that the most important predictors are retained after multicollinearity filtering when `preference_order` is used.
- Parallelization setup is now managed via `future::plan()` and a progress bar via `progressr::handlers()`. This setup is leveraged by `cor_numeric_vs_categorical()` and `cor_categorical_vs_categorical()` to speed up the pairwise correlation computation.
- Target encoding, along with the `response` and `encoding_method` arguments, has been removed from the function. This change also applies to `cor_df()`.
- A new function `validate_data_cor()` ensures the data is appropriate for analysis.

### Function `cor_df()`

- Fixed a bug that prevented `cor_numerics_vs_categorics()` and `cor_categorics_vs_categorics()` from triggering properly.

### Function `vif_select()`

- A new forward selection algorithm better preserves predictors with higher preference when `preference_order` is used.
- Target encoding, along with the `response` and `encoding_method` arguments, has been removed from the function. This change also applies to `vif_df()`. As a result, `vif_select()` now only accepts numeric predictors, but these can still be transformed using `target_encoding_lab()`.

### Function `target_encoding_lab()`

- The "rnorm" method has been deprecated, and the function `target_encoding_rnorm()` has been removed from the package.

## Other Changes

- Added the function `cor_clusters()` to group predictors via `stats::hclust()` their pairwise correlation matrix.
- Streamlined the package documentation using roxygen methods to inherit sections and parameters.
- Removed `dplyr` as a dependency.
- Added `mgcv`, `rpart`, and `ranger` to Imports.
- All warnings in data validation functions have been converted to messages, which now indicate the function that generated them. This helps with debugging and ensures that messages and warnings are printed in the correct order.



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

  + The previous version started removing predictors on a backwards fashion, from the last predictor in preference order, moving up one by one to the top. Under the wrong circumstances (low number of predictors, low cor_max, and high correlation between first and second predictors in preference order) this configuration would lead to keep only the first predictor, even when having others comply with the cor_max restriction lower down in the preference order. The new version produces smaller subsets of predictors with a higher diversity.

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
