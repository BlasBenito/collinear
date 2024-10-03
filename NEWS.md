# collinear 1.2.0

Function vif_select() now has a much better selection algorithm that preserves more predictors with higher preference.

The functions cor_select(), vif_select() and vif_df() now skip the analysis if only one predictor. This may happen in collinear() with highly correlated datasets, when cor_select() only returns one predictor and sends it to vif_select().

Streamlined cor_df(), and fixed a bug that prevented cor_numerics_vs_categorics() and cor_categorics_vs_categorics() to trigger properly.

Removed dplyr as dependency.

Added mgcv, rpart, and ranger to Imports

Parallelized cor_numerics_vs_categorical() and cor_categorical_vs_categorical()

All warnings in all data validation functions are now messages to ensure they are printed in the correct order.

Function vif_df() now has the internal function vif_f() to compute the vif data frame, and this function is applied twice, once without modifying the correlation matrix, and if this fails, again by replacing 1 and -1 with 0.999 and -0.999 in the correlation matrix to try overcome the "singular matrix" issue.

The function validate_df() now takes into account the number of predictors as reference, along with min_rows, to warn the user about potential issues in the multicollinearity analysis due to the data frame dimensions.

The function preference_order() no longer has the `workers` argument, but can accept a parallelization setup via `future::plan()` and a progress bar via de `progressr` package.

New function `validate_preference_order()` added to streamline `cor_select()` and `vif_select()`.

The "rnorm" method in target_encoding_lab() was deprecated, and the function target_encoding_rnorm() removed from the package.

Streamlined documentation using roxygen methods to inherit sections and parameters.

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

  + The previous version started removing predictors on a backwards fashion, from the last predictor in preference order, moving up one by one to the top. Under the wrong circumstances (low number of predictors, low cor_max, and high correlation between first and second predictors in preference order) this configuration would lead to keep only the first predictor, even when having others comply with the max_cor restriction lower down in the preference order. The new version produces smaller subsets of predictors with a higher diversity.

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
