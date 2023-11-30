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
