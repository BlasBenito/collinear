# collinear 1.0.2

Two bugs fixed in the variable selection algorithm in cor_select():

- When only one variable was left in the correlation matrix, the one column matrix became a vector with no colnames, which yielded an error. Now, to avoid this issue, drop = FALSE is used in the matrix subsetting.

- The previous version started removing predictors on a backwards fashion, from the last predictor in preference order, moving up one by one to the top. Under the wrong circumstances (low number of predictors, low cor_max, and high correlation between first and second predictors in preference order) this configuration would lead to keep only the first predictor, even when having others comply with the max_cor restriction lower down in the preference order. The new version produces smaller subsets of predictors with a higher diversity.

One bug fixed in cor_df():

- The data frame returned pairs of the same variable when cor_method was "spearman". Fixed with a dplyr::filter(x != y).


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
