library(testthat)
library(devtools)
library(collinear)

testthat::test_file("tests/testthat/test-code_examples.R")

#main functions

testthat::test_file("tests/testthat/test-collinear.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 56 ]

testthat::test_file("tests/testthat/test-cor_select.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 25 ]

testthat::test_file("tests/testthat/test-cor_df.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 17 ]

testthat::test_file("tests/testthat/test-cor_matrix.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 7 ]

testthat::test_file("tests/testthat/test-vif.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 7 ]

testthat::test_file("tests/testthat/test-vif_df.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 21 ]

testthat::test_file("tests/testthat/test-vif_select.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 25 ]

#secondary functions

testthat::test_file("tests/testthat/test-preference_order.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 34 ]

testthat::test_file("tests/testthat/test-preference_order_methods.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 17 ]

testthat::test_file("tests/testthat/test-target_encoding_lab.R")
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 7 ]

testthat::test_file("tests/testthat/test-target_encoding_methods.R")

testthat::test_file("tests/testthat/test-cor_clusters.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 7 ]

#helper functions
testthat::test_file("tests/testthat/test-auc.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ]

testthat::test_file("tests/testthat/test-case_weights.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3 ]

testthat::test_file("tests/testthat/test-cramer_v.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3 ]

testthat::test_file("tests/testthat/test-identify.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 8 ]

testthat::test_file("tests/testthat/test-validate_arg_df.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 10 ]

testthat::test_file("tests/testthat/test-validate_arg_predictors.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 13 ]

testthat::test_file("tests/testthat/test-validate_arg_predictors_vif.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 17 ]

testthat::test_file("tests/testthat/test-validate_arg_predictors_cor.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 15 ]

testthat::test_file("tests/testthat/test-validate_arg_response.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 10 ]

testthat::test_file("tests/testthat/test-validate_arg_preference_order.R")
#[ FAIL 0 | WARN 0 | SKIP 0 | PASS 8 ]

