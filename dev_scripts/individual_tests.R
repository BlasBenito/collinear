library(testthat)

#all code examples
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

testthat::test_file("tests/testthat/test-preference_order_methods.R")

testthat::test_file("tests/testthat/test-target_encoding_lab.R")

testthat::test_file("tests/testthat/test-target_encoding_methods.R")

testthat::test_file("tests/testthat/test-cor_clusters.R")

#helper functions
testthat::test_file("tests/testthat/test-auc.R")

testthat::test_file("tests/testthat/test-case_weights.R")

testthat::test_file("tests/testthat/test-cramer_v.R")

testthat::test_file("tests/testthat/test-identify.R")

testthat::test_file("tests/testthat/test-validate.R")

