# install.packages("exampletestr")

#create tests for all functions
exampletestr::make_tests_shells_pkg()

#TEST FILES
# test-collinear.R DONE
# test-cor_df.R DONE
# test-cor_matrix.R DONE
# test-cor_select.R DONE
# test-cramer_v.R DONE
# test-identify.R DONE
# test-preference_order.R DONE
# test-target_encoding_lab.R DONE
# test-target_encoding_methods.R DONE
# test-validate.R DONE
# test-vif_df.R DONE
# test-vif_select.R DONE

testthat::test_package("collinear")
