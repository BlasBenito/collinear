export _R_CHECK_CRAN_INCOMING_=TRUE
export _R_CHECK_FORCE_SUGGESTS_=TRUE



#enforcing NO CRAN to avoid running tests
NOT_CRAN="" R CMD check --as-cran --timings /home/blas/Dropbox/blas/GITHUB/R_packages/collinear_3.0.0.tar.gz

#example timings
R CMD check --as-cran /home/blas/Dropbox/blas/GITHUB/R_packages/collinear_3.0.0.tar.gz

read.table(
  "R/collinear.Rcheck/collinear-Ex.timings",
  header = TRUE
)
