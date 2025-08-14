outer <- function(f) {
  f_expr <- substitute(f)
  inner(f_expr)
}

inner <- function(f_expr) {
  f_name <- deparse(f_expr)
  cat("Function name:", f_name, "\n")

  f_fun <- eval(f_expr, envir = parent.frame())
  cat("Result:", f_fun(1:5), "\n")
}

outer(mean)
