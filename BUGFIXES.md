# Bug Fixes

## Issue #16 — `preference_order()` shows metric "custom" for namespace-qualified built-ins

**Root cause:** `R/preference_order.R:252` captures the function name via
`deparse(substitute(f))`. When the user writes `f = collinear::f_count_gam`,
this returns `"collinear::f_count_gam"` instead of `"f_count_gam"`. That string
is stored as the `name` attribute on `f` via `validate_arg_f()`. Later, at line
501, the code checks whether that name appears in the `f_df` lookup table (which
only contains unqualified names), the check fails, and `metric` is set to
`"custom"`.

**Fix:** Strip the namespace prefix before passing the name to `validate_arg_f()`:

```r
# R/preference_order.R ~line 252
f_name = sub(".*::", "", deparse(substitute(f)))
```

**Files modified:** `R/preference_order.R`
**Test added:** `tests/testthat/test-preference_order.R` — calls `preference_order()` with `f = collinear::f_count_gam` and asserts `metric != "custom"`.

---

## Issue #17 — `model_formula()` crashes on `sf` spatial data frames

**Root cause:** `R/model_formula.R:111-114` calls only `validate_arg_df_not_null()`
(which only checks for NULL), skipping the geometry-stripping step performed by
`drop_geometry_column()` in the full `validate_arg_df()`. Downstream,
`identify_numeric_variables()` treats the geometry list-column as numeric, and
`identify_zero_variance_variables()` then calls `is.finite()` on a list-column,
throwing `"default method not implemented for type 'list'"`.

**Fix:** Add a `drop_geometry_column()` call immediately after the NULL check in
`model_formula()`:

```r
# R/model_formula.R after validate_arg_df_not_null()
df <- drop_geometry_column(
  df = df,
  quiet = quiet,
  function_name = function_name
)
```

**Files modified:** `R/model_formula.R`
**Test added:** `tests/testthat/test-model_formula.R` — constructs a minimal fake-sf object (with `sf_column` attribute and a list-column named `geometry`) and verifies `model_formula()` returns a valid formula instead of erroring.

---

## Issue #18 — NA values in response variable crash `preference_order()` functions

**Root cause:** `validate_arg_df.R:292-297` converts `Inf`, `-Inf`, and `NaN` in
numeric columns to `NA` but does not drop the affected rows. When the response
column contains NA (e.g., from converted `Inf` values), the paired `(y, x)`
data frame passed into `f_*` functions may contain NA in `y`, causing failures
inside those functions even when they call `na.omit()` internally (because the
response vector's class invariants — e.g., being a valid binary integer vector —
can be violated by the NA rows).

**Fix:** Wrap the `data.frame(y, x)` construction in `stats::na.omit()` at the
call site in `preference_order()`:

```r
# R/preference_order.R ~lines 459-462
out <- f.response(
  df = stats::na.omit(data.frame(
    y = df[[response]],
    x = df[[x]]
  )),
  ...
)
```

**Files modified:** `R/preference_order.R`
**Test added:** `tests/testthat/test-preference_order.R` — sets `Inf` in several rows of the response column, calls `preference_order()`, and verifies it returns a data frame without error.

---

## Issue #19 — `score_auc()` crashes when observations or predictions contain NA

**Root cause:** `R/score_auc.R:49-52` computes `ones <- p[o == 1]` and
`zeros <- p[o == 0]` without first removing NA pairs. When `o` or `p` contain
NA, `o == 1` produces NA elements, which propagate into `ones`/`zeros`. The
subsequent `if (sum(ones) == 0)` guard then receives NA instead of TRUE/FALSE,
causing the condition to evaluate incorrectly or error.

**Fix:** Strip incomplete cases at the start of the function body:

```r
# R/score_auc.R before line ~49
complete <- !is.na(o) & !is.na(p)
o <- o[complete]
p <- p[complete]
```

**Files modified:** `R/score_auc.R`
**Test added:** `tests/testthat/test-score_auc.R` — three new assertions: NA in `o` only, NA in `p` only, and NA in both; each verifies the function returns a numeric value instead of erroring.
