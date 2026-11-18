## New submission

This is a patch release (v3.0.3) addressing two bug fixes.

### Bug fixes

Fixed bug in `preference_order()` that printed the message "collinear::preference_order()
└── collinear::validate_arg_responses(): argument 'response' must be of length 1, using response:" when providing several responses. This message was only triggered when `validate_arg_df()` removed a column (usual when `df` is an sf data frame).

Fixed bug that would let the argument `m` (precomputed correlation matrix introduced via `...`) partial match with `max_cor` or `max_vif`. The functions `validate_arg_max_cor()` and `validate_arg_max_vif()` now prevent this.

## Test environments

* macOS 14 (local), R 4.4.x
* Ubuntu 22.04 (GitHub Actions), R devel, release, oldrel-1
* Windows Server 2022 (GitHub Actions), R release
* win-builder (devel and release)

## R CMD check results

── R CMD check results ──────────────────────────────────────────────────────────────── collinear 3.0.3 ────
Duration: 2m 39s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## Test results

══ Results ════════════════════════
Duration: 144.6 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 818 ]
