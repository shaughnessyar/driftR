# driftR v1.1.0.9000

### major changes
* `dr_drop` now includes options to drop by date and (optionally) time as well as by using an expression to identify certain values
* `dr_replace` added to give the option to replace values that are measurement errors with `NA` based on date and (optionally) time as well as by using an expression to identify certain values
* The `format` argument for `dr_factor` has been deprecated - date formatting is now detected automatically by the function

# driftR v1.0.0

* CRAN release version

# driftR v0.2.2

* Correct documentation vignettes and package website for technical details
* Change plot on README to chloride
* Add `keepDateTime` argument to `dr_factor()`

# driftR v0.2.1

* Complete documentation vignettes and package website.

# driftR v0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Ground up re-write of the functions to allow for non-standard evaluation and tidy evaluation from `dplyr` and `rlang`.
* Functions now support piping.

