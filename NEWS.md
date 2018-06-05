# driftR 1.1.0.9000

## Major Changes
* `dr_drop` now includes options to drop by date and (optionally) time as well as by using an expression to identify certain values
* `dr_replace` added to give the option to replace values that are measurement errors with `NA` based on date and (optionally) time as well as by using an expression to identify certain values
* The `format` argument for `dr_factor` has been deprecated - date formatting is now detected automatically by the function
* `dr_read` added to correctly format data fron YSI Sonde 6600 and EXO products, as well as Onset HOBO sensors. The new function contains an argument to specify the instrument, so `dr_readSonde` has been deprecated.

# driftR 1.0.0

* CRAN release version

# driftR 0.2.2

* Correct documentation vignettes and package website for technical details
* Change plot on README to chloride
* Add `keepDateTime` argument to `dr_factor()`

# driftR 0.2.1

* Complete documentation vignettes and package website.

# driftR 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Ground up re-write of the functions to allow for non-standard evaluation and tidy evaluation from `dplyr` and `rlang`.
* Functions now support piping.

