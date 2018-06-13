# driftR 1.1.0

## Major Changes
* `dr_read` added to correctly format data fron YSI Sonde 6600 and EXO products, as well as Onset HOBO sensors. The new function contains an argument to specify the instrument, so `dr_readSonde` has been deprecated. The `dr_read` function now can reformat variables using the `janitor` package's `clean_names()` function. This is a significant change and may break workflows based on `driftR v1.0` that anticipate particular variable names. The `cleanVar` argument is, however, optional, so setting it to `FALSE` should leave the result compatible with legacy code.
* `dr_drop` now includes options to drop by date and (optionally) time as well as by using an expression to identify certain values
* `dr_replace` added to give the option to replace values that are measurement errors with `NA` based on date and (optionally) time as well as by using an expression to identify certain values
* The `format` argument for `dr_factor` has been deprecated - date formatting is now detected automatically by the function

## Minor Changes
* A redesigned hex logo has been added
* The `README` and package website have been updated to reflect changes to the package
* The vignette on tidy evaluation has been removed
* A vignette on dates and times in `driftR` has been added

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

