# datplot 1.1.0
* Using either the original calculation (weights) or calculation of 
year-wise probability is now an option in `datsteps()` with the 
argument `calc = "weight"` or `calc = "probability"`
* There is now an option to calculate the cumulative probability in 
`datsteps()` with the argument `cumulative = TRUE`. This only works with 
probability calculation instead of the original (weights) calculation.
* Significantly improved the efficiency of `datsteps()`.
* Change and improve error-handling of `scaleweight()`. 
* Remove UTF-8 characters from data and other files to comply with CRAN. 
* Update documentation and add a [pkgdown-site](https://lsteinmann.github.io/datplot/).


# datplot 1.0.1

* Change calculation in `get.weights()` to `1 / (abs(DAT_min - DAT_max) + 1)` 
to get real probability values for each year. This only has a real effect when 
using a stepsize of 1, as it makes the weight-values 
usable as "dating probability".
* Clean up `calculate.outputrows()` and `scaleweight()` somewhat.

# datplot 1.0.0

* Added a `NEWS.md` file to track changes to the package
* some style corrections
* First release for submission to CRAN, accepted -> datplot is now on CRAN

---

# datplot 0.2.4

* peer-review version for Advances in Archaeological Practice
