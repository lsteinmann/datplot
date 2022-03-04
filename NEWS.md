# datplot 1.0.1

* Change calculation in `get.weights()` to `1 / (abs(DAT_min - DAT_max) + 1)` to get real probability values for each year. This only has a real effect when using a stepsize of 1, as it makes the weight-values usable as "dating probability".
* Clean up `calculate.outputrows()` somewhat.

# datplot 1.0.0

* Added a `NEWS.md` file to track changes to the package
* some style corrections
* First release for submission to CRAN, accepted -> datplot is now on CRAN

---

# datplot 0.2.4

* peer-review version for Advances in Archaeological Practice
