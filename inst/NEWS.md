
# 1.1.2.9000

* Add `finished` variable, which can be queried to see if a progress bar is
  finished. @jimhester.

* Add a `terminate()` method, which can be called to explicitly remove the
  progress bar, @jimhester.

* Outputs greater than the bar width are automatically trimmed, @jimhester.

* Add a `message()` method to print a message line(s) above the progress bar,
  @jimhester.

* :elapsedfull token: elapsed time in hh:mm:ss format.

# 1.1.2

* Do not run tests on CRAN, Solaris does not have microbenchmark

# 1.1.1

* Move README.md, so that it is not parsed on CRAN
* Use https URLs instead of http, whenever possible

# 1.1.0

* Support for the `:spin` token which adds a simple ASCII spinner, @richfitz
* Respect custom token width for calculation of the bar width, @mllg

# 1.0.2

* Fix the C++ API on Windows, and on older compilers in general.

# 1.0.1

First version on CRAN.
