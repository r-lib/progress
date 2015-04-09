
# progress

[![Linux Build Status](https://travis-ci.org/gaborcsardi/progress.png?branch=master)](https://travis-ci.org/gaborcsardi/rcorpora)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/v7g1703b9f90ymvy)](https://ci.appveyor.com/project/gaborcsardi/progress)

> Progress bar in your R terminal

An R package to show ASCII progress bars. Heavily influenced by
the https://github.com/tj/node-progress JavaScript project.

## Installation

```r
devtools::install_github("gaborcsardi/progress")
```

## Usage

Use the `progress_bar` R6 class:

```r
pb <- progress_bar$new(total = 100)
for (i in 1:100) {
  pb$tick()
  Sys.sleep(1 / 100)
}
```

```
[==========================================================-------------]  81%
```

Custom format, with estimated time of completion:

```r
pb <- progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = 100, clear = FALSE, width= 60)
for (i in 1:100) {
  pb$tick()
  Sys.sleep(1 / 100)
}
```

```
  downloading [========----------------------]  28% eta:  1s
```

With elapsed time:

```r
pb <- progress_bar$new(
  format = "  downloading [:bar] :percent in :elapsed",
  total = 100, clear = FALSE, width= 60)
for (i in 1:100) {
  pb$tick()
  Sys.sleep(1 / 100)
}
```

```
  downloading [==========================------]  80% in  1s
```

With custom tokens:

```r
pb <- progress_bar$new(
  format = "  downloading :what [:bar] :percent eta: :eta",
  clear = FALSE, total = 200, width = 60)
f <- function() {
  for (i in 1:100) {
    pb$tick(tokens = list(what = "foo   "))
    Sys.sleep(2 / 100)
  }
  for (i in 1:100) {
    pb$tick(tokens = list(what = "foobar"))
    Sys.sleep(2 / 100)
  }
}
f()
```

```
  downloading foo    [======------------------]  27% eta:  4s
```

It can show download rates for files with unknown sizes:

```r
pb <- progress_bar$new(
  format = "  downloading foobar at :rate, got :bytes in :elapsed",
  clear = FALSE, total = 1e7, width = 60)
f <- function() {
  for (i in 1:100) {
    pb$tick(sample(1:100 * 1000, 1))
    Sys.sleep(2/100)
  }
  pb$tick(1e7)
  invisible()
}
f()
```

```
  downloading foobar at 5.42 MB/s, got 15.45 MB in  3s
```

See the manual for details and other options.

## License

MIT
