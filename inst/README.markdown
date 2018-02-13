
<h1 align="center">
    <br>
    <br>
    <img width="400" src="./inst/logo.png" alt="progress">
    <br>
    <br>
    <br>
</h1>

[![Linux Build Status](https://travis-ci.org/r-lib/progress.svg?branch=master)](https://travis-ci.org/r-lib/progress)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/r-lib/progress?svg=true)](https://ci.appveyor.com/project/gaborcsardi/progress)
[![](https://www.r-pkg.org/badges/version/progress)](https://r-pkg.org/pkg/progress)

> Progress bar in your R terminal

An R package to show ASCII progress bars. Heavily influenced by
the https://github.com/tj/node-progress JavaScript project.

## Installation

```r
devtools::install_github("r-lib/progress")
```

## Usage

Use the `progress_bar` R6 class:

```r
library(progress)
pb <- progress_bar$new(total = 100)
for (i in 1:100) {
  pb$tick()
  Sys.sleep(1 / 100)
}
```

```
[==========================================================-------------]  81%
```

The progress bar is displayed after the first `tick` command.
This might not be desirable for long computations, because
nothing is shown before the first tick. It is good practice to
call `tick(0)` at the beginning of the computation or download,
which shows the progress bar immediately.

```r
pb <- progress_bar$new(total = 100)
f <- function() {
  pb$tick(0)
  Sys.sleep(3)
  for (i in 1:100) {
    pb$tick()
    Sys.sleep(1 / 100)
  }
}
f()
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

```r
pb <- progress_bar$new(
  format = "  downloading [:bar] :elapsedfull",
  total = 1000, clear = FALSE, width= 60)
for (i in 1:1000) {
  pb$tick()
  Sys.sleep(1 / 100)
}
```

```
  downloading [=====================--------------] 00:00:08
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

Progress bars can also digress, by supplying negative values to `tick()`:

```r
pb <- progress_bar$new()
f <- function() {
  pb$tick(50)  ; Sys.sleep(1)
  pb$tick(-20) ; Sys.sleep(1)
  pb$tick(50)  ; Sys.sleep(1)
  pb$tick(-30) ; Sys.sleep(1)
  pb$tick(100)
}
f()
```

See the manual for details and other options.

## Creating a plyr compatible progress bar

It is easy to create progress bars for
[plyr](https://github.com/hadley/plyr):

```r
progress_progress <- function(...) {
  pb <- NULL
  list(
    init = function(x, ...) {
      pb <<- progress_bar$new(total = x, ...)
    },
	step = function() {
      pb$tick()
    },
	term = function() NULL
  )
}
```

You can try it with

```r
plyr::l_ply(
  1:100,
  .fun = function(...) Sys.sleep(0.01),
  .progress = 'progress'
)
```

## C++ API

The package also provides a C++ API, that can be used with or
without Rcpp. See [the example package](inst/progresstest/src/test.cpp) that
is [included](inst/progresstest) within `progress`. Here is a short excerpt
that shows how it works:

```CPP

#include <RProgress.h>

...

RProgress::RProgress pb("Downloading [:bar] ETA: :eta");

  pb.tick(0);
  for (int i = 0; i < 100; i++) {
    usleep(2.0 / 100 * 1000000);
    pb.tick();
  }

...

```

The C++ API has almost the same functionality as the R API, except that it
does not currently support custom tokens, custom streams, and callback functions.

Note that the C++ and the R APIs are independent and for a
single progress bar you need to use either one exclusively.

## License

MIT @ [Gábor Csárdi](https://github.com/gaborcsardi),
      [RStudio Inc](https://github.com/rstudio)
