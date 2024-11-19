
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rdantic

<!-- badges: start -->

[![R-CMD-check](https://github.com/eoda-dev/rdantic/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eoda-dev/rdantic/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of rdantic is to add type safety to your R code.

## Installation

You can install the development version of rdantic from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eoda-dev/rdantic")
```

## Examples

``` r
library(rdantic)
library(rlang)

# Models
my_model <- base_model(
  a = is_integer,
  b = is_integer
)

(m <- my_model(a = 2L, b = 4L))
#> $a
#> [1] 2
#> 
#> $b
#> [1] 4
```

``` r

try(m$a <- 10.5)
#> Error in check_assignment(x, name, value) : Type check failed.
#> {
#>     .Call(ffi_is_integer, x, n)
#> }
```

``` r

try(my_model(a = 2L, b = 4.5))
#> Error in my_model(a = 2L, b = 4.5) : Type check(s) failed
#> # ---
#> Type check failed for 'b'
#> value:  num 4.5
#> type: double
#> length: 1
#> {
#>     .Call(ffi_is_integer, x, n)
#> }
```

``` r

# Functions
f <- function(a, b = 5L) {
  check_args(
    a = is_integer,
    b = is_scalar_integer
  )
  a + b
}

f(5L)
#> [1] 10
```

``` r
try(f(5L, c(3L, 4L)))
#> Error in base_model(fields)(.x = e) : Type check(s) failed
#> # ---
#> Type check failed for 'b'
#> value:  int [1:2] 3 4
#> type: integer
#> length: 2
#> {
#>     .Call(ffi_is_integer, x, 1L)
#> }
```

``` r

# Data frames
df <- data.frame(
  id = 1:3,
  letter = letters[1:3]
)

my_model <- base_model(
  id = is_integer,
  letter = is_character
)

model_validate(df, my_model)
#>   id letter
#> 1  1      a
#> 2  2      b
#> 3  3      c
```

``` r

df$id <- as.double(df$id)
try(df, model_validate(my_model))
#> Error in model_fn(.x = obj) : Type check(s) failed
#> # ---
#> Type check failed for 'id'
#> value:  num [1:3] 1 2 3
#> type: double
#> length: 3
#> {
#>     .Call(ffi_is_integer, x, n)
#> }
```
