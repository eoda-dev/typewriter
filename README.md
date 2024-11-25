
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

# Functions
f <- function(a = "integer", b = "integer:1") {
  check_args()
  a + b
}

f(5L, 3L)
#> [1] 8
```

``` r
try(f(5L, c(3L, 4L)))
#> Error in base_model(fields)(.x = func_env) : Type check(s) failed
#> # ---
#> Type check failed for 'b'
#> value:  int [1:2] 3 4
#> type: integer
#> class: integer
#> length: 2
#> expected: {
#>     typeof(x) == "integer" & length(x) == 1L
#> }
```

``` r

# Data frames
df <- data.frame(
  id = 1:3,
  letter = letters[1:3]
)

my_model <- base_model(
  id = "integer",
  letter = "character"
)

model_validate(df, my_model)
#>   id letter
#> 1  1      a
#> 2  2      b
#> 3  3      c
```

``` r

df$id <- as.double(df$id)
try(model_validate(df, my_model))
#> Error in model_fn(.x = obj) : Type check(s) failed
#> # ---
#> Type check failed for 'id'
#> value:  num [1:3] 1 2 3
#> type: double
#> class: numeric
#> length: 3
#> expected: {
#>     typeof(x) == "integer"
#> }
```

``` r

# Models
my_model <- base_model(
  a = "integer",
  b = "integer"
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
#>     typeof(x) == "integer"
#> }
```

``` r

try(my_model(a = 2L, b = 4.5))
#> Error in my_model(a = 2L, b = 4.5) : Type check(s) failed
#> # ---
#> Type check failed for 'b'
#> value:  num 4.5
#> type: double
#> class: numeric
#> length: 1
#> expected: {
#>     typeof(x) == "integer"
#> }
```
