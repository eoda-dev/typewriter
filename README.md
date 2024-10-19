
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rdantic

<!-- badges: start -->

[![R-CMD-check](https://github.com/eodaGmbH/rdantic/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eodaGmbH/rdantic/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of rdantic is to add type safety to your R code.

## Installation

You can install the development version of rdantic from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eodaGmbH/rdantic")
```

## Examples

``` r
library(rdantic)
library(rlang)

# ---
# Models
numbers <- base_model(
  a = is_integer,
  b = is_integer
)

numbers(a = 2L, b = 4L)
#> $a
#> [1] 2
#> 
#> $b
#> [1] 4
```

``` r
try(numbers(a = 2L, b = 4.5))
#> Error in numbers(a = 2L, b = 4.5) : Type check failed.
#> ✖ b = 4.5
#> ℹ type: double
#> ℹ length: 1
#> ✖ function (x, n = NULL) { .Call(ffi_is_integer, x, n) }
```

``` r

# ---
# Functions
f <- function(
    a = is_integer,
    b = model_field(is_integer, default = 10L)
) {
  check_args()
  a + b
}

f(5L)
#> [1] 15
```

``` r
f(5L, 15L)
#> [1] 20
```

``` r
try(f(5, 5))
#> Error in base_model(fields)(.x = e) : Type check failed.
#> ✖ a = 5
#> ℹ type: double
#> ℹ length: 1
#> ✖ function (x, n = NULL) { .Call(ffi_is_integer, x, n) }
```

``` r

# ---
# Settings
postgres_settings <- base_settings(
  username = as.character,
  password = as.character,
  port = as.integer,
  .prefix = "POSTGRES"
)

Sys.setenv(POSTGRES_USERNAME = "postgres")
Sys.setenv(POSTGRES_PASSWORD = "superSecret!")
Sys.setenv(POSTGRES_PORT = 15432)

postgres_settings()
#> $username
#> [1] "postgres"
#> 
#> $password
#> [1] "superSecret!"
#> 
#> $port
#> [1] 15432
```

``` r

Sys.setenv(POSTGRES_PORT = "")

try(postgres_settings())
#> Error in raise_type_check_error(env_var_name, .obj[[k]], as_type) : 
#>   konnte Funktion "raise_type_check_error" nicht finden
```
