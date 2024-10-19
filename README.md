
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
my_model <- base_model(
  a = is_integer,
  b = is_integer
)

my_model(a = 2L, b = 4L)
#> $a
#> [1] 2
#> 
#> $b
#> [1] 4
```

``` r
try(my_model(a = 2L, b = 4.5))
#> Error in my_model(a = 2L, b = 4.5) : Type check failed.
#> ! field: b, type: double, length: 1
#> ✖ b = 4.5
#> ✖ function (x, n = NULL) { .Call(ffi_is_integer, x, n) }
```

``` r

# ---
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
#> Error in base_model(fields)(.x = e) : Type check failed.
#> ! field: b, type: integer, length: 2
#> ✖ b = 3:4
#> ✖ function (x) { .Call(ffi_is_integer, x, 1L) }
```

``` r

# ---
# Data frames
df <- data.frame(
  id = 1:3L,
  letter = letters[1:3]
)

my_model <- base_model(
  id = is_integer,
  letter = is_character
)

df |> model_validate(my_model)
#>   id letter
#> 1  1      a
#> 2  2      b
#> 3  3      c
```

``` r

df$id <- as.double(df$id)
try(df |> model_validate(my_model))
#> Error in model_fn(.x = obj) : Type check failed.
#> ! field: id, type: double, length: 3
#> ✖ id = c(1, 2, 3)
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
