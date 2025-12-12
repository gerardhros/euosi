# Calculate the potassium availability index in Denmark

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_dk(B_LU, A_K_AL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_K_AL:

  (numeric) The K-content of the soil extracted with ammonium lactate
  (mg K / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in Denmark derived from extractable
soil K fractions. A numeric value.

## Examples

``` r
osi_c_potassium_dk(B_LU = '3301000000',A_K_AL = 5)
#> [1] 0.0008290623
osi_c_potassium_dk(B_LU = c('3301000000','3301061299'),A_K_AL = c(3.5,5.5))
#> [1] 0.0006050253 0.0009188436
```
