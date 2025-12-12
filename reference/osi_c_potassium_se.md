# Calculate the potassium availability index in Sweden

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_se(B_LU, A_K_AL, unitcheck = TRUE)
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

The potassium availability index in Sweden derived from extractable soil
K fractions. A numeric value.

## Examples

``` r
osi_c_potassium_se(B_LU = '3301061299',A_K_AL = 5)
#> [1] 1.314358e-10
osi_c_potassium_se(B_LU =  c('3301061299','3301000000'),A_K_AL = c(3.5,5.5))
#> [1] 1.773801e-11 2.492160e-10
```
