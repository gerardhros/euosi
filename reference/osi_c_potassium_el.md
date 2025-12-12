# Calculate the potassium availability index in Greece

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_el(B_LU, A_K_AAA, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_K_AAA:

  (numeric) The K-content of the soil extracted with ammonium acetate
  (mg K/kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in Greece derived from extractable soil
K fractions. A numeric value.

## Examples

``` r
osi_c_potassium_el(B_LU = '3301061299',A_K_AAA = 50)
#> [1] 0.09934931
osi_c_potassium_el(B_LU = c('3301061299','3301000000'),A_K_AAA = c(5,15))
#> [1] 0.05621976 0.06380696
```
