# Calculate the potassium excess index index in Portugal

This function calculates the potassium excess index.

## Usage

``` r
osi_nut_k_pt(B_LU, A_K_AAA, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_K_AAA:

  (numeric) The K-content of the soil extracted with acid ammonium
  acetate (mg K / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in Portugal derived from extractable
soil K fractions. A numeric value.

## Examples

``` r
osi_nut_k_pt(B_LU = '3301061299',A_K_AAA = 50)
#> [1] 0.8949124
osi_nut_k_pt(B_LU =  c('3301061299','3301000000'),A_K_AAA = c(35,55))
#> [1] 0.9321845 0.8791031
```
