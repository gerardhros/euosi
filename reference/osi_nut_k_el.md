# Calculate the potassium excess index in Greece

This function calculates the potassium excess index.

## Usage

``` r
osi_nut_k_el(B_LU, A_K_AAA, unitcheck = TRUE)
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

The potassium excess index in Greece derived from extractable soil K
fractions. A numeric value.

## Examples

``` r
osi_nut_k_el(B_LU = '3301061299',A_K_AAA = 50)
#> [1] 0.974211
osi_nut_k_el(B_LU = c('3301061299','3301000000'),A_K_AAA = c(50,150))
#> [1] 0.9742110 0.9529806
```
