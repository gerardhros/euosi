# Calculate the phosphate availability index in Greece

This function calculates the phosphate availability.

## Usage

``` r
osi_c_phosphor_el(B_LU, A_P_OL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg P/kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate availability index in Greece derived from extractable soil
P fractions. A numeric value.

## Examples

``` r
osi_c_phosphor_el(B_LU = '3301061299',A_P_OL = 5)
#> [1] 0.001051181
osi_c_phosphor_el(B_LU = c('3301061299','3301000000'),A_P_OL = c(5,15))
#> [1] 0.001051181 0.706512742
```
