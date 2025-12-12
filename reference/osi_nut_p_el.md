# Calculate the phosphate excess index in Greece

This function calculates the phosphate excess index.

## Usage

``` r
osi_nut_p_el(B_LU, A_P_OL, unitcheck = TRUE)
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
osi_nut_p_el(B_LU = '3301061299',A_P_OL = 5)
#> [1] 0.9220071
osi_nut_p_el(B_LU = c('3301061299','3301000000'),A_P_OL = c(5,15))
#> [1] 0.9220071 0.8726970
```
