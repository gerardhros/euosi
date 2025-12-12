# Calculate the phosphate excess index in Portugal

This function calculates the phosphate excess.

## Usage

``` r
osi_nut_p_pt(B_LU, A_P_OL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg P/kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate availability index in Portugal derived from extractable
soil P fractions. A numeric value.

## Examples

``` r
osi_nut_p_pt(B_LU = '3301061299',A_P_OL = 5)
#> [1] 0.9653914
osi_nut_p_pt(B_LU = c('3301061299','3301000000'),A_P_OL = c(3.5,5.5))
#> [1] 0.9698392 0.9637747
```
