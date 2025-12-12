# Calculate the phosphate availability index in Spain

This function calculates the phosphate availability.

## Usage

``` r
osi_c_phosphor_es(B_LU, A_CLAY_MI, A_SAND_MI, A_P_OL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg/kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate availability index in Spain derived from extractable soil
P fractions. A numeric value.

## Examples

``` r
osi_c_phosphor_es(B_LU = '3301010301',A_CLAY_MI = 5,A_SAND_MI = 25,A_P_OL = 5)
#> [1] 0.04139369
osi_c_phosphor_es(B_LU = c('3301010901','3301010500'),A_CLAY_MI = c(5,10),
A_SAND_MI = c(50,50),A_P_OL = c(3.5,5.5))
#> [1] 0.005759387 0.067060321
```
