# Calculate the phosphate availability index in Denmark

This function calculates the phosphate availability.

## Usage

``` r
osi_c_phosphor_dk(B_LU, A_P_OL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg/kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate availability index in Denmark derived from extractable
soil P fractions. A numeric value.

## Examples

``` r
osi_c_phosphor_dk(B_LU = '3301010399',A_P_OL = 5)
#> [1] 7.695877e-80
osi_c_phosphor_dk(B_LU = c('3301010399','3301029800'),A_P_OL = c(3.5,5.5))
#> [1] 1.541574e-112  1.001905e-70
```
