# Calculate the phosphate excess index in Denmark

This function calculates the phosphate excess

## Usage

``` r
osi_nut_p_dk(B_LU, A_P_OL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg/kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate excess index in Denmark derived from extractable soil P
fractions. A numeric value.

## Examples

``` r
osi_nut_p_dk(B_LU = '3301010399',A_P_OL = 5)
#> [1] 0.9378507
osi_nut_p_dk(B_LU = c('3301010399','3301029800'),A_P_OL = c(3.5,5.5))
#> [1] 0.9399749 0.9371266
```
