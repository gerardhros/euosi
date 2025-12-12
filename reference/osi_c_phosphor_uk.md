# Calculate the phosphate availability index in United Kingdom

This function calculates the phosphate availability.

## Usage

``` r
osi_c_phosphor_uk(B_LU, A_SOM_LOI, A_P_OL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg P /kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate availability index in United Kingdom derived from
extractable soil P fractions. A numeric value.

## Examples

``` r
osi_c_phosphor_uk(B_LU = '3301000000',A_SOM_LOI = 3,A_P_OL = 5)
#> [1] 0.2179032
osi_c_phosphor_uk(B_LU = c('3301000000','3301010102'),A_SOM_LOI = c(3,4),A_P_OL = c(3.5,5.5))
#> [1] 0.1231208 0.2423314
```
