# Calculate the phosphate availability index in Romenia

This function calculates the phosphate availability.

## Usage

``` r
osi_c_phosphor_ro(B_LU, A_P_AL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_P_AL:

  (numeric) The P-content of the soil extracted with Acetate Lactate
  (mg/kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate availability index in Romenia derived from extractable
soil P fractions. A numeric value.

## Examples

``` r
osi_c_phosphor_ro(B_LU = '3301000000',A_P_AL = 5)
#> [1] 0.2051349
osi_c_phosphor_ro(B_LU = c('3301000000','3301010102'),A_P_AL = c(3.5,5.5))
#> [1] 0.1434697 0.2276597
```
