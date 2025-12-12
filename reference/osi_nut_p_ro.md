# Calculate the phosphate excess index index in Romenia

This function calculates the phosphate excess index.

## Usage

``` r
osi_nut_p_ro(B_LU, A_P_AL, unitcheck = TRUE)
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
osi_c_phosphor_ro(B_LU = 'testcrop1',A_P_AL = 5)
#> [1] 0.2051349
osi_c_phosphor_ro(B_LU = c('testcrop1','testcrop2'),A_P_AL = c(3.5,5.5))
#> [1] 0.1434697 0.2276597
```
