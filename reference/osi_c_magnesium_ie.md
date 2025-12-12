# Calculate the magnesium availability index in Ireland

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_ie(B_LU, A_SOM_LOI, A_MG_NaAAA, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- A_MG_NaAAA:

  (numeric) The Mg-content of the soil extracted with Morgan's solution,
  sodium acetate acetic acid (mg/ kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Ireland derived from extractable
soil Mg fractions. A numeric value.

## Examples

``` r
osi_c_magnesium_ie(B_LU = 'testcrop',A_SOM_LOI = 2,A_MG_NaAAA = 5)
#> [1] 0.061685
osi_c_magnesium_ie(B_LU = c('testcrop','testcrop2'),A_SOM_LOI = c(2,4),A_MG_NaAAA = c(3.5,5.5))
#> [1] 0.04482424 0.06334950
```
