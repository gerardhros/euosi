# Calculate the magnesium availability index in United Kingdom

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_uk(B_LU, A_SOM_LOI, A_MG_AN, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil

- A_MG_AN:

  (numeric) The Mg-content of the soil extracted with ammonium nitrate
  (mg Mg /kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in United Kingdom derived from
extractable soil Mg fractions. A numeric value.

## Examples

``` r
osi_c_magnesium_uk(B_LU = 'testcrop1',A_SOM_LOI=3,A_MG_AN = 50)
#> [1] 0.9038364
osi_c_magnesium_uk(B_LU = c('testcrop1','testcrop2'),A_SOM_LOI = c(3,5),A_MG_AN = c(35,55))
#> [1] 0.7304390 0.9133458
```
