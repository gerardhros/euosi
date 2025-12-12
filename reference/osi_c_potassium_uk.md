# Calculate the potassium availability index in United Kingdom

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_uk(B_LU, A_SOM_LOI, A_K_AN, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil

- A_K_AN:

  (numeric) The K-content of the soil extracted with ammonium nitrate
  (mg K /kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in United Kingdom derived from
extractable soil K fractions. A numeric value.

## Examples

``` r
osi_c_potassium_uk(B_LU = 'testcrop1',A_SOM_LOI=3,A_K_AN = 50)
#> [1] 0.3874347
osi_c_potassium_uk(B_LU = c('testcrop1','testcrop2'),A_SOM_LOI = c(3,5),A_K_AN = c(35,55))
#> [1] 0.2160057 0.4058592
```
