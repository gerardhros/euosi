# Calculate the pH index in United Kingdom

This function calculates the pH index

## Usage

``` r
osi_c_ph_uk(B_LU, A_PH_WA, A_SOM_LOI, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_PH_WA:

  (numeric) The pH measured in water

- A_SOM_LOI:

  (numeric) The organic matter content of soil in percentage

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The pH index in UK A numeric value.

## Examples

``` r
osi_c_ph_uk(B_LU = 'testcrop1',A_PH_WA = 5, A_SOM_LOI = 4)
#> [1] 6.117766e-12
osi_c_ph_uk(B_LU = c('testcrop1','testcrop2'),A_PH_WA = c(3.5,5.5), A_SOM_LOI = c(3.5,4))
#> [1] 5.757637e-31 1.097182e-05
```
