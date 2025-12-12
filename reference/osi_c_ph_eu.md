# Calculate the pH index in Europe when no country specific evaluation is present

This function calculates the pH index

## Usage

``` r
osi_c_ph_eu(B_LU, A_PH_CC, A_SOM_LOI, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_PH_CC:

  (numeric) The pH measured in 0.01M CaCl2

- A_SOM_LOI:

  (numeric) The organic matter content of soil in percentage

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The pH index in UK A numeric value.

## Examples

``` r
osi_c_ph_eu(B_LU = 'testcrop1',A_PH_CC = 5, A_SOM_LOI = 4)
#> [1] 0.5780705
osi_c_ph_eu(B_LU = c('testcrop1','testcrop2'),A_PH_CC = c(3.5,5.5), A_SOM_LOI = c(3.5,4))
#> [1] 3.801563e-08 8.482046e-01
```
