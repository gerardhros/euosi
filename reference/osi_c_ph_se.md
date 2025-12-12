# Calculate the pH index in Sweden

This function calculates the pH index

## Usage

``` r
osi_c_ph_se(B_LU, A_SOM_LOI, A_CLAY_MI, A_PH_WA, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_SOM_LOI:

  (numeric) The organic matter content of soil in percentage

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_PH_WA:

  (numeric) The pH measured in water

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The pH index in Sweden. A numeric value.

## Examples

``` r
osi_c_ph_se(B_LU = '3301010901',A_PH_WA = 5,A_CLAY_MI = 15, A_SOM_LOI = 2)
#> [1] 4.914867e-70
osi_c_ph_se(B_LU = c('3301010901','3304990000'),A_PH_WA = c(3.5,5.5),
A_CLAY_MI = c(5,15),A_SOM_LOI = c(2,4))
#> [1] 2.967452e-18 1.959678e-04
```
