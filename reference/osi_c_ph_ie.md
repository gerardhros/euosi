# Calculate the pH index in Ireland

This function calculates the pH index

## Usage

``` r
osi_c_ph_ie(B_LU, A_PH_WA, A_SOM_LOI, unitcheck = TRUE)
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

The pH index in Ireland. A numeric value.

## Examples

``` r
osi_c_ph_ie(B_LU = 'testcrop',A_PH_WA = 5, A_SOM_LOI = 3.5)
#> [1] 0.0250764
osi_c_ph_ie(B_LU = c('testcrop1','testcrop2'),A_PH_WA = c(3.5,5.5),A_SOM_LOI = c(3.5,6.5))
#> [1] 7.213695e-33 3.364289e-01
```
