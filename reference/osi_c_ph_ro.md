# Calculate the pH index in Romenia

This function calculates the pH index

## Usage

``` r
osi_c_ph_ro(B_LU, A_PH_WA, A_NA_CO_PO, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_PH_WA:

  (numeric) The pH measured in water

- A_NA_CO_PO:

  (numeric) The sodium occupation of the CEC (%)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The pH index in Romenia. A numeric value.

## Examples

``` r
osi_c_ph_ro(B_LU = 'testcrop1',A_PH_WA = 5,A_NA_CO_PO=3)
#> [1] 0.5988482
osi_c_ph_ro(B_LU = c('testcrop1','testcrop2'),A_PH_WA = c(3.5,5.5),A_NA_CO_PO=c(1,3))
#> [1] 5.118595e-08 8.739327e-01
```
