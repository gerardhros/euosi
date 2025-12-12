# Calculate the phosphate excess index in United Kingdom

This function calculates the phosphate excess.

## Usage

``` r
osi_nut_p_uk(B_LU, A_SOM_LOI, A_P_OL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg P /kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate excess index in United Kingdom derived from extractable
soil P fractions. A numeric value.

## Examples

``` r
osi_nut_p_uk(B_LU = 'testcrop1',A_SOM_LOI = 3,A_P_OL = 5)
#> [1] 0.9204884
osi_nut_p_uk(B_LU = c('testcrop1','testcrop2'),A_SOM_LOI = c(3,4),A_P_OL = c(3.5,5.5))
#> [1] 0.9261702 0.9192389
```
