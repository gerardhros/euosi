# Calculate the potassium availability index in Ireland

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_ie(B_LU, A_SOM_LOI, A_K_NaAAA, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- A_K_NaAAA:

  (numeric) The K-content of the soil extracted with Morgan's solution,
  sodium acetate acetic acid (mg/ kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in Ireland derived from extractable
soil K fractions. A numeric value.

## Examples

``` r
osi_c_potassium_ie(B_LU = 'testcrop',A_SOM_LOI = 2,A_K_NaAAA = 5)
#> [1] 0.04549616
osi_c_potassium_ie(B_LU = c('testcrop','testcrop2'),A_SOM_LOI = c(2,4),A_K_NaAAA = c(3.5,5.5))
#> [1] 0.04055972 0.04595615
```
