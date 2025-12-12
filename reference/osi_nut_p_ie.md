# Calculate the phosphate excess index in Ireland

This function calculates the phosphate excess.

## Usage

``` r
osi_nut_p_ie(B_LU, A_P_OL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg/kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate excess index in Ireland derived from extractable soil P
fractions. A numeric value.

## Examples

``` r
osi_nut_p_ie(B_LU = 'testcrop1',A_P_OL = 5)
#> [1] 0.9525676
osi_nut_p_ie(B_LU = c('testcrop1','testcrop2'),A_P_OL = c(3.5,5.5))
#> [1] 0.9605365 0.9495921
```
