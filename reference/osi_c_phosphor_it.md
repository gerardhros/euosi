# Calculate the phosphate availability index in Italy

This function calculates the phosphate availability.

## Usage

``` r
osi_c_phosphor_it(B_LU, A_P_OL, B_TEXTURE_HYPRES, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg/kg)

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate availability index in Italy derived from extractable soil
P fractions. A numeric value.

## Examples

``` r
osi_c_phosphor_it(B_LU = '3301061299',A_P_OL = 5,B_TEXTURE_HYPRES ='C')
#> [1] 0.4449934
osi_c_phosphor_it(B_LU = c('3301061299','3301000000'),
A_P_OL = c(3.5,5.5),B_TEXTURE_HYPRES =c('C','C'))
#> [1] 0.2799838 0.4994822
```
