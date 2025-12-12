# Calculate the potassium availability index in Italy

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_it(B_LU, B_TEXTURE_HYPRES, A_K_AAA, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- A_K_AAA:

  (numeric) The K-content of the soil extracted with ammoninium acetate
  (mg K/kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in Italy derived from extractable soil
K fractions. A numeric value.

## Examples

``` r
osi_c_potassium_it(B_LU = '3301000000',A_K_AAA = 5,B_TEXTURE_HYPRES='C')
#> [1] 3.936603e-21
osi_c_potassium_it(B_LU = c('3301000000','3301010101'),
A_K_AAA = c(3.5,5.5),B_TEXTURE_HYPRES=c('C','C'))
#> [1] 4.634969e-22 7.990989e-21
```
