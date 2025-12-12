# Calculate the magnesium availability index in Hungary

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_hu(
  B_TEXTURE_USDA,
  A_MG_KCL,
  B_LU = NA_character_,
  unitcheck = TRUE
)
```

## Arguments

- B_TEXTURE_USDA:

  (character) The soil texture according to USDA classification system

- A_MG_KCL:

  (numeric) The exchangeable Mg-content of the soil measured via KCL
  extracton (mg Mg/ kg)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Hungary estimated from extractable
magnesium. A numeric value.

## Examples

``` r
osi_c_magnesium_hu(B_LU = 'testcrop',A_MG_KCL = 45,B_TEXTURE_USDA = 'loam')
#> [1] 0.9574156
```
