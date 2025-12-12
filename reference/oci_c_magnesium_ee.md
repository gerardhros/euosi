# Calculate the magnesium availability index in Estonia

This function calculates the magnesium availability.

## Usage

``` r
oci_c_magnesium_ee(
  A_MG_M3,
  B_TEXTURE_USDA,
  B_LU = NA_character_,
  unitcheck = TRUE
)
```

## Arguments

- A_MG_M3:

  (numeric) The exchangeable Mg-content of the soil measured via Mehlich
  3 extracton (mg Mg/ kg)

- B_TEXTURE_USDA:

  (character) The soil texture according to USDA classification system

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Estonia estimated from extractable
magnesium. A numeric value.

## Examples

``` r
oci_c_magnesium_ee(B_LU = 'testcrop',A_MG_M3 = 45,B_TEXTURE_USDA = 'clay')
#> [1] 0.001582745
```
