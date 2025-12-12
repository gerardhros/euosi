# Calculate the magnesium availability index in Latvia

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_lv(
  A_MG_DL,
  B_TEXTURE_USDA,
  B_LU = NA_character_,
  unitcheck = TRUE
)
```

## Arguments

- A_MG_DL:

  (numeric) The exchangeable Mg-content of the soil measured via Double
  Lactate extraction (mg Mg/ kg)

- B_TEXTURE_USDA:

  (character) The soil texture according to USDA classification system

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Latvia estimated from extractable
magnesium. A numeric value.

## Examples

``` r
osi_c_magnesium_lv(B_LU = 'testcrop',A_MG_DL = 45,B_TEXTURE_USDA='sand')
#> [1] 0.5415835
```
