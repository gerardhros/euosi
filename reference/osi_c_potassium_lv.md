# Calculate the potassium availability index in Latvia

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_lv(
  A_K_DL,
  B_TEXTURE_USDA,
  B_LU = NA_character_,
  unitcheck = TRUE
)
```

## Arguments

- A_K_DL:

  (numeric) The exchangeable K-content of the soil measured via Double
  Lactate extraction (mg K/ kg)

- B_TEXTURE_USDA:

  (character) The soil texture according to USDA classification system

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in Latvia estimated from extractable
potassium A numeric value.

## Examples

``` r
osi_c_potassium_lv(B_LU = 'testcrop',A_K_DL = 45,B_TEXTURE_USDA='Sa')
#> [1] 0.7846594
```
