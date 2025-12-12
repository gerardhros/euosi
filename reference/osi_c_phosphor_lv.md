# Calculate the phosphorus availability index in Latvia

This function calculates the phosphorus availability.

## Usage

``` r
osi_c_phosphor_lv(
  A_P_DL,
  B_TEXTURE_USDA,
  B_LU = NA_character_,
  unitcheck = TRUE
)
```

## Arguments

- A_P_DL:

  (numeric) The exchangeable P-content of the soil measured via Double
  Lactate extraction (mg P/ kg)

- B_TEXTURE_USDA:

  (character) The soil texture according to USDA classification system

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus availability index in Latvia estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_c_phosphor_lv(B_LU ='3301000000',A_P_DL = 45,B_TEXTURE_USDA = 'Sa')
#> [1] 0.9951527
```
