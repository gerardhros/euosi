# Calculate the phosphorus excess index in Latvia

This function calculates the phosphorus excess.

## Usage

``` r
osi_nut_p_lv(A_P_DL, B_TEXTURE_USDA, B_LU = NA_character_, unitcheck = TRUE)
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

The phosphorus excess index in Latvia estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_nut_p_lv(B_LU ='testcrop1',A_P_DL = 45,B_TEXTURE_USDA = 'Sa')
#> [1] 0.735073
```
