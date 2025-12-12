# Calculate the potassium excess index in Estonia

This function calculates the potassium excess.

## Usage

``` r
osi_nut_k_ee(A_K_M3, B_TEXTURE_USDA, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_K_M3:

  (numeric) The exchangeable K-content of the soil measured via Mehlich
  3 extracton (mg P/ kg)

- B_TEXTURE_USDA:

  (character) The soil texture according to USDA classification system

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium excess index in Estonia estimated from extractable
potassium. A numeric value.

## Examples

``` r
osi_nut_k_ee(B_LU = 'testcrop',A_K_M3 = 45,B_TEXTURE_USDA = 'clay')
#> [1] NA
```
