# Calculate the potassium excess index in Belgium

This function calculates the potassium excess.

## Usage

``` r
osi_nut_k_be(B_LU, B_TEXTURE_BE, A_K_AAA = NA_real_, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- B_TEXTURE_BE:

  (character) The soil texture according to Belgium classification
  system

- A_K_AAA:

  (numeric) The exchangeable K-content of the soil measured via ammonium
  acetate extraction

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium excess index in Belgium estimated from extractable
potassium. A numeric value.

## Examples

``` r
osi_nut_k_be(B_LU = '8410',B_TEXTURE_BE ='S', A_K_AAA = 45)
#> [1] 0.9664633
```
