# Calculate thepotassium excess index in Romenia

This function calculates the potassium excess index

## Usage

``` r
osi_nut_k_ro(A_K_AL, B_TEXTURE_HYPRES, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_K_AL:

  (numeric) The exchangeable K-content of the soil measured via ammonium
  lactate extracton (mg K/ kg)

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium excess index in Romenia estimated from extractable
potassium. A numeric value.

## Examples

``` r
osi_nut_k_ro(B_LU = 'testcrop1',A_K_AL = 45,B_TEXTURE_HYPRES='M')
#> [1] 1
```
