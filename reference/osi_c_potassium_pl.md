# Calculate the potassium availability index in Poland

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_pl(
  A_K_DL,
  B_TEXTURE_HYPRES,
  B_LU = NA_character_,
  unitcheck = TRUE
)
```

## Arguments

- A_K_DL:

  (numeric) The exchangeable K-content of the soil measured via ammonium
  double lactate extracton (mg K/ kg)

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in Poland estimated from extractable
potassium. A numeric value.

## Examples

``` r
osi_c_potassium_pl(B_LU = 'testcrop1',A_K_DL = 45,B_TEXTURE_HYPRES='C')
#> [1] 0.6756913
```
