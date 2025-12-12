# Calculate the potassium availability index in Austria

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_at(B_LU, A_K_CAL, B_TEXTURE_HYPRES, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_K_CAL:

  (numeric) The exchangeable K-content of the soil measured via Calcium
  Ammonium Lactate (mg K/ kg)

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in Austria estimated from extractable
potassium. A numeric value.

## Examples

``` r
osi_c_potassium_at(B_LU = '3301000000',A_K_CAL = 47,B_TEXTURE_HYPRES = 'C')
#> [1] 0.0696102
```
