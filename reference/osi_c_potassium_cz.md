# Calculate the potassium availability index in Czech Republic

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_cz(
  A_K_M3,
  B_TEXTURE_HYPRES,
  B_LU = NA_character_,
  unitcheck = TRUE
)
```

## Arguments

- A_K_M3:

  (numeric) The exchangeable K-content of the soil measured via Mehlich
  3 extracton (mg K/ kg)

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in Czech Republic estimated from
extractable potassium. A numeric value.

## Examples

``` r
osi_c_potassium_cz(B_LU = '3301000000',A_K_M3 = 81,B_TEXTURE_HYPRES='C')
#> [1] 0.004524202
```
