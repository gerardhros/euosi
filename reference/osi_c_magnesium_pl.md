# Calculate the magnesium availability index in Poland

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_pl(
  A_MG_CC,
  B_TEXTURE_HYPRES,
  B_LU = NA_character_,
  unitcheck = TRUE
)
```

## Arguments

- A_MG_CC:

  (numeric) The exchangeable Mg-content of the soil measured via calcium
  chloride extracton (mg Mg/ kg)

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Poland estimated from extractable
magnesium. A numeric value.

## Examples

``` r
osi_c_magnesium_pl(A_MG_CC = 45,B_TEXTURE_HYPRES='C')
#> [1] 0.9998774
```
