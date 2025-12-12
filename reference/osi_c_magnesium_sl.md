# Calculate the magnesium availability index in Slovenia

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_sl(
  A_MG_AL,
  B_TEXTURE_HYPRES,
  B_LU = NA_character_,
  unitcheck = TRUE
)
```

## Arguments

- A_MG_AL:

  (numeric) The exchangeable Mg-content of the soil measured via
  ammonium lactate extracton (mg Mg/ kg)

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Slovenia estimated from extractable
magnesium. A numeric value.

## Examples

``` r
osi_c_magnesium_sl(A_MG_AL = 45,B_TEXTURE_HYPRES='C')
#> [1] 0.5687549
```
