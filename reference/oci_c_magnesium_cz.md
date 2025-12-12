# Calculate the magnesium availability index in Czech Republic

This function calculates the magnesium availability.

## Usage

``` r
oci_c_magnesium_cz(
  A_MG_M3,
  B_TEXTURE_HYPRES,
  B_LU = NA_character_,
  unitcheck = TRUE
)
```

## Arguments

- A_MG_M3:

  (numeric) The exchangeable Mg-content of the soil measured via Mehlich
  3 extracton (mg Mg/ kg)

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Czech Republic estimated from
extractable magnesium. A numeric value.

## Examples

``` r
oci_c_magnesium_cz(B_LU = '3301000000',A_MG_M3 = 81,B_TEXTURE_HYPRES='C')
#> [1] 0.1288058
```
