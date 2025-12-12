# Calculate the magnesium availability index in Austria

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_at(B_LU, A_MG_CC, B_TEXTURE_HYPRES, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_MG_CC:

  (numeric) The exchangeable Mg-content of the soil measured via Calcium
  Chloride (mg Mg/ kg)

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Austria estimated from extractable
magnesium. A numeric value.

## Examples

``` r
osi_c_magnesium_at(B_LU = '3301000000',A_MG_CC = 47,B_TEXTURE_HYPRES = 'C')
#> [1] 0.9641011
```
