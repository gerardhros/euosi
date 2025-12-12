# Calculate the magnesium availability index in Slovak Republic

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_sk(B_LU, B_TEXTURE_HYPRES, A_MG_M3, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- A_MG_M3:

  (numeric) The exchangeable Mg-content of the soil measured via Mehlich
  3 extracton (mg Mg/ kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Slovak Republic estimated from
extractable magnesium. A numeric value.

## Examples

``` r
osi_c_magnesium_sk(B_LU ='3301010901',A_MG_M3 = 45,B_TEXTURE_HYPRES='C')
#> [1] 9.396087e-06
```
