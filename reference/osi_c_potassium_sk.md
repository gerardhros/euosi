# Calculate the potassium availability index in Slovak Republic

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_sk(B_LU, B_TEXTURE_HYPRES, A_K_M3, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- A_K_M3:

  (numeric) The exchangeable K-content of the soil measured via Mehlich
  3 extracton (mg K/ kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in Slovak Republic estimated from
extractable potassium. A numeric value.

## Examples

``` r
osi_c_potassium_sk(B_LU = '3301010901', A_K_M3 = 45,B_TEXTURE_HYPRES='C')
#> [1] 1.209043e-07
```
