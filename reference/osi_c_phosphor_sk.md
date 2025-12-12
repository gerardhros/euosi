# Calculate the phosphorus availability index in Slovak Republic

This function calculates the phosphorus availability.

## Usage

``` r
osi_c_phosphor_sk(B_LU, B_TEXTURE_HYPRES, A_P_M3, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- A_P_M3:

  (numeric) The exchangeable P-content of the soil measured via Mehlich
  3 extracton (mg P/ kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus availability index in Slovak Republic estimated from
extractable phosphorus. A numeric value.

## Examples

``` r
osi_c_phosphor_sk(B_LU = '3301010901', A_P_M3 = 45,B_TEXTURE_HYPRES = 'C')
#> [1] 0.00632784
```
