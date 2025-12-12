# Calculate the phosphorus availability index in Czech Republic

This function calculates the phosphorus availability.

## Usage

``` r
osi_c_phosphor_cz(B_LU, A_P_M3, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_P_M3:

  (numeric) The exchangeable P-content of the soil measured via Mehlich
  3 extracton (mg P/ kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus availability index in Czech Republic estimated from
extractable phosphorus. A numeric value.

## Examples

``` r
osi_c_phosphor_cz(B_LU = '3303030100', A_P_M3 = 81)
#> [1] 0.7924082
```
