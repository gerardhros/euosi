# Calculate the phosphorus excess index in Czech Republic

This function calculates the phosphorus excess index

## Usage

``` r
osi_nut_p_cz(A_P_M3, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_P_M3:

  (numeric) The exchangeable P-content of the soil measured via Mehlich
  3 extracton (mg P/ kg)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus excess index in Czech Republic estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_nut_p_cz(B_LU = '3303030100', A_P_M3 = 81)
#> [1] 0.9187715
```
