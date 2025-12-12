# Calculate the phosphorus availability index in Slovenia

This function calculates the phosphorus availability.

## Usage

``` r
osi_c_phosphor_sl(A_P_AL, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_P_AL:

  (numeric) The exchangeable P-content of the soil measured via
  ammoniuml lactate extracton (mg P/ kg)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus availability index in Slovenia estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_c_phosphor_sl(B_LU = '3301000000',A_P_AL = 45)
#> [1] 0.6135327
```
