# Calculate the phosphorus availability index in Poland

This function calculates the phosphorus availability.

## Usage

``` r
osi_c_phosphor_pl(A_P_DL, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_P_DL:

  (numeric) The exchangeable P-content of the soil measured via ammonium
  double lactate extracton (mg P/ kg)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus availability index in Poland estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_c_phosphor_pl(B_LU = '3301000000',A_P_DL = 45)
#> [1] 0.8024909
```
