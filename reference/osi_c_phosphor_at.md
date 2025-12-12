# Calculate the phosphorus availability index in Austria

This function calculates the phosphorus availability.

## Usage

``` r
osi_c_phosphor_at(B_LU, A_P_CAL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_P_CAL:

  (numeric) The exchangeable P-content of the soil measured via Calcium
  Ammonium Lactate (mg P/ kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus availability index in Austria estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_c_phosphor_at(B_LU = '3301000000', A_P_CAL = 47)
#> [1] 0.8180216
```
