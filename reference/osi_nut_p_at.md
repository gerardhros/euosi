# Calculate the phosphorus excess index in Austria

This function calculates the phosphorus excess

## Usage

``` r
osi_nut_p_at(A_P_CAL, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_P_CAL:

  (numeric) The exchangeable P-content of the soil measured via Calcium
  Ammonium Lactate (mg P/ kg)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus excess index in Austria estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_nut_p_at(B_LU = '3301000000', A_P_CAL = 47)
#> [1] 0.8786775
```
