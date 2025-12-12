# Calculate the phosphorus availability index in Switzerland

This function calculates the phosphorus availability.

## Usage

``` r
osi_c_phosphor_ch(A_P_AAA, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_P_AAA:

  (numeric) The exchangeable P-content of the soil measured via acid
  ammonium acetate extraction (mg P/ kg)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus availability index in Switzerland estimated from
extractable phosphorus. A numeric value.

## Examples

``` r
osi_c_phosphor_ch(B_LU = '3301000000',A_P_AAA = 50)
#> [1] 0.9787502
```
