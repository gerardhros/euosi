# Calculate the phosphorus excess index in Switzerland

This function calculates the phosphorus excess

## Usage

``` r
osi_nut_p_ch(A_P_AAA, B_LU = NA_character_, unitcheck = TRUE)
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

The phosphorus excess index in Switzerland estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_nut_p_ch(B_LU = 'testcrop1',A_P_AAA = 50)
#> [1] 0.8736304
```
