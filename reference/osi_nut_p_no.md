# Calculate the phosphorus excess index in Norway

This function calculates the phosphorus excess.

## Usage

``` r
osi_nut_p_no(A_P_AL, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_P_AL:

  (numeric) The exchangeable P-content of the soil measured via Ammonium
  Lactate (mg P/ kg)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus excess index in Norway estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_nut_p_no(B_LU = 'testcrop1', A_P_AL = 50)
#> [1] 0.866994
```
