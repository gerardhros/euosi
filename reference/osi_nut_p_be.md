# Calculate the phosphorus excess index in Belgium

This function calculates the phosphorus excess index

## Usage

``` r
osi_nut_p_be(B_LU, A_P_AL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_P_AL:

  (numeric) The exchangeable P-content of the soil measured via ammonium
  lactate extraction

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus excess index in Belgium estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_nut_p_be(B_LU = '8410', A_P_AL = 45)
#> [1] 0.984932
```
