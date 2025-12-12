# Calculate the phosphorus availability index in Belgium

This function calculates the phosphorus availability.

## Usage

``` r
osi_c_phosphor_be(B_LU, A_P_AL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_P_AL:

  (numeric) The exchangeable P-content of the soil measured via ammonium
  lactate extraction (mg P/ kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus availability index in Belgium estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_c_phosphor_be(B_LU = '8410', A_P_AL = 45)
#> [1] 2.822428e-05
```
