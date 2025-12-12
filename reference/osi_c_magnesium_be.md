# Calculate the magnesium availability index in Belgium

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_be(B_LU, A_MG_CC, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_MG_CC:

  (numeric) The exchangeable Mg-content of the soil measured via calcium
  chloride extracton (mg Mg/ kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Belgium estimated from extractable
magnesium. A numeric value.

## Examples

``` r
osi_c_magnesium_be(B_LU = '8410',A_MG_CC = 45)
#> [1] 0.9882919
```
