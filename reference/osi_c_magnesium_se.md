# Calculate the magnesium availability index in Sweden

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_se(B_LU, A_MG_AL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_MG_AL:

  (numeric) The exchangeable Mg-content of the soil measured via
  ammonium lactate (mg K/kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Sweden estimated from the Mg
extracted soil pool. A numeric value.

## Examples

``` r
osi_c_magnesium_se(B_LU = '3301010901',A_MG_AL = 45)
#> [1] 1
```
