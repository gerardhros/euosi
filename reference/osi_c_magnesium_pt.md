# Calculate the magnesium availability index in Portugal

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_pt(B_LU, A_MG_AAA, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_MG_AAA:

  (numeric) The exchangeable Mg-content of the soil measured via
  ammonium acetate extraction (mg Mg / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Portugal estimated from the Mg
extracted soil pool. A numeric value.

## Examples

``` r
osi_c_magnesium_pt(B_LU = '3301010901',A_MG_AAA = 45)
#> [1] 0.5646872
```
