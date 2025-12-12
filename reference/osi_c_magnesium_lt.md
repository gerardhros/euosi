# Calculate the magnesium availability index in Lithuania

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_lt(A_MG_AL, A_PH_KCL, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_MG_AL:

  (numeric) The exchangeable Mg-content of the soil measured via
  Ammonium Lactate extraction (mg Mg/ kg)

- A_PH_KCL:

  (numeric) The pH measured in KCl

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Lithuania estimated from extractable
magnesium. A numeric value.

## Examples

``` r
osi_c_magnesium_lt(A_MG_AL = 45,A_PH_KCL= 5.5)
#> [1] 0.06044065
```
