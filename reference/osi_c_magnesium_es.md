# Calculate the magnesium availability index in Spain

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_es(B_LU, A_MG_CO_PO, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_MG_CO_PO:

  (numeric) The exchangeable Mg-content of the soil measured via Cohex
  extracton, percentage occupation at CEC (%)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Spain estimated from the magnesium
occupation at CEC. A numeric value.

## Examples

``` r
osi_c_magnesium_es(B_LU = '3301000000' , A_MG_CO_PO = 4.5)
#> [1] 0.3196515
```
