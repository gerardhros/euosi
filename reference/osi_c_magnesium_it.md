# Calculate the magnesium availability index in Italy

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_it(B_LU, A_MG_CO_PO, A_K_CO_PO, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_MG_CO_PO:

  (numeric) The exchangeable Mg-content of the soil measured via Cohex
  extracton, percentage occupation at CEC (%)

- A_K_CO_PO:

  (numeric) The exchangeable K-content of the soil measured via Cohex
  extracton, percentage occupation at CEC (%)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Italy estimated from the Mg
extracted soil pool, expressed as occupation of the CEC. A numeric
value.

## Examples

``` r
osi_c_magnesium_it(B_LU = '3301000000',A_MG_CO_PO = 4.5,A_K_CO_PO = 3)
#> [1] 0.5806308
```
