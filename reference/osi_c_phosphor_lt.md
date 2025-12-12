# Calculate the phosphorus availability index in Lithuania

This function calculates the phosphorus availability.

## Usage

``` r
osi_c_phosphor_lt(A_P_AL, A_SOM_LOI, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_P_AL:

  (numeric) The exchangeable P-content of the soil measured via Ammonium
  Lactate extraction (mg P/ kg)

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus availability index in Lithuania estimated from
extractable phosphorus. A numeric value.

## Examples

``` r
osi_c_phosphor_lt(B_LU = '3301000000', A_P_AL = 45,A_SOM_LOI = 4.5)
#> [1] 0.8476033
```
