# Calculate the phosphorus availability index in Hungary

This function calculates the phosphorus availability.

## Usage

``` r
osi_c_phosphor_hu(
  A_SOM_LOI,
  A_CLAY_MI,
  A_CACO3_IF,
  A_P_AL,
  B_LU = NA_character_,
  unitcheck = TRUE
)
```

## Arguments

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_CACO3_IF:

  (numeric) the percentage of CaCO3 (%)

- A_P_AL:

  (numeric) The exchangeable P-content of the soil measured via Ammonium
  Lactate extracton (mg P/ kg)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus availability index in Hungary estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_c_phosphor_hu(B_LU = '3301000000',A_P_AL = 45,A_CACO3_IF = 5,A_CLAY_MI = 5,A_SOM_LOI = 5)
#> [1] 0.06579038
```
