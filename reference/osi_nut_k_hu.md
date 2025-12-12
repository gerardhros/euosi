# Calculate the potassium excess index in Hungary

This function calculates the potassium excess.

## Usage

``` r
osi_nut_k_hu(
  A_SOM_LOI,
  A_CLAY_MI,
  A_CACO3_IF,
  A_K_AL,
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

- A_K_AL:

  (numeric) The exchangeable K-content of the soil measured via Ammonium
  Lactate extracton (mg K/ kg)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium excess index in Hungary estimated from extractable
potassium. A numeric value.

## Examples

``` r
osi_nut_k_hu(B_LU = 'testcrop',A_K_AL = 45,A_CACO3_IF = 5,A_CLAY_MI = 5,A_SOM_LOI = 5)
#> [1] 0.9695123
```
