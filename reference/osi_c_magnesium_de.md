# Calculate the magnesium availability index in Germany

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_de(
  B_LU,
  A_C_OF,
  A_CLAY_MI,
  A_SAND_MI,
  A_MG_CC,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_C_OF:

  (numeric) The carbon content of the soil layer (g/ kg)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_MG_CC:

  (numeric) The magnesium content extracted with CaCl2 (g / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Germany derived from extractable
soil Mg fractions. A numeric value.
