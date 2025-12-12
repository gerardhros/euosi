# Calculate the pH index for soils in Germany

This function calculates the soil pH.

## Usage

``` r
osi_c_ph_de(
  B_LU,
  A_SOM_LOI,
  A_C_OF,
  A_CLAY_MI,
  A_SAND_MI,
  A_PH_CC,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- A_C_OF:

  (numeric) The carbon content of the soil layer (g/ kg)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_PH_CC:

  (numeric) The soil acidity measures with CaCl2 method.

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The pH index in Germany derived for grassland and arable crops. A
numeric value.
