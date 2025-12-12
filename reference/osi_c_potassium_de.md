# Calculate the potassium availability index in Germany

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_de(
  B_LU,
  A_C_OF,
  A_CLAY_MI,
  A_SAND_MI,
  A_K_CAL,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_C_OF:

  (numeric) The carbon content of the soil layer (g C/ kg)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_K_CAL:

  (numeric) The potassium content extracted with CAL (mg K / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in Germany derived from extractable
soil K fractions. A numeric value.
