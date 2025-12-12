# Calculate the distance for target for soil pH in view of the crop production function

This functions evaluates the difference between the measured pH and the
optimal pH according to the Bemestingsadvies

## Usage

``` r
osi_c_ph_nl(
  ID,
  B_LU,
  B_SOILTYPE_AGR,
  A_SOM_LOI,
  A_CLAY_MI,
  A_PH_CC,
  unitcheck = TRUE
)
```

## Arguments

- ID:

  (character) A field id

- B_LU:

  (numeric) The crop code

- B_SOILTYPE_AGR:

  (character) The agricultural type of soil

- A_SOM_LOI:

  (numeric) The organic matter content of soil in percentage

- A_CLAY_MI:

  (numeric) The percentage of clay (%)

- A_PH_CC:

  (numeric) The pH-CaCl2 of the soil

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The osi indicator for the soil pH

## References

Handboek Bodem en Bemesting tabel 5.1, 5.2 en 5.3
