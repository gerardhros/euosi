# Calculate the B availability index for agricultural soils in the Netherlands

This function calculates the B availability of a soil, using the
agronomic index used in France

## Usage

``` r
osi_c_boron_nl(B_LU, A_CLAY_MI, A_SOM_LOI, A_B_HW, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop type

- A_CLAY_MI:

  (numeric) The clay content (%)

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil

- A_B_HW:

  (numeric) The plant available content of B in the soil (mg B per kg)
  extracted by hot water

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The boron availability index in the Netherlands estimated from hot water
extractable boron, and clay, A numeric value.
