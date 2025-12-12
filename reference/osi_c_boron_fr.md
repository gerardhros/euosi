# Calculate the B availability index for agricultural soils in France

This function calculates the B availability of a soil, using the
agronomic index used in France

## Usage

``` r
osi_c_boron_fr(B_LU, A_CLAY_MI, A_B_HW, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop type

- A_CLAY_MI:

  (numeric) The clay content (%)

- A_B_HW:

  (numeric) The plant available content of B in the soil (mg B per kg)
  extracted by hot water

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The boron availability index in France estimated from extractable boron,
clay, A numeric value.
