# Calculate the Cu availability index for France

This function calculates the Cu availability of a soil, using the
agronomic index used in France

## Usage

``` r
osi_c_copper_fr(B_LU, A_CLAY_MI, B_CF, A_SOM_LOI, A_CU_EDTA)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_CLAY_MI:

  (numeric) The clay content (%)

- B_CF:

  (numeric) The percentage of coarse fragments (%)

- A_SOM_LOI:

  (numeric) The organic matter content (%)

- A_CU_EDTA:

  (numeric) The plant available content of Cu in the soil (mg Cu per kg)
  extracted by EDTA

## Value

The copper availability index in France estimated from extractable
copper, clay, coarse fragments and soil organic matter, A numeric value.
