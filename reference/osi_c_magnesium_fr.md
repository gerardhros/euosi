# Calculate the Mg availability index for France

This function calculates the Mg availability of a soil, using the
agronomic index used in France

## Usage

``` r
osi_c_magnesium_fr(
  B_LU,
  A_CLAY_MI,
  A_CEC_CO,
  A_CACO3_IF,
  A_MG_AAA,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_CLAY_MI:

  (numeric) Soil clay content (%)

- A_CEC_CO:

  (numeric) The cation exchange capacity of the soil in mmol/kg

- A_CACO3_IF:

  (numeric) The CaCO3 content in the soil (%)

- A_MG_AAA:

  (numeric) The extractable Mg content in the soil (mg/kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in France estimated from extractable
magnesium. A numeric value.
