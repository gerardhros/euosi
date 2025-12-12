# Calculate the phosphate excess index in Germany

This function calculates the phosphate excess

## Usage

``` r
osi_nut_p_de(
  B_LU,
  A_SOM_LOI,
  A_P_CAL = NA_real_,
  A_P_DL = NA_real_,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_SOM_LOI:

  (numeric) The organic matter content of the soil (%)

- A_P_CAL:

  (numeric) The P-content of the soil extracted with ammonium lactate(mg
  P / kg)

- A_P_DL:

  (numeric) The P-content of the soil extracted with double lactate (mg
  P / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate excess index in Germany stimated from extractable soil P
fractions. A numeric value.
