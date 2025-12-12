# Calculate the pH index for Portugal

This function evaluates the pH index in Portugal

## Usage

``` r
osi_c_ph_pt(
  B_LU,
  A_CEC_CO = NA_real_,
  A_PH_WA = NA_real_,
  A_CA_CO_PO = NA_real_,
  A_MG_CO_PO = NA_real_,
  A_K_CO_PO = NA_real_,
  A_NA_CO_PO = NA_real_,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (character) The crop type

- A_CEC_CO:

  (numeric) The cation exchange capacity of the soil (mmol+ / kg),
  analyzed via Cobalt-hexamine extraction

- A_PH_WA:

  (numeric) The pH measured in h2o

- A_CA_CO_PO:

  (numeric) The calcium occupation of the CEC (%)

- A_MG_CO_PO:

  (numeric) The magnesium occupation of the CEC (%)

- A_K_CO_PO:

  (numeric) The potassium occupation of the CEC (%)

- A_NA_CO_PO:

  (numeric) The sodium occupation of the CEC (%)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The pH index in Portugal estimated from the base saturation with Ca, Mg
and K. For soils with pH water below 5.9 the soil is extracted with
HCL+H2SO4. Soils with higher pH are extracted with Bariumchloride.
