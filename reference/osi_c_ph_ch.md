# Calculate the pH index for Switzerland

This function evaluates the pH index in Switzerland

## Usage

``` r
osi_c_ph_ch(
  B_LU,
  A_CLAY_MI = NA_real_,
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

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

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

The pH index in Switzerland estimated from the base saturation with Ca,
Mg and K. For soils with pH water below 5.9 the soil is extracted with
HCL+H2SO4. Soils with higher pH are extracted with Bariumchloride.
