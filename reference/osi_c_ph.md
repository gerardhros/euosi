# Calculate the pH index (wrapper function)

This function calculates the pH index for all European countries (if
available).

## Usage

``` r
osi_c_ph(
  B_LU,
  B_SOILTYPE_AGR = NA_character_,
  A_CLAY_MI = NA_real_,
  A_SAND_MI = NA_real_,
  A_SOM_LOI = NA_real_,
  A_C_OF = NA_real_,
  A_CEC_CO = NA_real_,
  A_CA_CO_PO = NA_real_,
  A_MG_CO_PO = NA_real_,
  A_K_CO_PO = NA_real_,
  A_NA_CO_PO = NA_real_,
  A_PH_WA = NA_real_,
  A_PH_CC = NA_real_,
  A_PH_KCL = NA_real_,
  B_COUNTRY,
  pwarning = FALSE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- B_SOILTYPE_AGR:

  (character) The agricultural type of soil

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_SOM_LOI:

  (numeric) The organic matter content of the soil (%)

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

- A_CEC_CO:

  (numeric) The cation exchange capacity of the soil (mmol+ / kg),
  analyzed via Cobalt-hexamine extraction

- A_CA_CO_PO:

  (numeric) The calcium occupation of the CEC (%)

- A_MG_CO_PO:

  (numeric) The magnesium occupation of the CEC (%)

- A_K_CO_PO:

  (numeric) The potassium occupation of the CEC (%)

- A_NA_CO_PO:

  (numeric) The sodium occupation of the CEC (%)

- A_PH_WA:

  (numeric) The pH measured in h2o

- A_PH_CC:

  (numeric) The pH measured in cacl2

- A_PH_KCL:

  (numeric) The pH measured in KCl

- B_COUNTRY:

  (character) The country code

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The index to evaluate the soil pH
