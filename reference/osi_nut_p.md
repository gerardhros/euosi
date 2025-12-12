# Calculate the phoshporus excesss index (wrapper function)

This function calculates the P excess index for all European countries
(if available).

## Usage

``` r
osi_nut_p(
  B_LU,
  B_SOILTYPE_AGR = NA_character_,
  A_CLAY_MI = NA_real_,
  A_SAND_MI = NA_real_,
  A_C_OF = NA_real_,
  A_SOM_LOI = NA_real_,
  A_PH_WA = NA_real_,
  A_PH_CC = NA_real_,
  A_CACO3_IF = NA_real_,
  A_P_OL = NA_real_,
  A_P_M3 = NA_real_,
  A_P_CAL = NA_real_,
  A_P_AAA = NA_real_,
  A_P_DL = NA_real_,
  A_P_AL = NA_real_,
  A_P_CC = NA_real_,
  A_P_WA = NA_real_,
  B_COUNTRY,
  pwarning = FALSE
)
```

## Arguments

- B_LU:

  (character) The crop code

- B_SOILTYPE_AGR:

  (character) The soil type in a particular region

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

- A_SOM_LOI:

  (numeric) The organic matter content of the soil (%)

- A_PH_WA:

  (numeric) The pH measured in water.

- A_PH_CC:

  (numeric) The pH measured in CaCl2 extraction.

- A_CACO3_IF:

  (numeric) the percentage of CaCO3 (%)

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg P / kg)

- A_P_M3:

  (numeric) The exchangeable P-content of the soil measured via Mehlich
  3 extracton (mg P/ kg)

- A_P_CAL:

  (numeric) The exchangeable P-content of the soil measured via Calcium
  Ammonium Lactate (mg P/ kg)

- A_P_AAA:

  (numeric) The exchangeable P-content of the soil measured via acid
  ammonium acetate extraction (mg P / kg)

- A_P_DL:

  (numeric) The P-content of the soil extracted with double lactate (mg
  P / kg)

- A_P_AL:

  (numeric) The P-content of the soil extracted with ammonium lactate
  (mg P / kg)

- A_P_CC:

  (numeric) The P-content of the soil extracted with CaCl2 (mg P / kg)

- A_P_WA:

  (numeric) The P-content of the soil extracted with water (mg P / kg)

- B_COUNTRY:

  (character) The country code

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The capacity of the soil to supply and buffer boron, evaluated given an
optimum threshold for yield. A numeric value.
