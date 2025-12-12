# Calculate the Open Soil Index score for a single field using input from LUCAS

This functions wraps the functions of the OSI into one main function to
calculate the score for Open Soil Index (OBI) for a location where data
is available from LUCAS (and other open source datasets).

## Usage

``` r
osi_field(
  B_LU,
  B_SOILTYPE_AGR,
  B_COUNTRY,
  B_BGZ = NA_character_,
  B_PREC_SUM = NA_real_,
  B_PREC_WIN = NA_real_,
  B_PET_SUM = NA_real_,
  B_PET_WIN = NA_real_,
  B_TEMP_SUM = NA_real_,
  B_TEMP_WIN = NA_real_,
  B_RUSL_RE = NA_real_,
  B_RUSL_SE = NA_real_,
  B_RUSL_LS = NA_real_,
  B_RUSL_CM = NA_real_,
  A_CLAY_MI = NA_real_,
  A_SAND_MI = NA_real_,
  A_SOM_LOI = NA_real_,
  A_C_OF = NA_real_,
  A_CEC_CO = NA_real_,
  A_PH_CC = NA_real_,
  A_CACO3_IF = NA_real_,
  A_N_RT = NA_real_,
  A_N_PMN = NA_real_,
  A_P_OL = NA_real_,
  A_K_AAA = NA_real_,
  A_MG_AAA = NA_real_,
  A_B_HW = NA_real_,
  A_ZN_CC = NA_real_,
  A_ZN_EDTA = NA_real_,
  A_ZN_RT = NA_real_,
  ID = 1,
  output = "all",
  pwarning = FALSE
)
```

## Arguments

- B_LU:

  (numeric) a series with crop codes given the crop rotation plan
  (source: the BRP)

- B_SOILTYPE_AGR:

  (character) The agricultural type of soil

- B_COUNTRY:

  (character) The country code

- B_BGZ:

  (factor) an European region-id used for carbon analyses

- B_PREC_SUM:

  (numeric) Total potential precipitation in summer (mm)

- B_PREC_WIN:

  (numeric) Total potential precipitation in winter (mm)

- B_PET_SUM:

  (numeric) Total potential evapotranspiration in summer (mm)

- B_PET_WIN:

  (numeric) Total potential evapotranspiration in winter (mm)

- B_TEMP_SUM:

  (numeric) Mean winter temperature (degrees Celcius)

- B_TEMP_WIN:

  (numeric) Mean winter temperature (degrees Celcius)

- B_RUSL_RE:

  Rainfall erositivity R-factor used in Revised Universal Soil Loss
  Euqation model

- B_RUSL_SE:

  Soil erodobility K-factor used in Revised Universal Soil Loss Euqation
  model

- B_RUSL_LS:

  Topography LS-factor used in Revised Universal Soil Loss Euqation
  model

- B_RUSL_CM:

  Cover management C-factor used in Revised Universal Soil Loss Euqation
  model

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- A_C_OF:

  (numeric) The carbon content of the soil layer (g/ kg)

- A_CEC_CO:

  (numeric) The cation exchange capacity of the soil (mmol+ / kg),
  analyzed via Cobalt-hexamine extraction

- A_PH_CC:

  (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)

- A_CACO3_IF:

  (numeric) the percentage of CaCO3 (%)

- A_N_RT:

  (numeric) The organic nitrogen content of the soil in mg N / kg

- A_N_PMN:

  (numeric) The potentially mineralizable N pool (mg N / kg soil)

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg P / kg)

- A_K_AAA:

  (numeric) The exchangeable K-content of the soil measured via ammonium
  acetate extraction

- A_MG_AAA:

  (numeric) is the exchangeable Mg concentration (mg/kg)

- A_B_HW:

  (numeric) The plant available content of B in the soil (mg B per kg)
  extracted by hot water

- A_ZN_CC:

  (numeric) The plant available content of Zn in the soil (ug Zn per kg)
  extracted by CaCl2

- A_ZN_EDTA:

  (numeric) The plant available content of Zn in the soil (mg Zn per kg)
  extracted by EDTA

- A_ZN_RT:

  (numeric) The total Zn-content of the soil via XRF or Dumas (mg Zn/kg)

- ID:

  (character) A field id

- output:

  (character) An optional argument to select output: scores, indicators,
  or all. (default = all)

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The output of the Open Soil Index Calculator for a specific agricultural
field. Depending on the output type, different output objects can be
returned. These include the estimated OSI scores (both total and
aggregated subscores), the value of the underling indicators as well the
possible recommendations to improve the soil quality. The output is
always a data.table.

## Details

It is assumed that the crop series is a continuous series in decreasing
order of years. So most recent year first, oldest year last.
