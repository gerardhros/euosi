# Calculate the K excess index (wrapper function)

This function calculates the potassium excess for all European countries
(if available).

## Usage

``` r
osi_nut_k(
  B_LU,
  B_SOILTYPE_AGR = NA_character_,
  B_AER_FR = NA_character_,
  A_SOM_LOI = NA_real_,
  A_C_OF = NA_real_,
  A_CLAY_MI = NA_real_,
  A_SAND_MI = NA_real_,
  A_PH_CC = NA_real_,
  A_PH_WA = NA_real_,
  A_CACO3_IF = NA_real_,
  A_CEC_CO = NA_real_,
  A_K_AAA = NA_real_,
  A_K_AL = NA_real_,
  A_K_AN = NA_real_,
  A_K_CAL = NA_real_,
  A_K_CC = NA_real_,
  A_K_CO_PO = NA_real_,
  A_K_DL = NA_real_,
  A_K_M3 = NA_real_,
  A_K_NaAAA = NA_real_,
  A_K_WA = NA_real_,
  B_COUNTRY,
  pwarning = FALSE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- B_SOILTYPE_AGR:

  (character) The agricultural type of soil

- B_AER_FR:

  (character) An agroeconomic region in France

- A_SOM_LOI:

  (numeric) The organic matter content of the soil (%)

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_PH_CC:

  (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)

- A_PH_WA:

  (numeric) The acidity of the soil, measured in water

- A_CACO3_IF:

  (numeric) The percentage of carbonated lime (%)

- A_CEC_CO:

  (numeric) The cation exchange capacity of the soil (mmol+ / kg),
  analyzed via Cobalt-hexamine extraction

- A_K_AAA:

  (numeric) The exchangeable K-content of the soil measured via ammonium
  acetate extraction

- A_K_AL:

  (numeric) The exchangeable K-content of the soil measured via Ammonium
  Lactate extracton (mg K/ kg)

- A_K_AN:

  (numeric) The K-content of the soil extracted with ammonium nitrate
  (mg K /kg)

- A_K_CAL:

  (numeric) The exchangeable K-content of the soil measured via Calcium
  Ammonium Lactate (mg K/ kg)

- A_K_CC:

  (numeric) The plant available potassium, extracted with 0.01M CaCl2
  (mg / kg),

- A_K_CO_PO:

  (numeric) The occupation of the CEC with potassium (%)

- A_K_DL:

  (numeric) The exchangeable K-content of the soil measured via Double
  Lactate extraction (mg K/ kg)

- A_K_M3:

  (numeric) The exchangeable K-content of the soil measured via Mehlich
  3 extracton (mg K/ kg)

- A_K_NaAAA:

  (numeric) The K-content of the soil extracted with Morgan's solution,
  sodium acetate acetic acid (mg/ kg)

- A_K_WA:

  (numeric) The exchangeable K-content of the soil measured via water
  extracton (mg K/ kg)

- B_COUNTRY:

  (character) The country code

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The capacity of the soil to supply and buffer potassium, evaluated given
an optimum threshold for yield. If the value is exceeding this
threshold, then the efficiency of fertilizers decline. A numeric value.

## Examples

``` r
osi_nut_k(B_LU = '265', B_SOILTYPE_AGR = 'dekzand',A_SOM_LOI = 4, 
A_CLAY_MI = 11,A_SAND = 24, A_PH_CC = 5.4, A_CEC_CO = 125, 
A_K_AAA=346, A_K_CO_PO = 8.5, A_K_CC = 145,B_COUNTRY = 'NL')
#> [1] 0.8136363
```
