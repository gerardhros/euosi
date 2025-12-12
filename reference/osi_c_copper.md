# Calculate the copper availability index (wrapper function)

This function calculates the copper availability for all European
countries (if available).

## Usage

``` r
osi_c_copper(
  B_LU,
  A_CLAY_MI = NA_real_,
  B_CF = NA_real_,
  A_SOM_LOI = NA_real_,
  A_PH_WA = NA_real_,
  A_CACO3_IF = NA_real_,
  A_CU_RT = NA_real_,
  A_CU_EDTA = NA_real_,
  A_CU_CC = NA_real_,
  B_COUNTRY,
  pwarning = FALSE
)
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

- A_PH_WA:

  (numeric) The acidity of the soil, measured in water (-)

- A_CACO3_IF:

  (numeric) The carbonate content (%)

- A_CU_RT:

  (numeric) The pseudo-total content of Cu in the soil (mg Cu per kg)
  extracted by Aqua regia

- A_CU_EDTA:

  (numeric) The plant available content of Cu in the soil (mg Cu per kg)
  extracted by EDTA

- A_CU_CC:

  (numeric) The plant available content of Cu in the soil (mg Cu per kg)
  extracted by 0.01M CaCl2

- B_COUNTRY:

  (character) The country code

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The capacity of the soil to supply and buffer copper, evaluated given an
optimum threshold for yield. A numeric value.
