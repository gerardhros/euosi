# Calculate the risk for nitrogen leaching in Europe

This function calculates the N leaching risks for the soils in Europe

## Usage

``` r
osi_gw_nleach(
  B_LU = NA_character_,
  A_CLAY_MI = NA_real_,
  A_SAND_MI = NA_real_,
  A_CACO3_IF = NA_real_,
  A_N_RT = NA_real_,
  A_C_OF = NA_real_,
  B_PREC_SUM = NA_real_,
  B_PREC_WIN = NA_real_,
  B_PET_SUM = NA_real_,
  B_PET_WIN = NA_real_,
  B_TEMP_SUM = NA_real_,
  B_TEMP_WIN = NA_real_,
  B_COUNTRY,
  pwarning = FALSE
)
```

## Arguments

- B_LU:

  (character) The crop code

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_CACO3_IF:

  (numeric) The percentage of carbonated lime (%)

- A_N_RT:

  (numeric) The organic nitrogen content of the soil (mg N / kg)

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

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

- B_COUNTRY:

  (character) The country code

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The risk for N leaching from agricultural soils in Europe
