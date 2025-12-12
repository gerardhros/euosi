# Calculate the risk for nitrogen leaching in France

This function calculates the N leaching risks for the soil

## Usage

``` r
osi_gw_nleach_fr(
  B_LU,
  A_N_RT,
  A_C_OF,
  A_CLAY_MI,
  A_SAND_MI,
  A_CACO3_IF,
  B_PREC_SUM = NA_real_,
  B_PREC_WIN = NA_real_,
  B_PET_SUM = NA_real_,
  B_PET_WIN = NA_real_,
  B_TEMP_SUM = NA_real_,
  B_TEMP_WIN = NA_real_,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (character) The crop code

- A_N_RT:

  (numeric) The organic nitrogen content of the soil (mg N / kg)

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_CACO3_IF:

  (numeric) The percentage of carbonated lime (%)

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

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The risk of nitrogen leaching. A numeric value, converted to a OSI
score.

## Examples

``` r
osi_gw_nleach_fr(B_LU = 'BDH',A_N_RT = 1200, A_C_OF = 25, 
A_CLAY_MI = 3.5, A_SAND_MI = 15,A_CACO3_IF = 0.8)
#> [1] 1.761615e-07
```
