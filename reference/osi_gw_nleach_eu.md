# Calculate the risk for nitrogen leaching template for EU

This function calculates the N leaching risks for the soil

## Usage

``` r
osi_gw_nleach_eu(
  B_LU,
  A_N_RT,
  A_C_OF,
  A_CLAY_MI,
  A_SAND_MI,
  B_PREC_Y = NA_real_,
  B_PET_Y = NA_real_,
  B_TEMP_Y = NA_real_,
  B_COUNTRY,
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

- B_PREC_Y:

  (numeric) Total potential precipitation in year (mm)

- B_PET_Y:

  (numeric) Total potential evapotranspiration in year (mm)

- B_TEMP_Y:

  (numeric) Mean year temperature (degrees Celcius)

- B_COUNTRY:

  (character) The country code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The risk for N leaching

## Examples

``` r
osi_gw_nleach_eu(B_LU ='3301000000',A_N_RT = 4100, A_C_OF = 22, A_CLAY_MI = 4.5, 
A_SAND_MI = 15, B_PREC_Y = 900, B_PET_Y = 650,B_TEMP_Y = 4.5, B_COUNTRY='ES')
#> [1] 0.8681686
```
