# Calculate the soil nitrogen supplying capacity (wrapper function)

This function calculates the nitrogen supplying capacity for soils in
Europe

## Usage

``` r
osi_c_nitrogen(
  B_LU,
  B_SOILTYPE_AGR = NA_character_,
  A_CLAY_MI = NA_real_,
  A_SAND_MI = NA_real_,
  A_SOM_LOI = NA_real_,
  A_C_OF = NA_real_,
  A_N_RT,
  A_CACO3_IF = NA_real_,
  B_COUNTRY,
  pwarning = FALSE
)
```

## Arguments

- B_LU:

  (numeric) The crop code from the BRP

- B_SOILTYPE_AGR:

  (character) The agricultural type of soil

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

- A_N_RT:

  (numeric) The organic nitrogen content of the soil (mg N / kg)

- A_CACO3_IF:

  (numeric) The percentage of carbonated lime (%)

- B_COUNTRY:

  (character) The country code

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

the capacity of soils to supply nitorgen, converted to an OSI score.

## Examples

``` r
osi_c_nitrogen(B_LU = '256', B_SOILTYPE_AGR = 'dekzand',A_N_RT = 2500, 
A_CLAY_MI = 11, A_SAND_MI = 3,A_C_OF = NA,A_CACO3_IF = NA,
A_SOM_LOI = 4.5, B_COUNTRY = 'NL')
#> [1] 1
```
