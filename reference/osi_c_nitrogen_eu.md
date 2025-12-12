# Calculate the soil nitrogen supplying capacity template in Europe

This function calculates the nitrogen producing capacity of the soil,
and applies correction factors used affecting N availability.

## Usage

``` r
osi_c_nitrogen_eu(
  B_LU = NA_character_,
  A_CLAY_MI = NA_real_,
  A_SAND_MI = NA_real_,
  A_SOM_LOI = NA_real_,
  A_C_OF = NA_real_,
  A_N_RT = NA_real_,
  B_COUNTRY = NA_real_,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (character) The crop code

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

- A_N_RT:

  (numeric) The organic nitrogen content of the soil in mg N / kg

- B_COUNTRY:

  (character) The country code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric
value, converted to a OSI score.

## Examples

``` r
osi_c_nitrogen_eu(B_LU = '256',A_N_RT = 650, A_CLAY_MI = 25, A_SAND_MI = 3,
A_C_OF = NA_real_,A_SOM_LOI = 4.5,B_COUNTRY = 'NL')
#> [1] 0.9964055
```
