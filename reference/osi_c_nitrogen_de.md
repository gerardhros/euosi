# Calculate the soil nitrogen supplying capacity in Germany

This function calculates the nitrogen producing capacity of the soil,
and applies correction factors used affecting N availability.

## Usage

``` r
osi_c_nitrogen_de(
  B_LU = NA_character_,
  A_CLAY_MI = NA_real_,
  A_SAND_MI = NA_real_,
  A_SOM_LOI = NA_real_,
  A_C_OF = NA_real_,
  A_N_RT = NA_real_,
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

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric
value, converted to a OSI score.

## Examples

``` r
osi_c_nitrogen_de(B_LU= '3301010100', A_CLAY_MI = 25, 
A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#> [1] 0.9753038
```
