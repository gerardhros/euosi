# Calculate crumbleability index in the topsoil for all EU countries

This function calculates the crumbleability index

## Usage

``` r
osi_p_crumbleability(
  B_LU,
  A_CLAY_MI,
  A_SOM_LOI,
  A_PH_CC,
  B_COUNTRY,
  pwarning = FALSE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SOM_LOI:

  (numeric) The organic matter content of the soil (%)

- A_PH_CC:

  (numeric) The pH measured in CaCl2

- B_COUNTRY:

  (character) The country code

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The function returns the crumbleability index

## Examples

``` r
osi_p_crumbleability(B_LU = '256', A_CLAY_MI = 5, A_SOM_LOI = 3.5, A_PH_CC = 5.5, B_COUNTRY = 'NL')
#> [1] 1
```
