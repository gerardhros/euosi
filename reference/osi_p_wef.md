# Calculate indicator for wind erodibility

This function calculates the risk for wind erodibility of soils, derived
from Van Kerckhoven et al. (2009) and Ros & Bussink (2013)

## Usage

``` r
osi_p_wef(B_LU, A_CLAY_MI, A_SAND_MI, B_COUNTRY, pwarning = FALSE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- B_COUNTRY:

  (character) The country code

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The vulnerability of the soil for wind erosion. A numeric value.

## Examples

``` r
osi_p_wef(B_LU = 265, A_CLAY_MI = 4, A_SAND_MI = 15, B_COUNTRY='NL')
#> [1] 1
osi_p_wef(B_LU = c(265,1019), A_CLAY_MI = c(4,18), A_SAND_MI = c(15,65), B_COUNTRY=c('NL','NL'))
#> [1] 1.0000000 0.6904295
```
