# Calculate indicators for water holding capacity in the topsoil

This function calculates different kind of Water Retention Indices given
the continuous pedotransferfunctions of Wosten et al. (2001) These
include : 'water holding capacity','plant available water' and 'Ksat'

## Usage

``` r
osi_p_whc(
  A_CLAY_MI,
  A_SAND_MI,
  A_SOM_LOI,
  type = "whc",
  ptf = "Wosten1999",
  pwarning = FALSE
)
```

## Arguments

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_SOM_LOI:

  (numeric) The organic matter content of the soil (%)

- type:

  (character) The type of water retention index. Options include
  c('water holding capacity', 'whc','plant available water','paw','Ksat'
  or 'ksat')

- ptf:

  (character) Pedotransfer functions to calculate van Genuchten
  parameters. Options include c('Wosten1999', 'Wosten2001', 'Klasse')

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The function returns by default the water holding capacity ('whc') of
the top soil (in mm). A numeric value. Other outputs include 'plant
available water' (or 'paw') or 'ksat' being the saturated permeability.
Soil functions are evaluated given a threshold value and expressed as a
distance to target.

## References

Wosten et al. (2001) Pedotransfer functions: bridging the gap between
available basic soil data and missing hydraulic characteristics. Journal
of Hydrology 251, p123.

## Examples

``` r
osi_p_whc(A_CLAY_MI = 20.5,A_SAND_MI = 65,A_SOM_LOI = 3.5)
#> [1] 0.8273837
osi_p_whc(A_CLAY_MI = 5,A_SAND_MI = 15,A_SOM_LOI = 6.5)
#> [1] 0.8251179
osi_p_whc(A_CLAY_MI = 5,A_SAND_MI = 15,A_SOM_LOI = 6.5, 
type = 'water holding capacity')
#> [1] 0.8251179
```
