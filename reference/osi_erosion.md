# Calculate the water erosion index using RUSLE approach

This function calculates the water erosion index

## Usage

``` r
osi_erosion(
  B_LU,
  A_SOM_LOI,
  A_CLAY_MI,
  A_SAND_MI,
  B_COUNTRY,
  B_RUSL_RE = NA_real_,
  B_RUSL_SE = NA_real_,
  B_RUSL_LS = NA_real_,
  B_RUSL_CM = NA_real_,
  pwarning = FALSE
)
```

## Arguments

- B_LU:

  (character) The crop code

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- B_COUNTRY:

  (character) The country code

- B_RUSL_RE:

  Rainfall erositivity R-factor used in Revised Universal Soil Loss
  Euqation model

- B_RUSL_SE:

  Soil erodobility K-factor used in Revised Universal Soil Loss Euqation
  model

- B_RUSL_LS:

  Topography LS-factor used in Revised Universal Soil Loss Euqation
  model

- B_RUSL_CM:

  Cover management C-factor used in Revised Universal Soil Loss Euqation
  model

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The water erosion index. A numeric value.

## Examples

``` r
osi_erosion(B_LU = '265',A_SOM_LOI = 3.5,A_CLAY_MI = 5,A_SAND_MI = 15,B_COUNTRY='NL')
#> [1] 0.9731984
```
