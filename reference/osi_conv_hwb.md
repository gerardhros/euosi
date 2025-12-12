# Calculate A_B_HW

This function calculates the hot water extractable B content from A_B_CC
(mg / kg).

## Usage

``` r
osi_conv_hwb(
  B_SOILTYPE_AGR,
  A_SOM_LOI = NA_real_,
  A_B_CC = NA_real_,
  A_PH_CC = NA_real_
)
```

## Arguments

- B_SOILTYPE_AGR:

  (character) The agricultural type of soil. Options: duinzand, dekzand,
  zeeklei, rivierklei, maasklei, dalgrond, moerige_klei, veen en loess

- A_SOM_LOI:

  (numeric) The organic matter content of the soil (%)

- A_B_CC:

  (numeric) The extractable boron content of the soil (ug / kg),
  measured in a 0.01M CaCl2 extract

- A_PH_CC:

  (numeric) The pH of the soil, measured in a 0.01M CaCl2 extract (-)

## References

Van Rotterdam & Bussink (2017) and de Haas et al. (2004)
