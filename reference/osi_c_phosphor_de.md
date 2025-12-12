# Calculate the phosphate availability index in Germany

This function calculates the phosphate availability.

## Usage

``` r
osi_c_phosphor_de(
  B_LU,
  A_SOM_LOI,
  A_CLAY_MI,
  A_P_CAL,
  A_P_DL = NA_real_,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_SOM_LOI:

  (numeric) The organic matter content of the soil (%)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_P_CAL:

  (numeric) The P-content of the soil extracted with ammonium lactate(mg
  P / kg)

- A_P_DL:

  (numeric) The P-content of the soil extracted with double lactate (mg
  P / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate availability index in Germany stimated from extractable
soil P fractions. A numeric value.

## Examples

``` r
osi_c_phosphor_de(B_LU = '3301000000', A_SOM_LOI = 4.5, A_CLAY_MI = 5,A_P_CAL = 45,A_P_DL = 5)
#> [1] 0.5734736
osi_c_phosphor_de(B_LU = c('3301000000','3301061299'),A_SOM_LOI = c(3,3),A_CLAY_MI = c(3,15),
A_P_CAL = c(35,54),A_P_DL = c(3.5,5.5))
#> [1] 0.3875512 0.8143873
```
