# Calculate the Zn availability index for agricultural soils in Ireland

This function calculates the Zn availability of a soil, using the
agronomic index used in Ireland.

## Usage

``` r
osi_c_zinc_ie(B_LU, A_SOM_LOI, A_PH_WA, A_ZN_EDTA, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) crop type

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil

- A_PH_WA:

  (numeric) pH measured in water (-)

- A_ZN_EDTA:

  (numeric) Zn content measured in EDTA (mg / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The zinc availability index in Ireland estimated from extractable zinc
and pH measured in water, a numeric value.

## Examples

``` r
osi_c_zinc_ie(B_LU = 'testcrop', A_SOM_LOI = 4, A_ZN_EDTA = 45, A_PH_WA = 6.5)
#> [1] 1
```
