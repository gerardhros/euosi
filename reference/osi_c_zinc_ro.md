# Calculate the Zn availability index for agricultural soils in Romenia

This function calculates the Zn availability of a soil, using the
agronomic index used in RO.

## Usage

``` r
osi_c_zinc_ro(B_LU, A_PH_WA, A_P_AL, A_ZN_EDTA, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) crop type

- A_PH_WA:

  (numeric) pH measured in water (-)

- A_P_AL:

  (numeric) The P-content of the soil extracted with ammonium lactate
  (mg P / kg)

- A_ZN_EDTA:

  (numeric) Zn content measured in EDTA, pH 8.6, with ammonium carbonate
  (mg / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The zinc availability index in United Kingdom estimated from extractable
zinc and pH measured in water, a numeric value.

## Examples

``` r
osi_c_zinc_ro(B_LU = 'testcrop', A_ZN_EDTA = 45, A_P_AL = 45,A_PH_WA = 6.5)
#> [1] 1
```
