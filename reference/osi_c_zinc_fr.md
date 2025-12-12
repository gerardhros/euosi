# Calculate the Zn availability index for agricultural soils in France

This function calculates the Zn availability of a soil, using the
agronomic index used in France.

## Usage

``` r
osi_c_zinc_fr(B_LU, A_PH_WA, A_ZN_EDTA, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) crop type

- A_PH_WA:

  (numeric) pH measured in water (-)

- A_ZN_EDTA:

  (numeric) Zn content measured in EDTA (mg / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The zinc availability index in France estimated from extractable zinc
and pH measured in water, a numeric value.

## Examples

``` r
osi_c_zinc_fr(B_LU = 'SOJ', A_ZN_EDTA = 45, A_PH_WA = 6.5)
#> [1] 1
```
