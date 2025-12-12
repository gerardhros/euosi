# Calculate the Cu availability index for agricultural soils in Romania

This function calculates the Cu availability of a soil, using the
agronomic index used in Romania

## Usage

``` r
osi_c_copper_ro(B_LU, A_CU_EDTA)
```

## Arguments

- B_LU:

  (character) crop type

- A_CU_EDTA:

  (numeric) Cu content measured in EDTA (mg / kg)

## Value

The copper availability index in Romania estimated from extractable Cu
with 0.05M Na2EDTA, a numeric value.

## Examples

``` r
osi_c_copper_ro(B_LU = 'testcrop', A_CU_EDTA = 45)
#> [1] 1
```
