# Calculate the Zn availability index for the Netherlands

This function calculates the availability of Zn for plant uptake

## Usage

``` r
osi_c_zinc_nl(B_LU, A_PH_CC, A_ZN_CC, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code from the BRP

- A_PH_CC:

  (numeric) The acidity of the soil, determined in 0.01M CaCl2 (-)

- A_ZN_CC:

  The plant available Zn content, extracted with 0.01M CaCl2 (mg / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The function of the soil to supply zinc (a numeric value).

## Examples

``` r
osi_c_zinc_nl(B_LU = '265', A_ZN_CC = 45, A_PH_CC = 6.5)
#> [1] 0.3649075
```
