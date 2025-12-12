# Calculate the Zn availability index for Portugal

This function calculates the availability of Zn for plant uptake

## Usage

``` r
osi_c_zinc_pt(B_LU, A_ZN_AAA, A_PH_CC, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) A unique crop code

- A_ZN_AAA:

  The plant available Zn content, extracted with ammonium acetate (mg Zn
  / kg)

- A_PH_CC:

  (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The function of the soil to supply zinc (a numeric value).

## Examples

``` r
osi_c_zinc_pt(B_LU = '3301061299', A_ZN_AAA = 45,A_PH_CC = 4.5)
#> [1] 1
```
