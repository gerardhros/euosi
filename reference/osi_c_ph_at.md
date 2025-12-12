# Calculate the pH index in Austria

This function calculates the soil acidity

## Usage

``` r
osi_c_ph_at(B_LU, A_PH_CC, B_TEXTURE_HYPRES, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_PH_CC:

  (numeric) The pH measured in cacl2

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The pH index in Austria, depending on land use and soil type. A numeric
value.

## Examples

``` r
osi_c_ph_at(B_LU = '3301000000', A_PH_CC = 4.7,B_TEXTURE_HYPRES = 'C')
#> [1] 0.5143254
```
