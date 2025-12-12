# Calculate the pH index in Greece

This function calculates the pH index

## Usage

``` r
osi_c_ph_el(B_LU, A_PH_WA, A_NA_CO_PO, B_TEXTURE_HYPRES, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_PH_WA:

  (numeric) The pH measured in water

- A_NA_CO_PO:

  (numeric) The sodium occupation of the CEC (%)

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The pH index in Greece. A numeric value.

## Examples

``` r
osi_c_ph_el(B_LU = 'testcrop1',A_PH_WA = 5,A_NA_CO_PO = 3,B_TEXTURE_HYPRES='C')
#> [1] 0.9323999
osi_c_ph_el(B_LU = c('testcrop1','testcrop2'),A_PH_WA = c(3.5,5.5),
A_NA_CO_PO = c(1,6),B_TEXTURE_HYPRES=c('C','M'))
#> [1] 0.1432329 0.9818066
```
