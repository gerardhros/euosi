# Calculate the magnesium availability index in Romenia

This function calculates the magnesium availability index

## Usage

``` r
osi_c_magnesium_ro(
  B_LU,
  B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
  A_CEC_CO,
  A_MG_CC,
  A_MG_CO_PO,
  A_K_CO_PO,
  A_PH_WA,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- A_CEC_CO:

  (numeric) is the Cation exhange capacity in mmol+/kg

- A_MG_CC:

  (numeric) The plant available content of Mg in the soil (mg Mg per kg)
  extracted by 0.01M CaCl2

- A_MG_CO_PO:

  (numeric) The exchangeable Mg-content of the soil measured via Cohex
  extracton, percentage occupation at CEC (%)

- A_K_CO_PO:

  (numeric) The exchangeable K-content of the soil measured via Cohex
  extracton, percentage occupation at CEC (%)

- A_PH_WA:

  (numeric) The pH measured in water

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The Mg index in Romenia. A numeric value.

## Examples

``` r
osi_c_magnesium_ro(B_LU = 'testcrop1',B_TEXTURE_HYPRES ='M', A_CEC_CO = 140,
A_MG_CC = 60, A_MG_CO_PO = 8,A_K_CO_PO = 12, A_PH_WA = 5)
#> [1] 0.6392354
```
