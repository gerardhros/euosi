# Calculate the potassium excess index in France

This function calculates the potassium excess.

## Usage

``` r
osi_nut_k_fr(
  B_LU,
  A_K_AAA,
  B_TEXTURE_GEPPA = NA_character_,
  B_SOILTYPE_AGR = NA_character_,
  B_AER_FR = NA_character_,
  A_PH_WA = NA_real_,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (character) The crop code

- A_K_AAA:

  (numeric) The exchangeable K-content of the soil measured via ammonium
  acetate extraction

- B_TEXTURE_GEPPA:

  (character) The soil texture class in a particular region.

- B_SOILTYPE_AGR:

  (character) The agricultural soil type classification

- B_AER_FR:

  (character) An agroeconomic region in France. Optional argument.

- A_PH_WA:

  (numeric) The pH measured in water.

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium excess index in France estimated from extractable
potassium. A numeric value.

## Details

The function has two optional arguments soil type (B_SOILTYPE_AGR) and
agricultural region (B_AER_FR). When these are unknown, then the soil
type is estimated based on the pH value. Threshold values are then
generalized for calcareous and non-calcareous soils.

## Examples

``` r
osi_nut_k_fr(B_LU = 'SOJ', A_K_AAA = 45,B_TEXTURE_GEPPA ='As',
B_SOILTYPE_AGR = 'limons battants', B_AER_FR = 'nord picardie')
#> [1] 0.9482272
```
