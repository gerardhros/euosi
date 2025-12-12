# Calculate the potassium availability index in Finland

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_fi(
  B_LU,
  B_TEXTURE_USDA,
  A_K_AAA,
  A_C_OF = 0.5,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (character) The crop code

- B_TEXTURE_USDA:

  (character) The soil texture according to USDA classification system

- A_K_AAA:

  (numeric) The exchangeable K-content of the soil measured via ammonium
  acetate extraction

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in Finland estimated from extractable
potassium. A numeric value.

## Examples

``` r
osi_c_potassium_fi(B_LU = '1110', B_TEXTURE_USDA = 'Si',A_K_AAA = 45,A_C_OF = 15)
#> [1] 0.1835531
```
