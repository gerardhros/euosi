# Calculate the magnesium availability index in Finland

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_fi(
  B_LU,
  B_TEXTURE_USDA,
  A_MG_AAA,
  A_C_OF = 0.5,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (character) The crop code

- B_TEXTURE_USDA:

  (character) The soil texture according to USDA classification system

- A_MG_AAA:

  (numeric) The exchangeable Mg-content of the soil measured via
  ammonium acetate extraction (mg Mg / kg)

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Finland estimated from extractable
magnesium. A numeric value.

## Examples

``` r
osi_c_magnesium_fi(B_LU = '1110', B_TEXTURE_USDA = 'Si',A_MG_AAA = 45)
#> [1] 0.3811199
```
