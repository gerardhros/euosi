# Calculate the pH index for Finland

This function evaluates the pH index in Finland

## Usage

``` r
osi_c_ph_fi(B_LU, B_TEXTURE_USDA, A_PH_WA, A_C_OF = 0.5, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- B_TEXTURE_USDA:

  (character) The soil texture according to USDA classification system

- A_PH_WA:

  (numeric) The pH values determined via water extract

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The pH index in Finland estimated from pH water. A numeric value.

## Examples

``` r
osi_c_ph_fi(B_LU = '1110', B_TEXTURE_USDA = 'Si',A_PH_WA = 4.5)
#> [1] 0.07620944
```
