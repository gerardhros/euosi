# Calculate the phosphorus excess index in Finland

This function calculates the phosphorus excess index

## Usage

``` r
osi_nut_p_fi(B_LU, B_TEXTURE_USDA, A_P_AAA, A_C_OF = 0.5, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- B_TEXTURE_USDA:

  (character) The soil texture according to USDA classification system

- A_P_AAA:

  (numeric) The exchangeable P-content of the soil measured via ammonium
  acetate extraction

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus excess index in Finland estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_nut_p_fi(B_LU = '4010', B_TEXTURE_USDA = 'Si',
A_P_AAA = 45,A_C_OF=1.5)
#> [1] 0.9199666
```
