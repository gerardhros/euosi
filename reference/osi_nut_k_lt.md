# Calculate the potassium excess index in Lithuania

This function calculates the potassium excess.

## Usage

``` r
osi_nut_k_lt(A_K_AL, A_SOM_LOI, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_K_AL:

  (numeric) The exchangeable K-content of the soil measured via Ammonium
  Lactate extraction (mg P/ kg)

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium excess index in Lithuania estimated from extractable
potassium. A numeric value.

## Examples

``` r
osi_nut_k_lt(A_K_AL = 45,A_SOM_LOI= 1.5)
#> [1] 0.95383
```
