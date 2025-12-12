# Calculate the phosphorus excess index in Lithuania

This function calculates the phosphorus excess.

## Usage

``` r
osi_nut_p_lt(A_P_AL, A_SOM_LOI, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_P_AL:

  (numeric) The exchangeable P-content of the soil measured via Ammonium
  Lactate extraction (mg P/ kg)

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus excess index in Lithuania estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_nut_p_lt(B_LU = 'testcrop1', A_P_AL = 45,A_SOM_LOI = 4.5)
#> [1] 0.9123853
```
