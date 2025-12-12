# Calculate the phosphorus excess index in Estonia

This function calculates the phosphorus excess

## Usage

``` r
osi_nut_p_ee(A_P_M3, A_SOM_LOI, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_P_M3:

  (numeric) The exchangeable P-content of the soil measured via Mehlich
  3 extracton (mg P/ kg)

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphorus excess index in Estonia estimated from extractable
phosphorus. A numeric value.

## Examples

``` r
osi_nut_p_ee(A_P_M3 = 45,A_SOM_LOI = 3, B_LU='testcrop1')
#> [1] 0.8785792
```
