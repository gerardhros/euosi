# Calculate the phosphate availability index in France

This function calculates the phosphate availability.

## Usage

``` r
osi_c_phosphor_fr(B_LU, A_P_OL, A_PH_WA = NA_real_, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg P/ kg)

- A_PH_WA:

  (numeric) The pH measured in water.

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate availability index in France estimated from extractable
soil P Olsen (a numeric value).

## Examples

``` r
osi_c_phosphor_fr(B_LU = 'SOJ', A_P_OL = 45)
#> [1] 1
```
