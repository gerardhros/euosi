# Calculate the phosphorus excess index in France

This function calculates the phosphorus excess index

## Usage

``` r
osi_nut_p_fr(B_LU, A_P_OL, A_PH_WA = NA_real_, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop code

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen

- A_PH_WA:

  (numeric) The pH measured in water.

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate excess index in France estimated from extractable soil P
Olsen (a numeric value).

## Details

This function does not account for variability per agricultural region.
The pH is used to classify wether soils are calcareous.

## Examples

``` r
osi_nut_p_fr(B_LU = 'SOJ', A_P_OL = 45, A_PH_WA = 4.5)
#> [1] 2.807014e-05
```
