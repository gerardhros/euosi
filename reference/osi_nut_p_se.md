# Calculate the phosphate excess index in Sweden

This function calculates the phosphate excess.

## Usage

``` r
osi_nut_p_se(B_LU, A_P_AL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_P_AL:

  (numeric) The P-content of the soil extracted with ammonium lactate
  (mg P / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate excess index in Sweden derived from extractable soil P
fractions. A numeric value.

## Examples

``` r
osi_nut_p_se(B_LU = '3301061299',A_P_AL = 5)
#> [1] 0.9703357
osi_nut_p_se(B_LU = c('3301061299','3301000000'),A_P_AL = c(3.5,5.5))
#> [1] 0.9717457 0.9698508
```
