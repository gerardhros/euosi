# Calculate the phosphate availability index in Sweden

This function calculates the phosphate availability.

## Usage

``` r
osi_c_phosphor_se(B_LU, A_P_AL, unitcheck = TRUE)
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

The phosphate availability index in Sweden derived from extractable soil
P fractions. A numeric value.

## Examples

``` r
osi_c_phosphor_se(B_LU = '3301061299',A_P_AL = 5)
#> [1] 6.55759e-18
osi_c_phosphor_se(B_LU = c('3301061299','3301000000'),A_P_AL = c(3.5,5.5))
#> [1] 5.968525e-22 1.040336e-16
```
