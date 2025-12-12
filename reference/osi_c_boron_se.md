# Calculate the boron availability index in Sweden

This function calculates the boron availability.

## Usage

``` r
osi_c_boron_se(B_LU, A_PH_WA, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_PH_WA:

  (numeric) The pH measured in water

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The boron availability index in Sweden depends primarily on pH. A
numeric value.

## Examples

``` r
osi_c_boron_se(B_LU = '3301010901',A_PH_WA = 5.0)
#> [1] 5.324397e-05
osi_c_boron_se(B_LU = c('3301010901','3301061299'),A_PH_WA = c(3.5,5.5))
#> [1] 0.000000 0.207478
```
