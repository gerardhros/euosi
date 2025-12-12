# Calculate the magnesium availability index in Denmark

This function calculates the magnesium availability.

## Usage

``` r
osi_c_magnesium_dk(B_LU, A_MG_AL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_MG_AL:

  (numeric) The Mg-content of the soil extracted with ammonium lactate
  (mg Mg / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The magnesium availability index in Denmark derived from extractable
soil Mg fractions. A numeric value.

## Examples

``` r
osi_c_magnesium_dk(B_LU = '3301000000',A_MG_AL = 5)
#> [1] 0.002232308
osi_c_magnesium_dk(B_LU = c('3301000000','3301061299'),A_MG_AL = c(3.5,5.5))
#> [1] 0.001260067 0.002678372
```
