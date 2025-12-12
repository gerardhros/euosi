# Calculate the boron availability index in Portugal

This function calculates the boron availability.

## Usage

``` r
osi_c_boron_pt(B_LU, A_B_HW, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_B_HW:

  (numeric) The plant available content of B in the soil (mg B per kg)
  extracted by hot water

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The boron availability index in Portugal derived from extractable soil B
fractions. A numeric value.

## Examples

``` r
osi_c_boron_pt(B_LU = '3301010901',A_B_HW = 50)
#> [1] 1
osi_c_boron_pt(B_LU = c('3301010901','3304990000'),A_B_HW = c(35,55))
#> [1] 1 1
```
