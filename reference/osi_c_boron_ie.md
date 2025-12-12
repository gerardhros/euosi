# Calculate the boron availability index in Ireland

This function calculates the boron availability.

## Usage

``` r
osi_c_boron_ie(B_LU, A_B_HW, unitcheck = TRUE)
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

The boron availability index in Ireland derived from extractable soil B
fractions. A numeric value.

## Examples

``` r
osi_c_boron_ie(B_LU = 'testcrop',A_B_HW = 50)
#> [1] 1
osi_c_boron_ie(B_LU = c('testcrop1','testcrop2'),A_B_HW = c(35,55))
#> [1] 1 1
```
