# Calculate the boron availability index in United Kingdom

This function calculates the boron availability.

## Usage

``` r
osi_c_boron_uk(
  B_LU,
  B_TEXTURE_HYPRES,
  A_SOM_LOI,
  A_PH_CC,
  A_B_HW,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- B_TEXTURE_HYPRES:

  (character) The soil texture according to HYPRES classification system

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil

- A_PH_CC:

  (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)

- A_B_HW:

  (numeric) The plant available content of B in the soil (mg B per kg)
  extracted by hot water

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The boron availability index in UK derived from extractable soil B
fractions. A numeric value.

## Examples

``` r
osi_c_boron_uk(B_LU = 'testcrop1',B_TEXTURE_HYPRES='C',A_SOM_LOI=3,
A_PH_CC = 4,A_B_HW = 50)
#> [1] 1
osi_c_boron_uk(B_LU = c('testcrop1','testcrop2'),B_TEXTURE_HYPRES = c('C','F'),
A_SOM_LOI = c(3,3),A_PH_CC = c(4,6),A_B_HW = c(35,55))
#> [1] 1 1
```
