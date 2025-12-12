# Calculate the boron availability index in Germany

This function calculates the boron availability.

## Usage

``` r
osi_c_boron_de(
  B_LU,
  A_C_OF,
  A_CLAY_MI,
  A_SAND_MI,
  A_PH_CC,
  A_B_HW,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_C_OF:

  (numeric) The carbon content of the soil layer (g/ kg)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_PH_CC:

  (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)

- A_B_HW:

  (numeric) The plant available content of B in the soil (mg B per kg)
  extracted by hot water

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The boron availability index in Germany derived from extractable soil B
fractions. A numeric value.

## Examples

``` r
osi_c_boron_de(B_LU = '3301061299',A_C_OF=25, A_CLAY_MI=5,A_SAND_MI=15,A_PH_CC = 4,A_B_HW = 50)
#> [1] 1
 
```
