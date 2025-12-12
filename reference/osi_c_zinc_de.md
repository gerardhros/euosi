# Calculate the zinc availability index in Germany

This function calculates the zinc availability.

## Usage

``` r
osi_c_zinc_de(B_LU, A_C_OF, A_CLAY_MI, A_SAND_MI, A_ZN_EDTA, unitcheck = TRUE)
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

- A_ZN_EDTA:

  (numeric) Zn content measured in EDTA (mg / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The zinc availability index in Germany derived from extractable soil Zn
fractions. A numeric value.

## Examples

``` r
osi_c_zinc_de(B_LU = '3301061299',A_C_OF=25, A_CLAY_MI=5,A_SAND_MI=15,A_ZN_EDTA = 50)
#> [1] 1
 
```
