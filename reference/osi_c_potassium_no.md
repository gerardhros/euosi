# Calculate the potassium availability index in Norway

This function calculates the potassium availability.

## Usage

``` r
osi_c_potassium_no(B_LU, A_K_AL, A_CLAY_MI, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_K_AL:

  (numeric) The K-content of the soil extracted with ammonium lactate
  (mg K / kg)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium availability index in Norway derived from extractable soil
K fractions. A numeric value.

## Examples

``` r
osi_c_potassium_no(B_LU = 'testcrop1',A_K_AL = 5,A_CLAY_MI=5)
#> [1] 1.39261e-15
osi_c_potassium_no(B_LU = c('testcrop1','testcrop2'),A_K_AL = c(3.5,5.5),A_CLAY_MI=c(3,5))
#> [1] 1.527040e-16 2.845062e-15
```
