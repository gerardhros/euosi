# Calculate the phosphate availability index in Ireland

This function calculates the phosphate availability.

## Usage

``` r
osi_c_phosphor_ie(
  B_LU,
  A_P_OL = NA_real_,
  A_P_MORGAN = NA_real_,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg/kg)

- A_P_MORGAN:

  (numeric) The P-content of the soil extracted with sodium acetate (mg
  P / L)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate availability index in Ireland derived from extractable
soil P fractions. A numeric value.

## Examples

``` r
osi_c_phosphor_ie(B_LU = '3301000000',A_P_OL = 5)
#> [1] 0.0001154996
osi_c_phosphor_ie(B_LU = c('3301000000','3301010101'),A_P_OL = c(3.5,5.5))
#> [1] 0.0001043386 0.0001216680
```
