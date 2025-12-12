# Calculate the phosphate excess index in the Netherlands

This function calculates the phosphate excess.

## Usage

``` r
osi_nut_p_nl(
  B_LU,
  A_P_AL = NA_real_,
  A_P_CC = NA_real_,
  A_P_WA = NA_real_,
  B_SOILTYPE_AGR = NA_real_,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_P_AL:

  (numeric) The P-content of the soil extracted with ammonium lactate

- A_P_CC:

  (numeric) The P-content of the soil extracted with CaCl2

- A_P_WA:

  (numeric) The P-content of the soil extracted with water

- B_SOILTYPE_AGR:

  (character) The soil type in a particular region

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The phosphate excess index in the Netherlands estimated from extractable
soil P fractions. A numeric value.

## Examples

``` r
osi_nut_p_nl(B_LU = '265', A_P_AL = 45, A_P_CC = 2.5, B_SOILTYPE_AGR = 'dekzand')
#> [1] 0.907709
osi_nut_p_nl(B_LU = c('265','1019'),A_P_AL = c(35,54),
A_P_CC = c(2.5,4.5), A_P_WA = c(35,65), B_SOILTYPE_AGR = rep('dekzand',2))
#> [1] 0.9070752 0.1235547
```
