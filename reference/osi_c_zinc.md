# Calculate the Zinc availability index for agricultural soils (wrapper function)

This function calculates the zinc availability for all European
countries (if available).

## Usage

``` r
osi_c_zinc(
  B_LU,
  A_CLAY_MI = NA_real_,
  A_SAND_MI = NA_real_,
  A_C_OF = NA_real_,
  A_SOM_LOI = NA_real_,
  A_PH_WA = NA_real_,
  A_PH_CC = NA_real_,
  A_ZN_RT = NA_real_,
  A_ZN_EDTA = NA_real_,
  A_ZN_CC = NA_real_,
  A_P_OL = NA_real_,
  B_COUNTRY,
  pwarning = FALSE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_CLAY_MI:

  (numeric) The clay content (%)

- A_SAND_MI:

  (numeric) The sand content (%)

- A_C_OF:

  (numeric) The carbon content of the soil layer (g/ kg)

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- A_PH_WA:

  (numeric) The acidity of the soil, measured in water (-)

- A_PH_CC:

  (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)

- A_ZN_RT:

  (numeric) The total Zn-content of the soil via XRF or Dumas (mg Zn/kg)

- A_ZN_EDTA:

  (numeric) The plant available content of Zn in the soil (mg Zn per kg)
  extracted by EDTA

- A_ZN_CC:

  (numeric) The plant available content of Zn in the soil (ug Zn per kg)
  extracted by 0.01M CaCl2

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen (mg P / kg)

- B_COUNTRY:

  (character) The country code

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The capacity of the soil to supply and buffer zinc, evaluated given an
optimum threshold for yield. A numeric value.

## Examples

``` r
osi_c_zinc(B_LU = 'SOJ', A_CLAY_MI = 45, A_SAND_MI = 15,A_ZN_EDTA = 45, A_PH_WA = 6.5,
A_PH_CC = NA, A_ZN_CC = NA, A_ZN_RT = 51, B_COUNTRY='FR')
#> [1] 1
```
