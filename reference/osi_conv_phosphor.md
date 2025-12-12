# Estimate soil extractable phosphorus (-)

Estimate soil extractable phosphorus (-)

## Usage

``` r
osi_conv_phosphor(
  element,
  A_P_AL = NA_real_,
  A_P_CC = NA_real_,
  A_P_WA = NA_real_,
  A_P_OL = NA_real_,
  A_P_CAL = NA_real_,
  A_P_DL = NA_real_,
  A_P_AAA = NA_real_,
  A_P_AAA_EDTA = NA_real_,
  A_P_M3 = NA_real_,
  A_P_MORGAN = NA_real_,
  B_SOILTYPE_AGR = NA_real_,
  A_PH_CC = 5
)
```

## Arguments

- element:

  (character) the method requested to be calculated

- A_P_AL:

  (numeric) The P-content of the soil extracted with ammonium lactate

- A_P_CC:

  (numeric) The P-content of the soil extracted with CaCl2

- A_P_WA:

  (numeric) The P-content of the soil extracted with water

- A_P_OL:

  (numeric) The P-content of the soil extracted with Olsen

- A_P_CAL:

  (numeric) The P-content of the soil extracted with ammonium lactate(mg
  P2O5 / 100g)

- A_P_DL:

  (numeric) The P-content of the soil extracted with double lactate (mg
  P / kg)

- A_P_AAA:

  (numeric) The exchangeable P-content of the soil measured via acid
  ammonium acetate extraction (mg P/ kg)

- A_P_AAA_EDTA:

  (numeric) The exchangeable P-content of the soil measured via acid
  ammonium acetate+EDTA extraction

- A_P_M3:

  (numeric) The P-content of the soil extracted with Mehlig 3

- A_P_MORGAN:

  (numeric) The P-content of the soil extracted with sodium acetate (mg
  P / L)

- B_SOILTYPE_AGR:

  (character) The agricultural type of soil.

- A_PH_CC:

  (numeric) The pH measured in cacl2. If missing, then assume its
  agronomic common value of 5.

## References

Steinfurth et al., (2021) Conversion equations between Olsen-P and other
methods used to assess plant available soil phosphorus in Europe. A
review
