# Estimate soil extractable zinc (-)

Estimate soil extractable zinc (-)

## Usage

``` r
osi_conv_zinc(
  element,
  A_SOM_LOI,
  A_PH_CC,
  A_ZN_RT,
  A_ZN_AAA = NA_real_,
  A_ZN_CC = NA_real_,
  A_ZN_CO = NA_real_,
  A_ZN_DTPA = NA_real_,
  A_ZN_EDTA = NA_real_,
  A_ZN_M3 = NA_real_,
  A_ZN_WA = NA_real_
)
```

## Arguments

- element:

  (character) the method requested to be calculated

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- A_PH_CC:

  (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)

- A_ZN_RT:

  (numeric) The total Zn-content of the soil via XRF or Dumas (mg Zn/kg)

- A_ZN_AAA:

  (numeric) The exchangeable Zn-content of the soil measured via acid
  amonium acetate (mg Zn/kg)

- A_ZN_CC:

  (numeric) The exchangeable Zn-content of the soil measured via 0.01M
  CaCl2 (ug Zn/kg)

- A_ZN_CO:

  (numeric) The exchangeable Zn-content of the soil measured via Cohex
  extraction (mg Zn/kg)

- A_ZN_DTPA:

  (numeric) The exchangeable Zn-content of the soil measured via DTPA
  (mg Zn/kg)

- A_ZN_EDTA:

  (numeric) The exchangeable Zn-content of the soil measured via EDTA
  (mg Zn/kg)

- A_ZN_M3:

  (numeric) The exchangeable Zn-content of the soil measured via
  Mehlich-III (mg Zn/kg)

- A_ZN_WA:

  (numeric) The exchangeable Zn-content of the soil measured via water
  (mg Zn/kg)
