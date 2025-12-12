# Calculate the capacity of soils to supply Magnesium

This function calculates an index for the availability of Magnesium in
soil

## Usage

``` r
osi_c_magnesium_nl(
  B_LU,
  B_SOILTYPE_AGR,
  A_SOM_LOI,
  A_CLAY_MI,
  A_PH_CC,
  A_CEC_CO,
  A_K_CO_PO,
  A_MG_CC,
  A_K_CC,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (numeric) The crop code from the BRP

- B_SOILTYPE_AGR:

  (character) The agricultural type of soil

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_PH_CC:

  (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)

- A_CEC_CO:

  (numeric) The cation exchange capacity of the soil (mmol+ per kg),
  analyzed via Cobalt-hexamine extraction

- A_K_CO_PO:

  (numeric) The occupation of the CEC with potassium (%)

- A_MG_CC:

  (numeric) The plant available content of Mg in the soil (mg Mg per kg)
  extracted by 0.01M CaCl2

- A_K_CC:

  (numeric) The plant available potassium, extracted with 0.01M CaCl2
  (mg per kg),

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

An index representing the availability of Magnesium in a soil. A numeric
value.

## Examples

``` r
osi_c_magnesium_nl(B_LU = '265', B_SOILTYPE_AGR = 'dekzand',
A_SOM_LOI = 3.5,A_CLAY_MI = 8.5,A_PH_CC = 5.4, 
A_CEC_CO = 185,A_K_CO_PO = 4.5,A_MG_CC = 125,A_K_CC = 65)
#> [1] 0.9833179
```
