# Calculate the boron availability index (wrapper function)

This function calculates the boron availability for all European
countries (if available).

## Usage

``` r
osi_c_boron(
  B_LU,
  A_CLAY_MI,
  A_SAND_MI,
  A_SOM_LOI,
  A_PH_CC,
  A_B_HW,
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

- A_SOM_LOI:

  (numeric) The organic matter content of the soil (%)

- A_PH_CC:

  (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)

- A_B_HW:

  (numeric) The plant available content of B in the soil (mg B per kg)
  extracted by hot water

- B_COUNTRY:

  (character) The country code

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The capacity of the soil to supply and buffer boron, evaluated given an
optimum threshold for yield. A numeric value.
