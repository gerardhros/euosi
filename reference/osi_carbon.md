# Calculate a carbon index

This function calculates a soil organic carbon index.

## Usage

``` r
osi_carbon(
  B_LU,
  A_C_OF,
  B_BGZ,
  A_CLAY_MI,
  A_SAND_MI,
  B_COUNTRY,
  pwarning = FALSE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

- B_BGZ:

  (factor) an European region-id used for carbon analyses

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- B_COUNTRY:

  (character) The country code

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The carbon index. A numeric value.

## Examples

``` r
osi_carbon(B_LU = '172',A_C_OF = 25, B_BGZ = '4',A_CLAY_MI=5,A_SAND_MI=25,B_COUNTRY='NL')
#> [1] 0.9568473
```
