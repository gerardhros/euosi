# Calculate indicator for plant available water

This function calculates the plant available water index.

## Usage

``` r
osi_p_paw(A_CLAY_MI, A_SAND_MI, A_SILT_MI, A_C_OF, pwarning = FALSE)
```

## Arguments

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_SILT_MI:

  (numeric) The silt content of the soil (%)

- A_C_OF:

  (numeric) The organic carbon content of the soil (g / kg)

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Examples

``` r
osi_p_paw(A_CLAY_MI = 4.5, A_SAND_MI = 23, A_SILT_MI = 72.5,A_C_OF = 23)
#> [1] 0.7171519
```
