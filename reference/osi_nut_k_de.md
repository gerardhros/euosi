# Calculate the potassium excess index in Germany

This function calculates the potassium excess.

## Usage

``` r
osi_nut_k_de(B_LU, A_C_OF, A_CLAY_MI, A_SAND_MI, A_K_CAL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_C_OF:

  (numeric) The carbon content of the soil layer (g/ kg)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_K_CAL:

  (numeric) The potassium content extracted with calcium ammonium
  lactate (g / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium excess index in Germany derived from extractable soil K
fractions. A numeric value.
