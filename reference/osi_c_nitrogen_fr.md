# Calculate the soil nitrogen supplying capacity in France

This function calculates the NSC (nitrogen supply capacity) for the soil

## Usage

``` r
osi_c_nitrogen_fr(
  B_LU,
  A_CLAY_MI,
  A_SAND_MI,
  A_C_OF,
  A_N_RT,
  A_CACO3_IF,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (character) The crop code

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

- A_N_RT:

  (numeric) The organic nitrogen content of the soil (mg N / kg)

- A_CACO3_IF:

  (numeric) The percentage of carbonated lime (%)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric
value, converted to a OSI score.

## Examples

``` r
osi_c_nitrogen_fr(B_LU = 'CML', A_CLAY_MI = 15, A_SAND_MI = 20, 
A_C_OF = 45,A_N_RT = 2500,A_CACO3_IF = 0)
#> [1] 0.9999756
```
