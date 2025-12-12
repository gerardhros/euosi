# Calculate the soil nitrogen supplying capacity in Belgium

This function calculates the NSC (nitrogen supply capacity) for the soil

## Usage

``` r
osi_c_nitrogen_be(
  B_LU,
  A_N_RT,
  A_C_OF,
  A_CLAY_MI,
  A_SAND_MI,
  A_CACO3_IF,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (character) The crop code

- A_N_RT:

  (numeric) The organic nitrogen content of the soil (mg N / kg)

- A_C_OF:

  (numeric) The organic carbon content in the soil (g C / kg)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_SAND_MI:

  (numeric) The sand content of the soil (%)

- A_CACO3_IF:

  (numeric) The percentage of carbonated lime (%)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric
value, converted to a OSI score.

## Examples

``` r
osi_c_nitrogen_be(B_LU = '9823',A_N_RT = 1200, A_C_OF = 25, 
A_CLAY_MI = 3.5, A_SAND_MI = 15,A_CACO3_IF = 0.8)
#> [1] 0.8048873
```
