# Calculate the soil nitrogen supplying capacity in the Netherlands

This function calculates the NLV (nitrogen producing capacity) for the
soil

## Usage

``` r
osi_c_nitrogen_nl(B_LU, B_SOILTYPE_AGR, A_SOM_LOI, A_N_RT, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code from the BRP

- B_SOILTYPE_AGR:

  (character) The agricultural type of soil

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil

- A_N_RT:

  (numeric) The organic nitrogen content of the soil in mg N / kg

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric
value, converted to a OSI score.

## Examples

``` r
osi_c_nitrogen_nl(B_LU = '256', B_SOILTYPE_AGR = 'dekzand',
A_SOM_LOI = 4.5,A_N_RT = 2500)
#> [1] 1
osi_c_nitrogen_nl(1019,'dekzand',5.5,2315)
#> [1] 1
```
