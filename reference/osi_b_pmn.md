# Calculate the index for the microbial biological activity (wrapper function)

This function assesses the microbial biological activity (of microbes
and fungi) via the Potentially Mineralizable N pool, also called PMN (or
SoilLife by Eurofins in the past) for all European countries (if
available).

## Usage

``` r
osi_b_pmn(
  B_LU,
  B_SOILTYPE_AGR,
  A_CLAY_MI = NA_real_,
  A_N_PMN = NA_real_,
  A_N_RT = NA_real_,
  B_COUNTRY,
  pwarning = FALSE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- B_SOILTYPE_AGR:

  (character) The agricultural type of soil

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- A_N_PMN:

  (numeric) The potentially mineralizable N pool (mg N / kg soil)

- A_N_RT:

  (numeric) The organic nitrogen content of the soil in mg N / kg

- B_COUNTRY:

  (character) The country code

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

the normalized potentially mineralizable Nitrogen pool (mg N / kg), a
numeric value, converted to an OSI score.

## Examples

``` r
osi_b_pmn(B_LU = '256', B_SOILTYPE_AGR = 'dekzand', A_N_PMN = 125, B_COUNTRY = 'NL')
#> [1] 0.9999999
osi_b_pmn(B_LU = c('256','1027'), B_SOILTYPE_AGR = c('dekzand','rivierklei'), 
A_N_PMN = c(125,45),B_COUNTRY = c('NL','NL'))
#> [1] 0.9999999 0.7174464
```
