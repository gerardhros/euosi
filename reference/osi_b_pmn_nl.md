# Calculate the index for the microbial biological activity in the Netherlands

This function assesses the microbial biological activity (of microbes
and fungi) via the Potentially Mineralizable N pool, also called PMN (or
SoilLife by Eurofins in the past).

## Usage

``` r
osi_b_pmn_nl(B_LU, B_SOILTYPE_AGR, A_N_PMN, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- B_SOILTYPE_AGR:

  (character) The agricultural type of soil

- A_N_PMN:

  (numeric) The potentially mineralizable N pool (mg N / kg soil)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

the normalized potentially mineralizable Nitrogen pool (mg N / kg), a
numeric value, converted to an OSI score.

## Examples

``` r
osi_b_pmn_nl(B_LU = '256', B_SOILTYPE_AGR = 'dekzand', A_N_PMN = 125)
#> [1] 0.9999999
osi_b_pmn_nl(c('256','1027'), c('dekzand','rivierklei'), c(125,45))
#> [1] 0.9999999 0.7174464
```
