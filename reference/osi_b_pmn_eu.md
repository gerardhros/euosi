# Calculate the index for the microbial biological activity across the EU

This function assesses the microbial biological activity (of microbes
and fungi) via the Potentially Mineralizable N pool, also called PMN (or
SoilLife by Eurofins in the past).

## Usage

``` r
osi_b_pmn_eu(
  B_LU,
  A_N_RT,
  A_CLAY_MI,
  B_COUNTRY = NA_character_,
  unitcheck = TRUE
)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_N_RT:

  (numeric) The total N content of the soil(mg N / kg soil)

- A_CLAY_MI:

  (numeric) The aclay content of the soil (%)

- B_COUNTRY:

  (character) The country code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

the normalized potentially mineralizable Nitrogen pool (mg N / kg), a
numeric value, converted to an OSI score.

## Examples

``` r
osi_b_pmn_eu(B_LU = '256', A_CLAY_MI = 4.5, A_N_RT = 1250)
#> [1] 0.9999881
osi_b_pmn_eu(B_LU = c('256','1027'),A_CLAY_MI = c(4,48), A_N_RT = c(3125,1450))
#> [1] 1.0000000 0.9999926
```
