# Calculate the bulk density

This pedotransfer function calculates the bulk density of the soil based
on texture and organic matter

## Usage

``` r
osi_p_density(A_SOM_LOI, A_CLAY_MI, pwarning = FALSE)
```

## Arguments

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The bulk density of an arable soil (kg / m3) evaluated given the maximum
density that limit the root penetration.

## Examples

``` r
osi_p_density(A_SOM_LOI = 6.5, A_CLAY_MI = 28)
#> [1] 0.9466822
osi_p_density(A_SOM_LOI = 3.5, A_CLAY_MI = 2)
#> [1] 0.8395679
osi_p_density(A_SOM_LOI = c(3.5,8.5),A_CLAY_MI = c(2,28))
#> [1] 0.8395679 0.9618062
```
