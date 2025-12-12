# Calculate a biodiversity index

This function calculates the biodiversity index bsed on soil organic
matter and pH.

## Usage

``` r
osi_biodiversity(A_SOM_LOI, A_PH_CC, pwarning = FALSE)
```

## Arguments

- A_SOM_LOI:

  (numeric) The percentage organic matter in the soil (%)

- A_PH_CC:

  (numeric) The pH measured in CaCl2 solution

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  for input checks (TRUE or FALSE)

## Value

The biodiversity index. A numeric value.

## Examples

``` r
osi_biodiversity(A_SOM_LOI = 3,A_PH_CC = 4.5)
#> [1] 0.886244
```
