# Calculate the potassium excess index in Denmark

This function calculates the potassium excess.

## Usage

``` r
osi_nut_k_dk(B_LU, A_K_AL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (numeric) The crop code

- A_K_AL:

  (numeric) The K-content of the soil extracted with ammonium lactate
  (mg K / kg)

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium excess index in Denmark derived from extractable soil K
fractions. A numeric value.

## Examples

``` r
osi_nut_k_dk(B_LU = '3301000000',A_K_AL = 5)
#> [1] 0.9733443
osi_nut_k_dk(B_LU = c('3301000000','3301061299'),A_K_AL = c(3.5,5.5))
#> [1] 0.9737556 0.9732058
```
