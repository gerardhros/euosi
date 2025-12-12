# Calculate the potassium excess index in Switzerland

This function calculates the potassium excess.

## Usage

``` r
osi_nut_k_ch(A_K_AAA, A_CLAY_MI, B_LU = NA_character_, unitcheck = TRUE)
```

## Arguments

- A_K_AAA:

  (numeric) The exchangeable K-content of the soil measured via acid
  ammonium acetate extraction (mg K/ kg)

- A_CLAY_MI:

  (numeric) The clay content of the soil (%)

- B_LU:

  (character) The crop code

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The potassium excess index in Switzerland estimated from extractable
potassium. A numeric value.

## Examples

``` r
osi_nut_k_ch(B_LU = '8410',A_K_AAA = 50, A_CLAY_MI = 4.5)
#> [1] 0.9602221
```
