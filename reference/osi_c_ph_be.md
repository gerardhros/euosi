# Calculate the pH index for Belgium

This function evaluates the pH index in Belgium

## Usage

``` r
osi_c_ph_be(B_LU, B_TEXTURE_BE, A_PH_KCL, unitcheck = TRUE)
```

## Arguments

- B_LU:

  (character) The crop type

- B_TEXTURE_BE:

  (character) The soil texture according to Belgium classification
  system

- A_PH_KCL:

  (numeric) The pH measured in KCl

- unitcheck:

  (character) Option to switch off unit checks (TRUE or FALSE)

## Value

The pH index in Belgium estimated from pH in KCL, the textural class and
the crop type
