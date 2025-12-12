# Test euosi input variables for the correct input values or options

This function selects the correct boundary values (min and max) or
possible options for a given parameter

## Usage

``` r
osi_checkvarfun(parm, fname = NULL, na_allowed = FALSE, unitcheck = TRUE)
```

## Arguments

- parm:

  (list) The osi parameter - value combination to be checked

- fname:

  (character) The name of the function where the check is done

- na_allowed:

  (boolean) Are NA allowed in categorial inputs (TRUE or FALSE)

- unitcheck:

  (boolean) Option to switch off unit checks (TRUE or FALSE)

## Value

warning or error messages when parameter value is beyond the allowed
range
