# Wrapper function to test euosi input variables for the correct input values or options

This function is a wrapper around the function osi_checkvarfun and
allows the user to print warning messages

## Usage

``` r
osi_checkvar(
  parm,
  fname = NULL,
  na_allowed = FALSE,
  unitcheck = TRUE,
  pwarning = FALSE
)
```

## Arguments

- parm:

  (list) The osi parameter - value combination to be checked

- fname:

  (character) The name of the function where the check is done

- na_allowed:

  (character) Are NA allowed in categorial inputs (TRUE or FALSE)

- unitcheck:

  (boolean) Option to switch off unit checks (TRUE or FALSE)

- pwarning:

  (boolean) Option to print a warning rather than error (stop) message
  (TRUE or FALSE)

## Value

warning or error messages when parameter value is beyond the allowed
range
