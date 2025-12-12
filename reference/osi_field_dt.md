# Calculate the OSI score for one field using a data.table as input

This functions wraps the functions of the euosi into one main function
to calculate the soil quality score for a single field. Whereas the
function \`osi_field\` requires all variables as separate inputs, the
function \`osi_field_dt\` allows one to send in a data.table

## Usage

``` r
osi_field_dt(dt, output = "all")
```

## Arguments

- dt:

  (data.table) A data.table with all input required to calculate BLN on
  field level

- output:

  (character) An optional argument to select output: scores, indicators,
  or all. (default = all)
