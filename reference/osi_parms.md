# Table with OSI parameters being used in the package

This table contains all parameters being used in the OSI package to
calculate the soil quality index.

## Usage

``` r
osi_parms
```

## Format

A data.table with x rows and x columns:

- osi_parm_id:

  the parameter id

- osi_parm_name:

  the name of the parameter

- osi_parm_type:

  the type of the parameter. Options: measurement, field property

- osi_parm_description:

  a short description of the parameters

- osi_parm_unit:

  the unit of the parameter

- osi_parm_min:

  the maximum allowed value for the parameter

- osi_parm_max:

  the minimum allowed value for the parameter

- osi_parm_data_type:

  the data type of the parameter: numeric, character or boolean

- osi_parm_enum:

  does the parameter have predefined options

- osi_parm_options:

  allowed options for the parameteer
