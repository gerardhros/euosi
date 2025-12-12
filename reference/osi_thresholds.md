# Table with country specific threshold values to evaluate soilquality' This table contains the evaluation coefficients to convert a soil function value into a soil quality score ranging from 0 to 1. The scoring function migh vary per country, indicator and subcatogries defined by variable categories of soil and crop types.

Table with country specific threshold values to evaluate soilquality'

This table contains the evaluation coefficients to convert a soil
function value into a soil quality score ranging from 0 to 1. The
scoring function migh vary per country, indicator and subcatogries
defined by variable categories of soil and crop types.

## Usage

``` r
osi_thresholds
```

## Format

A data.table with x rows and 12 columns:

- osi_trh_id:

  the threshold category id

- osi_country:

  the country name where the threshold coefficients apply

- osi_esd:

  the soil ecosystem service for which the threshold applies

- osi_category:

  the soil quality category: chemical, biological, physical, other

- osi_indicator:

  the osi indicator for which the threshold applies

- osi_indicator_name:

  the osi indicator name

- osi_threshold_cropcat:

  the osi crop category for which specific thresholds apply. if empy,
  then the threshold is generally applicable

- osi_threshold_soilcat:

  the osi soil category for which specific thresholds apply. if empy,
  then the threshold is generally applicable

- osi_scoringtype:

  the soil evaluation function to score the soil function. Options:
  parabolic, logistic, linear

- osi_st_c1:

  the first coefficient of the scoring function selected

- osi_st_c2:

  the second coefficient of the scoring function selected

- osi_st_c3:

  the third coefficient of the scoring function selected
