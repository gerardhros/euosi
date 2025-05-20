# tables with supportive information

#' Table with country codes used in the package
#'
#' This table contains country names as used for the spatial extent where sptfs have been derived from.
#'
#' @format A data.table with 249 rows and 3 columns:
#' \describe{
#'   \item{country_name}{the name of the country}
#'   \item{country_code}{the country code;from https://datahub.io/core/country-codes}
#'}
"osi_countries"

#' Table with OSI parameters being used in the package
#' 
#' This table contains all parameters being used in the OSI package to calculate the soil quality index.
#' 
#' @format A data.table with x rows and x columns:
#' \describe{
#'   \item{osi_parm_id}{the parameter id}
#'   \item{osi_parm_name}{the name of the parameter}
#'   \item{osi_parm_type}{the type of the parameter. Options: measurement, field property}
#'   \item{osi_parm_description}{a short description of the parameters}
#'   \item{osi_parm_unit}{the unit of the parameter}
#'   \item{osi_parm_min}{the maximum allowed value for the parameter}
#'   \item{osi_parm_max}{the minimum allowed value for the parameter}
#'   \item{osi_parm_data_type}{the data type of the parameter: numeric, character or boolean}
#'   \item{osi_parm_enum}{does the parameter have predefined options}
#'   \item{osi_parm_options}{allowed options for the parameteer}
#'}
"osi_parms"

#' Table with country specific soil types being used to evaluate soil quality
#' 
#' This table contains categories of soil types being used to evaluate soil quality. Categories vary per country.
#' 
#' @format A data.table with 9 rows and 4 columns:
#' \describe{
#'   \item{osi_soil_id}{the soil category id}
#'   \item{osi_country}{the name of the country where the soil category is used}
#'   \item{osi_soil_cat1}{soil category 1, might be in the language of the country}
#'   \item{osi_soil_cat2}{soil category 2, in english}
#'}
"osi_soiltype"

#' Table with country specific threshold values to evaluate soilquality'
#'  
#' This table contains the evaluation coefficients to convert a soil function value into a soil quality score ranging from 0 to 1. 
#' The scoring function migh vary per country, indicator and subcatogries defined by variable categories of soil and crop types.
#' 
#' @format A data.table with x rows and 12 columns:
#' \describe{
#'   \item{osi_trh_id}{the threshold category id}
#'   \item{osi_country}{the country name where the threshold coefficients apply}
#'   \item{osi_esd}{the soil ecosystem service for which the threshold applies}
#'   \item{osi_category}{the soil quality category: chemical, biological, physical, other}
#'   \item{osi_indicator}{the osi indicator for which the threshold applies}
#'   \item{osi_indicator_name}{the osi indicator name}
#'   \item{osi_threshold_cropcat}{the osi crop category for which specific thresholds apply. if empy, then the threshold is generally applicable}
#'   \item{osi_threshold_soilcat}{the osi soil category for which specific thresholds apply. if empy, then the threshold is generally applicable}
#'   \item{osi_scoringtype}{the soil evaluation function to score the soil function. Options: parabolic, logistic, linear}
#'   \item{osi_st_c1}{the first coefficient of the scoring function selected}
#'   \item{osi_st_c2}{the second coefficient of the scoring function selected}
#'   \item{osi_st_c3}{the third coefficient of the scoring function selected}
#'}
"osi_thresholds"

#' Linking table between crops and different functions in OBIC
#' 
#' This table helps to link the different crops in the OBIC functions with the crops selected by the user
#' 
#' @format A data.frame with 465 rows and 3 columns:
#' \describe{
#'   \item{crop_code}{The BRP gewascode of the crop}
#'   \item{crop_name}{The name of the crop, in lower case}
#'   \item{crop_cat1}{Classification of crop per land use type (arable, maize, grass, nature)}
#'   \item{osi_country}{the country name where the crop codes are applicable}
#' }
"osi_crops"

#' Averaged climatic conditions per country
#' 
#' @format A data.frame with 37 rows and 10 columns:
#' \describe{
#'   \item{osi_country}{the country name}
#'   \item{b_prec_y}{the total annual precipitation, ERA5 (mm)}
#'   \item{b_prec_sum}{the total summer precipitation, ERA5 (mm)}
#'   \item{b_prec_win}{the total winter precipitation, ERA5 (mm)}
#'   \item{b_pet_y}{the total annual evaporation, ERA5 (mm)}
#'   \item{b_pet_sum}{the total summer evaporation, ERA5 (mm)}
#'   \item{b_pet_win}{the total winter evaporation, ERA5 (mm)} 
#'   \item{b_temp_y}{the mean annual temperature, ERA5 (degrees C)}
#'   \item{b_temp_sum}{the mean summer temperature, ERA5 (degrees C)}
#'   \item{b_temp_win}{the mean winter temperature, ERA5 (degrees C)} 
#' }
"osi_clim"