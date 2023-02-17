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
#' This table contains all parameters being used in the OSI package to calculate the soil quality idnex.
#' 
#' @format A data.table with x rows and x columns:
#' \describe{
#'   \item{osi_parm_id}{the parameter id}
#'   \item{osi_parm_name}{the name of the parameter}
#'   \item{osi_parm_type}{the type of the parameter. Options: measurement, field property}
#'   \item{osi_parm_description}{a short description of the parameters}
#'   \item{osi_parm_unit}{the unit of the parameter}
#'}
"osi_parms"

