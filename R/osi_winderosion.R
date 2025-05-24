#' Calculate indicator for wind erodibility
#'
#' This function calculates the risk for wind erodibility of soils, derived from Van Kerckhoven et al. (2009) and Ros & Bussink (2013)
#' 
#' @param B_LU (numeric) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param B_COUNTRY (character) The country code
#' 
#' @examples 
#' osi_p_wef(B_LU = 265, A_CLAY_MI = 4, A_SAND_MI = 15, B_COUNTRY='NL')
#' osi_p_wef(B_LU = c(265,1019), A_CLAY_MI = c(4,18), A_SAND_MI = c(15,65), B_COUNTRY=c('NL','NL'))
#' 
#' @return 
#' The vulnerability of the soil for wind erosion. A numeric value.
#'   
#' @export
osi_p_wef <- function(B_LU,A_CLAY_MI,A_SAND_MI,B_COUNTRY) {
  
  # add visual bindings
  id = crop_code = crop_cat1 = loam = A_SILT_MI = osi_country = NULL
  
  # ensure B_LU is character
  B_LU <- as.character(B_LU)
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)

  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_SAND_MI),length(B_COUNTRY))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = pmax(0,100-A_SAND_MI-A_CLAY_MI),
                   B_COUNTRY = B_COUNTRY,
                   value = NA_real_
                  )
  
  # add crop names
  dt <- merge(dt, 
              dt.crops[, list(crop_code, crop_cat1, osi_country)], 
              by.x = c("B_LU","B_COUNTRY") ,
              by.y = c("crop_code",'osi_country'),
              all.x = TRUE)
  
  # calculate silt + clay = loam content
  dt[,loam := A_CLAY_MI + A_SILT_MI]
  
  # WEF function applicable over range 3-100%
  dt[loam <= 3,loam := 3]
  
  # Evaluate the wind erodibility factor (WEF)
  dt[,value := -0.286 * log(loam) + 1.3264]
  
  # set WEF on zero for all non-arable crops
  dt[crop_cat1 != "arable",value := 0]
  
  # restrict values between 0 and 1
  dt[, value := pmax(pmin(value, 1), 0)]
  
  # Evaluate the wind erodibility factor (WEF)
  dt[,value := 1 - value]
  
  # return Wind Erodibility Factor
  setorder(dt, id)
  value <- dt[, value]
  
  # return
  return(value)
  
}