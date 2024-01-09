#' Calculate indicator for wind erodibility
#'
#' This function calculates the risk for wind erodibility of soils, derived from Van Kerckhoven et al. (2009) and Ros & Bussink (2013)
#' 
#' @param B_LU (numeric) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#'
#' @examples 
#' osi_p_wef(B_LU = 265, A_CLAY_MI = 4, A_SILT_MI = 15)
#' osi_p_wef(B_LU = c(265,1019), A_CLAY_MI = c(4,18), A_SILT_MI = c(15,65))
#' 
#' @return 
#' The vulnerability of the soil for wind erosion. A numeric value.
#'   
#' @export
osi_p_wef <- function(B_LU,A_CLAY_MI,A_SILT_MI) {
  
  # add visual bindings
  id = crop_code = crop_cat1 = loam = NULL
  
  # ensure B_LU is character
  B_LU <- as.character(B_LU)
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)

  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_
                  )
  
  # add crop names
  dt <- merge(dt, dt.crops[, list(crop_code, crop_cat1)], by.x = "B_LU", by.y = "crop_code",all.x = TRUE)
  
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