#' Calculate the boron availability index (wrapper function)
#' 
#' This function calculates the boron availability for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_B_HW (numeric) The plant available content of B in the soil (mg  Cu per kg) extracted by hot water
#' @param A_CLAY_MI (numeric) The clay content (\%)
#' @param B_COUNTRY (character) The country code
#' 
#' @import data.table
#' 
#' @return
#' The capacity of the soil to supply and buffer boron, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_c_boron <- function(B_LU,A_CLAY_MI,A_B_HW, B_COUNTRY) {
  
  # add visual bindings
  i_c_bo = NULL
  
  # desired length of inputs
  arg.length <- max(length(A_CLAY_MI), length(A_B_HW), length(B_LU))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI = A_CLAY_MI,
                   A_B_HW = A_B_HW,
                   B_LU = B_LU,
                   value = NA_real_
                   )
  
  # calculate the open soil index score for magnesium availability per country
  dt[B_COUNTRY == 'FR', value := osi_c_boron_fr(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI,A_B_HW = A_B_HW)]
  
  # select the output variable
  value <- dt[,value]
  
  # return the OSI score
  return(value)
  
}



#' Calculate the B availability index for agricultural soils in France 
#' 
#' This function calculates the B availability of a soil, using the agronomic index used in France
#' 
#' @param B_LU (character) The crop type
#' @param A_CLAY_MI (numeric) The clay content (\%)
#' @param A_B_HW (numeric) The plant available content of B in the soil (mg B per kg) extracted by hot water 
#'
#' @import data.table
#' 
#' @return 
#' The boron availability index in France estimated from extractable boron, clay, A numeric value.
#' 
#' @export
osi_c_boron_fr <- function(B_LU,A_CLAY_MI, A_B_HW) {
  
  # set visual bindings
  i_c_bo = osi_country = osi_indicator = id = crop_cat1 = NULL
  soil_cat_bo = osi_st_c1 = osi_st_c2 = osi_st_c3 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # Load in parms dataset (to be used later for upper and lowe rlimits)
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # load the threshold values
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='FR' & osi_indicator=='i_c_b']
  
  # Check length of desired input
  arg.length <- max(length(A_CLAY_MI),length(A_B_HW),length(B_LU))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_B_HW, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_data_table(dt.thresholds,max.rows = 2)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_B_HW = A_B_HW,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # set soil class for merging with threshold
  dt[, soil_cat_bo := fifelse(A_CLAY_MI > 50,'clay','other')]
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = 'soil_cat_bo',
              by.y = 'osi_threshold_soilcat',
              all.x = TRUE)
  
  # convert to the OSI score
  dt[, value := evaluate_logistic(x = A_B_HW, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # exlcude other crops that a subset
  dt[!B_LU %in% c('DFV','TRN','FVL'), value := 1]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select and return value
  value <- dt[,value]
  
  return(value)
  
}