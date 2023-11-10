#' Calculate the boron availability index (wrapper function)
#' 
#' This function calculates the boron availability for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_BO_HW (numeric) The plant available content of B in the soil (mg  Cu per kg) extracted by hot water
#' @param A_CLAY_MI (numeric) The clay content %

#' 
#' @import data.table
#' 
#' @examples 

#' @return
#' The capacity of the soil to supply and buffer boron, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_c_boron <- function(A_CLAY_MI,A_BO_HW, B_COUNTRY,B_LU) {
  
  # add visual bindings
  i_c_bo = NULL
  
  # desired length of inputs
  arg.length <- max(length(A_CLAY_MI), length(A_BO_HW), length(B_LU))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI = A_CLAY_MI,
                   A_BO_HW = A_BO_HW,
                   B_LU = B_LU,
                   value = NA_real_
  )
  
  # calculate the open soil index score for magnesium availability per country
  dt[B_COUNTRY == 'FR', i_c_bo := osi_c_boron_fr(A_CLAY_MI = A_CLAY_MI,B_LU,
                                                 A_BO_HW = A_BO_HW)]
  
  # select the output variable
  out <- dt[,i_c_bo]
  
  # return the OSI score
  return(out)
  
}


#' 
#' #' Calculate the B availability index for France 
#' 
#' This function calculates the B availability of a soil, using the agronomic index used in France
#' 
#' @param A_BO_HW (numeric) The plant available content of B in the soil (mg B per kg) extracted by hot water 
#' @param A_CLAY_MI (numeric) The clay content %
#' @param B_LU (character) The crop type
#'
#' @import data.table
#' 
#' @examples 
#' 
#' 
#' @return 
#' The boron availability index in France estimated from extractable boron, clay, A numeric value.
#' 
#' @export

osi_c_boron_fr <- function(A_CLAY_MI, A_BO_HW, B_LU) {
  
  # set visual bindings
  i_c_bo = osi_country = osi_indicator = id = crop_cat1 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # Load in the threshold data set and the parms dataset
  dt.parms <- as.data.table(euosi::osi_parms)
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  
  # subset thresholds to French situation for Magnesium
  soil_cat_bo <- ifelse(A_CLAY_MI>50,'Clay','Other')
  dt.thresholds <- dt.thresholds[dt.thresholds$osi_country=='FR' & dt.thresholds$osi_indicator=='i_c_bo' & 
                                   dt.thresholds$osi_threshold_soilcat==soil_cat_bo]
  
  
  # Check length of desired input
  arg.length <- max(length(A_CLAY_MI),length(A_BO_HW),length(B_LU))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_numeric(A_BO_HW, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)

  
  # check that there are only 1 scoring function for K
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_BO_HW = A_BO_HW,A_CLAY_MI=A_CLAY_MI,B_LU=B_LU,
                   value = A_BO_HW)
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # convert to the OSI score
  i_c_bo <- ifelse(B_LU=='DFV'|B_LU=='TRN'|B_LU=='FVL', osi::evaluate_logistic(x = dt$value, b= dt.thresholds$osi_st_c1,x0 = dt.thresholds$osi_st_c2,v = dt.thresholds$osi_st_c3),1)
  
  return(value)
  
}

#' 