#' Calculate the phosphate surplus (wrapper function)
#' 
#' This function calculates the phosphate surplus for all European countries (if available). 
#' 
#' @param B_LU (character) The crop code

#' 
#' @import data.table
#' 
#' @examples 
#' 
#' 
#' 
#' @export
osi_c_posphor <- function(B_LU, 
                          B_SOILTYPE_AGR = NA_character_, A_P_OL = NA_real_,
                          A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_, B_COUNTRY) {
  
  # add visual bindings
  A_P_OL = value = id = . = NULL
  
  # note that qualitative checks on the inputs are done by the country specific functions
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_AL),length(A_P_CC),length(A_P_WA),length(B_COUNTRY),length(A_PH_WA))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA,
                   A_P_OL = A_P_OL,
                   B_COUNTRY = B_COUNTRY,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_
  )
  
  # calculate the open soil index score for phosphor availability in the Netherlands
  dt[B_COUNTRY == 'NL', value := osi_e_posphor_nl(B_LU = B_LU, A_P_AL = A_P_AL, A_P_CC = A_P_CC, A_P_WA = A_P_WA)]
  
  # calculate the open soil index score for phosphor availability in France
  dt[B_COUNTRY == 'FR', value := osi_e_posphor_fr(B_LU = B_LU, A_PH_WA = A_PH_WA, A_P_OL= A_P_OL)]
  
  # sort data.table
  setorder(dt,id)
  
  # select the output variable
  out <- dt[,value]
  
  # return the OSI score
  return(out)
  
}


#' Calculate the phosphate surplus index in the Netherlands
#' 
#' This function calculates the phosphate surplus 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2
#' @param A_P_WA (numeric) The P-content of the soil extracted with water
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_posphor_nl(B_LU = 265, A_P_AL = 45, A_P_CC = 2.5)
#' osi_c_posphor_nl(B_LU = c(265,1019),A_P_AL = c(35,54),A_P_CC = c(2.5,4.5), A_P_WA = c(35,65))
#' 
#'
#' Calculate the phosphate surplus index in France
#' 
#' @param B_LU (character) The crop code
#' @param B_SOILTYPE_AGR (character) The soil type in a particular region
#' @param B_AER_FR (character) An agroeconomic region in France
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen
#'  
#' @import data.table
#' 
#' @examples 
#' osi_e_posphor_fr(B_LU = 'SOJ', A_P_OL = 45, 
#' 
#' @return 
#' The phosphate availability index in France estimated from extractable soil P Olsen (a numeric value).
#' 
#' @export
osi_e_posphor_fr <- function(B_LU, A_PH_WA, A_P_OL = NA_real_) {
  
  # set visual bindings
  i_e_p = osi_country = osi_indicator = id = crop_cat1 = NULL
  
  # Load in the crops data set and the parms dataset
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.parms <- as.data.table(euosi::osi_parms)
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  
  # subset crops dataset to French situation 
  dt.crops <- dt.crops[dt.crops$crop_code==B_LU,]
  B_SOILTYPE_AGR <- ifelse(A_PH_WA>8,'craie','general')
  
  # subset thresholds to French situation for phosphorus
  dt.thresholds <- dt.thresholds[dt.thresholds$osi_country=='FR' & dt.thresholds$osi_indicator=='i_e_p' & 
                                   dt.thresholds$osi_threshold_cropcat==dt.crops$crop_phosphate &
                                   dt.thresholds$osi_threshold_soilcat==B_SOILTYPE_AGR,] 
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_OL),length(A_PH_WA))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_numeric(A_P_OL, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(dt.soiltype$soil_cat1), empty.ok = FALSE)
  
  # check that there are only 1 scoring function for P
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_P_OL = A_P_OL,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   value = A_P_OL)
  
  dt <- merge(dt,dt.crops,by.x = 'B_LU', by.y = 'crop_code',all.x=TRUE)
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # convert to the OSI score
  dt[,i_e_p := osi_evaluate_logistic(x = value, b= dt.thresholds$osi_st_c1,x0 = dt.thresholds$osi_st_c2,v = dt.thresholds$osi_st_c3)]
  
  # return value
  value <- dt[, i_c_p]
  
  return(value)
  
}
