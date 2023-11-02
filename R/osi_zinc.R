#' Calculate the magnesium availability index (wrapper function)
#' 
#' This function calculates the zinc availability for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_ZN_CC (numeric) The plant available content of Zn in the soil (mg  Zn per kg) extracted by 0.01M CaCl2
#' @param A_ZN_RT (numeric) The pseudo-total content of Zn in the soil (mg Zn per kg) extracted by Aqua regia
#' @param A_PH_WA (numeric) The acidity of the soil, measured in water (-)
#' @param A_ZN_ED (numeric) The plant available content of Zn in the soil (mg Zn per kg) extracted by EDTA 

#' 
#' @import data.table
#' 
#' @examples 

#' @return
#' The capacity of the soil to supply and buffer zinc, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_c_zinc <- function(B_LU, A_PH_WA,A_ZN_ED, B_COUNTRY) {
  
  # add visual bindings
  i_c_zn = NULL
  
  # desired length of inputs
  arg.length <- max(length(B_LU), length(A_PH_WA), length(A_ZN_ED))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_COUNTRY = B_COUNTRY,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   A_CEC_CO = A_CEC_CO,
                   A_K_CO_PO = A_K_CO_PO,
                   A_MG_EX = A_MG_EX,
                   A_CACO3_IF = A_CACO3_IF,
                   value = NA_real_
  )
  
  # calculate the open soil index score for magnesium availability per country
  dt[B_COUNTRY == 'FR', i_c_k := osi_c_zinc_fr(A_PH_WA = A_PH_WA,
                                               A_ZN_ED = A_ZN_ED, B_LU = B_LU)]
  
  # select the output variable
  out <- dt[,i_c_zn]
  
  # return the OSI score
  return(out)
  
}

#' Calculate the Zn availability index for the Netherlands 
#' 
#' This function calculates the Zn availability of a soil, using the agronomic index used in the Netherlands.
#' 
#' 
#' #' Calculate the Zn availability index for France 
#' 
#' This function calculates the Zn availability of a soil, using the agronomic index used in France
#' 
#' @param A_PH_WA (numeric) pH measured in water 
#' @param A_ZN_ED (numeric) Zn content measured in EDTA 
#' @param B_LU  (character) crop type 
#'
#' @import data.table
#' 
#' @examples 
#' 
#' 
#' @return 
#' The zinc availability index in France estimated from extractable zinc and pH measured in water, A numeric value.
#' 
#' @export

osi_c_zinc_fr <- function(A_PH_WA, A_ZN_ED, B_LU) {
  
  # set visual bindings
  i_c_zn = osi_country = osi_indicator = id = crop_cat1 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # Load in the threshold data set and the parms dataset
  dt.parms <- as.data.table(euosi::osi_parms)
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  
  # subset thresholds to French situation for Magnesium
  soil_cat_zn <- ifelse(A_PH_WA < 6.2,'Acid', 'Alkaline')
  dt.thresholds <- dt.thresholds[dt.thresholds$osi_country=='FR' & dt.thresholds$osi_indicator=='i_c_zn' & dt.thresholds$osi_threshold_soilcat==soil_cat_zn]
  
  
  # Check length of desired input
  arg.length <- max(length(A_ZN_ED),length(B_LU))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_numeric(A_ZN_ED, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(osi_crops$crop_code), empty.ok = FALSE)
  
  # check that there are only 1 scoring function for K
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_ZN_ED = A_ZN_ED,B_LU = B_LU,
                   value = A_ZN_ED)
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # convert to the OSI score
  # subset and evaluate for lin and mais soils
  i_c_zn <- ifelse(B_LU=='DLN'|B_LU=='MID'|B_LU=='MIE'|B_LU=='MIS',osi::evaluate_logistic(x = dt$value, b= dt.thresholds$osi_st_c1,x0 = dt.thresholds$osi_st_c2,v = dt.thresholds$osi_st_c3),1)
  
  return(value)
  
}

#' 