#' Calculate the copper availability index (wrapper function)
#' 
#' This function calculates the copper availability for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_CU_CC (numeric) The plant available content of Cu in the soil (mg  Cu per kg) extracted by 0.01M CaCl2
#' @param A_CU_RT (numeric) The pseudo-total content of Cu in the soil (mg Cu per kg) extracted by Aqua regia
#' @param A_PH_WA (numeric) The acidity of the soil, measured in water (-)
#' @param A_CU_ED (numeric) The plant available content of Cu in the soil (mg Cu per kg) extracted by EDTA 
#' @param A_CACO3_IF (numeric) The carbonate content %
#' @param A_CLAY_MI (numeric) The clay content %
#' @param A_SOM_LOI (numeric) The organic matter content %
#' @param B_CF (numeric) The % of coarse fragments

#' 
#' @import data.table
#' 
#' @examples 

#' @return
#' The capacity of the soil to supply and buffer copper, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_c_copper <- function(A_CLAY_MI,A_SOM_LOI,A_CU_ED, B_COUNTRY) {
  
  # add visual bindings
  i_c_cu = NULL
  
  # desired length of inputs
  arg.length <- max(length(B_LU), length(A_PH_WA), length(A_Cu_ED))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   A_CU_ED = A_CU_ED,
                   B_COUNTRY = B_COUNTRY,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_CACO3_IF = A_CACO3_IF,
                   value = NA_real_
  )
  
  # calculate the open soil index score for magnesium availability per country
  dt[B_COUNTRY == 'FR', i_c_cu := osi_c_copper_fr(A_CLAY_MI = A_CLAY_MI,A_SOM_LOI=A_SOM_LOI,
                                               A_CU_ED = A_CU_ED, A_CACO3_IF=A_CACO3_IF)]
  
  # select the output variable
  out <- dt[,i_c_cu]
  
  # return the OSI score
  return(out)
  
}

#' Calculate the Cu availability index for the Netherlands 
#' 
#' This function calculates the Cu availability of a soil, using the agronomic index used in the Netherlands.
#' 
#' 
#' #' Calculate the Cu availability index for France 
#' 
#' This function calculates the Cu availability of a soil, using the agronomic index used in France
#' 
#' @param A_CU_ED (numeric) The plant available content of Cu in the soil (mg Cu per kg) extracted by EDTA 
#' @param A_CLAY_MI (numeric) The clay content %
#' @param A_SOM_LOI (numeric) The organic matter content %
#' @param B_CF (numeric) The % of coarse fragments 
#'
#' @import data.table
#' 
#' @examples 
#' 
#' 
#' @return 
#' The copper availability index in France estimated from extractable copper, clay, coarse fragments and soil organic matter, A numeric value.
#' 
#' @export

osi_c_copper_fr <- function(A_CLAY_MI, A_CU_ED, A_SOM_LOI,B_CF) {
  
  # set visual bindings
  i_c_cu = osi_country = osi_indicator = id = crop_cat1 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # Load in the threshold data set and the parms dataset
  dt.parms <- as.data.table(euosi::osi_parms)
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  
  # subset thresholds to French situation for Magnesium
  soil_cat_cu <- ifelse(A_CLAY_MI<50,'Clay',
                        ifelse(B_CF>35,'Stony Loam','Loam'))
  dt.thresholds <- dt.thresholds[dt.thresholds$osi_country=='FR' & dt.thresholds$osi_indicator=='i_c_cu' & dt.thresholds$osi_threshold_soilcat==soil_cat_cu]
  
  
  # Check length of desired input
  arg.length <- max(length(A_CLAY_MI),length(A_CU_ED),length(A_SOM_LOI))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_numeric(A_CU_ED, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  
  
  # check that there are only 1 scoring function for K
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_CU_ED = A_CU_ED,A_CLAY_MI=A_CLAY_MI,A_SOM_LOI=A_SOM_LOI,
                   value = A_CU_ED/A_SOM_LOI)
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # convert to the OSI score
  i_c_cu <- osi::evaluate_logistic(x = dt$value, b= dt.thresholds$osi_st_c1,x0 = dt.thresholds$osi_st_c2,v = dt.thresholds$osi_st_c3)
  
  return(value)
  
}

#' 