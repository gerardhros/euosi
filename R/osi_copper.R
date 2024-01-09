#' Calculate the copper availability index (wrapper function)
#' 
#' This function calculates the copper availability for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_CLAY_MI (numeric) The clay content (\%)
#' @param B_CF (numeric) The percentage of coarse fragments (\%)
#' @param A_SOM_LOI (numeric) The organic matter content (\%)
#' @param A_PH_WA (numeric) The acidity of the soil, measured in water (-)
#' @param A_CACO3_IF (numeric) The carbonate content (\%)
#' @param A_CU_CC (numeric) The plant available content of Cu in the soil (mg  Cu per kg) extracted by 0.01M CaCl2
#' @param A_CU_RT (numeric) The pseudo-total content of Cu in the soil (mg Cu per kg) extracted by Aqua regia
#' @param A_CU_EDTA (numeric) The plant available content of Cu in the soil (mg Cu per kg) extracted by EDTA 
#' @param B_COUNTRY (character) The country code
#'  
#' @import data.table
#' 
#' @return
#' The capacity of the soil to supply and buffer copper, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_c_copper <- function(B_LU,A_CLAY_MI = NA_real_,B_CF= NA_real_,A_SOM_LOI= NA_real_,A_PH_WA= NA_real_,
                         A_CACO3_IF= NA_real_,A_CU_RT= NA_real_,A_CU_EDTA= NA_real_,A_CU_CC= NA_real_, B_COUNTRY) {
  
  # add visual bindings
  i_c_cu = NULL
  
  # desired length of inputs
  arg.length <- max(length(B_LU), length(A_PH_WA), length(A_CU_EDTA),length(A_SOM_LOI),length(A_CLAY_MI))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_CACO3_IF = A_CACO3_IF,
                   A_PH_WA = A_PH_WA,
                   B_CF = B_CF,
                   A_CU_EDTA = A_CU_EDTA,
                   A_CU_CC = A_CU_CC,
                   A_CU_RT = A_CU_RT,
                   B_COUNTRY = B_COUNTRY,
                   value = NA_real_
                   )
  
  # calculate the open soil index score for magnesium availability per country
  dt[B_COUNTRY == 'FR', value := osi_c_copper_fr(B_LU = B_LU,
                                                 A_CLAY_MI = A_CLAY_MI,
                                                 B_CF = B_CF,
                                                 A_SOM_LOI=A_SOM_LOI,
                                                 A_CU_EDTA = A_CU_EDTA)]
  
  # select the output variable
  value <- dt[,value]
  
  # return the OSI score
  return(value)
  
}


#' Calculate the Cu availability index for France 
#' 
#' This function calculates the Cu availability of a soil, using the agronomic index used in France
#' 
#' @param B_LU (numeric) The crop code
#' @param A_CLAY_MI (numeric) The clay content (\%)
#' @param B_CF (numeric) The percentage of coarse fragments (\%)
#' @param A_SOM_LOI (numeric) The organic matter content (\%)
#' @param A_CU_EDTA (numeric) The plant available content of Cu in the soil (mg Cu per kg) extracted by EDTA 
#' 
#' @import data.table
#' 
#' @return 
#' The copper availability index in France estimated from extractable copper, clay, coarse fragments and soil organic matter, A numeric value.
#' 
#' @export
osi_c_copper_fr <- function(B_LU,A_CLAY_MI,B_CF, A_SOM_LOI,A_CU_EDTA) {
  
  # set visual bindings
  i_c_cu = osi_country = osi_indicator = id = crop_cat1 = value = NULL
  soil_cat_cu = osi_st_c1 = osi_st_c2 = osi_st_c3 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # Load in parms dataset (to be used later for upper and lowe rlimits)
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # load the threshold values
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='FR' & osi_indicator=='i_c_cu']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_CLAY_MI),length(A_CU_EDTA),length(A_SOM_LOI),length(B_CF))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_CU_EDTA, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_data_table(dt.thresholds,max.rows = 3)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI=A_CLAY_MI,
                   A_SOM_LOI=A_SOM_LOI,
                   A_CU_EDTA = A_CU_EDTA,
                   value = NA_real_)
  
  # set soil class for merging with threshold
  dt[,soil_cat_cu := fifelse(A_CLAY_MI>50,'clay',fifelse(B_CF>35,'stony loam','loam'))]
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = 'soil_cat_cu',
              by.y = 'osi_threshold_soilcat',
              all.x = TRUE)
  
  # convert to the OSI score
  dt[, value := evaluate_logistic(x = A_CU_EDTA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select and return value
  value <- dt[,value]
  
  return(value)
  
}