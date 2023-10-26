#' Calculate the magnesium availability index (wrapper function)
#' 
#' This function calculates the magnesium availability for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_MG_CC (numeric) The plant available content of Mg in the soil (mg  Mg per kg) extracted by 0.01M CaCl2
#' @param A_CEC_CO (numeric) is the Cation exhange capacity in mmol+/kg 
#' @param A_CLAY_MI (numeric) is the Clay content in %
#' @param A_MG_EX (numeric) is the exchangeable Mg concentration (mg/kg)
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_K_CC (numeric) The plant available potassium, extracted with 0.01M CaCl2 (mg per kg)
#' @param A_CACO3_IF (numeric) the % of CaCO3

#' 
#' @import data.table
#' 
#' @examples 

#' @return
#' The capacity of the soil to supply and buffer potassium, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_c_magnesium <- function(B_LU, B_SOILTYPE_AGR,A_SOM_LOI, A_CLAY_MI,A_PH_CC, A_CACO3_IF,
                            A_CEC_CO, A_K_CO_PO, A_K_CC,B_COUNTRY,A_MG_EX) {
  
  # add visual bindings
  i_c_mg = NULL
  
  # desired length of inputs
  arg.length <- max(length(A_MG_CC), length(A_PH_CC), length(A_SOM_LOI), length(A_CEC_CO), 
                     length(A_K_CO_PO), length(A_CLAY_MI), length(B_SOILTYPE_AGR), length(B_LU),
                    length(B_COUNTRY),length(A_MG_EX),length(A_CACO3_IF))
  
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
                   A_CACO3_IF = A_CACO3_IF
                   value = NA_real_
  )
  
  # calculate the open soil index score for magnesium availability per country
  dt[B_COUNTRY == 'NL', i_c_k := osi_c_magnesium_nl(B_LU_BRP,B_SOILTYPE_AGR,A_SOM_LOI,A_CLAY_MI,
                                                    A_PH_CC, A_CEC_CO,A_K_CO_PO,A_MG_CC,A_K_CC)]
  dt[B_COUNTRY == 'FR', i_c_k := osi_c_magnesium_fr(A_CLAY_MI = A_CLAY_MI,
                                                    A_CEC_CO = A_CEC_CO, A_MG_EX = A_MG_EX,
                                                    A_CACO3_IF = A_CACO3_IF)]
  
  # select the output variable
  out <- dt[,i_c_mg]
  
  # return the OSI score
  return(out)
  
}

#' Calculate the Mg availability index for the Netherlands 
#' 
#' This function calculates the Mg availability of a soil, using the agronomic index used in the Netherlands.
#' 
#' 
#' #' Calculate the Mg availability index for France 
#' 
#' This function calculates the Mg availability of a soil, using the agronomic index used in France
#' 
#' @param A_CLAY_MI (numeric) % clay
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil in mmol/kg
#' @param A_CACO3_IF (numeric) The CaCO3 content in the soil (%)
#' @param A_MG_EX (numeric) The Mg content in the soil (mg/kg)
#' 
#' @import data.table
#' 
#' @examples 
#' A_CLAY_MI  = 45
#' A_CEC_CO = 20
#' A_CACO3_IF = 0
#' A_MG_EX = 15
#' 
#' 
#' @return 
#' The magnesium availability index in France estimated from extractable magnesium. A numeric value.
#' 
#' @export

osi_c_magnesium_fr <- function(A_CLAY_MI, A_CEC_CO, A_CACO3_IF, A_MG_EX) {
  
  # set visual bindings
  i_c_mg = osi_country = osi_indicator = id = crop_cat1 = NULL
  
  # Load in the threshold data set and the parms dataset
  dt.parms <- as.data.table(euosi::osi_parms)
  dt.thresholds <- as.data.table(euosi::osi_thresholds)

    # subset thresholds to French situation for Magnesium
  soil_cat_mg <- ifelse(A_CEC_CO < 7,'Sand',
                          ifelse(A_CEC_CO > 7 & A_CEC_CO < 12,'Loam',
                                 ifelse(A_CACO3_IF>0, 'Clay Calcareous',
                                       'Clay')))
  dt.thresholds <- dt.thresholds[dt.thresholds$osi_country=='FR' & dt.thresholds$osi_indicator=='i_c_mg' & dt.thresholds$osi_threshold_soilcat==soil_cat_mg]
  
  
  # Check length of desired input
  arg.length <- max(length(A_CLAY_MI),length(A_CEC_CO),length(A_CACO3_IF),length(A_MG_EX))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_numeric(A_MG_EX, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)

  # check that there are only 1 scoring function for K
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_MG_EX = A_MG_EX,
                   value = A_MG_EX)
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # convert to the OSI score
  dt[,i_c_mg := osi_evaluate_logistic(x = value, b= dt.thresholds$osi_st_c1,x0 = dt.thresholds$osi_st_c2,v = dt.thresholds$osi_st_c3)]
  
  # return value
  value <- dt[, i_c_mg]
  
  return(value)
  
}

#' 