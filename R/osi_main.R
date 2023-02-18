#' Calculate the Open Soil Index score for a single field
#' 
#' This functions wraps the functions of the OSI into one main function to calculate the score for Open Soil Index (OBI) for a single field.
#' 
#' @param ID (character) A field id
#' @param B_LU (numeric) a series with crop codes given the crop rotation plan (source: the BRP) 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_COUNTRY (character) The country code
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analyzed via Cobalt-hexamine extraction
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param A_CN_FR (numeric) The carbon to nitrogen ratio (-)
#' @param A_N_PMN (numeric) The potentially mineralizable N pool (mg N / kg soil) 
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_CC (numeric) The plant available P content, extracted with 0.01M CaCl2 (mg / kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / 100 ml soil)
#' @param A_K_CO_PO (numeric) The occupation of the CEC with K (\%)
#' @param A_K_CC (numeric) The plant available K content, extracted with 0.01M CaCl2 (mg / kg)
#' @param A_CA_CO_PO (numeric) The The occupation of the CEC with Ca (\%)
#' @param A_MG_CO_PO (numeric) The The occupation of the CEC with Mg (\%)
#' @param B_COUNTRY (character) The country code
#' @param output (character) An optional argument to select output: obic_score, scores, indicators, recommendations, or all. (default = all)
#' 
#' @details 
#' It is assumed that the crop series is a continuous series in decreasing order of years. So most recent year first, oldest year last.
#' 
#' @import data.table
#' 
#'  
#' @return 
#' The output of the Open Soil Index Calculator for a specific agricultural field. 
#' Depending on the output type, different output objects can be returned.
#' These include the estimated OSI scores (both total and aggregated subscores), the value of the underling indicators as well the possible recommendations to improve the soil quality.
#' The output is always a data.table.
#' 
#' @export
osi_field <- function(B_LU,B_SOILTYPE_AGR,B_COUNTRY,
                      A_SOM_LOI, A_SAND_MI, A_SILT_MI, A_CLAY_MI,A_PH_CC,
                      A_CEC_CO,
                      A_N_RT,A_CN_FR, A_N_PMN,
                      A_P_AL, A_P_CC, A_P_WA,
                      A_CA_CO_PO, A_MG_CO_PO, A_K_CO_PO,A_K_CC,
                      ID = 1, output = 'all') {
  
  # add visual bindings
  i_c_p = i_p_whc = i_p_dens = i_p_wef = i_p_paw = i_p_ksat = i_b_pmn = NULL
  i_c_n = i_c_k = NULL
  
  # define variables used within the function
  
  # combine input into one data.table
  # field properties start with B, soil analysis with A, Soil Visual Assessment ends with BCS and management starts with M
  dt <- data.table(ID = ID,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_COUNTRY = B_COUNTRY,
                   A_SOM_LOI = A_SOM_LOI, 
                   A_SAND_MI = A_SAND_MI, 
                   A_SILT_MI = A_SILT_MI, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_CEC_CO = A_CEC_CO,
                   A_PH_CC = A_PH_CC,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_CN_FR,
                   A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC, 
                   A_P_WA = A_P_WA,
                   A_CA_CO_PO = A_CA_CO_PO, 
                   A_MG_CO_PO = A_MG_CO_PO, 
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_CC = A_K_CC)
  
  # Load in the crops data set
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.soils <- as.data.table(euosi::osi_crops)
  
  # merge relevant properties from package tables
  
  
  # calculate all soil chemical indicators
  dt[,i_c_n := osi_c_nitrogen(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                              A_SOM_LOI = A_SOM_LOI,A_N_RT = A_N_RT,B_COUNTRY = B_COUNTRY)]
  dt[,i_c_p := osi_c_posphor(B_LU = B_LU, A_P_AL = A_P_AL, A_P_CC = A_P_CC, 
                             A_P_WA = A_P_WA, B_COUNTRY)]
  
  dt[,i_c_k := osi_c_potassium(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                               A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,
                               A_PH_CC = A_PH_CC, A_CEC_CO = A_CEC_CO, 
                               A_K_CO_PO = A_K_CO_PO, A_K_CC = A_K_CC,B_COUNTRY = B_COUNTRY)]
   
  # calculate all soil physical indicators
  dt[,i_p_whc := osi_p_whc(A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI,A_SOM_LOI = A_SOM_LOI,type = 'whc')]
  dt[,i_p_paw := osi_p_whc(A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI,A_SOM_LOI = A_SOM_LOI,type = 'paw')]
  dt[,i_p_ksat := osi_p_whc(A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI,A_SOM_LOI = A_SOM_LOI,type = 'ksat')]
  dt[,i_p_dens := osi_p_density(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[,i_p_wef := osi_p_wef(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI)]
  
  # calculate all soil biological indicators
  dt[,i_b_pmn := osi_b_pmn(B_LU = B_LU,B_SOILTYPE_AGR = B_SOILTYPE_AGR,A_N_PMN = A_N_PMN,B_COUNTRY = B_COUNTRY)]
  
  # aggregate indicators before scoring
  
    # aggregate per year per indicator
  
    # aggregate per indicator
  
  
  # add scores
  
    # add score per function
  
    # add score per aggregated soil category and ecosystem function
  
    # add total score
  
  
  #  Step 6 Combine all outputs into one ------------------
  
  # combine both outputs
  if(output == 'all'){out <- NULL}
  if(output == 'indicators'){out <- NULL}
  if(output == 'recommendations'){out <- NULL}
  if(output == 'scores'){out <- NULL}
  if(output == 'obic_score'){out <- NULL}
  if(output == 'unaggregated'){out <- NULL}
  
  
  # return output
  return(out)
}