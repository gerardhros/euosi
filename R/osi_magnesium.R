#' Calculate the magnesium availability index (wrapper function)
#' 
#' This function calculates the magnesium availability for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_CLAY_MI (numeric) is the Clay content (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CEC_CO (numeric) is the Cation exhange capacity in mmol+/kg 
#' @param A_CACO3_IF (numeric) the percentage of CaCO3 (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_MG_CC (numeric) The plant available content of Mg in the soil (mg  Mg per kg) extracted by 0.01M CaCl2
#' @param A_MG_AA (numeric) is the exchangeable Mg concentration (mg/kg)
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_K_CC (numeric) The plant available potassium, extracted with 0.01M CaCl2 (mg per kg)
#' @param B_COUNTRY (character) The country code
#'  
#' @import data.table
#' 
#' @return
#' The capacity of the soil to supply and buffer potassium, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_c_magnesium <- function(B_LU, B_SOILTYPE_AGR = NA_character_,A_SOM_LOI = NA_real_, 
                            A_CLAY_MI = NA_real_,A_PH_CC = NA_real_, A_CACO3_IF = NA_real_,
                            A_CEC_CO = NA_real_, A_K_CO_PO = NA_real_, A_K_CC = NA_real_,A_MG_AA = NA_real_,
                            A_MG_CC = NA_real_,
                            B_COUNTRY) {
  
  # add visual bindings
  
  # desired length of inputs
  arg.length <- max(length(B_LU),length(B_SOILTYPE_AGR), 
                    length(A_MG_CC), length(A_PH_CC), length(A_SOM_LOI), length(A_CEC_CO), 
                    length(A_CLAY_MI),length(A_CACO3_IF),length(A_K_CO_PO),
                    length(A_MG_AA), length(A_K_CC),length(B_COUNTRY))
  
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
                   A_MG_AA = A_MG_AA,
                   A_K_CC = A_K_CC,
                   A_MG_CC = A_MG_CC,
                   A_CACO3_IF = A_CACO3_IF,
                   value = NA_real_
  )
  
  # calculate the open soil index score for magnesium availability for the Netherlands
  dt[B_COUNTRY == 'NL', value := osi_c_magnesium_nl(B_LU = B_LU,
                                                    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                                    A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI,
                                                    A_PH_CC = A_PH_CC, A_CEC_CO = A_CEC_CO,
                                                    A_K_CO_PO = A_K_CO_PO,A_MG_CC = A_MG_CC,A_K_CC = A_K_CC)]
  # calculate the open soil index score for magnesium availability for France
  dt[B_COUNTRY == 'FR', value := osi_c_magnesium_fr(B_LU = B_LU,
                                                    A_CLAY_MI = A_CLAY_MI,
                                                    A_CEC_CO = A_CEC_CO, 
                                                    A_MG_AA = A_MG_AA,
                                                    A_CACO3_IF = A_CACO3_IF)]
  
  # select the output variable
  value <- dt[,value]
  
  # return the OSI score
  return(value)
  
}

#' Calculate the magnesium availability index in Austria
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_MG_CC (numeric) The exchangeable Mg-content of the soil measured via Calcium Chloride (mg Mg/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_at(A_MG_CC = 47,B_TEXTURE_HYPRES = 'C')
#' 
#' @return 
#' The magnesium availability index in Austria estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_at <- function(A_MG_CC,B_TEXTURE_HYPRES,B_LU = NA_character_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # parameters
  # dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_MG_CC = A_MG_CC,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU', 
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = 'B_SOILTYPE_AGR',
  #             by.y = 'osi_threshold_soilcat',
  #             all.x = TRUE)
  
  # calculate the OSI score light textured soils
  dt[B_TEXTURE_HYPRES %in% c('C'),
     value := osi_evaluate_logistic(x = A_MG_CC, b= 0.1212611,x0 = 10.9464807,v = 0.3432328)]
  # calculate the OSI score medium texture soils
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_MG_CC, b= 0.1129579,x0 = 2.4917716,v = 0.0212170)]
  # calculate the OSI score for heavy textured soils
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_MG_CC, b= 0.08246231,x0 = 3.16282481,v = 0.02204703)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the magnesium availability index in Switzerland
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_MG_AAA (numeric) The exchangeable Mg-content of the soil measured via acid ammonium acetate extraction (mg Mg/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_ch(A_MG_AAA = 50)
#' 
#' @return 
#' The potassium availability index in Switzerland estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_c_magnesium_ch <- function(A_MG_AAA,A_CLAY_MI,B_LU = NA_character_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # parameters
  # dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_MG_AAA = A_MG_AAA,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU', 
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = 'B_SOILTYPE_AGR',
  #             by.y = 'osi_threshold_soilcat',
  #             all.x = TRUE)
  
  # convert to the OSI score (Tabelle 18)
  
  # arable crops, maize and grassland / vegetables are mainly different at higher level (class H to VH)
  # so no need to differentiate
  dt[A_CLAY_MI <= 10, 
     value := osi_evaluate_logistic(x = A_MG_AAA, b= 0.04288244,x0 = -38.22495053,v = 0.06959847 )]
  dt[A_CLAY_MI > 10 & A_CLAY_MI <= 20, 
     value := osi_evaluate_logistic(x = A_MG_AAA, b= 0.03679377 ,x0 = 0.44180683 ,v = 0.27821994 )]
  dt[A_CLAY_MI > 20 & A_CLAY_MI <= 30, 
     value := osi_evaluate_logistic(x = A_MG_AAA, b= 0.04034187 ,x0 = 51.16252893,v = 0.84481562 )]
  dt[A_CLAY_MI > 30 & A_CLAY_MI <= 40, 
     value := osi_evaluate_logistic(x = A_MG_AAA, b= 0.02440062 ,x0 = 0.54966486  ,v = 0.20908778 )]
  dt[A_CLAY_MI > 40, 
     value := osi_evaluate_logistic(x = A_MG_AAA, b= 0.01940137  ,x0 = 0.50465157  ,v = 0.15617097 )]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}
#' Calculate the magnesium availability index in Czech Republic
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_MG_M3 (numeric) The exchangeable Mg-content of the soil measured via Mehlich 3 extracton (mg Mg/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_cz(A_MG_M3 = 81,B_TEXTURE_HYPRES='C')
#' 
#' @return 
#' The magnesium availability index in Czech Republic estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_cz <- function(A_MG_M3,B_TEXTURE_HYPRES,B_LU = NA_character_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # parameters
  # dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_MG_M3 = A_MG_M3,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU', 
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = 'B_SOILTYPE_AGR',
  #             by.y = 'osi_threshold_soilcat',
  #             all.x = TRUE)
  
  # convert to the OSI score
  dt[B_TEXTURE_HYPRES %in% c('C'),
     value := osi_evaluate_logistic(x = A_K_M3, b= 0.04947471,x0 = 3.29991394 ,v = 0.01033346)]
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_K_M3, b= 0.051319601,x0 = 2.593526993 ,v = 0.002350958)]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_M3, b= 0.02996501,x0 = 87.56290295  ,v = 0.14816731)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the magnesium availability index in Germany
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_C_OF (numeric) The carbon content of the soil layer (g/ kg)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_K_MG (numeric) The magnesium content extracted with CaCl2 (g / kg)
#' 
#' @import data.table
#' 
#' @return 
#' The magnesium availability index in Germany derived from extractable soil Mg fractions. A numeric value.
#' 
#' @export
osi_c_magnesium_de <- function(B_LU, A_C_OF, A_CLAY_MI,A_SAND_MI, A_MG_CC) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_C_OF= A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_MG_CC = A_MG_CC,
                   value = NA_real_)
  
  # add soil type
  dt.ge[A_SAND_MI >= 85 & A_SILT_MI <= 25 & A_CLAY_MI <= 5 & A_C_OF < 150, stype := "BG1"]
  dt.ge[A_SAND_MI >= 42 & A_SAND_MI <= 95 & A_SILT_MI <= 40 & A_CLAY_MI <= 17 & A_C_OF < 150,  stype :="BG2"]
  dt.ge[A_SAND_MI >= 33 & A_SAND_MI <= 83 & A_SILT_MI <= 50 & A_CLAY_MI >= 8 & A_CLAY_MI <= 25 & A_C_OF < 150,  stype :="BG3"]
  dt.ge[A_SAND_MI <= 75 & A_SILT_MI <= 100 & A_CLAY_MI <= 35 & A_C_OF < 150,  stype :="BG4"]
  dt.ge[A_SAND_MI <= 65 & A_SILT_MI <= 75 & A_CLAY_MI >= 25 & A_CLAY_MI <= 100 & A_C_OF < 150, stype := "BG5"]
  dt.ge[ A_C_OF >= 150,  stype := "BG6"]
  
  # evaluate A_MG_CC for arable an grassland soils
  dt.ge[stype=='BG1', value := OBIC::evaluate_logistic(A_MG_CC, b = 0.17394558, x0 = 1.97132628, v = 0.01974615)]
  dt.ge[stype=='BG2', value := OBIC::evaluate_logistic(A_MG_CC, b = 0.131007506, x0 = -8.898667288, v = 0.005444774)]
  dt.ge[stype=='BG3', value := OBIC::evaluate_logistic(A_MG_CC, b = 0.105975715, x0 = -13.136695181, v = 0.004774989)]
  dt.ge[stype=='BG4', value := OBIC::evaluate_logistic(A_MG_CC, b = 0.07756033, x0 = 3.04955824, v = 0.02572424)]
  dt.ge[stype=='BG5', value := OBIC::evaluate_logistic(A_MG_CC, b = 0.06074047, x0 = 3.55690767, v = 0.02687046)]
  dt.ge[stype=='BG6', value := OBIC::evaluate_logistic(A_MG_CC, b = 0.17394558, x0 = 1.97132628, v = 0.01974615)]
  
  # select value and return
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the magnesium availability index in Estonia
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param A_MG_M3 (numeric) The exchangeable Mg-content of the soil measured via Mehlich 3 extracton (mg Mg/ kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_ee(A_MG_M3 = 45,B_TEXTURE_USDA = 'clay')
#' 
#' @return 
#' The magnesium availability index in Estonia estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_ee <- function(A_MG_M3,B_TEXTURE_USDA,B_LU = NA_character_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # parameters
  # dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA=B_TEXTURE_USDA,
                   A_MG_M3 = A_MG_M3,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU', 
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = 'B_SOILTYPE_AGR',
  #             by.y = 'osi_threshold_soilcat',
  #             all.x = TRUE)
  
  # convert to the OSI score
  dt[B_TEXTURE_USDA == 'sand',
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.13379398,x0 = 2.32860029,v = 0.01118338)]
  dt[B_TEXTURE_USDA %in% c('loamy sand'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.09055787,x0 = 2.54738930,v = 0.02382157)]
  dt[B_TEXTURE_USDA %in% c('sandy loam'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.091975296,x0 = 2.951269556,v = 0.008792147)]
  dt[B_TEXTURE_USDA %in% c('loam','sandy clay','sandy clay loam'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.06050444,x0 = 2.83249967,v = 0.01937565)]
  dt[B_TEXTURE_USDA %in% c('clay','clay loam','silty clay','silty clay loam','silt loam','silt'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.04557622,x0 = 0.55322883,v = 0.01921304)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the Mg availability index for France 
#' 
#' This function calculates the Mg availability of a soil, using the agronomic index used in France
#' 
#' @param B_LU (numeric) The crop code
#' @param A_CLAY_MI (numeric) Soil clay content (\%)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil in mmol/kg
#' @param A_CACO3_IF (numeric) The CaCO3 content in the soil (\%)
#' @param A_MG_AA (numeric) The extractable Mg content in the soil (mg/kg)
#' 
#' @import data.table
#'  
#' @return 
#' The magnesium availability index in France estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_fr <- function(B_LU,A_CLAY_MI, A_CEC_CO, A_CACO3_IF, A_MG_AA) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  soil_cat_mg = osi_st_c1 = osi_st_c2 = osi_st_c3 = NULL
  
  # Load in the crop datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # Load in the threshold data set and the parms dataset (to be used later for min-max checking)
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # load and subset thresholds for situation in France
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='FR' & osi_indicator=='i_c_mg']

  # Check length of desired input
  arg.length <- max(length(A_CLAY_MI),length(A_CEC_CO),length(A_CACO3_IF),length(A_MG_AA))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_MG_AA, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  checkmate::assert_data_table(dt.thresholds,max.rows = 4)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_MG_AA = A_MG_AA,
                   A_CEC_CO = A_CEC_CO,
                   A_CACO3_IF = A_CACO3_IF,
                   value = NA_real_)
  
  # add soil category based in CEC and CACO3
  dt[, soil_cat_mg := fifelse(A_CEC_CO < 7,'sand',
                              fifelse(A_CEC_CO > 7 & A_CEC_CO < 12,'loam',
                                 fifelse(A_CACO3_IF>0, 'clay calcareous', 'clay')))]
  
  # merge with threshold
  dt <- merge(dt,
              dt.thresholds,
              by.x = 'soil_cat_mg',
              by.y = 'osi_threshold_soilcat',
              all.x = TRUE)
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = A_MG_AA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the magnesium availability index in Hungary
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param A_MG_KCL (numeric) The exchangeable Mg-content of the soil measured via KCL extracton (mg Mg/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_hu(A_MG_KCL = 45,B_TEXTURE_USDA = 'loam')
#' 
#' @return 
#' The magnesium availability index in Hungary estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_hu <- function(B_TEXTURE_USDA,A_MG_KCL,B_LU = NA_character_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # parameters
  # dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA = B_TEXTURE_USDA,
                   A_MG_KCL = A_MG_KCL,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU', 
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = 'B_SOILTYPE_AGR',
  #             by.y = 'osi_threshold_soilcat',
  #             all.x = TRUE)
  
  # convert to the OSI score
  dt[B_TEXTURE_USDA == 'sand',
     value := osi_evaluate_logistic(x = A_MG_KCL, b= 0.1560195 ,x0 = 2.8715883 ,v = 0.1279928 )]
  dt[B_TEXTURE_USDA %in% c('loamy sand','sandy loam'),
     value := osi_evaluate_logistic(x = A_MG_KCL, b= 0.1102304 ,x0 = 10.2981516  ,v = 0.2085848 )]
  dt[B_TEXTURE_USDA %in% c('loam','sandy clay','sandy clay loam'),
     value := osi_evaluate_logistic(x = A_MG_KCL,b= 0.05181633 ,x0 = 2.39759172 ,v = 2.39759172 )]
  dt[B_TEXTURE_USDA %in% c('clay','clay loam','silty clay','silty clay loam','silt loam','silt'),
     value := osi_evaluate_logistic(x = A_MG_KCL, b= 0.05181633 ,x0 = 2.39759172 ,v = 2.39759172 )]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the magnesium availability index in Ireland
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_MG_NaAAA (numeric) The Mg-content of the soil extracted with Morgan's solution, sodium acetate acetic acid (mg/ kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_ie(B_LU = 265,A_SOM_LOI = 2,A_MG_NaAAA = 5)
#' osi_c_magnesium_ie(B_LU = c(265,1019),A_SOM_LOI = c(2,4),A_MG_NaAAA = c(3.5,5.5))
#' 
#' @return 
#' The magnesium availability index in Ireland derived from extractable soil Mg fractions. A numeric value.
#' 
#' @export
osi_c_magnesium_ie <- function(B_LU, A_SOM_LOI,A_MG_NaAAA) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_MG_NaAAA = A_MG_NaAAA,
                   value = NA_real_)
  
  # Index derived following K index system (Mg in mg/L)
  
  # convert from mg / kg to mg / liter sample volume
  dt[, BDS := (1/(0.02525 * A_SOM_LOI + 0.6541))]
  dt[,A_MG_NaAAA := A_MG_NaAAA * BDS]
  
  # evaluation soil K status
  dt[A_SOM_LOI <= 20, value := OBIC::evaluate_logistic(A_MG_NaAAA, b = 0.05513917 , x0 = -25.82660695, v = 0.05413915 )]
  dt[A_SOM_LOI > 20, value := OBIC::evaluate_logistic(A_MG_NaAAA, b = 0.04081745, x0 = 53.86819280, v = 0.66253526)]
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the magnesium availability index in Latvia
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_MG_DL (numeric) The exchangeable Mg-content of the soil measured via Double Lactate extraction (mg Mg/ kg)
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_lv(A_MG_DL = 45,B_TEXTURE_USDA='sand')
#' 
#' @return 
#' The magnesium availability index in Latvia estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_lv <- function(A_MG_DL,B_TEXTURE_USDA, B_LU = NA_character_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # parameters
  # dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA = B_TEXTURE_USDA,
                   A_MG_DL = A_MG_DL,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU', 
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = 'B_SOILTYPE_AGR',
  #             by.y = 'osi_threshold_soilcat',
  #             all.x = TRUE)
  
  # convert to the OSI score
  dt[B_TEXTURE_USDA == 'sand',
     value := osi_evaluate_logistic(x = A_MG_DL, b= 0.06556918,x0 = -27.38667996,v = 0.01409843)]
  dt[B_TEXTURE_USDA %in% c('loamy sand','sandy loam'),
     value := osi_evaluate_logistic(x = A_MG_DL, b= 0.06208305,x0 = 3.09989977,v = 0.07585618)]
  dt[B_TEXTURE_USDA %in% c('loam','sandy clay','sandy clay loam'),
     value := osi_evaluate_logistic(x = A_MG_DL, b= 0.04707890,x0 = 2.05827044,v = 0.07764412)]
  dt[B_TEXTURE_USDA %in% c('clay','clay loam','silty clay','silty clay loam','silt loam','silt'),
     value := osi_evaluate_logistic(x = A_MG_DL, b= 0.03987059,x0 = 3.17977103,v = 0.08840998)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the magnesium availability index in Lithuania
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_MG_AL (numeric) The exchangeable Mg-content of the soil measured via Ammonium Lactate extraction (mg Mg/ kg)
#' @param A_PH_KCL (numeric) The pH measured in KCl
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_lt(A_MG_AL = 45,A_PH_KCL= 5.5)
#' 
#' @return 
#' The magnesium availability index in Lithuania estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_lt <- function(A_MG_AL,A_PH_KCL,B_LU = NA_character_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # parameters
  # dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_MG_AL = A_MG_AL,
                   A_PH_KCL = A_PH_KCL,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU', 
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = 'B_SOILTYPE_AGR',
  #             by.y = 'osi_threshold_soilcat',
  #             all.x = TRUE)
  
  # estimate the OSI score for mineral and peat soils
  dt[A_PH_KCL <= 6.1,
     value := osi_evaluate_logistic(x = A_MG_AL, b= 0.05501951,x0 = 2.41904617,v = 0.03268667)]
  dt[A_PH_KCL >6.1 & A_PH_KCL <= 7.0,
     value := osi_evaluate_logistic(x = A_MG_AL, b= 0.02773952,x0 = 2.90831748,v = 0.03040226)]
  dt[A_PH_KCL > 7.0,
     value := osi_evaluate_logistic(x = A_MG_AL, b= 0.01752798,x0 = 219.29885552,v = 0.39490553)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the capacity of soils to supply Magnesium
#' 
#' This function calculates an index for the availability of Magnesium in soil
#' 
#' @param B_LU (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ per kg), analyzed via Cobalt-hexamine extraction
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_MG_CC (numeric) The plant available content of Mg in the soil (mg  Mg per kg) extracted by 0.01M CaCl2
#' @param A_K_CC (numeric) The plant available potassium, extracted with 0.01M CaCl2 (mg per kg), 
#' 
#' @import data.table
#' 
#' @examples
#' osi_c_magnesium_nl(B_LU = 265, B_SOILTYPE_AGR = 'dekzand',
#' A_SOM_LOI = 3.5,A_CLAY_MI = 8.5,A_PH_CC = 5.4, 
#' A_CEC_CO = 185,A_K_CO_PO = 4.5,A_MG_CC = 125,A_K_CC = 65)
#' 
#' @return 
#' An index representing the availability of Magnesium in a soil. A numeric value.
#' 
#' @export
osi_c_magnesium_nl <- function(B_LU,B_SOILTYPE_AGR,A_SOM_LOI,A_CLAY_MI,
                               A_PH_CC, A_CEC_CO,A_K_CO_PO,A_MG_CC,A_K_CC) {
  
  # set variables to NULL
  B_LU_BRP = D_MG = A_MG_NC = A_PH_KCL = A_SLIB_MI = cF = A_K_CO = kg1 = kg2 = kg = mg_pred = mg_aim = NULL
  id = crop_code = soiltype = soiltype.n = crop_category = NULL
  
  # Load in the datasets for soil and crop types
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check inputs
  arg.length <- max(length(A_MG_CC), length(A_PH_CC), length(A_SOM_LOI), length(A_CEC_CO), 
                    length(A_K_CO_PO), length(A_CLAY_MI), length(B_SOILTYPE_AGR), length(B_LU_BRP))
  checkmate::assert_numeric(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(A_MG_CC, lower = 1, upper = 1100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 1, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CC, lower = 1, upper = 600, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CO_PO, lower = 0.1, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  
  # Settings
  param.re = 180 # protein content of first cut grassland in spring (g/kg)
  param.k = 33.9 # potassium content of first cut grass in spring (g/kg)
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU_BRP = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   A_CEC_CO = A_CEC_CO,
                   A_K_CO_PO = A_K_CO_PO,
                   A_MG_CC = A_MG_CC,
                   A_K_CC = A_K_CC,
                   value = NA_real_
                  )
  
  # add crop names and soiltypes
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # Calculate the Mg availability for arable land
  dt.arable <- dt[crop_category == "akkerbouw"]
  dt.arable[,D_MG := A_MG_CC]
  
  # Calculate the Mg availability for maize land
  dt.maize <- dt[crop_category == "mais"]
  dt.maize[,D_MG := A_MG_CC]
  
  # Calculate Mg availability for grassland on sandy and loamy soils
  dt.grass.sand <- dt[crop_category == "grasland" & grepl('zand|loess|dalgrond',B_SOILTYPE_AGR)]
  dt.grass.sand[,D_MG := A_MG_CC]
  
  # Calculate Mg availability for grassland on clay and peat soils
  dt.grass.other <- dt[crop_category == "grasland" & grepl('klei|veen',B_SOILTYPE_AGR)]
  
  # convert CaCl2 method for Mg to former NaCl method
  dt.grass.other[,A_MG_NC := A_MG_CC * 1.987 - 6.8]
  
  # estimate pH-kcl from pH-cacl2
  dt.grass.other[,A_PH_KCL := (A_PH_CC - 0.5262)/0.9288]
  
  # estimate slib via lutum-slib-ratio (Source: bemestingsadvies.nl)
  dt.grass.other[grepl('zeeklei|veen|moerige_klei',B_SOILTYPE_AGR),A_SLIB_MI := A_CLAY_MI / 0.67]
  dt.grass.other[grepl('rivierklei',B_SOILTYPE_AGR),A_SLIB_MI := A_CLAY_MI / 0.61]
  dt.grass.other[grepl('maasklei',B_SOILTYPE_AGR),A_SLIB_MI := A_CLAY_MI / 0.55]
  
  # additional temporary variable called cF (Source: Adviesbasis, 2002)
  dt.grass.other[A_SOM_LOI <= 3,cF:= 2.08]
  dt.grass.other[A_SOM_LOI > 3,cF:= 5.703 * A_SOM_LOI^-0.7996]
  
  # calculate A_K_CO in mg K/ kg grond
  dt.grass.other[,A_K_CO := A_CEC_CO * A_K_CO_PO * 0.01 * 39.098]
  
  # estimate K-index from K-CEC (A_K_CO, mg K/ kg grond) and K-PAE (mg K/ kg grond) (Source: NMI notitie 1436.N.11)
  dt.grass.other[,kg1 := (1.56 * A_K_CC - 17 + 0.29 * A_CEC_CO) * cF * 0.12046]
  dt.grass.other[,kg2 := A_K_CO * cF * 0.12046]
  dt.grass.other[,kg := 0.5 * (kg1 + kg2)]
  
  # remove columns not needed any more
  dt.grass.other[,c('kg1','kg2','cF'):=NULL]
  
  # calculate expected Mg-content in grass (g/kg) in the spring on peat soils (den Boer 2003)
  dt.grass.other[grepl('veen', B_SOILTYPE_AGR),mg_pred := pmax(3.3284 + 0.001058* A_MG_NC - 0.02059* kg -0.01163*A_CLAY_MI -0.2691* A_PH_KCL, 0)]
  
  # calculate expected Mg-content in grass (g/kg) in the spring on clay soils (den Boer 2003)
  dt.grass.other[grepl('klei',B_SOILTYPE_AGR),mg_pred := pmax(2.6688 + 0.001563* A_MG_NC - 0.01738* kg -0.04175* A_SOM_LOI -0.015128* A_SLIB_MI, 0)]
  
  # estimate optimum mg-content in grass in spring (Kemp, in Handboek Melkveehouderij)
  dt.grass.other[,mg_aim := (2.511 - 86.46/((param.k * param.re)^0.5))^2]
  
  # Mg index
  dt.grass.other[,D_MG := pmin(100 * (mg_pred /2.0), 100)] 
  
  # nature parcels
  dt.nature <- dt[crop_category == "natuur"]
  dt.nature[,D_MG := 0]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass.sand,dt.grass.other, dt.arable,dt.maize,dt.nature), fill = TRUE)
  
  # avoid values below -1
  dt[value < -1, value := -1]
  
  # setorder
  setorder(dt, id)
  
  # convert to indicator score
  dt[crop_category == "akkerbouw",value := evaluate_logistic(D_MG, b = 0.206, x0 = 45, v = 2.39)]
  dt[crop_category == "grasland" & grepl('zand|loess|dalgrond',B_SOILTYPE_AGR),value := evaluate_logistic(D_MG, b = 0.075, x0 = 80, v = 2)]
  dt[crop_category == "grasland" & grepl('klei|veen',B_SOILTYPE_AGR), value := evaluate_logistic(D_MG, b = 0.15, x0 = 75, v = 1)]
  dt[crop_category == "natuur", value := 1]
 
  # select and return OSI indicator
  value <- dt[, value]
  
  return(value)
}

#' Calculate the magnesium availability index in Poland
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_MG_CC (numeric) The exchangeable Mg-content of the soil measured via calcium chloride extracton (mg Mg/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_pl(A_MG_CC = 45,B_TEXTURE_HYPRES='C')
#' 
#' @return 
#' The magnesium availability index in Poland estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_pl <- function(A_MG_CC,B_TEXTURE_HYPRES,B_LU = NA_character_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # parameters
  # dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_MG_CC = A_MG_CC,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU', 
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = 'B_SOILTYPE_AGR',
  #             by.y = 'osi_threshold_soilcat',
  #             all.x = TRUE)
  
  # calculate the OSI score very light textured soils
  dt[B_TEXTURE_HYPRES %in% c('C'),
     value := osi_evaluate_logistic(x = A_MG_CC, b= 0.28300785,x0 = 2.99872620,v = 0.05611183)]
  # calculate the OSI score light textured soils
  dt[B_TEXTURE_HYPRES %in% c('MF'),
     value := osi_evaluate_logistic(x = A_MG_CC, b= 0.23985236,x0 = 3.56538786,v = 0.01013386)]
  # calculate the OSI score medium texture soils
  dt[B_TEXTURE_HYPRES %in% c('M'),
     value := osi_evaluate_logistic(x = A_MG_CC, b= 0.12696212,x0 = 3.64496611,v = 0.01817593)]
  # calculate the OSI score for heavy textured soils
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_MG_CC, b= 0.125275181,x0 = 2.612899607,v = 0.004850648)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the magnesium availability index in  Slovak Republic
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_M3 (numeric) The exchangeable Mg-content of the soil measured via Mehlich 3 extracton (mg Mg/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_sk(A_MG_M3 = 45,B_TEXTURE_HYPRES='C')
#' 
#' @return 
#' The magnesium availability index in Slovak Republic estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_sk <- function(B_TEXTURE_HYPRES,A_MG_M3,B_LU = NA_character_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # parameters
  # dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_MG_M3 = A_MG_M3,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU', 
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = 'B_SOILTYPE_AGR',
  #             by.y = 'osi_threshold_soilcat',
  #             all.x = TRUE)
  
  # convert to the OSI score
  dt[B_TEXTURE_HYPRES %in% c('C'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.04947471 ,x0 = 3.29991394,v = 0.01033346)]
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.03614588 ,x0 = 2.27785045,v = 0.01123181)]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.03224561 ,x0 = 65.84935480,v = 0.04160495)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}


#' Calculate the magnesium availability index in Slovenia
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_MG_AL (numeric) The exchangeable Mg-content of the soil measured via ammonium lactate extracton (mg Mg/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_sl(A_MG_AL = 45)
#' 
#' @return 
#' The magnesium availability index in Slovenia estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_sl <- function(A_MG_AL,B_TEXTURE_HYPRES,B_LU = NA_character_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # parameters
  # dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_MG_AL = A_MG_AL,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU', 
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = 'B_SOILTYPE_AGR',
  #             by.y = 'osi_threshold_soilcat',
  #             all.x = TRUE)
  
  # evaluate the OSI score
  dt[B_TEXTURE_HYPRES %in% c('C','MF','M'),
     value := osi_evaluate_logistic(x = A_MG_AL, b= 0.09200033 ,x0 = 4.64747241 ,v = 0.04275055)]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_MG_AL, b= 0.07289460 ,x0 = 3.46347879 ,v = 0.01456737)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the magnesium availability index in United Kingdom
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_MG_AN (numeric) The Mg-content of the soil extracted with ammonium nitrate (mg Mg /kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_uk(B_LU = 265,A_SOM_LOI=3,A_MG_AN = 50)
#' osi_c_magnesium_uk(B_LU = c(265,1019),A_SOM_LOI = c(3,5),A_MG_AN = c(35,55))
#' 
#' @return 
#' The magnesium availability index in United Kingdom derived from extractable soil Mg fractions. A numeric value.
#' 
#' @export
osi_c_magnesium_uk <- function(B_LU, A_SOM_LOI,A_MG_AN) {
  
  # crop properties
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_MG_AN = A_MG_AN,
                   value = NA_real_)
  
  # merge with crop
  dt <- merge(dt,
              dt.crops[,.(B_LU, crop_name, crop_cat1)],
              by = 'B_LU',
              all.x = TRUE)
  
  # convert from mg / kg to mg / liter sample volume
  dt[, BDS := (1/(0.02525 * A_SOM_LOI + 0.6541))]
  dt[, A_MG_AN := A_MG_AN * BDS]
  
  # optimum value is index 2 for all land uses
  dt[, value := osi_evaluate_logistic(A_MG_AN, b = 0.05557028 , x0 = -18.14423092, v = 0.07984178)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}