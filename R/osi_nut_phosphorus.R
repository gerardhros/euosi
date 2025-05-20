#' Calculate the phoshporus excesss index (wrapper function)
#' 
#' This function calculates the P excess index for all European countries (if available). 
#' 
#' @param B_LU (character) The crop code
#' @param B_SOILTYPE_AGR (character) The soil type in a particular region
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_PH_WA (numeric) The pH measured in water.
#' @param A_PH_CC (numeric) The pH measured in CaCl2 extraction.
#' @param A_CACO3_IF (numeric) the percentage of CaCO3 (\%)
#' @param A_P_CAL (numeric) The exchangeable P-content of the soil measured via Calcium Ammonium Lactate (mg P/ kg)
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate (mg P / kg)
#' @param A_P_DL (numeric) The P-content of the soil extracted with double lactate (mg P / kg)
#' @param A_P_AAA (numeric) The exchangeable P-content of the soil measured via acid ammonium acetate extraction (mg P / kg)
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2 (mg P / kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P / kg)
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg P / kg)
#' @param A_P_M3 (numeric) The exchangeable P-content of the soil measured via Mehlich 3 extracton (mg P/ kg)
#' @param B_COUNTRY (character) The country code
#' 
#' @import data.table
#' 
#' @return
#' The capacity of the soil to supply and buffer boron, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_nut_p <- function(B_LU, 
                      B_SOILTYPE_AGR = NA_character_, 
                      A_CLAY_MI = NA_real_, A_SAND_MI = NA_real_,A_C_OF = NA_real_,
                      A_SOM_LOI = NA_real_, A_PH_WA = NA_real_,A_PH_CC = NA_real_, A_CACO3_IF = NA_real_,
                      A_P_OL = NA_real_, A_P_M3 = NA_real_, A_P_CAL = NA_real_,
                      A_P_AAA = NA_real_,A_P_DL = NA_real_,
                      A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_, B_COUNTRY) {
  
  # add visual bindings
  
  # Check length of desired input
  arg.length <- max(length(B_LU),
                    length(B_SOILTYPE_AGR),length(A_CLAY_MI), length(A_SAND_MI),
                    length(A_SOM_LOI),length(A_C_OF),
                    length(A_P_AAA),length(A_P_AL),length(A_P_CAL),
                    length(A_P_CC),length(A_P_DL),length(A_P_M3),
                    length(A_P_OL),length(A_P_WA),
                    length(B_COUNTRY))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = pmax(0,100 - A_CLAY_MI - A_SAND_MI),
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_PH_CC = A_PH_CC,
                   A_PH_WA = A_PH_WA,
                   A_CACO3_IF = A_CACO3_IF,
                   A_P_AAA = A_P_AAA,
                   A_P_AL = A_P_AL,
                   A_P_CAL = A_P_CAL,
                   A_P_CC = A_P_CC,
                   A_P_DL = A_P_DL,
                   A_P_M3 = A_P_M3,
                   A_P_OL = A_P_OL,
                   A_P_WA = A_P_WA,
                   B_COUNTRY = B_COUNTRY,
                   value = NA_real_)
  
  # estimate texture information
  dt[,B_TEXTURE_USDA := osi_get_TEXTURE_USDA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_HYPRES := osi_get_TEXTURE_HYPRES(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_BE := osi_get_TEXTURE_BE(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_GEPPA := osi_get_TEXTURE_GEPPA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  
  # estimate missing soil properties
  dt[is.na(A_PH_WA) & !is.na(A_PH_CC), A_PH_WA := osi_conv_ph(element='A_PH_WA',A_PH_CC = A_PH_CC)]
  dt[!is.na(A_PH_WA) & is.na(A_PH_CC), A_PH_CC := osi_conv_ph(element='A_PH_CC',A_PH_WA = A_PH_WA)]
  
  
  # calculate the OSI score for P excess
  
  # Austria (AT), Belgium (BE), Switzerland (CH), Czech Republic (CZ), Germany (DE)
  dt[B_COUNTRY == 'AT', value := osi_nut_p_at(B_LU = B_LU, A_P_CAL = A_P_CAL)]
  dt[B_COUNTRY == 'BE', value := osi_nut_p_be(B_LU = B_LU,A_P_AL = A_P_AL)]
  dt[B_COUNTRY == 'CH', value := osi_nut_p_ch(B_LU = B_LU, A_P_AAA = A_P_AAA)]
  dt[B_COUNTRY == 'CZ', value := osi_nut_p_cz(B_LU = B_LU, A_P_M3 = A_P_M3)]
  dt[B_COUNTRY == 'DE', value := osi_nut_p_de(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_P_CAL = A_P_CAL, A_P_DL = A_P_DL)]
  
  # Denmark (DK), Estonia (EE), Spain (ES),France (FR), Finland (FI) 
  dt[B_COUNTRY == 'DK', value := osi_nut_p_dk(B_LU = B_LU, A_P_OL = A_P_OL)]
  dt[B_COUNTRY == 'EE', value := osi_nut_p_ee(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_P_M3 = A_P_M3)]
  dt[B_COUNTRY == 'ES', value := osi_nut_p_es(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_P_OL = A_P_OL)]
  dt[B_COUNTRY == 'FR', value := osi_nut_p_fr(B_LU = B_LU, A_P_OL = A_P_OL,A_PH_WA = A_PH_WA)]
  dt[B_COUNTRY == 'FI', value := osi_nut_p_fi(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_P_AAA = A_P_AAA, A_C_OF = A_C_OF )]
  
  # Hungary (HU), Ireland (IE), Italy (IT), Latvia (LV), Lithuania (LT)
  dt[B_COUNTRY == 'HU', value := osi_nut_p_hu(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_CACO3_IF = A_CACO3_IF,A_P_AL = A_P_AL )]
  dt[B_COUNTRY == 'IE', value := osi_nut_p_ie(B_LU = B_LU, A_P_OL = A_P_OL)]
  dt[B_COUNTRY == 'IT', value := osi_nut_p_it(B_LU = B_LU, A_P_OL = A_P_OL)]
  dt[B_COUNTRY == 'LV', value := osi_nut_p_lv(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_P_DL = A_P_DL)]
  dt[B_COUNTRY == 'LT', value := osi_nut_p_lt(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_P_AL = A_P_AL)]
  
  # the Netherlands (NL), Norway (NO),  Sweden (SE), Slovak Republic (SK), Slovenia (SL)
  dt[B_COUNTRY == 'NL', value := osi_nut_p_nl(B_LU = B_LU, A_P_AL = A_P_AL, A_P_CC = A_P_CC, A_P_WA = A_P_WA)]
  dt[B_COUNTRY == 'NO', value := osi_nut_p_no(B_LU = B_LU, A_P_AL = A_P_AL)]
  dt[B_COUNTRY == 'SE', value := osi_nut_p_se(B_LU = B_LU, A_P_AL = A_P_AL)]
  dt[B_COUNTRY == 'SK', value := osi_nut_p_sk(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_P_M3 = A_P_M3)]
  dt[B_COUNTRY == 'SL', value := osi_nut_p_sl(B_LU = B_LU, A_P_AL = A_P_AL)]
  
  # Poland (PL), United Kingdom (UK)
  dt[B_COUNTRY == 'PL', value := osi_nut_p_pl(B_LU = B_LU, A_P_DL = A_P_DL)]
  dt[B_COUNTRY == 'IE', value := osi_nut_p_uk(B_LU = B_LU, A_P_OL = A_P_OL)]
  
  # select the output variable
  value <- dt[,value]
  
  # return the OSI score
  return(value)
  
}

#' Calculate the phosphorus excess index in Austria
#' 
#' This function calculates the phosphorus excess 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_CAL (numeric) The exchangeable P-content of the soil measured via Calcium Ammonium Lactate (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_at(A_P_CAL = 47)
#' 
#' @return 
#' The phosphorus excess index in Austria estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_at <- function(A_P_CAL,B_LU = NA_character_) {
  
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
                   A_P_CAL = A_P_CAL,
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
  dt[,value := osi_evaluate_logistic(x = A_P_CAL, b= -0.02002226 x0=199.66245188  v= 0.35544312)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphorus excess index in Belgium
#' 
#' This function calculates the phosphorus excess index 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_AL (numeric) The exchangeable P-content of the soil measured via ammonium lactate extraction
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_posphor_be(B_LU = 'SOJ', A_P_AL = 45)
#' 
#' @return 
#' The phosphorus excess index in Belgium estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_be <- function(B_LU, A_P_AL) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = crop_k = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='BE']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'BE' & osi_indicator =='i_e_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_P_AL = A_P_AL,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = 'crop_cat1',
              by.y = 'osi_threshold_cropcat',
              all.x = TRUE)
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = A_P_AL, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}


#' Calculate the phosphorus excess index in Switzerland
#' 
#' This function calculates the phosphorus excess 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_AAA (numeric) The exchangeable P-content of the soil measured via acid ammonium acetate extraction (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_ch(A_P_AAA = 50)
#' 
#' @return 
#' The phosphorus excess index in Switzerland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_ch <- function(A_P_AAA,B_LU = NA_character_) {
  
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
                   A_P_AAA = A_P_AAA,
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
  dt[,value := osi_evaluate_logistic(x = A_P_AAA, b= -0.03340067,x0 = 93.0280189,v = 1.57792525)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  }


#' Calculate the phosphorus excess index in Czech Republic
#' 
#' This function calculates the phosphorus excess index 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_M3 (numeric) The exchangeable P-content of the soil measured via Mehlich 3 extracton (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_cz(A_P_M3 = 81)
#' 
#' @return 
#' The phosphorus excess index in Czech Republic estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_cz <- function(A_P_M3,B_LU = NA_character_) {
  
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
                   A_P_M3 = A_P_M3,
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
  dt[,value := osi_evaluate_logistic(x = A_P_M3, b= -0.02196993,x0= 146.79292298,  v= 2.49753746)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphate excess index in Germany
#' 
#' This function calculates the phosphate excess 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_CAL (numeric) The P-content of the soil extracted with ammonium lactate(mg P / kg)
#' @param A_P_DL (numeric) The P-content of the soil extracted with double lactate (mg P / kg)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_de(B_LU = 265, A_P_AL = 45,A_P_DL = 5)
#' osi_nut_p_de(B_LU = c(265,1019),A_P_AL = c(35,54),A_P_DL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate excess index in Germany stimated from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_de <- function(B_LU, A_SOM_LOI,A_P_CAL = NA_real_, A_P_DL = NA_real_) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_SOM_LOI= A_SOM_LOI,
                   A_P_CAL = A_P_CAL * 2.29 * 0.1,
                   A_P_DL = A_P_DL,
                   value1 = NA_real_,
                   value2 = NA_real_,
                   value = NA_real_)
  
  # evaluation conform VDLUFA for cropland and soil types
  dt[!is.na(A_P_CAL), value1 := osi_evaluate_logistic(A_P_CAL,b= -0.045383281,x0= 200.152028481,v=   0.001801177)]
  
  # adjust for peat soils
  dt[!is.na(A_P_CAL) & A_SOM_LOI > 20, value1 := osi_evaluate_logistic(A_P_CAL, b = -0.04638022,x0= 68.73331583,v=  1.67697573)]
  
  # evaluation conform VDLUFA for cropland and soil types
  dt[!is.na(A_P_DL), value2 := osi_evaluate_logistic(A_P_DL, b = 0.5357, x0 = -4.03796, v = 0.01856)]
  
  # set value
  dt[,value := fifelse(!is.na(A_P_CAL),value1,value2)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphate excess index in Denmark
#' 
#' This function calculates the phosphate excess 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg/kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_dk(B_LU = 265,A_P_OL = 5)
#' osi_nut_p_dk(B_LU = c(265,1019),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate excess index in Denmark derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_dk <- function(B_LU, A_P_OL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # evaluation P-Olsen for cropland and soil types
  dt[, value := osi_evaluate_logistic(A_P_OL, b = -0.02403355, x0 = 202.32313601,v =   0.13527924)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphorus excess index in Estonia
#' 
#' This function calculates the phosphorus excess 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_M3 (numeric) The exchangeable P-content of the soil measured via Mehlich 3 extracton (mg P/ kg)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_ee(A_P_M3 = 45)
#' 
#' @return 
#' The phosphorus excess index in Estonia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_ee <- function(A_P_M3,A_SOM_LOI,B_LU = NA_character_) {
  
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
                   A_P_M3 = A_P_M3,
                   A_SOM_LOI = A_SOM_LOI,
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
  dt[,value := osi_evaluate_logistic(x = A_P_M3, b = -0.01966403, x0 = 199.66773872, v= 0.36047007)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphate excess index in Spain
#' 
#' This function calculates the phosphate excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg/kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_es(B_LU = 265,A_CLAY_MI = 5,A_P_OL = 5)
#' osi_nut_p_es(B_LU = c(265,1019),A_CLAY_MI = c(5,10),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate excess index in Spain derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_es <- function(B_LU, A_CLAY_MI,A_P_OL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # assess P excess for sandy soils (Arenoso)
  dt[A_CLAY_MI < 15 & A_SAND_MI > 50, value := osi_evaluate_logistic(A_P_OL, b = -0.1369895, x0 = 22.7738155, v=  1.5927931 )]
  
  # assess P excess for loamy? soils (Franco)
  dt[A_CLAY_MI < 15 & A_SAND_MI <=50 , value := osi_evaluate_logistic(A_P_OL, b = -0.07218576, x0 = 43.60115997, v =  1.62711859)]
  
  # assess P excess for clayey soils (Arcilloso)
  dt[A_CLAY_MI > 15, value := osi_evaluate_logistic(A_P_OL, b = -0.05414748, x0 = 58.11857292, v=  1.62796595)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphorus excess index in Finland
#' 
#' This function calculates the phosphorus excess index 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_P_AA (numeric) The exchangeable P-content of the soil measured via ammonium acetate extraction
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_posphor_fi(B_LU = 'SOJ', B_TEXTURE_USDA = 'Si',A_P_AA = 45)
#' 
#' @return 
#' The phosphorus excess index in Finland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_fi <- function(B_LU, B_TEXTURE_USDA, A_P_AA,A_C_OF = 0) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FI']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_e_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA = B_TEXTURE_USDA,
                   B_SOILTYPE_AGR = NA_character_,
                   A_P_AA = A_P_AA,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # set agricultural soiltype
  dt[A_C_OF > 200, B_SOILTYPE_AGR := 'organic']
  dt[grepl('^Cl$|^SiCL$|^SaCL$',B_TEXTURE_USDA), B_SOILTYPE_AGR := 'clay']  
  dt[grepl('^ClLo$|^SiClLo$|^Lo$|^SiLo$|^LoSa$|^Si$|^SaClLo$',B_TEXTURE_USDA), B_SOILTYPE_AGR := 'loam']
  dt[is.na(B_SOILTYPE_AGR),B_SOILTYPE_AGR := 'sand']
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = 'B_SOILTYPE_AGR',
              by.y = 'osi_threshold_soilcat',
              all.x = TRUE)
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = A_P_AA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}



#' Calculate the phosphorus excess index in France
#' 
#' This function calculates the phosphorus excess index 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen
#' @param A_PH_WA (numeric) The pH measured in water.
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_fr(B_LU = 'SOJ', A_P_OL = 45, A_PH_WA = 4.5)
#' 
#' @details
#' This function does not account for variability per agricultural region. The pH is used to classify wether soils are calcareous.
#' 
#' @return 
#' The phosphate excess index in France estimated from extractable soil P Olsen (a numeric value). 
#' 
#' @export
osi_nut_p_fr <- function(B_LU, A_P_OL,A_PH_WA = NA_real_) {
  
  # set visual bindings
  value = osi_country = osi_indicator = id = crop_cat1 = B_SOILTYPE_AGR = NULL
  crop_code = crop_p = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # Load in the interal datasets
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FR']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FR' & osi_indicator =='i_e_p']
  
  # soil types
  dt.soiltype <- as.data.table(euosi::osi_soiltype)
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_OL))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_P_OL, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_p)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # estimate agricultural soil type
  dt[, B_SOILTYPE_AGR := fifelse(A_PH_WA > 8,'craie','general')]
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = c('B_SOILTYPE_AGR', 'crop_p'),
              by.y = c('osi_threshold_soilcat','osi_threshold_cropcat'),
              all.x = TRUE)
  
  # estimate OSI score
  dt[,value := osi_evaluate_logistic(x = A_P_OL, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}


#' Calculate the phosphorus excess index in Hungary
#' 
#' This function calculates the phosphorus excess. 
#' 
#' @param B_LU (character) The crop code
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_CACO3_IF (numeric) the percentage of CaCO3 (\%)
#' @param A_P_AL (numeric) The exchangeable P-content of the soil measured via Ammonium Lactate extracton (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_hu(A_P_AL = 45,A_CACO3_IF = 5,A_CLAY_MI = 5,A_SOM_LOI = 5)
#' 
#' @return 
#' The phosphorus excess index in Hungary estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_hu <- function(A_SOM_LOI,A_CLAY_MI,A_CACO3_IF,A_P_AL,B_LU = NA_character_) {
  
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
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_P_AL = A_P_AL,
                   A_CACO3_IF = A_CACO3_IF,
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
  
  # derive the OSI score for chernozem (proxied by SOM)
  dt[A_SOM_LOI > 4 & A_CACO3_IF <= 1,value := osi_evaluate_logistic(x = A_P_AL,b = -0.01696956, x0 = 189.1883, v = 1.704288)]
  dt[A_SOM_LOI > 4 & A_CACO3_IF > 1,value := osi_evaluate_logistic(x = A_P_AL, b = -0.01632613, x0 = 198.8279, v = 1.744901)]
  
  # derive the OSI score for brown forest soil (proxied by clay)
  dt[A_CLAY_MI > 20 & A_SOM_LOI <= 4 & A_CACO3_IF <= 1,value := osi_evaluate_logistic(x = A_P_AL, b = -0.02198686, x0 = 145.2304, v = 1.684570)]
  dt[A_CLAY_MI > 20 & A_SOM_LOI <= 4 & A_CACO3_IF > 1,value := osi_evaluate_logistic(x = A_P_AL, b = -0.02095636, x0 = 155.6513, v = 1.764200)]
  
  # derive the OSI score for sandy soil (proxied by clay)
  dt[A_CLAY_MI <= 20 & A_SOM_LOI <= 4 & A_CACO3_IF <= 1,value := osi_evaluate_logistic(x = A_P_AL, b = -0.02199801, x0 = 145.2238, v = 1.685114)]
  dt[A_CLAY_MI <= 20 & A_SOM_LOI <= 4 & A_CACO3_IF > 1,value := osi_evaluate_logistic(x = A_P_AL, b = -0.01544691, x0 = 221.7077, v = 1.008897)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphate excess index in Ireland
#' 
#' This function calculates the phosphate excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg/kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_ie(B_LU = 265,A_P_OL = 5)
#' osi_nut_p_ie(B_LU = c(265,1019),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate excess index in Ireland derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_ie <- function(B_LU, A_P_OL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # P index derived following P-Olsen.
  # evaluation soil P status for grasslands
  dt[, value := OBIC::evaluate_logistic(A_P_OL, b = -0.1624248,x0 = 19.3785353,v =  1.6269463)]
  
  # evaluation soil P status for other crops
  dt[, value := OBIC::evaluate_logistic(A_P_OL, b = -0.1299494,x0 = 24.2190050,v=  1.6273494)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}


#' Calculate the phosphate excess index in Italy
#' 
#' This function calculates the phosphate excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg/kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_it(B_LU = 265,A_P_OL = 5)
#' osi_nut_p_it(B_LU = c(265,1019),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate excess index in Italy derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_it <- function(B_LU, A_P_OL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # evaluation P-Olsen for cropland and soil types
  dt[, value := osi_evaluate_logistic(A_P_OL, b = -0.1299494, x0 = 24.2190050, v= 1.6273494)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphorus excess index in Latvia
#' 
#' This function calculates the phosphorus excess. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_DL (numeric) The exchangeable P-content of the soil measured via Double Lactate extraction (mg P/ kg)
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_lv(A_P_DL = 45,B_TEXTURE_USDA = 'S')
#' 
#' @return 
#' The phosphorus excess index in Latvia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_lv <- function(A_P_DL,B_TEXTURE_USDA, B_LU = NA_character_) {
  
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
                   A_P_DL = A_P_DL,
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
  dt[B_TEXTURE_USDA == 'Sa',value := osi_evaluate_logistic(x = A_P_DL, b = -0.03589604,x0 = 203.72383476,v= 0.01087945)]
  dt[B_TEXTURE_USDA %in% c('LoSa','SaLo'),
     value := osi_evaluate_logistic(x = A_P_DL, b =-0.02943368, x0 = 199.31356128, v =   0.04578143 )]
  dt[B_TEXTURE_USDA %in% c('Lo','SaCl','SaCL'),
     value := osi_evaluate_logistic(x = A_P_DL, b =-0.02536686, x0 = 199.60361851, v=   0.10392233)]
  dt[B_TEXTURE_USDA %in% c('Cl','ClLo','SiCl','SiCL', 'SiLo','Si'),
     value := osi_evaluate_logistic(x = A_P_DL, b =-0.02265603, x0 = 201.43952659,v=   0.17776923)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphorus excess index in Lithuania
#' 
#' This function calculates the phosphorus excess. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_AL (numeric) The exchangeable P-content of the soil measured via Ammonium Lactate extraction (mg P/ kg)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_lt(A_P_AL = 45)
#' 
#' @return 
#' The phosphorus excess index in Lithuania estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_lt <- function(A_P_AL,A_SOM_LOI,B_LU = NA_character_) {
  
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
                   A_P_AL = A_P_AL,
                   A_SOM_LOI = A_SOM_LOI,
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
  dt[A_SOM_LOI <= 20,value := osi_evaluate_logistic(x = A_P_AL, -0.05005517,x0= 56.52725326,v=  4.86077799)]
  dt[A_SOM_LOI > 20,value := osi_evaluate_logistic(x = A_P_AL, b= -0.02426193, x0 = 133.03847304,v=   1.72473043)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}
#' Calculate the phosphate excess index in the Netherlands
#' 
#' This function calculates the phosphate excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2
#' @param A_P_WA (numeric) The P-content of the soil extracted with water
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_nl(B_LU = 265, A_P_AL = 45, A_P_CC = 2.5)
#' osi_nut_p_nl(B_LU = c(265,1019),A_P_AL = c(35,54),A_P_CC = c(2.5,4.5), A_P_WA = c(35,65))
#' 
#' @return 
#' The phosphate excess index in the Netherlands estimated from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_nl <- function(B_LU, A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = NULL
  
  # convert B_LU to integer
  B_LU <- as.integer(B_LU)
  
  # Load in the crops data set and the parms dataset
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country =='NL']
  dt.crops[, crop_code := as.integer(crop_code)]
  
  # select parms (to check min and max, to be done later)
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # subset thresholds to Dutch situation for phosphorus
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='NL' & osi_indicator=='i_c_p']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_AL),length(A_P_CC),length(A_P_WA))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_numeric(A_P_AL, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_P_CC, lower = 0.1, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_P_WA, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  
  # check that there is only 1 scoring function for P
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # Collect the data into a table, convert to Dutch units
  dt <- data.table(id = 1:arg.length,
                   A_P_AL = A_P_AL * 2.29 * 0.1,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA,
                   B_LU = B_LU,
                   value = NA_real_
  )
  
  dt <- merge(dt,dt.crops,by.x = 'B_LU', by.y = 'crop_code',all.x=TRUE)
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # Calculate the phosphate excess for grass (PBI)
  dt[grepl("gras",crop_cat1), value := pmax(0,log(A_P_CC) * (-0.0114 * A_P_AL + 2.5) + 0.0251 * A_P_CC + 2)]
  
  # Calculate the phosphate excess for maize (PBI)
  dt[grepl("maize",crop_cat1), value := A_P_CC + 0.05 * (A_P_AL / A_P_CC)]
  
  # calculate the P-excess for arable systems, normalized to a scale with maximum around 6
  dt[grepl("arable",crop_cat1), value := A_P_WA * 0.1]
  
  # calculate the P-excess for nature 
  dt[grepl("nature",crop_cat1), value := 0]
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = value, b = -0.3248758, x0 =  9.6880541, v =  1.6272915 )]
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphorus excess index in Norway
#' 
#' This function calculates the phosphorus excess. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_AL (numeric) The exchangeable P-content of the soil measured via Ammonium Lactate (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_no(A_P_AL = 50)
#' 
#' @return 
#' The phosphorus excess index in Norway estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_no <- function(A_P_AL,B_LU = NA_character_) {
  
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
                   A_P_AL = A_P_AL,
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
  dt[,value := osi_evaluate_logistic(x = A_P_AL, b = -0.01912416, x0 = 206.32851686, v=   0.34388620)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphate excess index in Sweden
#' 
#' This function calculates the phosphate excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate (mg P / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_se(B_LU = 265,A_P_AL = 5)
#' osi_nut_p_se(B_LU = c(265,1019),A_P_AL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate excess index in Sweden derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_se <- function(B_LU, A_P_AL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_AL = A_P_AL,
                   value = NA_real_)
  
  # evaluation soil P status III for maize and cereals
  dt[, value := OBIC::evaluate_logistic(A_P_AL, b = -0.03374637, x0 =  93.47462, v= 1.636168)]
  
  # evaluation soil P status II for hostvete
  dt[, value := OBIC::evaluate_logistic(A_P_AL,b =-0.07080519, x0 = 200.73869, v= 1.209206e-05)]
  
  # evaluation soil P status III for oil crops
  dt[, value := OBIC::evaluate_logistic(A_P_AL, b = -0.03374637, x0 =  93.47462,  v= 1.636168)]
  
  # evaluation soil P status IVA for potato and sugar beet
  dt[, value := OBIC::evaluate_logistic(A_P_AL, b = -0.01969912,x0= 158.94441,v= 1.606657)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphorus excess index in  Slovak Republic
#' 
#' This function calculates the phosphorus excess. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_P_M3 (numeric) The exchangeable P-content of the soil measured via Mehlich 3 extracton (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_sk(A_P_M3 = 45)
#' 
#' @return 
#' The phosphorus excess index in Slovak Republic estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_sk <- function(B_TEXTURE_HYPRES,A_P_M3,B_LU = NA_character_) {
  
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
                   A_P_M3 = A_P_M3,
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
  dt[B_TEXTURE_HYPRES %in% c('C'),value := osi_evaluate_logistic(x = A_P_M3,b= -0.01624901,x0 = 202.99049001, v =   1.80519106 )]
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),value := osi_evaluate_logistic(x = A_P_M3,b= -0.01731495,x0 = 188.56710994, v =   1.76801999 )]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),value := osi_evaluate_logistic(x = A_P_M3, b= -0.01225165,x0 = 351.92210058, v =   0.24005529)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphorus excess index in Slovenia
#' 
#' This function calculates the phosphorus excess. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_AL (numeric) The exchangeable P-content of the soil measured via ammoniuml lactate extracton (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_sl(A_P_AL = 45)
#' 
#' @return 
#' The phosphorus excess index in Slovenia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_sl <- function(A_P_AL,B_LU = NA_character_) {
  
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
                   A_P_AL = A_P_AL,
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
  dt[,value := osi_evaluate_logistic(x = A_P_AL, b = -0.0262186, x0 = 122.4380883, v =   1.7036038 )]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}
#' Calculate the phosphorus excess index in Poland
#' 
#' This function calculates the phosphorus excess. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_DL (numeric) The exchangeable P-content of the soil measured via ammonium double lactate extracton (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_pl(A_P_DL = 45)
#' 
#' @return 
#' The phosphorus excess index in Poland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_pl <- function(A_P_DL,B_LU = NA_character_) {
  
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
                   A_P_DL = A_P_DL,
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
  dt[,value := osi_evaluate_logistic(x = A_P_DL, b = -0.02283399, x0 = 151.86202214, v =   0.78388489)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}



#' Calculate the phosphate excess index in United Kingdom
#' 
#' This function calculates the phosphate excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg P /kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_uk(B_LU = 265,A_SOM_LOI = 3,A_P_OL = 5)
#' osi_nut_p_uk(B_LU = c(265,1019),A_SOM_LOI = c(3,4),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate excess index in United Kingdom derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_uk <- function(B_LU, A_SOM_LOI,A_P_OL) {
  
  # crop properties
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # merge with crop
  dt <- merge(dt,
              dt.crops[,.(B_LU, crop_name, crop_cat1)],
              by = 'B_LU',
              all.x = TRUE)
  
  # P index derived following P-Olsen.
  
  # convert from mg / kg to mg / liter sample volume
  dt[, BDS := (1/(0.02525 * A_SOM_LOI + 0.6541))]
  dt[, A_P_OL := A_P_OL * BDS]
  
  # optimum value is index 2 for all land uses except vegetables (index 3)
  
  # evaluation P-Olsen for cropland and soil types
  dt[, value := osi_evaluate_logistic(A_P_OL, b = -0.037563483, x0 = 200.467352714, v =   0.008374629)]
  
  # assess soil P status for vegetables
  dt[grepl('vegetab|cabbag|leek|carrot|sprout|celery|onion|potato|lettu',tolower(crop_name)), 
     value := osi_evaluate_logistic(A_P_OL, b =-0.02609572, x0 = 200.37328734, v= 0.08518902  )]
  
  # select value and return
  value <- dt[,value]
  return(value)
}
