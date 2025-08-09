#' Calculate the phosphate availability index (wrapper function)
#' 
#' This function calculates the phosphate availability for all European countries (if available). 
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
#' @param A_P_AAA (numeric) The exchangeable P-content of the soil measured via acid ammonium acetate extraction (mg P / kg)
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate (mg P / kg)
#' @param A_P_CAL (numeric) The exchangeable P-content of the soil measured via Calcium Ammonium Lactate (mg P/ kg)
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2 (mg P / kg)
#' @param A_P_DL (numeric) The P-content of the soil extracted with double lactate (mg P / kg)
#' @param A_P_M3 (numeric) The exchangeable P-content of the soil measured via Mehlich 3 extracton (mg P/ kg)
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg P / kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P / kg)
#' @param B_COUNTRY (character) The country code
#' 
#' @import data.table
#' 
#' @return 
#' The phosphate availability index in European countries estimated from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor <- function(B_LU, 
                          B_SOILTYPE_AGR = NA_character_, A_CLAY_MI = NA_real_, A_SAND_MI = NA_real_,
                          A_SOM_LOI = NA_real_,A_C_OF = NA_real_,
                          A_PH_WA = NA_real_,A_PH_CC = NA_real_,A_CACO3_IF = NA_real_,
                          A_P_AAA = NA_real_,A_P_AL = NA_real_, A_P_CAL = NA_real_,
                          A_P_CC = NA_real_, A_P_DL = NA_real_, A_P_M3 = NA_real_,
                          A_P_OL = NA_real_,A_P_WA = NA_real_, 
                          B_COUNTRY) {
  
  # add visual bindings
  id = B_TEXTURE_USDA = B_TEXTURE_HYPRES = A_SILT_MI = value = NULL
  
  # note that qualitative checks on the inputs are done by the country specific functions
  
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
                   value = NA_real_
                   )
  
  # estimate texture information
  dt[,B_TEXTURE_USDA := osi_get_TEXTURE_USDA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_HYPRES := osi_get_TEXTURE_HYPRES(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  
  # estimate missing soil properties from P-Olsen (default in LUCAS)
  dt[is.na(A_PH_WA) & !is.na(A_PH_CC), A_PH_WA := osi_conv_ph(element='A_PH_WA',A_PH_CC = A_PH_CC)]
  dt[!is.na(A_PH_WA) & !is.na(A_PH_CC), A_PH_CC := osi_conv_ph(element='A_PH_CC',A_PH_WA = A_PH_WA)]
  dt[is.na(A_P_AL) & !is.na(A_P_OL), A_P_AL := osi_conv_phosphor(element='A_P_AL',A_P_OL = A_P_OL,A_PH_CC = A_PH_CC)]
  dt[is.na(A_P_CAL) & !is.na(A_P_OL), A_P_CAL := osi_conv_phosphor(element='A_P_CAL',A_P_OL = A_P_OL)]
  dt[is.na(A_P_DL) & !is.na(A_P_OL), A_P_DL := osi_conv_phosphor(element='A_P_DL',A_P_OL = A_P_OL)]
  dt[is.na(A_P_AAA) & !is.na(A_P_OL), A_P_AAA := osi_conv_phosphor(element='A_P_AAA',A_P_OL = A_P_OL)]
  dt[is.na(A_P_M3) & !is.na(A_P_OL), A_P_M3 := osi_conv_phosphor(element='A_P_M3',A_P_OL = A_P_OL)]
  dt[is.na(A_P_WA) & !is.na(A_P_OL), A_P_WA := osi_conv_phosphor(element='A_P_WA',A_P_OL = A_P_OL)]
  dt[is.na(A_P_CC) & !is.na(A_P_OL), A_P_CC := osi_conv_phosphor(element='A_P_CC',A_P_OL = A_P_OL)]
  
  # calculate the OSI score per country
  
  # Austria (AT), Belgium (BE), Switzerland (CH), Czech Republic (CZ), Germany (DE)
  dt[B_COUNTRY == 'AT', value := osi_c_phosphor_at(B_LU = B_LU, A_P_CAL = A_P_CAL)]
  dt[B_COUNTRY == 'BE', value := osi_c_phosphor_be(B_LU = B_LU, A_P_AL = A_P_AL)]
  dt[B_COUNTRY == 'CH', value := osi_c_phosphor_ch(B_LU = B_LU, A_P_AAA = A_P_AAA)]
  dt[B_COUNTRY == 'CZ', value := osi_c_phosphor_cz(B_LU = B_LU, A_P_M3 = A_P_M3)]
  dt[B_COUNTRY == 'DE', value := osi_c_phosphor_de(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_P_CAL = A_P_CAL, A_P_DL = A_P_DL)]
  
  # Denmark (DK), Estonia (EE), Spain (ES),France (FR), Finland (FI) 
  dt[B_COUNTRY == 'DK', value := osi_c_phosphor_dk(B_LU = B_LU, A_P_OL = A_P_OL)]
  dt[B_COUNTRY == 'EE', value := osi_c_phosphor_ee(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_P_M3 = A_P_M3)]
  dt[B_COUNTRY == 'ES', value := osi_c_phosphor_es(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_P_OL = A_P_OL)]
  dt[B_COUNTRY == 'FR', value := osi_c_phosphor_fr(B_LU = B_LU, B_SOILTYPE_AGR = NA_character_, B_AER_FR = NA_character_, A_P_OL= A_P_OL)]
  dt[B_COUNTRY == 'FI', value := osi_c_phosphor_fi(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_P_AAA = A_P_AAA, A_C_OF = A_C_OF )]
  
  # Hungary (HU), Ireland (IE), Italy (IT), Latvia (LV), Lithuania (LT)
  dt[B_COUNTRY == 'HU', value := osi_c_phosphor_hu(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_CACO3_IF = A_CACO3_IF,A_P_AL = A_P_AL )]
  dt[B_COUNTRY == 'IE', value := osi_c_phosphor_ie(B_LU = B_LU, A_P_OL = A_P_OL)]
  dt[B_COUNTRY == 'IT', value := osi_c_phosphor_it(B_LU = B_LU, A_P_OL = A_P_OL)]
  dt[B_COUNTRY == 'LV', value := osi_c_phosphor_lv(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_P_DL = A_P_DL)]
  dt[B_COUNTRY == 'LT', value := osi_c_phosphor_lt(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_P_AL = A_P_AL)]
  
  # the Netherlands (NL), Norway (NO),  Sweden (SE), Slovak Republic (SK), Slovenia (SL)
  dt[B_COUNTRY == 'NL', value := osi_c_phosphor_nl(B_LU = B_LU, A_P_AL = A_P_AL, A_P_CC = A_P_CC, A_P_WA = A_P_WA)]
  dt[B_COUNTRY == 'NO', value := osi_c_phosphor_no(B_LU = B_LU, A_P_AL = A_P_AL)]
  dt[B_COUNTRY == 'SE', value := osi_c_phosphor_se(B_LU = B_LU, A_P_AL = A_P_AL)]
  dt[B_COUNTRY == 'SK', value := osi_c_phosphor_sk(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_P_M3 = A_P_M3)]
  dt[B_COUNTRY == 'SL', value := osi_c_phosphor_sl(B_LU = B_LU, A_P_AL = A_P_AL)]
  
  # Poland (PL), United Kingdom (UK)
  dt[B_COUNTRY == 'PL', value := osi_c_phosphor_pl(B_LU = B_LU, A_P_DL = A_P_DL)]
  dt[B_COUNTRY == 'IE', value := osi_c_phosphor_uk(B_LU = B_LU, A_P_OL = A_P_OL)]
  
  # sort data.table
  setorder(dt,id)
  
  # select the output variable
  out <- dt[,value]
  
  # return the OSI score
  return(out)
  
}

#' Calculate the phosphorus availability index in Austria
#' 
#' This function calculates the phosphorus availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_CAL (numeric) The exchangeable P-content of the soil measured via Calcium Ammonium Lactate (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_at(A_P_CAL = 47)
#' 
#' @return 
#' The phosphorus availability index in Austria estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_at <- function(A_P_CAL,B_LU = NA_character_) {
  
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
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_P_CAL))
  
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
  dt[,value := osi_evaluate_logistic(x = A_P_CAL, b= 0.138491,x0 = 2.81405015,v = 0.01965865)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphorus availability index in Belgium
#' 
#' This function calculates the phosphorus availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_AL (numeric) The exchangeable P-content of the soil measured via ammonium lactate extraction (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_be(B_LU = 'SOJ', A_P_AL = 45)
#' 
#' @return 
#' The phosphorus availability index in Belgium estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_be <- function(B_LU, A_P_AL) {
  
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
  dt.thresholds <- dt.thresholds[osi_country == 'BE' & osi_indicator =='i_c_p']
  
  # get length of input arguments
  arg.length <- max(length(B_LU),length(A_P_AL))
  
  # Collect the data into a table, convert from mg P/kg to units being used in Belgium (mg P/100g)
  dt <- data.table(id = 1:arg.length,
                   B_LU = as.character(B_LU),
                   A_P_AL = A_P_AL * 0.1,
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
  dt[grepl('gras',crop_cat1),value := osi_evaluate_logistic(x = A_P_AL, b= 0.34526357,x0 = -5.8598799,v = 0.01086684)]
  dt[grepl('arable',crop_cat1),value := osi_evaluate_logistic(x = A_P_AL, b= 0.22848283,x0 = -7.07661435,v = 0.01468356)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphorus availability index in Switzerland
#' 
#' This function calculates the phosphorus availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_AAA (numeric) The exchangeable P-content of the soil measured via acid ammonium acetate extraction (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_ch(A_P_AAA = 50)
#' 
#' @return 
#' The phosphorus availability index in Switzerland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_ch <- function(A_P_AAA,B_LU = NA_character_) {
  
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
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_P_AAA))
  
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
  dt[,value := osi_evaluate_logistic(x = A_P_AAA, b= 0.113193,x0 = -21.4503795,v = 0.01430497)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphorus availability index in Czech Republic
#' 
#' This function calculates the phosphorus availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_M3 (numeric) The exchangeable P-content of the soil measured via Mehlich 3 extracton (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_cz(A_P_M3 = 81)
#' 
#' @return 
#' The phosphorus availability index in Czech Republic estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_cz <- function(A_P_M3,B_LU = NA_character_) {
  
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
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_P_M3))
  
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
  dt[,value := osi_evaluate_logistic(x = A_P_M3, b= 0.0890545,x0 = 4.429710417,v = 0.007972486)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphate availability index in Germany
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_CAL (numeric) The P-content of the soil extracted with ammonium lactate(mg P / kg)
#' @param A_P_DL (numeric) The P-content of the soil extracted with double lactate (mg P / kg)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_de(B_LU = 265, A_SOM_LOI = 4.5, A_P_CAL = 45,A_P_DL = 5)
#' osi_c_phosphor_de(B_LU = c(265,1019),A_SOM_LOI = c(3,3),
#' A_P_CAL = c(35,54),A_P_DL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Germany stimated from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_de <- function(B_LU, A_SOM_LOI,A_P_CAL = NA_real_, A_P_DL = NA_real_) {
  
  # add visual bindings
  value1 = value2 = NULL
  
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
  dt[!is.na(A_P_CAL), value1 := osi_evaluate_logistic(A_P_CAL, b = 0.2711, x0 = -5.9449, v = 0.0239)]
  
  # adjust for peat soils
  dt[!is.na(A_P_CAL) & A_SOM_LOI > 20, value1 := osi_evaluate_logistic(A_P_CAL, b = 0.1743, x0 = 2.92395, v = 0.096079)]
  
  # evaluation conform VDLUFA for cropland and soil types
  dt[!is.na(A_P_DL), value2 := osi_evaluate_logistic(A_P_DL, b = 0.5357, x0 = -4.03796, v = 0.01856)]
  
  # set value
  dt[,value := fifelse(!is.na(A_P_CAL),value1,value2)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphate availability index in Denmark
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg/kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_dk(B_LU = '265',A_P_OL = 5)
#' osi_c_phosphor_dk(B_LU = c('265','1019'),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Denmark derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_dk <- function(B_LU, A_P_OL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # evaluation P-Olsen for cropland and soil types
  dt[, value := osi_evaluate_logistic(A_P_OL, b = 0.226612, x0 = 30.137321,v = 1.247315)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphorus availability index in Estonia
#' 
#' This function calculates the phosphorus availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_M3 (numeric) The exchangeable P-content of the soil measured via Mehlich 3 extracton (mg P/ kg)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_ee(A_P_M3 = 45,A_SOM_LOI = 3, B_LU='23')
#' 
#' @return 
#' The phosphorus availability index in Estonia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_ee <- function(A_P_M3,A_SOM_LOI,B_LU = NA_character_) {
  
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
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_P_M3),length(A_SOM_LOI))
  
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
  dt[A_SOM_LOI <= 2,value := osi_evaluate_logistic(x = A_P_M3, b= 0.1078429,x0 = -17.16723,v = 0.0153443)]
  dt[A_SOM_LOI > 2,value := osi_evaluate_logistic(x = A_P_M3, b= 0.173065,x0 = -17.18668,v = 0.0264163)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphate availability index in Spain
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg/kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_es(B_LU = '265',A_CLAY_MI = 5,A_SAND_MI = 25,A_P_OL = 5)
#' osi_c_phosphor_es(B_LU = c('265','1019'),A_CLAY_MI = c(5,10),
#' A_SAND_MI = c(50,50),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Spain derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_es <- function(B_LU, A_CLAY_MI,A_SAND_MI, A_P_OL) {
  
  # get max length of input
  arg.length <- max(length(B_LU),length(A_CLAY_MI),length(A_SAND_MI),length(A_P_OL))
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # assess P availability for sandy soils (Arenoso)
  dt[A_CLAY_MI < 15 & A_SAND_MI > 50, value := osi_evaluate_logistic(A_P_OL, b = 0.47947, x0 = -1.94363, v = 0.074075)]
  
  # assess P availability for loamy? soils (Franco)
  dt[A_CLAY_MI < 15 & A_SAND_MI <=50 , value := osi_evaluate_logistic(A_P_OL, b = 0.27155, x0 = 2.81733, v = 0.154671)]
  
  # assess P availability for clayey soils (Arcilloso)
  dt[A_CLAY_MI > 15, value := osi_evaluate_logistic(A_P_OL, b = 0.20196, x0 = 2.87602, v = 0.133171)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphorus availability index in Finland
#' 
#' This function calculates the phosphorus availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_P_AAA (numeric) The exchangeable P-content of the soil measured via acid ammonium acetate extraction
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_fi(B_LU = 'SOJ', B_TEXTURE_USDA = 'Si',
#' A_P_AAA = 45,A_C_OF=1.5)
#' 
#' @return 
#' The phosphorus availability index in Finland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_fi <- function(B_LU, B_TEXTURE_USDA, A_P_AAA,A_C_OF = 0) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  B_SOILTYPE_AGR = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FI']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # get length of input arguments
  arg.length <- max(length(B_LU),length(B_TEXTURE_USDA),length(A_P_AAA))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA = B_TEXTURE_USDA,
                   B_SOILTYPE_AGR = NA_character_,
                   A_P_AAA = A_P_AAA,
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
  dt[,value := osi_evaluate_logistic(x = A_P_AAA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}


#' Calculate the phosphate availability index in France
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_SOILTYPE_AGR (character) The soil type in a particular region. Optional.
#' @param B_AER_FR (character) An agroeconomic region in France. Optional.
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg P/ kg)
#' @param A_PH_WA (numeric) The pH measured in water.
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_fr(B_LU = 'SOJ', A_P_OL = 45, 
#' B_SOILTYPE_AGR = 'limons battants', B_AER_FR = 'nord-picardie')
#' 
#' @details
#' The function has two optional arguments soil type (B_SOILTYPE_AGR) and agricultural region (B_AER_FR). When these are unknown, then the soil type is estimated based on the pH value. Threshold values are then generalized for calcareous and non-calcareous soils.
#' 
#' @return 
#' The phosphate availability index in France estimated from extractable soil P Olsen (a numeric value). 
#' 
#' @export
osi_c_phosphor_fr <- function(B_LU, A_P_OL,B_SOILTYPE_AGR = NA_character_, B_AER_FR = NA_character_, A_PH_WA = NA_real_) {
  
  # set visual bindings
  value = osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = crop_p = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  osi_threshold_region = NULL
  
  # Load in the interal datasets
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FR']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FR' & osi_indicator =='i_c_p']
  
  # filter the thresholds when no B_AER_FR is given
  if(sum(is.na(B_AER_FR))>0){
    dt.thresholds <- dt.thresholds[is.na(osi_threshold_region)]
  } else {
    dt.thresholds <- dt.thresholds[!is.na(osi_threshold_region)]
  }
  
  # soil types
  dt.soiltype <- as.data.table(euosi::osi_soiltype)
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_OL),length(B_SOILTYPE_AGR), length(B_AER_FR),length(A_PH_WA))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_P_OL, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 1, upper = 10, any.missing = TRUE, len = arg.length)
  
  # check optional parameters 
  if(sum(!is.na(B_SOILTYPE_AGR))>0){
    checkmate::assert_character(B_SOILTYPE_AGR, any.missing = TRUE, min.len = 1, len = arg.length)
    checkmate::assert_subset(B_SOILTYPE_AGR, choices = c(NA,unique(dt.soiltype$osi_soil_cat1)), empty.ok = FALSE)
  }
  if(sum(!is.na(B_AER_FR))>0){
    checkmate::assert_character(B_AER_FR, any.missing = TRUE, min.len = 1, len = arg.length)
    checkmate::assert_subset(B_AER_FR, choices = unique(dt.thresholds$osi_threshold_region), empty.ok = FALSE)
  }
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_AER_FR = B_AER_FR,
                   A_P_OL = A_P_OL * 2.291,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_p)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # estimate agricultural soil type
  dt[is.na(B_SOILTYPE_AGR), B_SOILTYPE_AGR := fifelse(A_PH_WA > 8,'craie','general')]
  dt[is.na(B_SOILTYPE_AGR) & is.na(A_PH_WA), B_SOILTYPE_AGR := 'general']
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = c('B_SOILTYPE_AGR','B_AER_FR', 'crop_p'),
              by.y = c('osi_threshold_soilcat','osi_threshold_region','osi_threshold_cropcat'),
              all.x = TRUE)
  
  # estimate OSI score
  dt[,value := osi_evaluate_logistic(x = A_P_OL, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}


#' Calculate the phosphorus availability index in Hungary
#' 
#' This function calculates the phosphorus availability. 
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
#' osi_c_phosphor_hu(A_P_AL = 45,A_CACO3_IF = 5,A_CLAY_MI = 5,A_SOM_LOI = 5)
#' 
#' @return 
#' The phosphorus availability index in Hungary estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_hu <- function(A_SOM_LOI,A_CLAY_MI,A_CACO3_IF,A_P_AL,B_LU = NA_character_) {
  
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
  
  # get max length of inputs
  arg.length <- max(length(A_SOM_LOI),length(A_CLAY_MI),length(A_CACO3_IF),length(A_P_AL),length(B_LU))
  
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
  dt[A_SOM_LOI > 4 & A_CACO3_IF <= 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.068946,x0 = 3.064834,v = 0.0349155)]
  dt[A_SOM_LOI > 4 & A_CACO3_IF > 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.07114313,x0 = 3.6741042,v = 0.016646)]
  
  # derive the OSI score for brown forest soil (proxied by clay)
  dt[A_CLAY_MI > 20 & A_SOM_LOI <= 4 & A_CACO3_IF <= 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.08938405,x0 = -17.8025024,v = 0.006430696)]
  dt[A_CLAY_MI > 20 & A_SOM_LOI <= 4 & A_CACO3_IF > 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.0919443,x0 = 1.95992725,v = 0.01361575)]
  
  # derive the OSI score for sandy soil (proxied by clay)
  dt[A_CLAY_MI <= 20 & A_SOM_LOI <= 4 & A_CACO3_IF <= 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.08938405,x0 = -17.80250244,v = 0.006430696)]
  dt[A_CLAY_MI <= 20 & A_SOM_LOI <= 4 & A_CACO3_IF > 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.09148938,x0 = 5.716193495,v = 0.007882485)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphate availability index in Ireland
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg/kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_ie(B_LU = '265',A_P_OL = 5)
#' osi_c_phosphor_ie(B_LU = c('265','1019'),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Ireland derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_ie <- function(B_LU, A_P_OL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # P index derived following P-Olsen.
  # evaluation soil P status for grasslands
  dt[, value := OBIC::evaluate_logistic(A_P_OL, b = 0.6560111, x0 = 3.44709, v = 0.588379)]
  
  # evaluation soil P status for other crops
  dt[, value := OBIC::evaluate_logistic(A_P_OL, b = 0.50194, x0 = 3.91821, v = 0.5799892)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}


#' Calculate the phosphate availability index in Italy
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg/kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_it(B_LU = '265',A_P_OL = 5)
#' osi_c_phosphor_it(B_LU = c('265','1019'),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Italy derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_it <- function(B_LU, A_P_OL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # evaluation P-Olsen for cropland and soil types
  dt[, value := osi_evaluate_logistic(A_P_OL, b = 0.43987, x0 = -5.7314, v = 0.011909)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphorus availability index in Latvia
#' 
#' This function calculates the phosphorus availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_DL (numeric) The exchangeable P-content of the soil measured via Double Lactate extraction (mg P/ kg)
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_lv(A_P_DL = 45,B_TEXTURE_USDA = 'S')
#' 
#' @return 
#' The phosphorus availability index in Latvia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_lv <- function(A_P_DL,B_TEXTURE_USDA, B_LU = NA_character_) {
  
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

  # get max length of inputs
  arg.length <- max(length(B_LU),length(B_TEXTURE_USDA),length(A_P_DL))
  
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
  dt[B_TEXTURE_USDA == 'Sa',value := osi_evaluate_logistic(x = A_P_DL, b= 0.249934,x0 = 2.907279,v = 0.070707)]
  dt[B_TEXTURE_USDA %in% c('LoSa','SaLo'),
     value := osi_evaluate_logistic(x = A_P_DL, b= 0.211789,x0 = 2.937473,v = 0.050529)]
  dt[B_TEXTURE_USDA %in% c('Lo','SaCl','SaCL'),
     value := osi_evaluate_logistic(x = A_P_DL, b= 0.17789728,x0 = -7.742856,v = 0.007900254)]
  dt[B_TEXTURE_USDA %in% c('Cl','ClLo','SiCl','SiCL', 'SiLo','Si'),
     value := osi_evaluate_logistic(x = A_P_DL, b= 0.1513621,x0 = -9.9252899,v = 0.00760908)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphorus availability index in Lithuania
#' 
#' This function calculates the phosphorus availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_AL (numeric) The exchangeable P-content of the soil measured via Ammonium Lactate extraction (mg P/ kg)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_lt(A_P_AL = 45,A_SOM_LOI = 4.5)
#' 
#' @return 
#' The phosphorus availability index in Lithuania estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_lt <- function(A_P_AL,A_SOM_LOI,B_LU = NA_character_) {
  
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

  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_SOM_LOI),length(A_P_AL))
  
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
  dt[A_SOM_LOI <= 20,value := osi_evaluate_logistic(x = A_P_AL, b= 0.115141,x0 = -13.66102,v = 0.0084859)]
  dt[A_SOM_LOI > 20,value := osi_evaluate_logistic(x = A_P_AL, b= 0.1014425,x0 = -10.704884,v = 0.00674754)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}
#' Calculate the phosphate availability index in the Netherlands
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate (mg P/ kg)
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2 (mg P/ kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_nl(B_LU = 265, A_P_AL = 45, A_P_CC = 2.5)
#' osi_c_phosphor_nl(B_LU = c(265,1019),A_P_AL = c(35,54),A_P_CC = c(2.5,4.5), A_P_WA = c(35,65))
#' 
#' @return 
#' The phosphate availability index in the Netherlands estimated from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_nl <- function(B_LU, A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_) {
  
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
  # assume 3.5% organic matter for the calculation of bulk density
  dt <- data.table(id = 1:arg.length,
                   A_P_AL = A_P_AL * 2.29 * 0.1,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA *(1 / (0.02525 * 3.5 + 0.6541))/0.43646,
                   B_LU = B_LU,
                   value = NA_real_
                  )
  
  dt <- merge(dt,dt.crops,by.x = 'B_LU', by.y = 'crop_code',all.x=TRUE)
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # Calculate the phosphate availability for grass (PBI)
  dt[grepl("gras",crop_cat1), value := pmax(0,log(A_P_CC) * (-0.0114 * A_P_AL + 2.5) + 0.0251 * A_P_CC + 2)]
  
  # Calculate the phosphate availability for maize (PBI)
  dt[grepl("maize",crop_cat1), value := A_P_CC + 0.05 * pmin(37,A_P_AL / A_P_CC)]
  
  # calculate the P-availability for arable systems, normalized to a scale with maximum around 6
  dt[grepl("arable",crop_cat1), value := A_P_WA * 0.1]
  
  # calculate the P-availability for nature 
  dt[grepl("nature",crop_cat1), value := 0]
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = value, b= dt.thresholds$osi_st_c1,x0 = dt.thresholds$osi_st_c2,v = dt.thresholds$osi_st_c3)]
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphorus availability index in Norway
#' 
#' This function calculates the phosphorus availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_AL (numeric) The exchangeable P-content of the soil measured via Ammonium Lactate (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_no(A_P_AL = 50)
#' 
#' @return 
#' The phosphorus availability index in Norway estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_no <- function(A_P_AL,B_LU = NA_character_) {
  
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
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_P_AL))
  
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
  dt[,value := osi_evaluate_logistic(x = A_P_AL, b= 0.09801777,x0 = -20.28710038,v = 0.0218218)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphorus availability index in Poland
#' 
#' This function calculates the phosphorus availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_DL (numeric) The exchangeable P-content of the soil measured via ammonium double lactate extracton (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_pl(A_P_DL = 45)
#' 
#' @return 
#' The phosphorus availability index in Poland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_pl <- function(A_P_DL,B_LU = NA_character_) {
  
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
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_P_DL))
  
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
  dt[,value := osi_evaluate_logistic(x = A_P_DL, b= 0.1199736,x0 = -12.565624,v = 0.0072809)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphate availability index in Sweden
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate (mg P / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_se(B_LU = 265,A_P_AL = 5)
#' osi_c_phosphor_se(B_LU = c(265,1019),A_P_AL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Sweden derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_se <- function(B_LU, A_P_AL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_AL = A_P_AL,
                   value = NA_real_)
  
  # evaluation soil P status III for maize and cereals
  dt[, value := OBIC::evaluate_logistic(A_P_AL, b = 0.126197, x0 = 14.6487, v = 0.46202)]
  
  # evaluation soil P status II for hostvete
  dt[, value := OBIC::evaluate_logistic(A_P_AL, b = 0.60458, x0 = 2.8517965, v = 0.0256494)]
  
  # evaluation soil P status III for oil crops
  dt[, value := OBIC::evaluate_logistic(A_P_AL, b = 0.126197, x0 = 14.6487, v = 0.46202)]
  
  # evaluation soil P status IVA for potato and sugar beet
  dt[, value := OBIC::evaluate_logistic(A_P_AL, b = 0.0695783, x0 = -27.867195, v = 0.0163328)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphorus availability index in  Slovak Republic
#' 
#' This function calculates the phosphorus availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_P_M3 (numeric) The exchangeable P-content of the soil measured via Mehlich 3 extracton (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_sk(A_P_M3 = 45,B_TEXTURE_HYPRES = 'C')
#' 
#' @return 
#' The phosphorus availability index in Slovak Republic estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_sk <- function(B_TEXTURE_HYPRES,A_P_M3,B_LU = NA_character_) {
  
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
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(B_TEXTURE_HYPRES),length(A_P_M3))
  
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
  dt[B_TEXTURE_HYPRES %in% c('C'),value := osi_evaluate_logistic(x = A_P_M3, b= 0.07743388,x0 = -1.23994065,v = 0.00401143)]
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),value := osi_evaluate_logistic(x = A_P_M3, b= 0.07683386,x0 = -7.05760285,v = 0.00574734)]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),value := osi_evaluate_logistic(x = A_P_M3, b= 0.089515,x0 = 3.0679272,v = 0.01673862)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphorus availability index in Slovenia
#' 
#' This function calculates the phosphorus availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_AL (numeric) The exchangeable P-content of the soil measured via ammoniuml lactate extracton (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_sl(A_P_AL = 45)
#' 
#' @return 
#' The phosphorus availability index in Slovenia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_sl <- function(A_P_AL,B_LU = NA_character_) {
  
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

  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_P_AL))
  
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
  dt[,value := osi_evaluate_logistic(x = A_P_AL, b= 0.1080206,x0 = -11.5603261,v = 0.00766528)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}



#' Calculate the phosphate availability index in United Kingdom
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg P /kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_uk(B_LU = 265,A_SOM_LOI = 3,A_P_OL = 5)
#' osi_c_phosphor_uk(B_LU = c(265,1019),A_SOM_LOI = c(3,4),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in United Kingdom derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_uk <- function(B_LU, A_SOM_LOI,A_P_OL) {
  
  # set visual bindings
  crop_name = crop_cat1 = BD = . = NULL
  
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
  dt[, BD := (1/(0.02525 * A_SOM_LOI + 0.6541))]
  dt[, A_P_OL := A_P_OL * BD]
  
  # optimum value is index 2 for all land uses except vegetables (index 3)
  
  # evaluation P-Olsen for cropland and soil types
  dt[, value := osi_evaluate_logistic(A_P_OL, b = 0.2417380 , x0 = 7.8550060 , v = 0.5393981 )]
  
  # assess soil P status for vegetables
  dt[grepl('vegetab|cabbag|leek|carrot|sprout|celery|onion|potato|lettu',tolower(crop_name)), 
     value := osi_evaluate_logistic(A_P_OL, b = 0.17689431 , x0 = 0.45050382 , v = 0.05213803 )]
  
  # select value and return
  value <- dt[,value]
  return(value)
}















