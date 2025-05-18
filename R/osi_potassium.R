#' Calculate the K availability index (wrapper function)
#' 
#' This function calculates the potassium availability for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_AER_FR (character) An agroeconomic region in France
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analyzed via Cobalt-hexamine extraction
#' @param A_K_AA (numeric) The exchangeable K-content of the soil measured via ammonium acetate extraction 
#' @param A_K_AL (numeric) The exchangeable K-content of the soil measured via Ammonium Lactate extracton (mg K/ kg)
#' @param A_K_AN (numeric) The K-content of the soil extracted with ammonium nitrate (mg K /kg)
#' @param A_K_CAL (numeric) The exchangeable K-content of the soil measured via Calcium Ammonium Lactate (mg K/ kg)
#' @param A_K_CC (numeric) The plant available potassium, extracted with 0.01M CaCl2 (mg / kg), 
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_K_DL (numeric) The exchangeable K-content of the soil measured via Double Lactate extraction (mg K/ kg)
#' @param A_K_M3 (numeric) The exchangeable K-content of the soil measured via Mehlich 3 extracton (mg K/ kg)
#' @param A_K_NaAAA (numeric) The K-content of the soil extracted with Morgan's solution, sodium acetate acetic acid (mg/ kg)
#' @param B_COUNTRY (character) The country code
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium(B_LU = 265, B_SOILTYPE_AGR = 'dekzand',A_SOM_LOI = 4, 
#' A_CLAY_MI = 11,A_PH_CC = 5.4, A_CEC_CO = 125, 
#' A_K_CO_PO = 8.5, A_K_CC = 145,B_COUNTRY = 'NL')
#' 
#' @return
#' The capacity of the soil to supply and buffer potassium, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_c_potassium <- function(B_LU, B_SOILTYPE_AGR = NA_character_,
                            A_SOM_LOI = NA, A_C_OF = NA, 
                            A_CLAY_MI = NA,A_SAND_MI = NA,
                            A_PH_CC = NA, A_PH_WA = NA,
                            A_CEC_CO = NA, 
                            A_K_AA = NA,A_K_AL = NA,A_K_AN = NA,A_K_CAL = NA,A_K_CC = NA,
                            A_K_CO_PO = NA,A_K_DL = NA,A_K_M3 = NA,A_K_NaAAA = NA,
                            B_COUNTRY) {
  
  # add visual bindings
  i_c_k = NULL
  
  # desired length of inputs
  arg.length <- max(length(A_PH_CC), length(A_SOM_LOI), length(A_CEC_CO),length(A_CLAY_MI), length(B_SOILTYPE_AGR),
                    length(B_LU),
                    length(B_COUNTRY))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_COUNTRY = B_COUNTRY,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_PH_CC = A_PH_CC,
                   A_PH_WA = A_PH_WA,
                   A_CEC_CO = A_CEC_CO,
                   A_K_AA = A_K_AA,
                   A_K_AL = A_K_AL,
                   A_K_AN = A_K_AN,
                   A_K_CAL = A_K_CAL,
                   A_K_CC = A_K_CC,
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_DL = A_K_DL,
                   A_K_M3 = A_K_M3,
                   A_K_NaAAA = A_K_NaAAA,
                   value = NA_real_
                   )
  
  # estimate texture information
  dt[,B_TEXTURE_USDA := osi_get_TEXTURE_USDA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_HYPRES := osi_get_TEXTURE_HYPRES(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_BE := osi_get_TEXTURE_BE(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_GEPPA := osi_get_TEXTURE_GEPPA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  
  # estimate missing soil properties (from defaults in LUCAS)
  dt[is.na(A_PH_WA) & !is.na(A_PH_CC), A_PH_WA := osi_conv_ph(element='A_PH_WA',A_PH_CC = A_PH_CC)]
  dt[!is.na(A_PH_WA) & is.na(A_PH_CC), A_PH_CC := osi_conv_ph(element='A_PH_CC',A_PH_WA = A_PH_WA)]
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 2]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 * 0.5]
  
  # calculate the OSI score per country
  
  # Austria (AT), Belgium (BE), Switzerland (CH), Czech Republic (CZ), Germany (DE)
  dt[B_COUNTRY == 'AT', value := osi_c_potassium_at(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,A_K_CAL = A_K_CAL)]
  dt[B_COUNTRY == 'BE', value := osi_c_potassium_be(B_LU = B_LU, B_TEXTURE_BE = B_TEXTURE_BE,A_K_AA  = A_K_AA )]
  dt[B_COUNTRY == 'CH', value := osi_c_potassium_ch(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_K_AAA = A_K_AAA)]
  dt[B_COUNTRY == 'CZ', value := osi_c_potassium_cz(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,A_K_M3 = A_K_M3)]
  dt[B_COUNTRY == 'DE', value := osi_c_potassium_de(B_LU = B_LU, A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_K_AA = A_K_AA)]
  
  # Denmark (DK), Estonia (EE), Spain (ES),France (FR), Finland (FI) 
  dt[B_COUNTRY == 'DK', value := NA_real_]
  dt[B_COUNTRY == 'EE', value := osi_c_potassium_ee(B_LU = B_LU,B_TEXTURE_USDA = B_TEXTURE_USDA,A_K_M3 = A_K_M3)]
  dt[B_COUNTRY == 'ES', value := osi_c_potassium_es(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,A_K_AA = A_K_AA)]
  dt[B_COUNTRY == 'FR', value := osi_c_potassium_fr(B_LU = B_LU, B_TEXTURE_GEPPA  = B_TEXTURE_GEPPA, A_PH_WA = A_PH_WA,
                                                    B_AER_FR = NA_character_, A_K_AA = A_K_AA)]
  dt[B_COUNTRY == 'FI', value := osi_c_potassium_fi(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_K_AA = A_K_AA, A_C_OF = A_C_OF)]
  
  # Hungary (HU), Ireland (IE), Italy (IT), Latvia (LV), Lithuania (LT)
  dt[B_COUNTRY == 'HU', value := osi_c_potassium_hu(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_CACO3_IF = A_CACO3_IF,A_K_AL = A_K_AL)]
  dt[B_COUNTRY == 'IE', value := osi_c_potassium_ie(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI,A_K_NaAAA = A_K_NaAAA)]
  dt[B_COUNTRY == 'IT', value := osi_c_potassium_it(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_K_AA = A_K_AA)]
  dt[B_COUNTRY == 'LV', value := osi_c_potassium_lv(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_K_DL = A_K_DL)]
  dt[B_COUNTRY == 'LT', value := osi_c_potassium_lt(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_K_AL = A_K_AL)]
  
  # the Netherlands (NL), Norway (NO),  Sweden (SE), Slovak Republic (SK), Slovenia (SL)
  dt[B_COUNTRY == 'NL', value := osi_c_potassium_nl(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                                    A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,
                                                    A_PH_CC = A_PH_CC, A_CEC_CO = A_CEC_CO, 
                                                    A_K_CO_PO = A_K_CO_PO, A_K_CC = A_K_CC)]
  
  dt[B_COUNTRY == 'NO', value := osi_c_potassium_no(B_LU = B_LU, A_K_AL = A_K_AL,A_CLAY_MI = A_CLAY_MI)]
  dt[B_COUNTRY == 'SE', value := osi_c_potassium_se(B_LU = B_LU, A_K_AL = A_K_AL)]
  dt[B_COUNTRY == 'SK', value := osi_c_potassium_sk(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_K_M3 = A_K_M3)]
  dt[B_COUNTRY == 'SL', value := osi_c_potassium_sl(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_K_AL = A_K_AL)]
  
  # Poland (PL), United Kingdom (UK)
  dt[B_COUNTRY == 'PL', value := osi_c_potassium_pl(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_K_DL = A_K_DL)]
  dt[B_COUNTRY == 'UK', value := osi_c_posphor_uk(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_K_AN = A_K_AN)]
  
  # select the output variable
  value <- dt[,value]
  
  # return the OSI score
  return(value)
  
}

#' Calculate the potassium availability index in Austria
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_CAL (numeric) The exchangeable K-content of the soil measured via Calcium Ammonium Lactate (mg K/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_at(A_K_CAL = 47,B_TEXTURE_HYPRES)
#' 
#' @return 
#' The potassium availability index in Austria estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_c_potassium_at <- function(A_K_CAL,B_TEXTURE_HYPRES,B_LU = NA_character_) {
  
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
                   A_K_CAL = A_K_CAL,
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
     value := osi_evaluate_logistic(x = A_K_CAL, b= 0.07778603,x0 = 3.34792775,v = 0.01237349)]
  # calculate the OSI score medium texture soils
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_K_CAL, b= 0.063061856,x0 = 3.246009394,v = 0.008754153)]
  # calculate the OSI score for heavy textured soils
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_CAL, b= 0.05395119,x0 = 1.739323688,v = 0.005680624)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium availability index in Belgium
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_BE (character) The soil texture according to Belgium classification system
#' @param A_K_AA (numeric) The exchangeable K-content of the soil measured via ammonium acetate extraction
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_be(B_LU = 'SOJ',B_TEXTURE_BE, A_K_AA = 45)
#' 
#' @return 
#' The potassium availability index in Belgium estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_c_potassium_be <- function(B_LU, B_TEXTURE_BE, A_K_AA = NA_real_) {
  
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
  dt.thresholds <- dt.thresholds[osi_country == 'BE' & osi_indicator =='i_c_k']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_BE = B_TEXTURE_BE,
                   A_K_AA = A_K_AA,
                   value = NA_real_)
  
  # set soil type to categories
  dt[B_TEXTURE_BE %in% c('S','Z'),B_SOILTYPE_AGR := 'zand']
  dt[B_TEXTURE_BE %in% c('P','L'),B_SOILTYPE_AGR := 'zandleem']
  dt[B_TEXTURE_BE %in% c('A'),B_SOILTYPE_AGR := 'leem']
  dt[is.na(B_SOILTYPE_AGR), B_SOILTYPE_AGR := 'polder']
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = c('B_SOILTYPE_AGR', 'crop_cat1'),
              by.y = c('osi_threshold_soilcat','osi_threshold_cropcat'),
              all.x = TRUE)
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = A_K_AA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium availability index in Switzerland
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_K_AAA (numeric) The exchangeable K-content of the soil measured via acid ammonium acetate extraction (mg K/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_ch(A_K_AAA = 50)
#' 
#' @return 
#' The potassium availability index in Switzerland estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_c_potassium_ch <- function(A_K_AAA,A_CLAY_MI,B_LU = NA_character_) {
  
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
                   A_K_AAA = A_K_AAA,
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
  
  # convert to the OSI score (Tabelle 17)
  
  # arable crops, maize and grassland / vegetables are mainly different at higher level (class H to VH)
  # so no need to differentiate
  dt[A_CLAY_MI <= 10, value := osi_evaluate_logistic(x = A_K_AAA, b= 0.02920275,x0 = 5.86503285,v = 0.14855417)]
  dt[A_CLAY_MI > 10 & A_CLAY_MI <= 20, value := osi_evaluate_logistic(x = A_K_AAA, b= 0.03016788,x0 = 0.47412598,v = 0.21149593)]
  dt[A_CLAY_MI > 20 & A_CLAY_MI <= 30, value := osi_evaluate_logistic(x = A_K_AAA, b= 0.03615689,x0 = 0.62427132 ,v = 0.26501704)]
  dt[A_CLAY_MI > 30 & A_CLAY_MI <= 40, value := osi_evaluate_logistic(x = A_K_AAA, b= 0.03888415,x0 = -62.31854852 ,v = 0.03100743)]
  dt[A_CLAY_MI > 40, value := osi_evaluate_logistic(x = A_K_AAA, b= 0.08417474 ,x0 = -22.01125870 ,v = 0.04457961)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium availability index in Czech Republic
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_M3 (numeric) The exchangeable K-content of the soil measured via Mehlich 3 extracton (mg K/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_cz(A_K_M3 = 81,B_TEXTURE_HYPRES='C')
#' 
#' @return 
#' The potassium availability index in Czech Republic estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_c_potassium_cz <- function(A_K_M3,B_TEXTURE_HYPRES,B_LU = NA_character_) {
  
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
                   B_TEXTURE_HYPRES=B_TEXTURE_HYPRES,
                   A_K_M3 = A_K_M3,
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
     value := osi_evaluate_logistic(x = A_K_M3, b= 0.04721530,x0 = 0.86394309,v = 0.00416517)]
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_K_M3, b= 0.044200350,x0 = 3.073835166,v = 0.004912691)]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_M3, b= 0.03095766,x0 = 91.95343771,v = 0.03970912)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium availability index in Germany
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_C_OF (numeric) The carbon content of the soil layer (g/ kg)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_K_AA (numeric) The potassium content extracted with AA (g / kg)
#' 
#' @import data.table
#' 
#' @return 
#' The potassium availability index in Germany derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_c_potassium_de <- function(B_LU, A_C_OF, A_CLAY_MI,A_SAND_MI, A_K_AA) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_C_OF= A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_K_AA = A_K_AA,
                   value = NA_real_)
  
  # add soil type
  dt[A_SAND_MI >= 85 & A_SILT_MI <= 25 & A_CLAY_MI <= 5 & A_C_OF < 150, stype := "BG1"]
  dt[A_SAND_MI >= 42 & A_SAND_MI <= 95 & A_SILT_MI <= 40 & A_CLAY_MI <= 17 & A_C_OF < 150,  stype :="BG2"]
  dt[A_SAND_MI >= 33 & A_SAND_MI <= 83 & A_SILT_MI <= 50 & A_CLAY_MI >= 8 & A_CLAY_MI <= 25 & A_C_OF < 150,  stype :="BG3"]
  dt[A_SAND_MI <= 75 & A_SILT_MI <= 100 & A_CLAY_MI <= 35 & A_C_OF < 150,  stype :="BG4"]
  dt[A_SAND_MI <= 65 & A_SILT_MI <= 75 & A_CLAY_MI >= 25 & A_CLAY_MI <= 100 & A_C_OF < 150, stype := "BG5"]
  dt[ A_C_OF >= 150,  stype := "BG6"]
  
  # evaluate A_K_AA for arable soils
  dt[stype=='BG1' & B_LU_CAT=='arable', value := osi_evaluate_logistic(A_K_AA, b = 0.4488277, x0 = 9.718321, v = 1.254134)]
  dt[stype=='BG2' & B_LU_CAT=='arable', value := osi_evaluate_logistic(A_K_AA, b = 0.4025205, x0 = 11.358496, v = 1.186427)]
  dt[stype=='BG3' & B_LU_CAT=='arable', value := osi_evaluate_logistic(A_K_AA, b = 0.3578666, x0 = 14.656902, v = 1.373431)]
  dt[stype=='BG4' & B_LU_CAT=='arable', value := osi_evaluate_logistic(A_K_AA, b = 0.3457865, x0 = 17.303152, v = 1.543785)]
  dt[stype=='BG5' & B_LU_CAT=='arable', value := osi_evaluate_logistic(A_K_AA, b = 0.2675499, x0 = 25.566018, v = 1.827952)]
  dt[stype=='BG6' & B_LU_CAT=='arable', value := osi_evaluate_logistic(A_K_AA, b = 0.3735224, x0 = 17.424734, v = 1.874988)]
  
  # evaluate A_K_AA for grassland soils
  dt[stype=='BG1' & B_LU_CAT=='grassland', value := osi_evaluate_logistic(A_K_AA, b = 9.030400, x0 = 6.895466, v = 5.5870309)]
  dt[stype=='BG2' & B_LU_CAT=='grassland', value := osi_evaluate_logistic(A_K_AA, b = 5.269679, x0 = 8.321927, v = 4.4726794)]
  dt[stype=='BG3' & B_LU_CAT=='grassland', value := osi_evaluate_logistic(A_K_AA, b = 4.616016, x0 = 9.324117, v = 4.1282965)]
  dt[stype=='BG4' & B_LU_CAT=='grassland', value := osi_evaluate_logistic(A_K_AA, b = 1.713005, x0 = 15.406360, v = 0.4744978)]
  dt[stype=='BG5' & B_LU_CAT=='grassland', value := osi_evaluate_logistic(A_K_AA, b = 1.595327, x0 = 15.460018, v = 0.1341047)]
  dt[stype=='BG6' & B_LU_CAT=='grassland', value := osi_evaluate_logistic(A_K_AA, b = 1.662136, x0 = 15.228511, v = 0.4373691)]
  
  # select value and return
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the potassium availability index in Estonia
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param A_K_M3 (numeric) The exchangeable K-content of the soil measured via Mehlich 3 extracton (mg P/ kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_ee(A_K_M3 = 45,B_TEXTURE_USDA = 'clay')
#' 
#' @return 
#' The potassium availability index in Estonia estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_c_potassium_ee <- function(A_K_M3,B_TEXTURE_USDA,B_LU = NA_character_) {
  
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
                   A_K_M3 = A_K_M3,
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
  dt[B_TEXTURE_USDA == 'Sa',value := osi_evaluate_logistic(x = A_K_M3, b= 0.111228,x0 = 4.8454716,v = 0.00894408)]
  dt[B_TEXTURE_USDA %in% c('LoSa'),value := osi_evaluate_logistic(x = A_K_M3, b= 0.0681246,x0 = 2.8658315,v = 0.01832195)]
  dt[B_TEXTURE_USDA %in% c('SaLo'),value := osi_evaluate_logistic(x = A_K_M3, b= 0.06901738,x0 = 3.22236569,v = 0.006392278)]
  dt[B_TEXTURE_USDA %in% c('Lo','SaCl','SaCL'),
     value := osi_evaluate_logistic(x = A_K_M3, b= 0.04955406,x0 = 2.80768003,v = 0.01280431)]
  dt[B_TEXTURE_USDA %in% c('Cl','ClLo','SiCL','SiClLo','SiLo','Si'),
     value := osi_evaluate_logistic(x = A_K_M3, b= 0.0429129,x0 = 1.37161548,v = 0.001844779)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium availability index in Spain
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_AA (numeric) The K-content of the soil extracted with ammoninium acetate (mg K/kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_es(B_LU = 265,A_K_AA = 5,B_TEXTURE_HYPRES='C')
#' osi_c_potassium_es(B_LU = c(265,1019),A_K_AA = c(3.5,5.5),B_TEXTURE_HYPRES=c('C','C'))
#' 
#' @return 
#' The potassium availability index in Spain derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_c_potassium_es <- function(B_LU, B_TEXTURE_HYPRES,A_K_AA) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_K_AA = A_K_AA,
                   value = NA_real_)
  
  # franco = loam (M) ; arena = sand (C); silt = limoso (MF); arcilla = clay (F to VF)
  
  # evaluate the OSI score for arenose
  dt[B_TEXTURE_HYPRES %in% c('C'),
     value := osi_evaluate_logistic(x = A_K_AA, b= 0.02948845,x0 = 23.21207454,v = 0.25785455)]
  # evaluate the OSI score for franco
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_K_AA, b= 0.02108017,x0 = 5.43747198,v = 0.17298447)]
  # evaluate the OSI score for arcilloso
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_AA, b= 0.01665276,x0 = 5.18031011,v = 0.17195884)]
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the potassium availability index in France
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_GEPPA (character) The soil texture class in a particular region. 
#' @param B_AER_FR (character) An agroeconomic region in France. Optional argument.
#' @param A_K_AA (numeric) The exchangeable K-content of the soil measured via ammonium acetate extraction
#' @param A_PH_WA (numeric) The pH measured in water.
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_fr(B_LU = 'SOJ', A_K_AA = 45,
#' B_SOILTYPE_AGR = 'limons battants', B_AER_FR = 'nord picardie')
#' 
#' @details
#' The function has two optional arguments soil type (B_SOILTYPE_AGR) and agricultural region (B_AER_FR). When these are unknown, then the soil type is estimated based on the pH value. Threshold values are then generalized for calcareous and non-calcareous soils.
#' 
#' @return 
#' The potassium availability index in France estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_c_potassium_fr <- function(B_LU, A_K_AA, B_TEXTURE_GEPPA = NA_character_, B_AER_FR = NA_character_, A_PH_WA = NA_real_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = crop_k = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FR']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FR' & osi_indicator =='i_c_k']
  
  # filter the thresholds when no B_AER_FR is given
  if(sum(is.na(B_AER_FR))>0){
    dt.thresholds <- dt.thresholds[is.na(osi_threshold_region)]
  } else {
    dt.thresholds <- dt.thresholds[!is.na(osi_threshold_region)]
  }
  
  # soil types
  dt.soiltype <- as.data.table(euosi::osi_soiltype)
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(B_TEXTURE_GEPPA),length(B_AER_FR),length(A_K_AA))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_subset(B_TEXTURE_GEPPA, choices =c("L","LL","Ls","Lsa","La","LAS","AA","A","As","Als","Al","Sa","Sal","S","SS","Sl"), empty.ok = FALSE)
  checkmate::assert_character(B_AER_FR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_AER_FR, choices = unique(dt.thresholds$osi_threshold_region), empty.ok = FALSE)
  checkmate::assert_numeric(A_K_AA, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_GEPPA = B_TEXTURE_GEPPA,
                   B_AER_FR = B_AER_FR,
                   A_K_AA = A_K_AA,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_k)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)

  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = c('B_TEXTURE_GEPPA','B_AER_FR', 'crop_k'),
              by.y = c('osi_threshold_soilcat','osi_threshold_region','osi_threshold_cropcat'),
              all.x = TRUE)
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = A_K_AA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}


#' Calculate the potassium availability index in Hungary
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_CACO3_IF (numeric) the percentage of CaCO3 (\%)
#' @param A_K_AL (numeric) The exchangeable K-content of the soil measured via Ammonium Lactate extracton (mg K/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_hu(A_K_AL = 45,A_CACO3_IF = 5,A_CLAY_MI = 5,A_SOM_LOI = 5)
#' 
#' @return 
#' The potassium availability index in Hungary estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_c_potassium_hu <- function(A_SOM_LOI,A_CLAY_MI,A_CACO3_IF,A_K_AL,B_LU = NA_character_) {
  
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
                   A_K_AL = A_K_AL,
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
  dt[A_SOM_LOI > 4 & A_CACO3_IF <= 1,
     value := osi_evaluate_logistic(x = A_K_AL, b= 0.054768759 ,x0 = 3.135654568 ,v = 0.006806117 )]
  dt[A_SOM_LOI > 4 & A_CACO3_IF > 1,
     value := osi_evaluate_logistic(x = A_K_AL, b= 0.046479554 ,x0 = 2.791118699 ,v = 0.005011318 )]
  
  # derive the OSI score for brown forest soil (proxied by clay)
  dt[A_CLAY_MI > 20 & A_SOM_LOI <= 4 & A_CACO3_IF <= 1,
     value := osi_evaluate_logistic(x = A_K_AL, b= 0.068578055,x0 = 2.128475547,v = 0.008613139)]
  dt[A_CLAY_MI > 20 & A_SOM_LOI <= 4 & A_CACO3_IF > 1,
     value := osi_evaluate_logistic(x = A_K_AL, b= 0.054626825,x0 = 2.745261676,v = 0.003915165)]
  
  # derive the OSI score for sandy soil (proxied by clay)
  dt[A_CLAY_MI <= 20 & A_SOM_LOI <= 4 & A_CACO3_IF <= 1,
     value := osi_evaluate_logistic(x = A_K_AL, b= 0.089604257,x0 = 3.920048277,v = 0.007403738)]
  dt[A_CLAY_MI <= 20 & A_SOM_LOI <= 4 & A_CACO3_IF > 1,
     value := osi_evaluate_logistic(x = A_K_AL, b= 0.0890913887,x0 = 2.2516305413,v = 0.0001867756)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium availability index in Ireland
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_K_NaAAA (numeric) The K-content of the soil extracted with Morgan's solution, sodium acetate acetic acid (mg/ kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_ie(B_LU = 265,A_SOM_LOI = 2,A_K_NaAAA = 5)
#' osi_c_potassium_ie(B_LU = c(265,1019),A_SOM_LOI = c(2,4),A_K_NaAAA = c(3.5,5.5))
#' 
#' @return 
#' The potassium availability index in Ireland derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_c_potassium_ie <- function(B_LU, A_SOM_LOI,A_K_NaAAA) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_K_NaAAA = A_K_NaAAA,
                   value = NA_real_)
  
  # Index derived following K index system (K in mg/L)
  
  # convert from mg / kg to mg / liter sample volume
  dt[, BDS := (1/(0.02525 * A_SOM_LOI + 0.6541))]
  dt[,A_K_NaAAA := A_K_NaAAA * BDS]
 
  # evaluation soil K status
  dt[A_SOM_LOI <= 20, value := osi_evaluate_logistic(A_K_NaAAA, b = 0.04081745, x0 = 53.86819280, v = 0.66253526)]
  dt[A_SOM_LOI > 20, value := osi_evaluate_logistic(A_K_NaAAA, b = 0.02000395 , x0 = 10.63516808  , v = 0.15763251 )]
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the potassium availability index in Italy
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_AA (numeric) The K-content of the soil extracted with ammoninium acetate (mg K/kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_it(B_LU = 265,A_K_AA = 5,B_TEXTURE_HYPRES='C')
#' osi_c_potassium_it(B_LU = c(265,1019),A_K_AA = c(3.5,5.5),B_TEXTURE_HYPRES=c('C','C'))
#' 
#' @return 
#' The potassium availability index in Italy derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_c_potassium_it <- function(B_LU, B_TEXTURE_HYPRES,A_K_AA) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_K_AA = A_K_AA,
                   value = NA_real_)
  
  # franco = loam (M) ; arena = sand (C); silt = limoso (MF); arcilla = clay (F to VF)
  
  # evaluate the OSI score for terreni sabbiosi
  dt[B_TEXTURE_HYPRES %in% c('C'),
     value := osi_evaluate_logistic(x = A_K_AA, b= 0.07914907 ,x0 = 33.82005169  ,v = 0.05062077 )]
  # evaluate the OSI score for terreni medio impasto
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_K_AA, b= 0.075840953 ,x0 = 5.349772330 ,v = 0.001498863 )]
  # evaluate the OSI score for terreni argillosi e limos
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_AA, b= 0.04242224 ,x0 = 5.62056414 ,v = 0.01662994 )]
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the potassium availability index in Latvia
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_K_DL (numeric) The exchangeable K-content of the soil measured via Double Lactate extraction (mg K/ kg)
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_lv(A_K_DL = 45,B_TEXTURE_USDA='sand')
#' 
#' @return 
#' The potassium availability index in Latvia estimated from extractable potassium A numeric value.
#' 
#' @export
osi_c_potassium_lv <- function(A_K_DL,B_TEXTURE_USDA, B_LU = NA_character_) {
  
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
                   A_K_DL = A_K_DL,
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
  dt[B_TEXTURE_USDA == 'S',value := osi_evaluate_logistic(x = A_K_DL, b= 0.11109104,x0 = -11.43898675,v = 0.007796498)]
  dt[B_TEXTURE_USDA %in% c('LoSa',
                           'SaLo'),value := osi_evaluate_logistic(x = A_K_DL, b= 0.085102248,x0 = -17.31157273,v = 0.00616734)]
  dt[B_TEXTURE_USDA %in% c('Lo','SaCl',
                           'SaCL'),value := osi_evaluate_logistic(x = A_K_DL, b= 0.0754598,x0 = 3.001728,v = 0.03331127)]
  dt[B_TEXTURE_USDA %in% c('Cl','ClLo',
                           'SiCL','SiClLo',
                           'SiLo','Si'),value := osi_evaluate_logistic(x = A_K_DL, b= 0.0685035,x0 = 2.97672246,v = 0.03203499)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium availability index in Lithuania
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_K_AL (numeric) The exchangeable K-content of the soil measured via Ammonium Lactate extraction (mg P/ kg)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_lt(A_K_AL = 45,A_SOM_LOI= 1.5)
#' 
#' @return 
#' The potassium availability index in Lithuania estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_c_potassium_lt <- function(A_K_AL,A_SOM_LOI,B_LU = NA_character_) {
  
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
                   A_K_AL = A_K_AL,
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
  dt[A_SOM_LOI <= 20,value := osi_evaluate_logistic(x = A_K_AL, b= 0.06542787,x0 = -23.62084916,v = 0.006326486)]
  dt[A_SOM_LOI > 20,value := osi_evaluate_logistic(x = A_K_AL, b= 0.04211846,x0 = 3.19983935,v = 0.007473128)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium availability index in Finland
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_K_AA (numeric) The exchangeable K-content of the soil measured via ammonium acetate extraction
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_fi(B_LU = 'SOJ', B_TEXTURE_USDA = 'Si',A_K_AA = 45)
#' 
#' @return 
#' The potassium availability index in Finland estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_c_potassium_fi <- function(B_LU, B_TEXTURE_USDA, A_K_AA,A_C_OF = 0) {
  
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
  dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_k']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA = B_TEXTURE_USDA,
                   B_SOILTYPE_AGR = NA_character_,
                   A_K_AA = A_K_AA,
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
  dt[,value := osi_evaluate_logistic(x = A_K_AA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium availability index in Norway
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_K_AL (numeric) The K-content of the soil extracted with ammonium lactate (mg K / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_no(B_LU = 265,A_K_AL = 5,A_CLAY_MI=5)
#' osi_c_potassium_no(B_LU = c(265,1019),A_K_AL = c(3.5,5.5),A_CLAY_MI=c(3,5))
#' 
#' @return 
#' The potassium availability index in Norway derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_c_potassium_no <- function(B_LU, A_K_AL,A_CLAY_MI) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_K_AL = A_K_AL,
                   value = NA_real_)
  
  # evaluation soil K status
  # https://www.nibio.no/tema/jord/gjodslingshandbok/korreksjonstabeller/kaliumkorreksjon-til-eng
  # https://www.nibio.no/tema/jord/gjodslingshandbok/korreksjonstabeller/kalium--korn-oljevekster-potet-og-gronnsaker
  dt[, value := OBIC::evaluate_logistic(A_K_AL, b = 0.05548162  , x0 = 0.66920970  , v = 0.01696135 )]
  
  # select value 
  value <- dt[,value]
  
  # return value
  return(value)
}
#' Calculate the K availability index for the Netherlands 
#' 
#' This function calculates the K availability of a soil, using the agronomic index used in the Netherlands.
#' 
#' @param B_LU (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analyzed via Cobalt-hexamine extraction
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_K_CC (numeric) The plant available potassium, extracted with 0.01M CaCl2 (mg / kg), 
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_nl(B_LU = 265, B_SOILTYPE_AGR = 'dekzand',
#' A_SOM_LOI = 4, A_CLAY_MI = 11,A_PH_CC = 5.4, A_CEC_CO = 125, 
#' A_K_CO_PO = 8.5, A_K_CC = 145)
#' osi_c_potassium_nl(265, 'dekzand',4, 11,5.4,  125,8.5, 145)
#' osi_c_potassium_nl(c(265,1019), rep('dekzand',2),c(4,6), c(11,14),
#' c(5.4,5.6),  c(125,145),c(8.5,3.5), c(145,180))
#' 
#' @return
#' The capacity of the soil to supply and buffer potassium, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_c_potassium_nl <- function(B_LU, B_SOILTYPE_AGR,A_SOM_LOI, A_CLAY_MI,A_PH_CC, 
                               A_CEC_CO, A_K_CO_PO, A_K_CC) {
  
  # add visual bindings
  id = crop_category = soiltype.n = crop_code = soiltype = NULL
  b = cF = kindex1 = kindex2 = A_PH_KCL = A_K_CO = NULL
  osi_country = osi_indicator = crop_cat1 = osi_soil_cat1 = osi_soil_cat2 = value = NULL
  osi_threshold_cropcat = osi_threshold_soilcat = i_c_k = osi_st_c1 = osi_st_c2 = osi_st_c3 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'NL']
  dt.crops[, crop_code := as.integer(crop_code)]
  
  dt.soils <- as.data.table(euosi::osi_soiltype)
  dt.soils <- dt.soils[osi_country == 'NL']
  
  # Load in the thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'NL' & osi_indicator == 'i_c_k']
  
  # convert B_LU to integer
  B_LU <- as.integer(B_LU)
  
  # Check inputs
  arg.length <- max(length(A_PH_CC), length(A_SOM_LOI), length(A_CEC_CO), length(A_K_CO_PO), 
                    length(A_K_CC), length(A_CLAY_MI), length(B_SOILTYPE_AGR), length(B_LU))
  
  checkmate::assert_numeric(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(dt.soils$osi_soil_cat1), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CC, lower = 0, upper = 800, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CO_PO, lower = 0.1, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 1, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_data_table(dt.thresholds,max.rows = 6, min.rows = 6)
  
  # Collect the data
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   A_CEC_CO = A_CEC_CO,
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_CC = A_K_CC,
                   value = NA_real_
  )
  
  # merge with crop and soil classification tables
  dt <- merge(dt, dt.crops[, list(crop_code, crop_cat1)], 
              by.x = "B_LU", by.y = "crop_code", all.x = TRUE)
  dt <- merge(dt, dt.soils[, list(osi_soil_cat1, osi_soil_cat2)], 
              by.x = "B_SOILTYPE_AGR", by.y = "osi_soil_cat1",all.x = TRUE)
  
  # Calculate the K availability for grassland (CBGV, 2019)
  dt.grass <- dt[crop_cat1 == 'grassland']
  
  # add K-index where CEC is maximized at 400 mmol+ / kg
  dt.grass[A_CEC_CO > 400, A_CEC_CO := 400]
  dt.grass[,value := 4 - exp(-0.08551 * A_K_CC + 0.5264 * log(A_K_CC) - 0.001607 * A_CEC_CO + 
                               0.1275 * log(A_CEC_CO) + 0.010836 * A_K_CC * log(A_CEC_CO))]
  
  # Calculate the K availability for maize (CBGV, 2019)
  dt.maize <- dt[crop_cat1 == 'maize']
  dt.maize[,value := (1 - (120 - A_K_CC) / 120) * 2.5]
  
  # Calculate the K availability for arable crops (Ros & Bussink, 2011)
  dt.arable <- dt[crop_cat1 == 'arable']
  
  # derive b-factor, texture dependent correction
  dt.arable[grepl('duin|rivier|maas|klei',B_SOILTYPE_AGR) & A_SOM_LOI <= 10 & A_CLAY_MI <= 11, b := 1.513]
  dt.arable[grepl('duin|rivier|maas|klei',B_SOILTYPE_AGR) & A_SOM_LOI <= 10 & A_CLAY_MI > 11, b := 0.60226 + 1.27576 /(1 + 0.09891 * A_CLAY_MI)]
  dt.arable[grepl('duin|rivier|maas|klei',B_SOILTYPE_AGR) & A_SOM_LOI > 10 & A_CLAY_MI <= 11, b := 1.513] # SHOULD BE CHANGED; UNKNOWN FROM FACTSHEETS
  dt.arable[grepl('duin|rivier|maas|klei',B_SOILTYPE_AGR) & A_SOM_LOI > 10 & A_CLAY_MI > 11, b := 0.60226 + 1.27576 /(1 + 0.09891 * A_CLAY_MI)] # SHOULD BE CHANGED; UNKNOWN FROM FACTSHEETS
  dt.arable[grepl('zeeklei',B_SOILTYPE_AGR) & A_SOM_LOI > 10 & A_CLAY_MI <= 5, b := 1.513]
  dt.arable[grepl('zeeklei',B_SOILTYPE_AGR) & A_SOM_LOI > 10 & A_CLAY_MI > 5, b := 0.60226 + 1.27576 /(1 + 0.09891 * A_CLAY_MI)]
  dt.arable[grepl('zeeklei',B_SOILTYPE_AGR) & A_SOM_LOI <= 10 & A_CLAY_MI <= 5, b := 1.513]  # SHOULD BE CHANGED; UNKNOWN FROM FACTSHEETS
  dt.arable[grepl('zeeklei',B_SOILTYPE_AGR) & A_SOM_LOI <= 10 & A_CLAY_MI > 5, b := 0.60226 + 1.27576 /(1 + 0.09891 * A_CLAY_MI)]  # SHOULD BE CHANGED; UNKNOWN FROM FACTSHEETS
  dt.arable[grepl('loess',B_SOILTYPE_AGR) & A_CLAY_MI <= 11, b := 1.513]
  dt.arable[grepl('loess',B_SOILTYPE_AGR) & A_CLAY_MI > 11, b := 1.75 - 0.04 * 2 * A_CLAY_MI + 0.00068 * (2 * A_CLAY_MI)^2 - 0.0000041 * (2 * A_CLAY_MI)^3]
  
  # pH-KCl needed (not higher than pH is 7)
  dt.arable[,A_PH_KCL := pmin(7,(A_PH_CC - 0.5262)/0.9288)]
  
  # correction factor for texture and OS (the so called F-factor)
  dt.arable[grepl('zand|dal|veen',B_SOILTYPE_AGR), cF := 20 / (10 + A_SOM_LOI)]
  dt.arable[grepl('duin|rivier|maas|klei|loess',B_SOILTYPE_AGR) & A_SOM_LOI <= 10, cF := b /(0.15 * A_PH_KCL-0.05)]
  dt.arable[grepl('duin|rivier|maas|klei|loess',B_SOILTYPE_AGR) & A_SOM_LOI > 10, cF := b /(0.15 * A_PH_KCL-0.05)] # SHOULD BE CHANGED; UNKNOWN FROM FACTSHEETS
  dt.arable[grepl('zeeklei',B_SOILTYPE_AGR) & A_SOM_LOI > 10, cF := b]
  dt.arable[grepl('zeeklei',B_SOILTYPE_AGR) & A_SOM_LOI <= 10, cF := b] # SHOULD BE CHANGED; UNKNOWN FROM FACTSHEETS
  
  # calculate K-COHEX as mg K per kg soil
  dt.arable[,A_K_CO := A_K_CO_PO * A_CEC_CO * 0.01 * 39.098]
  
  # calculate K-index based on CaCl2-extracble K as wel ass K-CEC
  dt.arable[grepl('zand|dal|veen',B_SOILTYPE_AGR),kindex1 := A_K_CC * cF * 0.12046]
  dt.arable[grepl('zand|dal|veen',B_SOILTYPE_AGR),kindex2 := (A_K_CO - 0.17 * A_CEC_CO) * cF * 0.12046]
  dt.arable[grepl('klei',B_SOILTYPE_AGR),kindex1 := (1.56 * A_K_CC - 17 + 0.29 * A_CEC_CO) * cF * 0.12046]
  dt.arable[grepl('klei',B_SOILTYPE_AGR),kindex2 := A_K_CO * cF * 0.12046]
  
  # calculate K-HCL (mg K2O/ 100 g) for loess, assuming Cohex similar to HCl extraction
  dt.arable[grepl('loess',B_SOILTYPE_AGR), c('kindex1','kindex2') := A_K_CO * 1.2047 * 0.1]
  
  # add check for K-CEC derived Kindex: should not below zero
  dt.arable[kindex2 < 0, kindex2 := kindex1]
  dt.arable[kindex1 < 0, kindex1 := kindex2]
  dt.arable[,value := 0.5 * (kindex1 + kindex2)]
  
  # replace negative values by zero
  dt.arable[value < 0, value := 0]
  
  # Calculate the K availability for nature
  dt.nature <- dt[crop_cat1 == 'nature']
  dt.nature[,value := 0]
  
  # score the K index given threshold for agronomic production / product quality
  
  # subset
  dths <- dt.thresholds[osi_threshold_cropcat == 'grassland']
  
  # evaluate grassland
  dt.grass[, i_c_k := osi_evaluate_logistic(value, b = dths[,osi_st_c1], x0 = dths[,osi_st_c2],v = dths[,osi_st_c3])]
  
  # subset and evaluate for maize
  dths <- dt.thresholds[osi_threshold_cropcat == 'maize']
  dt.grass[, i_c_k := osi_evaluate_logistic(value, b = dths[,osi_st_c1], x0 = dths[,osi_st_c2],v = dths[,osi_st_c3])]
  
  # subset and evaluate for arable sandy soils
  dths <- dt.thresholds[osi_threshold_cropcat == 'arable' & osi_threshold_soilcat == 'sand']
  dt.arable[grepl('zand|dal',B_SOILTYPE_AGR), i_c_k := osi_evaluate_logistic(value, b = dths[,osi_st_c1], x0 = dths[,osi_st_c2],v = dths[,osi_st_c3])]
  
  # subset and evaluate for arable peat soils
  dths <- dt.thresholds[osi_threshold_cropcat == 'arable' & osi_threshold_soilcat == 'peat']
  dt.arable[grepl('peat',B_SOILTYPE_AGR), i_c_k := osi_evaluate_logistic(value, b = dths[,osi_st_c1], x0 = dths[,osi_st_c2],v = dths[,osi_st_c3])]
  
  # subset and evaluate for arable clay soils
  dths <- dt.thresholds[osi_threshold_cropcat == 'arable' & osi_threshold_soilcat == 'clay']
  dt.arable[grepl('klei',B_SOILTYPE_AGR), i_c_k := osi_evaluate_logistic(value, b = dths[,osi_st_c1], x0 = dths[,osi_st_c2],v = dths[,osi_st_c3])]
  
  # subset and evaluate for arable loess soils
  dths <- dt.thresholds[osi_threshold_cropcat == 'arable' & osi_threshold_soilcat == 'loess']
  dt.arable[grepl('loess',B_SOILTYPE_AGR), i_c_k := osi_evaluate_logistic(value, b = dths[,osi_st_c1], x0 = dths[,osi_st_c2],v = dths[,osi_st_c3])]
  
  # evaluate nature soils
  dt.nature[, i_c_k := 1]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.arable,dt.grass, dt.maize,dt.nature), fill = TRUE)
  setorder(dt, id)
  
  # select the output variable
  out <- dt[,i_c_k]
  
  # return the OSI score
  return(out)
}

#' Calculate the potassium availability index in Poland
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_DL (numeric) The exchangeable K-content of the soil measured via ammonium double lactate extracton (mg K/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_pl(A_K_DL = 45,B_TEXTURE_HYPRES='C')
#' 
#' @return 
#' The potassium availability index in Poland estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_c_potassium_pl <- function(A_K_DL,B_TEXTURE_HYPRES,B_LU = NA_character_) {
  
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
                   A_K_DL = A_K_DL,
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
     value := osi_evaluate_logistic(x = A_K_CAL, b= 0.07752183,x0 = 3.04311052,v = 0.09679474)]
  # calculate the OSI score light textured soils
  dt[B_TEXTURE_HYPRES %in% c('MF'),
     value := osi_evaluate_logistic(x = A_K_CAL, b= 0.07134238,x0 = 3.07588403,v = 0.02805476)]
  # calculate the OSI score medium texture soils
  dt[B_TEXTURE_HYPRES %in% c('M'),
     value := osi_evaluate_logistic(x = A_K_CAL, b= 0.070369841,x0 = 4.237044272,v = 0.007390134)]
  # calculate the OSI score for heavy textured soils
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_CAL, b= 0.070110090,x0 = 0.702553221,v = 0.001352746)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium availability index in Sweden
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_K_AL (numeric) The K-content of the soil extracted with ammonium lactate (mg K / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_se(B_LU = 265,A_K_AL = 5)
#' osi_c_potassium_se(B_LU = c(265,1019),A_K_AL = c(3.5,5.5))
#' 
#' @return 
#' The potassium availability index in Sweden derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_c_potassium_se <- function(B_LU, A_K_AL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_K_AL = A_K_AL,
                   value = NA_real_)
  
  # evaluation soil K status
  dt[, value := OBIC::evaluate_logistic(A_K_AL, b = 0.07362818 , x0 = 0.51818429 , v = 0.02380852)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the potassium availability index in  Slovak Republic
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_M3 (numeric) The exchangeable K-content of the soil measured via Mehlich 3 extracton (mg K/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_sk(A_K_M3 = 45,B_TEXTURE_HYPRES='C')
#' 
#' @return 
#' The potassium availability index in Slovak Republic estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_c_potassium_sk <- function(B_TEXTURE_HYPRES,A_K_M3,B_LU = NA_character_) {
  
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
                   A_K_M3 = A_K_M3,
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
     value := osi_evaluate_logistic(x = A_K_M3, b= 0.04569915,x0 = 2.78099169 ,v = 0.00851402 )]
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_K_M3, b= 0.039305100,x0 = 2.045554487 ,v = 0.003009242 )]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_M3, b= 0.03070035,x0 = 65.84134127,v = 0.01856598)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium availability index in Slovenia
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_AL (numeric) The exchangeable K-content of the soil measured via ammoniuml lactate extracton (mg K/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_sl(A_K_AL = 45)
#' 
#' @return 
#' The potassium availability index in Slovenia estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_c_potassium_sl <- function(A_K_AL,B_TEXTURE_HYPRES,B_LU = NA_character_) {
  
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
                   A_K_AL = A_K_AL,
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
     value := osi_evaluate_logistic(x = A_K_AL, b= 0.03689968,x0 = 2.80510928,v = 0.02341685)]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_AL, b= 0.03432105,x0 = 2.90565094,v = 0.01630095)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium availability index in United Kingdom
#' 
#' This function calculates the potassium availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_K_AN (numeric) The K-content of the soil extracted with ammonium nitrate (mg K /kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_potassium_uk(B_LU = 265,A_SOM_LOI=3,A_K_AN = 50)
#' osi_c_potassium_uk(B_LU = c(265,1019),A_SOM_LOI = c(3,5),A_K_AN = c(35,55))
#' 
#' @return 
#' The potassium availability index in United Kingdom derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_c_potassium_uk <- function(B_LU, A_SOM_LOI,A_K_AN) {
  
  # crop properties
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_K_AN = A_K_AN,
                   value = NA_real_)
  
  # merge with crop
  dt <- merge(dt,
              dt.crops[,.(B_LU, crop_name, crop_cat1)],
              by = 'B_LU',
              all.x = TRUE)
  
  # convert from mg / kg to mg / liter sample volume
  dt[, BDS := (1/(0.02525 * A_SOM_LOI + 0.6541))]
  dt[, A_K_AN := A_K_AN * BDS]
  
  # optimum value is index 2 for all land uses
  dt[, value := osi_evaluate_logistic(A_K_AN, b = 0.03399723, x0 = 64.381726, v = 0.65964038)]
  
  # optimum value for vegatables
  dt[grepl('vegetab|cabbag|leek|carrot|sprout|celery|onion|potato|lettu',tolower(crop_name)), 
     value := osi_evaluate_logistic(A_K_AN, b = 0.03023062, x0 = 46.87684513, v = 0.45288470)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}
