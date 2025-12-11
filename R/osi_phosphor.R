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
#' @param A_P_MORGAN (numeric) The P-content of the soil extracted with sodium acetate (mg P / L)
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg P / kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P / kg)
#' @param B_COUNTRY (character) The country code
#' @param pwarning (boolean) Option to print a warning rather than error (stop) message for input checks (TRUE or FALSE)
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
                          A_P_MORGAN = NA_real_,
                          A_P_OL = NA_real_,A_P_WA = NA_real_, 
                          B_COUNTRY, pwarning = FALSE) {
  
  # add visual bindings
  id = B_TEXTURE_USDA = B_TEXTURE_HYPRES = A_SILT_MI = value = NULL
  
  # note that qualitative checks on the inputs are done by the country specific functions
  
  # Check length of desired input
  arg.length <- max(length(B_LU),
                    length(B_SOILTYPE_AGR),length(A_CLAY_MI), length(A_SAND_MI),
                    length(A_SOM_LOI),length(A_C_OF),
                    length(A_P_AAA),length(A_P_AL),length(A_P_CAL),
                    length(A_P_CC),length(A_P_DL),length(A_P_M3),length(A_P_MORGAN),
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
                   A_P_MORGAN = A_P_MORGAN,
                   A_P_OL = A_P_OL,
                   A_P_WA = A_P_WA,
                   B_COUNTRY = B_COUNTRY,
                   value = NA_real_
                   )
  
  # check required inputs
  osi_checkvar(parm = list(B_COUNTRY = dt$B_COUNTRY, B_LU = dt$B_LU,
                           B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
                           A_CLAY_MI = dt$A_CLAY_MI,
                           A_SAND_MI = dt$A_SAND_MI,
                           A_SILT_MI = dt$A_SILT_MI,
                           B_COUNTRY = dt$B_COUNTRY,
                           A_SOM_LOI = dt$A_SOM_LOI,
                           A_C_OF = dt$A_C_OF,
                           A_PH_CC = dt$A_PH_CC,
                           A_C_OF = dt$A_C_OF,
                           A_P_OL = dt$A_P_OL,
                           A_P_CC = dt$A_P_CC),
               fname='oci_c_phosphor',
               na_allowed = TRUE,
               unitcheck = TRUE,
               pwarning = pwarning)
  
  # estimate texture information
  dt[,B_TEXTURE_USDA := osi_get_TEXTURE_USDA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_HYPRES := osi_get_TEXTURE_HYPRES(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[is.na(B_SOILTYPE_AGR), B_SOILTYPE_AGR := osi_get_SOILTYPE_AGR(A_CLAY_MI, A_SAND_MI, A_SOM_LOI, A_PH_CC)]
  
  # estimate missing soil properties from P-Olsen (default in LUCAS)
  dt[is.na(A_PH_WA) & !is.na(A_PH_CC), A_PH_WA := osi_conv_ph(element='A_PH_WA',A_PH_CC = A_PH_CC)]
  dt[!is.na(A_PH_WA) & !is.na(A_PH_CC), A_PH_CC := osi_conv_ph(element='A_PH_CC',A_PH_WA = A_PH_WA)]
  dt[is.na(A_P_AL) & !is.na(A_P_OL), A_P_AL := osi_conv_phosphor(element='A_P_AL',A_P_OL = A_P_OL,A_PH_CC = A_PH_CC)]
  dt[is.na(A_P_CAL) & !is.na(A_P_OL), A_P_CAL := osi_conv_phosphor(element='A_P_CAL',A_P_OL = A_P_OL)]
  dt[is.na(A_P_DL) & !is.na(A_P_OL), A_P_DL := osi_conv_phosphor(element='A_P_DL',A_P_OL = A_P_OL)]
  dt[is.na(A_P_AAA) & !is.na(A_P_OL), A_P_AAA := osi_conv_phosphor(element='A_P_AAA',A_P_OL = A_P_OL)]
  dt[is.na(A_P_M3) & !is.na(A_P_OL), A_P_M3 := osi_conv_phosphor(element='A_P_M3',A_P_OL = A_P_OL)]
  dt[is.na(A_P_WA) & !is.na(A_P_OL), A_P_WA := osi_conv_phosphor(element='A_P_WA',A_P_OL = A_P_OL,B_SOILTYPE_AGR = B_SOILTYPE_AGR)]
  dt[is.na(A_P_CC) & !is.na(A_P_OL), A_P_CC := osi_conv_phosphor(element='A_P_CC',A_P_OL = A_P_OL)]
  dt[is.na(A_P_MORGAN) & !is.na(A_P_OL), A_P_MORGAN := osi_conv_phosphor(element='A_P_MORGAN',A_P_OL = A_P_OL)]
  
  #check required calculated inputs
  osi_checkvar(parm = list(B_TEXTURE_USDA = dt$B_TEXTURE_USDA, 
                           B_TEXTURE_HYPRES = dt$B_TEXTURE_HYPRES,
                           B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
                           A_PH_WA = dt$A_PH_WA,
                           A_P_AL = dt$A_P_AL,
                           A_P_CAL = dt$A_P_CAL,
                           A_P_DL = dt$A_P_DL,
                           A_P_AAA = dt$A_P_AAA,
                           A_P_M3 = dt$A_P_M3,
                           A_P_WA = dt$A_P_WA,
                           A_P_CC = dt$A_P_CC),
               fname='oci_c_phosphor',
               unitcheck = TRUE,
               pwarning = pwarning)
  
  # calculate the OSI score per country
  
  # Austria (AT), Belgium (BE), Switzerland (CH), Czech Republic (CZ), Germany (DE)
  dt[B_COUNTRY == 'AT', value := osi_c_phosphor_at(B_LU = B_LU, A_P_CAL = A_P_CAL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'BE', value := osi_c_phosphor_be(B_LU = B_LU, A_P_AL = A_P_AL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'CH', value := osi_c_phosphor_ch(B_LU = B_LU, A_P_AAA = A_P_AAA, unitcheck = FALSE)]
  dt[B_COUNTRY == 'CZ', value := osi_c_phosphor_cz(B_LU = B_LU, A_P_M3 = A_P_M3, unitcheck = FALSE)]
  dt[B_COUNTRY == 'DE', value := osi_c_phosphor_de(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_P_CAL = A_P_CAL, A_P_DL = A_P_DL, unitcheck = FALSE)]
  
  # Denmark (DK), Estonia (EE), Greece (EL), Spain (ES),France (FR), Finland (FI) 
  dt[B_COUNTRY == 'DK', value := osi_c_phosphor_dk(B_LU = B_LU, A_P_OL = A_P_OL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'EE', value := osi_c_phosphor_ee(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_P_M3 = A_P_M3, unitcheck = FALSE)]
  dt[B_COUNTRY == 'EL', value := osi_c_phosphor_el(B_LU = B_LU, A_P_OL = A_P_OL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'ES', value := osi_c_phosphor_es(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_P_OL = A_P_OL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'FR', value := osi_c_phosphor_fr(B_LU = B_LU, A_P_OL= A_P_OL,A_PH_WA=A_PH_WA, unitcheck = FALSE)]
  dt[B_COUNTRY == 'FI', value := osi_c_phosphor_fi(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_P_AAA = A_P_AAA, A_C_OF = A_C_OF, unitcheck = FALSE)]
  
  # Hungary (HU), Ireland (IE), Italy (IT), Latvia (LV), Lithuania (LT)
  dt[B_COUNTRY == 'HU', value := osi_c_phosphor_hu(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_CACO3_IF = A_CACO3_IF,A_P_AL = A_P_AL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'IE', value := osi_c_phosphor_ie(B_LU = B_LU, A_P_OL = A_P_OL, A_P_MORGAN = A_P_MORGAN, unitcheck = FALSE)]
  dt[B_COUNTRY == 'IT', value := osi_c_phosphor_it(B_LU = B_LU, A_P_OL = A_P_OL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'LV', value := osi_c_phosphor_lv(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_P_DL = A_P_DL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'LT', value := osi_c_phosphor_lt(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_P_AL = A_P_AL, unitcheck = FALSE)]
  
  # the Netherlands (NL), Norway (NO),  Sweden (SE), Slovak Republic (SK), Slovenia (SL)
  dt[B_COUNTRY == 'NL', value := osi_c_phosphor_nl(B_LU = B_LU, A_P_AL = A_P_AL, A_P_CC = A_P_CC, A_P_WA = A_P_WA, B_SOILTYPE_AGR = B_SOILTYPE_AGR, unitcheck = FALSE)]
  dt[B_COUNTRY == 'NO', value := osi_c_phosphor_no(B_LU = B_LU, A_P_AL = A_P_AL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'SE', value := osi_c_phosphor_se(B_LU = B_LU, A_P_AL = A_P_AL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'SK', value := osi_c_phosphor_sk(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_P_M3 = A_P_M3, unitcheck = FALSE)]
  dt[B_COUNTRY == 'SL', value := osi_c_phosphor_sl(B_LU = B_LU, A_P_AL = A_P_AL, unitcheck = FALSE)]
  
  # Poland (PL), Portugal (PT), and United Kingdom (UK)
  dt[B_COUNTRY == 'PL', value := osi_c_phosphor_pl(B_LU = B_LU, A_P_DL = A_P_DL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'PT', value := osi_c_phosphor_pt(B_LU = B_LU, A_P_OL = A_P_OL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'RO', value := osi_c_phosphor_ro(B_LU = B_LU, A_P_AL = A_P_AL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'UK', value := osi_c_phosphor_uk(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_P_OL = A_P_OL, unitcheck = FALSE)]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_at(B_LU = '3301000000', A_P_CAL = 47)
#' 
#' @return 
#' The phosphorus availability index in Austria estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_at <- function(B_LU, A_P_CAL,unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='AT']
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_P_CAL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('AT',arg.length),
                           B_LU = B_LU,
                           A_P_CAL = A_P_CAL),
               fname = 'osi_c_phosphor_at',
               unitcheck = unitcheck)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_P_CAL = A_P_CAL,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # convert to the OSI score
  dt[crop_cat1 %in% c('arable','maize','permanent'),value := osi_evaluate_logistic(x = A_P_CAL, b= 0.11491039,x0 = -6.53091980,v =  0.01059674)]
  dt[crop_cat1 =='grassland',value := osi_evaluate_logistic(x = A_P_CAL, b=  0.110539504,x0 = -11.014830447,v = 0.007619118)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_be(B_LU = '8410', A_P_AL = 45)
#' 
#' @return 
#' The phosphorus availability index in Belgium estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_be <- function(B_LU, A_P_AL,unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = crop_k = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='BE']
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'BE' & osi_indicator =='i_c_p']
  
  # get length of input arguments
  arg.length <- max(length(B_LU),length(A_P_AL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('BE',arg.length),
                           B_LU = B_LU,
                           A_P_AL = A_P_AL),
               fname = 'osi_c_phoshor_be',
               unitcheck = unitcheck)
  
  # Collect the data into a table, units mg P/kg
  dt <- data.table(id = 1:arg.length,
                   B_LU = as.character(B_LU),
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
  dt[grepl('grass',crop_cat1),value := osi_evaluate_logistic(x = A_P_AL, b= 0.03519128,x0 = 3.36161250,v = 0.01311450)]
  dt[grepl('arable|maize|permanent',crop_cat1),value := osi_evaluate_logistic(x = A_P_AL, b= 0.077613131,x0 = 0.632608091,v = 0.003002582)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_ch(B_LU = 'testcrop1',A_P_AAA = 50)
#' 
#' @return 
#' The phosphorus availability index in Switzerland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_ch <- function(A_P_AAA,B_LU = NA_character_,unitcheck = TRUE) {
  
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
  
  # check inputs
  osi_checkvar(parm = list(A_P_AAA = A_P_AAA),
               fname = 'osi_c_phoshor_ch',
               unitcheck = unitcheck)
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_cz(B_LU = '3303030100', A_P_M3 = 81)
#' 
#' @return 
#' The phosphorus availability index in Czech Republic estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_cz <- function(B_LU, A_P_M3,unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='CZ']
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_P_M3))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('CZ',arg.length),
                           B_LU = B_LU,
                           A_P_M3 = A_P_M3),
               fname = 'osi_c_phoshor_cz',
               unitcheck = unitcheck)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_P_M3 = A_P_M3,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = A_P_M3, b= 0.07193411,x0 = 2.46027902 ,v = 0.01509571 )]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_de(B_LU = '3301000000', A_SOM_LOI = 4.5, A_CLAY_MI = 5,A_P_CAL = 45,A_P_DL = 5)
#' osi_c_phosphor_de(B_LU = c('3301000000','3301061299'),A_SOM_LOI = c(3,3),A_CLAY_MI = c(3,15),
#' A_P_CAL = c(35,54),A_P_DL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Germany stimated from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_de <- function(B_LU, A_SOM_LOI,A_CLAY_MI,A_P_CAL, A_P_DL = NA_real_,unitcheck = TRUE) {
  
  # add visual bindings
  value1 = value2 = A_P_CAL2 = NULL
  osi_country = . = crop_code = crop_cat1 = id = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='DE']
  
  # argument length
  arg.length <- max(length(B_LU),length(A_SOM_LOI),length(A_CLAY_MI),
                    length(A_P_CAL),length(A_P_DL))
  
  # repeat A_P_DL if only one default is given
  if(length(A_P_DL)==1 & arg.length > 1){A_P_DL <- rep(A_P_DL,arg.length)}
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('DE',arg.length),
                           B_LU = B_LU,
                           A_P_CAL = A_P_CAL),
               fname = 'osi_c_phoshor_de',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_SOM_LOI= A_SOM_LOI,
                   A_P_CAL = A_P_CAL, # update Elise in mg P/kg; 0.1, # in mg P/100g
                   A_P_CAL2 = A_P_CAL * ((1 / (0.02525 * A_SOM_LOI + 0.6541))/1), #update Elise in mg/L, # in mg P/100ml
                   A_P_DL = A_P_DL,
                   value1 = NA_real_,
                   value2 = NA_real_,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # check calculated inputs
  osi_checkvar(parm = list(A_P_CAL = A_P_CAL,
                           A_P_DL = A_P_DL),
               fname = 'osi_c_phoshor_de',
               unitcheck = TRUE)
  
  # evaluation conform VDLUFA for cropland and soil types
  # with updated VDLUFA thresholds in 2020
  dt[!is.na(A_P_CAL) & A_CLAY_MI <= 5 & A_SOM_LOI <= 8, value1 := osi_evaluate_logistic(A_P_CAL, b = 0.05394736, x0 =-30.21631941, v = 0.03082604)]
  dt[!is.na(A_P_CAL) & A_CLAY_MI > 5 & A_SOM_LOI <= 8, value1 := osi_evaluate_logistic(A_P_CAL, b = 0.06795038, x0 =-24.42219356, v = 0.02356369)]
  dt[!is.na(A_P_CAL) & A_SOM_LOI > 8 & A_SOM_LOI <=15, value1 := osi_evaluate_logistic(A_P_CAL, b = 0.04467730, x0 =-43.25011500, v = 0.02681524)]
  
  # adjust for peat soils (mg P/ 100 ml soil)
  dt[!is.na(A_P_CAL) & A_SOM_LOI > 15, value1 := osi_evaluate_logistic(A_P_CAL2, b = 0.1436028, x0 = 2.8983285, v = 0.1185802)]
  
  # evaluation conform VDLUFA for cropland and soil types
  dt[!is.na(A_P_DL), value2 := osi_evaluate_logistic(A_P_DL, b = 0.5357, x0 = -4.03796, v = 0.01856)]
  
  # set value
  dt[,value := fifelse(!is.na(A_P_CAL),value1,value2)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_dk(B_LU = '3301010399',A_P_OL = 5)
#' osi_c_phosphor_dk(B_LU = c('3301010399','3301029800'),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Denmark derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_dk <- function(B_LU, A_P_OL,unitcheck = TRUE) {
  
  # add visual bindings
  id = NULL
  
  # get argument length
  arg.length <- max(length(B_LU),length(A_P_OL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('DK',arg.length),
                           B_LU = B_LU,
                           A_P_OL = A_P_OL),
               fname = 'osi_c_phoshor_dk',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # evaluation P-Olsen for cropland and soil types
  dt[, value := osi_evaluate_logistic(A_P_OL, b = 0.309110073, x0 = 3.733401272,v = 0.002834927)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphate availability index in Greece
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg P/kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_el(B_LU = '3301061299',A_P_OL = 5)
#' osi_c_phosphor_el(B_LU = c('3301061299','3301000000'),A_P_OL = c(5,15))
#' 
#' @return 
#' The phosphate availability index in Greece derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_el <- function(B_LU, A_P_OL,unitcheck = TRUE) {
  
  # add visual bindings
  osi_country = crop_code = crop_cat1 = . = id = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='EL']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_OL))
  
  # check inputs (crop code is not yet in osi_crops, so no check)
  osi_checkvar(parm = list(A_P_OL = A_P_OL),
               fname = 'osi_c_phosphor_el',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU',
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # P index derived following P-Olsen.
  
  # evaluation soil P status for grasslands and croplands
  # source ChatGPT, reference to https://ir.lib.uth.gr/xmlui/bitstream/handle/11615/1729/P0001729.pdf
  # updated by Elise:
  # Stampoulos, G, 2000. Availability of soil phosphorus and determination of phosphorus in tobacco in characteristic Territorial Units of Thessaly. (Table Page 40)
  
  # temporary fix since crop codes are unknown
  dt[grepl('grass',B_LU),cropcat1 := 'grassland']
  dt[!grepl('grass',B_LU),cropcat1 := 'arable']
  
  # evaluate P olsen for grassland
  dt[cropcat1 =='grassland', value := OBIC::evaluate_logistic(A_P_OL, b = 0.12764972 , x0 = -12.12405646, v = 0.03103546)]
  
  # evaluate for cropland
  dt[cropcat1 == 'arable', value := OBIC::evaluate_logistic(A_P_OL, b = 0.30203251 , x0 = -3.27824283, v = 0.01150028 )]
  
  # set the order to the original inputs
  setorder(dt, id)
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_ee(A_P_M3 = 45,A_SOM_LOI = 3, B_LU='testcrop1')
#' 
#' @return 
#' The phosphorus availability index in Estonia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_ee <- function(A_P_M3,A_SOM_LOI,B_LU = NA_character_,unitcheck = TRUE) {
  
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
  
  # check inputs (no check on crop type due to missing crop codes)
  osi_checkvar(parm = list(A_P_M3 = A_P_M3,
                           A_SOM_LOI = A_SOM_LOI),
               fname = 'osi_c_phoshor_ee',
               unitcheck = unitcheck)
  
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
  dt[A_SOM_LOI <= 2,value := osi_evaluate_logistic(x = A_P_M3, b= 0.07933674 ,x0 = -27.75956874,v = 0.01682012 )]
  dt[A_SOM_LOI > 2,value := osi_evaluate_logistic(x = A_P_M3, b= 0.11798156 ,x0 = -19.96608769,v = 0.01592764 )]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_es(B_LU = '3301010301',A_CLAY_MI = 5,A_SAND_MI = 25,A_P_OL = 5)
#' osi_c_phosphor_es(B_LU = c('3301010901','3301010500'),A_CLAY_MI = c(5,10),
#' A_SAND_MI = c(50,50),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Spain derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_es <- function(B_LU, A_CLAY_MI,A_SAND_MI, A_P_OL,unitcheck = TRUE) {
  
  # add visual bindings
  osi_country = . = crop_code = crop_cat1 = id = NULL
  
  # get max length of input
  arg.length <- max(length(B_LU),length(A_CLAY_MI),length(A_SAND_MI),length(A_P_OL))
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='ES']
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('ES',arg.length),
                           B_LU = B_LU,
                           A_CLAY_MI = A_CLAY_MI,
                           A_SAND_MI = A_SAND_MI,
                           A_P_OL = A_P_OL),
               fname = 'osi_c_phoshor_es',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  #Jimenez et al., 2010. GUÍA PRÁCTICA DE LA FERTILIZACIÓN RACIONAL DE LOS CULTIVOS EN ESPAÑA (Table 10.1)
  
  # assess P availability for sandy soils (Arenoso)
  dt[A_CLAY_MI <= 15 & A_SAND_MI > 50, value := osi_evaluate_logistic(A_P_OL, b = 0.5180530, x0 = 2.6601790 , v = 0.1816486 )]
  
  # assess P availability for loamy? soils (Franco)
  dt[A_CLAY_MI <= 15 & A_SAND_MI <=50 , value := osi_evaluate_logistic(A_P_OL, b = 0.34792372, x0 = -0.78642558, v = 0.03936397 )]
  
  # assess P availability for clayey soils (Arcilloso)
  dt[A_CLAY_MI > 15, value := osi_evaluate_logistic(A_P_OL, b = 0.2742725 , x0 = 3.0649379 , v = 0.1024065 )]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
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
#' @param A_P_AAA (numeric) The exchangeable P-content of the soil measured via acid ammonium acetate extraction (mg P/kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_fi(B_LU = '4010', B_TEXTURE_USDA = 'Si',
#' A_P_AAA = 45,A_C_OF=1.5)
#' 
#' @return 
#' The phosphorus availability index in Finland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_fi <- function(B_LU, B_TEXTURE_USDA, A_P_AAA,A_C_OF = 0.5,unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  B_SOILTYPE_AGR = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FI']
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # get length of input arguments
  arg.length <- max(length(B_LU),length(B_TEXTURE_USDA),length(A_P_AAA),length(A_P_AAA))
  
  # repeat A_C_OF if only one default is given
  if(length(A_C_OF)==1 & arg.length > 1){A_C_OF <- rep(A_C_OF,arg.length)}
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('FI',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_USDA = B_TEXTURE_USDA,
                           A_C_OF = A_C_OF,
                           A_P_AAA = A_P_AAA),
               fname = 'osi_c_phoshor_fi',
               unitcheck = unitcheck)
  
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
  
  # convert from mg / kg to mg / liter soil sample volume
  dt[, BD := (1/(0.02525 * (A_C_OF * 0.1 * 2) + 0.6541))]
  dt[, A_P_AAA := A_P_AAA * BD]
  
  # evaluation from Eurofins report (from Elise) in units mg P/L
  # since method A_P_AAA is in mg/kg, and SSR is 1:10, adjust
  dt[B_SOILTYPE_AGR =='clay',value := osi_evaluate_logistic(x = A_P_AAA, b= 0.442872 ,x0 = 7.948519 ,v = 1.160189 )]
  dt[B_SOILTYPE_AGR =='loam',value := osi_evaluate_logistic(x = A_P_AAA, b= 0.14013150  ,x0 = -15.76962688 ,v = 0.03193195)]
  dt[B_SOILTYPE_AGR =='sand',value := osi_evaluate_logistic(x = A_P_AAA, b= 0.162814448   ,x0 = -25.836568167 ,v = 0.004510061 )]
  dt[B_SOILTYPE_AGR =='organic',value := osi_evaluate_logistic(x = A_P_AAA, b= 0.172803388   ,x0 = -45.940731793 ,v = 0.000134853 )]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg P/ kg)
#' @param A_PH_WA (numeric) The pH measured in water.
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_fr(B_LU = 'SOJ', A_P_OL = 45)
#' 
#' @details
#' 
#' @return 
#' The phosphate availability index in France estimated from extractable soil P Olsen (a numeric value). 
#' 
#' @export
osi_c_phosphor_fr <- function(B_LU, A_P_OL,A_PH_WA = NA_real_,unitcheck = TRUE) {
  
  # set visual bindings
  value = osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = crop_p = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL

  # Load in the interal datasets
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FR']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_OL),length(A_PH_WA))
  
  # repeat A_PH_WA if only one default is given
  if(length(A_PH_WA)==1 & arg.length > 1){A_PH_WA <- rep(A_PH_WA,arg.length)}
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('FR',arg.length),
                           B_LU = B_LU,
                           A_PH_WA = A_PH_WA,
                           A_P_OL = A_P_OL),
               fname = 'osi_c_phoshor_fr',
               unitcheck = unitcheck)
  
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
  
  # update with more generic assessment all soils (input Elise)
  dt[(A_PH_WA <= 8 | is.na(A_PH_WA)) & crop_p =='high',
     value := osi_evaluate_logistic(x = A_P_OL, b= 0.1315210,x0 = 12.4776644,v = 0.2292241)]
  dt[(A_PH_WA <= 8 | is.na(A_PH_WA)) & crop_p =='moderate',
     value := osi_evaluate_logistic(x = A_P_OL, b= 0.16655505,x0 = 1.00887810,v = 0.04520388)]
  dt[(A_PH_WA <= 8 | is.na(A_PH_WA)) & crop_p =='low',
     value := osi_evaluate_logistic(x = A_P_OL, b= 0.63653193,x0 = 0.92884029,v = 0.01649963)]
  
  # update with more generic assessment calcareous soils (input Elise)
  dt[A_PH_WA > 8 & crop_p =='high',value := osi_evaluate_logistic(x = A_P_OL, b= 0.05164429 ,x0 = -15.84740833,v = 0.02403234 )]
  dt[A_PH_WA > 8 & crop_p =='moderate',value := osi_evaluate_logistic(x = A_P_OL, b= 0.1859116 ,x0 = 44.8868208,v = 1.1969239 )]
  dt[A_PH_WA > 8 & crop_p =='low',value := osi_evaluate_logistic(x = A_P_OL, b= 0.26836 ,x0 = -15.77183,v = 1.265049e-04)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_hu(B_LU = 'testcrop1',A_P_AL = 45,A_CACO3_IF = 5,A_CLAY_MI = 5,A_SOM_LOI = 5)
#' 
#' @return 
#' The phosphorus availability index in Hungary estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_hu <- function(A_SOM_LOI,A_CLAY_MI,A_CACO3_IF,A_P_AL,
                              B_LU = NA_character_,unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # get max length of inputs
  arg.length <- max(length(A_SOM_LOI),length(A_CLAY_MI),length(A_CACO3_IF),length(A_P_AL),length(B_LU))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,
                           A_CLAY_MI = A_CLAY_MI,
                           A_CACO3_IF = A_CACO3_IF,
                           A_P_AL = A_P_AL),
               fname = 'osi_c_phoshor_hu',
               unitcheck = unitcheck)
  
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
  dt[A_SOM_LOI > 4 & A_CACO3_IF <= 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.053518656 ,x0 = -32.992805792,v = 0.009209084 )]
  dt[A_SOM_LOI > 4 & A_CACO3_IF > 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.0534394 ,x0 = 0.7360886 ,v = 0.0329833 )]
  
  # derive the OSI score for brown forest soil (proxied by clay)
  dt[A_CLAY_MI > 20 & A_SOM_LOI <= 4 & A_CACO3_IF <= 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.07197854 ,x0 = 3.03893977 ,v = 0.06281465 )]
  dt[A_CLAY_MI > 20 & A_SOM_LOI <= 4 & A_CACO3_IF > 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.06878580 ,x0 = 3.02032249 ,v = 0.03604915)]
  
  # derive the OSI score for sandy soil (proxied by clay)
  dt[A_CLAY_MI <= 20 & A_SOM_LOI <= 4 & A_CACO3_IF <= 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.07197854 ,x0 = 3.03893977 ,v = 0.06281465 )]
  dt[A_CLAY_MI <= 20 & A_SOM_LOI <= 4 & A_CACO3_IF > 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.06795617 ,x0 = 2.22891756 ,v = 0.01820161 )]
  
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
#' @param A_P_MORGAN (numeric) The P-content of the soil extracted with sodium acetate (mg P / L)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_ie(B_LU = 'testcrop1',A_P_OL = 5)
#' osi_c_phosphor_ie(B_LU = c('testcrop1','testcrop2'),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Ireland derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_ie <- function(B_LU, A_P_OL = NA_real_,A_P_MORGAN = NA_real_, unitcheck = TRUE) {
  
  # add visual binding
  cropcat1 = id = NULL
  
  # length of inputs
  arg.length <- max(length(B_LU),length(A_P_OL))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_P_OL = A_P_OL),
               fname = 'osi_c_phoshor_ie',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   A_P_MORGAN = A_P_MORGAN,
                   value1 = NA_real_,
                   value2 = NA_real_,
                   value = NA_real_)
  
  # estimate P-Morgan (mg P/L) when only P-Olsen is given
  dt[is.na(A_P_MORGAN) & !is.na(A_P_OL), A_P_MORGAN := osi_conv_phosphor(element='A_P_MORGAN',A_P_OL = A_P_OL)]
  
  # temporary fix since crop codes are unknown
  dt[grepl('grass',B_LU),cropcat1 := 'grassland']
  dt[!grepl('grass',B_LU),cropcat1 := 'arable']
  
  # Northern Ireland Index System – Olsen’s Reagent
  # AHDB (2019). Nutrient management guide (RB209). Agriculture & Horticulture Development Board
  
    # evaluation soil P status for grasslands
    dt[cropcat1 =='grassland', value1 := OBIC::evaluate_logistic(A_P_OL, b = 0.6560111, x0 = 3.44709, v = 0.588379)]
    
    # evaluation soil P status for other crops
    dt[cropcat1 == 'arable', value1 := OBIC::evaluate_logistic(A_P_OL, b = 0.50194, x0 = 3.91821, v = 0.5799892)]
  
  # Republic of Ireland Index System – Morgan’s Reagent
  # https://teagasc.ie/crops/soil--soil-fertility/soil-analysis/soil-index-system/
  
    # evaluation soil P status for grasslands
    dt[cropcat1 =='grassland', value2 := OBIC::evaluate_logistic(A_P_MORGAN, b = 1.008795924 , x0 = -1.203255117, v = 0.006754552 )]
    
    # evaluation soil P status for other crops
    dt[cropcat1 == 'arable', value2 := OBIC::evaluate_logistic(A_P_MORGAN, b = 0.69367145 , x0 = -1.60506857, v = 0.01890689)]
    
  # P index is default the one for Republic of Ireland
  dt[,value := value2]

  # set the order to the original inputs
  setorder(dt, id)
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_it(B_LU = '3301061299',A_P_OL = 5)
#' osi_c_phosphor_it(B_LU = c('3301061299','3301000000'),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Italy derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_it <- function(B_LU, A_P_OL,unitcheck = TRUE) {
  
  # add visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='IT']
  
  # length of inputs
  arg.length <- max(length(B_LU),length(A_P_OL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('IT',arg.length),
                           B_LU = B_LU,
                           A_P_OL = A_P_OL),
               fname = 'osi_c_phoshor_it',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # evaluation P-Olsen for cropland and soil types
  dt[, value := osi_evaluate_logistic(A_P_OL, b = 0.43987, x0 = -5.7314, v = 0.011909)]
  
  # add OSI score for "other" crops: nature, forest, other
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_lv(B_LU ='testcrop1',A_P_DL = 45,B_TEXTURE_USDA = 'Sa')
#' 
#' @return 
#' The phosphorus availability index in Latvia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_lv <- function(A_P_DL,B_TEXTURE_USDA, B_LU = NA_character_,unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']

  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']

  # get max length of inputs
  arg.length <- max(length(B_LU),length(B_TEXTURE_USDA),length(A_P_DL))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(B_TEXTURE_USDA = B_TEXTURE_USDA,
                           A_P_DL = A_P_DL),
               fname = 'osi_c_phoshor_lv',
               unitcheck = unitcheck)
  
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
  dt[B_TEXTURE_USDA == 'Sa',value := osi_evaluate_logistic(x = A_P_DL, b= 0.17368273 ,x0 = -9.80600716  ,v = 0.01511606 )]
  dt[B_TEXTURE_USDA %in% c('LoSa','SaLo'),
     value := osi_evaluate_logistic(x = A_P_DL, b= 0.148258625 ,x0 = -13.419516902   ,v = 0.009429051 )]
  dt[B_TEXTURE_USDA %in% c('Lo','SaCl','SaCL'),
     value := osi_evaluate_logistic(x = A_P_DL, b= 0.13008200 ,x0 = -12.96792439   ,v = 0.01244835 )]
  dt[B_TEXTURE_USDA %in% c('Cl','ClLo','SiCl','SiCL', 'SiLo','Si'),
     value := osi_evaluate_logistic(x = A_P_DL, b= 0.109229932 ,x0 = -19.520013341   ,v = 0.008779705 )]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_lt(B_LU = 'testcrop1', A_P_AL = 45,A_SOM_LOI = 4.5)
#' 
#' @return 
#' The phosphorus availability index in Lithuania estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_lt <- function(A_P_AL,A_SOM_LOI,B_LU = NA_character_,unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']

  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_SOM_LOI),length(A_P_AL))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,
                           A_P_AL = A_P_AL),
               fname = 'osi_c_phoshor_lt',
               unitcheck = unitcheck)
  
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
  dt[A_SOM_LOI <= 20,value := osi_evaluate_logistic(x = A_P_AL, b= 0.10689135 ,x0 = -14.66844427   ,v = 0.01026358 )]
  dt[A_SOM_LOI > 20,value := osi_evaluate_logistic(x = A_P_AL, b= 0.08430721 ,x0 = 2.91042776 ,v = 0.04099394 )]
  
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
#' @param B_SOILTYPE_AGR (character) The soil type in a particular region
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate (mg P/ kg)
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2 (mg P/ kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P/ kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_nl(B_LU = '265', A_P_AL = 45, A_P_CC = 2.5, B_SOILTYPE_AGR = 'dekzand')
#' osi_c_phosphor_nl(B_LU = c('265','1019'),A_P_AL = c(35,54),
#' A_P_CC = c(2.5,4.5), A_P_WA = c(35,65), B_SOILTYPE_AGR = rep('dekzand',2))
#' 
#' @return 
#' The phosphate availability index in the Netherlands estimated from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_nl <- function(B_LU, A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_,
                              B_SOILTYPE_AGR = NA_real_,unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = NULL
  
  # convert B_LU to integer
  B_LU <- as.character(B_LU)
  
  # Load in the crops data set and the parms dataset
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country =='NL']
  dt.crops[, crop_code := as.character(crop_code)]
  
  # subset thresholds to Dutch situation for phosphorus
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='NL' & osi_indicator=='i_c_p']
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_AL),length(A_P_CC),length(A_P_WA),length(B_SOILTYPE_AGR))
  
  # repeat A_P_WA if only one default is given
  if(length(A_P_WA)==1 & arg.length > 1){
    
    # estimate A_P_WA from A_P_CC (both in mg P/kg)
    A_P_WA <- osi_conv_phosphor(element='A_P_WA',B_SOILTYPE_AGR = B_SOILTYPE_AGR, A_P_CC = A_P_CC)
    }
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('NL',arg.length),
                           B_LU = B_LU,
                           A_P_AL = A_P_AL,
                           A_P_CC = A_P_CC,
                           A_P_WA = A_P_WA),
               fname = 'osi_c_phoshor_nl',
               na_allowed = TRUE,
               unitcheck = unitcheck)
  
  # Collect the data into a table, convert to Dutch units
  # assume 3.5% organic matter for the calculation of bulk density
  dt <- data.table(id = 1:arg.length,
                   B_LU = as.character(B_LU),
                   A_P_AL = A_P_AL * 2.29 * 0.1,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA *(1 / (0.02525 * 3.5 + 0.6541))/0.43646,
                   value = NA_real_
                  )
  
  # merge with crop properties
  dt <- merge(dt,
              dt.crops,
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # Calculate the phosphate availability for grass (PBI)
  dt[grepl("gras",crop_cat1), value := pmax(0,log(A_P_CC) * (-0.0114 * A_P_AL + 2.5) + 0.0251 * A_P_CC + 2)]
  
  # Calculate the phosphate availability for maize (PBI)
  dt[grepl("maize",crop_cat1), value := A_P_CC + 0.05 * pmin(37,A_P_AL / A_P_CC)]
  
  # calculate the P-availability for arable systems, normalized to a scale with maximum around 6
  dt[grepl("arable|cropland|perman",crop_cat1), value := A_P_WA * 0.1]
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = value, b= 1.3,x0 = 1.3,v = 0.35)]
  
  # add OSI score for "other" crops: nature, forest, other
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_no(B_LU = 'testcrop1', A_P_AL = 50)
#' 
#' @return 
#' The phosphorus availability index in Norway estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_no <- function(A_P_AL,B_LU = NA_character_,unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_P_AL))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_P_AL = A_P_AL),
               fname = 'osi_c_phoshor_no',
               unitcheck = unitcheck)
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_pl(B_LU = 'testcrop1',A_P_DL = 45)
#' 
#' @return 
#' The phosphorus availability index in Poland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_pl <- function(A_P_DL,B_LU = NA_character_,unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_P_DL))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_P_DL = A_P_DL),
               fname = 'osi_c_phoshor_pl',
               unitcheck = unitcheck)
  
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
  dt[,value := osi_evaluate_logistic(x = A_P_DL, b= 0.10171067 ,x0 = -12.72910795   ,v = 0.01278984 )]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphate availability index in Portugal
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg P/kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_pt(B_LU = '3301061299',A_P_OL = 5)
#' osi_c_phosphor_pt(B_LU = c('3301061299','3301000000'),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Portugal derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_pt <- function(B_LU, A_P_OL,unitcheck = TRUE) {
  
  # add visual bindings
  osi_country = crop_code = crop_cat1 = . = id = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='PT']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_OL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('PT',arg.length),
                           B_LU = B_LU,
                           A_P_OL = A_P_OL),
               fname = 'osi_c_phoshor_pt',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_P_OL = A_P_OL, # updated Elise in unit mg P /kg, # * 2.29,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # P index derived following P-Olsen.
  
  # evaluation soil P status for grasslands and croplands
  dt[, value := OBIC::evaluate_logistic(A_P_OL, b = 0.30484573  , x0 = -4.67456595   , v = 0.01024068  )]
  
  # add OSI score for "other" crops: nature, forest, other
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphate availability index in Romenia
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_AL (numeric) The P-content of the soil extracted with Acetate Lactate (mg/kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_ro(B_LU = 'testcrop1',A_P_AL = 5)
#' osi_c_phosphor_ro(B_LU = c('testcrop1','testcrop2'),A_P_AL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Romenia derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_ro <- function(B_LU, A_P_AL,unitcheck = TRUE) {
  
  # add visual binding
  cropcat1 = id = NULL
  
  # length of inputs
  arg.length <- max(length(B_LU),length(A_P_AL))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_P_AL = A_P_AL),
               fname = 'osi_c_phoshor_ro',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_P_AL = A_P_AL,
                   value = NA_real_)
  
  # P index derived following P-AL
  
  # evaluation soil P status
  # https://icpa.ro/site_vechi/documente/coduri/Planuri_de_fertilizare.pdf
  dt[, value := OBIC::evaluate_logistic(A_P_AL, b = 0.1387092 , x0 = -18.0674770, v = 0.0252298)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphate availability index in Sweden
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate (mg P / kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_se(B_LU = '3301061299',A_P_AL = 5)
#' osi_c_phosphor_se(B_LU = c('3301061299','3301000000'),A_P_AL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Sweden derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_se <- function(B_LU, A_P_AL,unitcheck = TRUE) {
  
  # add visual binding
  crop_cat1 = osi_country = . = crop_code = crop_cat2 = id = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='SE']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_AL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('SE',arg.length),
                           B_LU = B_LU,
                           A_P_AL = A_P_AL),
               fname = 'osi_c_phoshor_se',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_P_AL = A_P_AL,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_cat2)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # evaluation soil P status III for maize and cereals
  dt[grepl('maize|cere|whea',crop_cat2), value := osi_evaluate_logistic(A_P_AL, b = 0.17523368 , x0 = 1.09283723 , v = 0.01031949 )]
  
  # evaluation soil P status II for hostvete
  dt[, value := osi_evaluate_logistic(A_P_AL, b = 0.60458, x0 = 2.8517965, v = 0.0256494)]
  
  # evaluation soil P status III for oil crops
  dt[grepl('oilcrops',crop_cat2), value := osi_evaluate_logistic(A_P_AL, b = 0.07434209 , x0 = 41.85137331  , v = 0.83179444)]
  
  # evaluation soil P status IVA for potato and sugar beet
  dt[grepl('potato|beet',crop_cat2), value := osi_evaluate_logistic(A_P_AL, b = 0.07434209 , x0 = 41.85137331  , v = 0.83179444)]
  
  # evaluation soil P status III for maize and cereals for all other crops as well
  dt[grepl('arable|other|vegeta|fodd|legum|grass|fruit',crop_cat2), value := osi_evaluate_logistic(A_P_AL, b = 0.17523368 , x0 = 1.09283723 , v = 0.01031949 )]
  
  # add OSI score for "other" crops: nature, forest, other
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_sk(B_LU = '3301010901', A_P_M3 = 45,B_TEXTURE_HYPRES = 'C')
#' 
#' @return 
#' The phosphorus availability index in Slovak Republic estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_sk <- function(B_LU, B_TEXTURE_HYPRES,A_P_M3,unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='SE']
 
  # get max length of inputs
  arg.length <- max(length(B_LU),length(B_TEXTURE_HYPRES),length(A_P_M3))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('SK',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                           A_P_M3 = A_P_M3),
               fname = 'osi_c_phoshor_sk',
               unitcheck = unitcheck)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_P_M3 = A_P_M3,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # convert to the OSI score
  dt[B_TEXTURE_HYPRES %in% c('C'),value := osi_evaluate_logistic(x = A_P_M3, b= 0.05944561 ,x0 = 2.17842096 ,v = 0.01491386 )]
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),value := osi_evaluate_logistic(x = A_P_M3, b= 0.06242334 ,x0 = 2.29248099 ,v = 0.02274146 )]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),value := osi_evaluate_logistic(x = A_P_M3, b= 0.09371117,x0 = 3.09974519 ,v = 0.02330211 )]
  
  # add OSI score for "other" crops: nature, forest, other
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_sl(B_LU = 'testcrop1',A_P_AL = 45)
#' 
#' @return 
#' The phosphorus availability index in Slovenia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_phosphor_sl <- function(A_P_AL,B_LU = NA_character_,unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']

  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_P_AL))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_P_AL = A_P_AL),
               fname = 'osi_c_phoshor_sl',
               unitcheck = unitcheck)
  
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
  dt[,value := osi_evaluate_logistic(x = A_P_AL, b= 0.076756822 ,x0 = -24.482122135   ,v = 0.009860042 )]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphor_uk(B_LU = 'testcrop1',A_SOM_LOI = 3,A_P_OL = 5)
#' osi_c_phosphor_uk(B_LU = c('testcrop1','testcrop2'),A_SOM_LOI = c(3,4),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in United Kingdom derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_phosphor_uk <- function(B_LU, A_SOM_LOI,A_P_OL,unitcheck = TRUE) {
  
  # set visual bindings
  crop_name = crop_cat1 = BD = . = NULL
  
  # crop properties
  # dt.crops <- as.data.table(euosi::osi_crops)
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_P_OL))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,
                           A_P_OL = A_P_OL),
               fname = 'osi_c_phoshor_uk',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # merge with crop
  # dt <- merge(dt,
  #             dt.crops[,.(B_LU, crop_name, crop_cat1)],
  #             by = 'B_LU',
  #             all.x = TRUE)
  
  # temporary fix
  dt[,crop_name := B_LU]
  
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
