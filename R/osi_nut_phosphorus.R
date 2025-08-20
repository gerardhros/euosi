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
#' @param pwarning (boolean) Option to print a warning rather than error (stop) message for input checks (TRUE or FALSE)
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
                      A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_, B_COUNTRY, pwarning = FALSE) {
  
  # add visual bindings
  B_TEXTURE_BE = B_TEXTURE_GEPPA = B_TEXTURE_HYPRES = B_TEXTURE_USDA = A_SILT_MI = NULL
  
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
               fname='oci_nut_p',
               na_allowed = TRUE,
               pwarning = pwarning)
  
  
  # estimate texture information
  dt[,B_TEXTURE_USDA := osi_get_TEXTURE_USDA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_HYPRES := osi_get_TEXTURE_HYPRES(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_BE := osi_get_TEXTURE_BE(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_GEPPA := osi_get_TEXTURE_GEPPA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[is.na(B_SOILTYPE_AGR), B_SOILTYPE_AGR := osi_get_SOILTYPE_AGR(A_CLAY_MI, A_SAND_MI, A_SOM_LOI, A_PH_CC)]
  
  # estimate missing soil properties
  dt[is.na(A_PH_WA) & !is.na(A_PH_CC), A_PH_WA := osi_conv_ph(element='A_PH_WA',A_PH_CC = A_PH_CC)]
  dt[!is.na(A_PH_WA) & is.na(A_PH_CC), A_PH_CC := osi_conv_ph(element='A_PH_CC',A_PH_WA = A_PH_WA)]
  dt[is.na(A_P_AL) & !is.na(A_P_OL), A_P_AL := osi_conv_phosphor(element='A_P_AL',A_P_OL = A_P_OL,A_PH_CC = A_PH_CC)]
  dt[is.na(A_P_CAL) & !is.na(A_P_OL), A_P_CAL := osi_conv_phosphor(element='A_P_CAL',A_P_OL = A_P_OL)]
  dt[is.na(A_P_DL) & !is.na(A_P_OL), A_P_DL := osi_conv_phosphor(element='A_P_DL',A_P_OL = A_P_OL)]
  dt[is.na(A_P_AAA) & !is.na(A_P_OL), A_P_AAA := osi_conv_phosphor(element='A_P_AAA',A_P_OL = A_P_OL)]
  dt[is.na(A_P_M3) & !is.na(A_P_OL), A_P_M3 := osi_conv_phosphor(element='A_P_M3',A_P_OL = A_P_OL)]
  dt[is.na(A_P_WA) & !is.na(A_P_OL), A_P_WA := osi_conv_phosphor(element='A_P_WA',A_P_OL = A_P_OL,B_SOILTYPE_AGR = B_SOILTYPE_AGR)]
  dt[is.na(A_P_CC) & !is.na(A_P_OL), A_P_CC := osi_conv_phosphor(element='A_P_CC',A_P_OL = A_P_OL)]
  
  #check required calculated inputs
  osi_checkvar(parm = list(B_TEXTURE_USDA = dt$B_TEXTURE_USDA, 
                           B_TEXTURE_HYPRES = dt$B_TEXTURE_HYPRES,
                           B_TEXTURE_BE = dt$B_TEXTURE_BE,
                           B_TEXTURE_GEPPA = dt$B_TEXTURE_GEPPA,
                           B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
                           A_PH_WA = dt$A_PH_WA,
                           A_P_AL = dt$A_P_AL,
                           A_P_CAL = dt$A_P_CAL,
                           A_P_DL = dt$A_P_DL,
                           A_P_AAA = dt$A_P_AAA,
                           A_P_M3 = dt$A_P_M3,
                           A_P_WA = dt$A_P_WA,
                           A_P_CC = dt$A_P_CC),
               fname='oci_nut_p',
               pwarning = pwarning)
  
  # calculate the OSI score for P excess
  
  # Austria (AT), Belgium (BE), Switzerland (CH), Czech Republic (CZ), Germany (DE)
  dt[B_COUNTRY == 'AT', value := osi_nut_p_at(B_LU = B_LU, A_P_CAL = A_P_CAL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'BE', value := osi_nut_p_be(B_LU = B_LU,A_P_AL = A_P_AL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'CH', value := osi_nut_p_ch(B_LU = B_LU, A_P_AAA = A_P_AAA, unitcheck = FALSE)]
  dt[B_COUNTRY == 'CZ', value := osi_nut_p_cz(B_LU = B_LU, A_P_M3 = A_P_M3, unitcheck = FALSE)]
  dt[B_COUNTRY == 'DE', value := osi_nut_p_de(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_P_CAL = A_P_CAL, A_P_DL = A_P_DL, unitcheck = FALSE)]
  
  # Denmark (DK), Estonia (EE), Spain (ES),France (FR), Finland (FI) 
  dt[B_COUNTRY == 'DK', value := osi_nut_p_dk(B_LU = B_LU, A_P_OL = A_P_OL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'EE', value := osi_nut_p_ee(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_P_M3 = A_P_M3, unitcheck = FALSE)]
  dt[B_COUNTRY == 'EL', value := osi_nut_p_el(B_LU = B_LU, A_P_OL = A_P_OL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'ES', value := osi_nut_p_es(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_P_OL = A_P_OL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'FR', value := osi_nut_p_fr(B_LU = B_LU, A_P_OL = A_P_OL,A_PH_WA = A_PH_WA, unitcheck = FALSE)]
  dt[B_COUNTRY == 'FI', value := osi_nut_p_fi(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_P_AAA = A_P_AAA, A_C_OF = A_C_OF, unitcheck = FALSE)]
  
  # Hungary (HU), Ireland (IE), Italy (IT), Latvia (LV), Lithuania (LT)
  dt[B_COUNTRY == 'HU', value := osi_nut_p_hu(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_CACO3_IF = A_CACO3_IF,A_P_AL = A_P_AL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'IE', value := osi_nut_p_ie(B_LU = B_LU, A_P_OL = A_P_OL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'IT', value := osi_nut_p_it(B_LU = B_LU, A_P_OL = A_P_OL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'LV', value := osi_nut_p_lv(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_P_DL = A_P_DL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'LT', value := osi_nut_p_lt(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_P_AL = A_P_AL, unitcheck = FALSE)]
  
  # the Netherlands (NL), Norway (NO),  Sweden (SE), Slovak Republic (SK), Slovenia (SL)
  dt[B_COUNTRY == 'NL', value := osi_nut_p_nl(B_LU = B_LU, A_P_AL = A_P_AL, A_P_CC = A_P_CC, A_P_WA = A_P_WA,B_SOILTYPE_AGR = B_SOILTYPE_AGR, unitcheck = FALSE)]
  dt[B_COUNTRY == 'NO', value := osi_nut_p_no(B_LU = B_LU, A_P_AL = A_P_AL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'SE', value := osi_nut_p_se(B_LU = B_LU, A_P_AL = A_P_AL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'SK', value := osi_nut_p_sk(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_P_M3 = A_P_M3, unitcheck = FALSE)]
  dt[B_COUNTRY == 'SL', value := osi_nut_p_sl(B_LU = B_LU, A_P_AL = A_P_AL, unitcheck = FALSE)]
  
  # Poland (PL), Portugal (PT), Romenia (RO) and United Kingdom (UK)
  dt[B_COUNTRY == 'PL', value := osi_nut_p_pl(B_LU = B_LU, A_P_DL = A_P_DL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'PT', value := osi_nut_p_pt(B_LU = B_LU, A_P_OL = A_P_OL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'RO', value := osi_nut_p_ro(B_LU = B_LU, A_P_AL = A_P_AL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'UK', value := osi_nut_p_uk(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_P_OL = A_P_OL, unitcheck = FALSE)]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_at(B_LU = '3301000000', A_P_CAL = 47)
#' 
#' @return 
#' The phosphorus excess index in Austria estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_at <- function(A_P_CAL,B_LU = NA_character_, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='AT']
  
  # get max length of input variables
  arg.length <- max(length(A_P_CAL),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('AT',arg.length),
                           B_LU = B_LU,
                           A_P_CAL = A_P_CAL),
               fname = 'osi_nut_p_at',
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
  dt[,value := osi_evaluate_logistic(x = A_P_CAL, b= -0.02002226, x0=199.66245188,  v= 0.35544312)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_be(B_LU = '8410', A_P_AL = 45)
#' 
#' @return 
#' The phosphorus excess index in Belgium estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_be <- function(B_LU, A_P_AL, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = crop_k = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='BE']
   
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'BE' & osi_indicator =='i_e_p']
  
  # get the max length of input variables
  arg.length <- max(length(B_LU),length(A_P_AL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('BE',arg.length),
                           B_LU = B_LU,
                           A_P_AL = A_P_AL),
               fname = 'osi_nut_p_be',
               unitcheck = unitcheck)
  
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
  dt[grepl('grass',crop_cat1),value := osi_evaluate_logistic(x = A_P_AL, b= -0.012,x0 = 340,v = 1)]
  dt[grepl('arable|maize|perman',crop_cat1),value := osi_evaluate_logistic(x = A_P_AL, b= -0.011,x0 = 425,v = 1)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_ch(B_LU = 'testcrop1',A_P_AAA = 50)
#' 
#' @return 
#' The phosphorus excess index in Switzerland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_ch <- function(A_P_AAA,B_LU = NA_character_, unitcheck = TRUE) {
  
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
  
  # get max length of input variables
  arg.length <- max(length(A_P_AAA),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(A_P_AAA = A_P_AAA),
               fname = 'osi_nut_p_ch',
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_cz(B_LU = '3303030100', A_P_M3 = 81)
#' 
#' @return 
#' The phosphorus excess index in Czech Republic estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_cz <- function(A_P_M3,B_LU = NA_character_, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='CZ']
 
  # get max length of input variables
  arg.length <- max(length(A_P_M3),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('CZ',arg.length),
                           B_LU = B_LU,
                           A_P_M3 = A_P_M3),
               fname = 'osi_nut_p_cz',
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
  dt[,value := osi_evaluate_logistic(x = A_P_M3, b= -0.02196993,x0= 146.79292298,  v= 2.49753746)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @return 
#' The phosphate excess index in Germany stimated from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_de <- function(B_LU, A_SOM_LOI,A_P_CAL = NA_real_, A_P_DL = NA_real_, unitcheck = TRUE) {
  
  # add visual bindings
  value1 = value2 = A_P_CAL2 = NULL
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='DE']
  
  # argument length
  arg.length <- max(length(B_LU),length(A_SOM_LOI),
                    length(A_P_CAL),length(A_P_DL))
  
  # repeat A_P_DL if only one default is given
  if(length(A_P_DL)==1 & arg.length > 1){A_P_DL <- rep(A_P_DL,arg.length)}
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('DE',arg.length),
                           B_LU = B_LU,
                           A_P_CAL = A_P_CAL),
               fname = 'osi_nut_p_de',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_SOM_LOI= A_SOM_LOI,
                   A_P_CAL = A_P_CAL * 0.1, # in mg P/100g
                   A_P_CAL2 = A_P_CAL * ((1 / (0.02525 * A_SOM_LOI + 0.6541))/10), #in mg P/100ml
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
               fname = 'osi_nut_p_de',
               unitcheck = TRUE)
  
  # evaluation conform VDLUFA for cropland and soil types
  dt[!is.na(A_P_CAL), value1 := osi_evaluate_logistic(A_P_CAL,b= -0.045383281,x0= 200.152028481,v=   0.001801177)]
  
  # adjust for peat soils
  dt[!is.na(A_P_CAL) & A_SOM_LOI > 20, value1 := osi_evaluate_logistic(A_P_CAL, b = -0.04638022,x0= 68.73331583,v=  1.67697573)]
  
  # evaluation conform VDLUFA for cropland and soil types
  dt[!is.na(A_P_DL), value2 := osi_evaluate_logistic(A_P_DL, b = 0.5357, x0 = -4.03796, v = 0.01856)]
  
  # set value
  dt[,value := fifelse(!is.na(A_P_CAL),value1,value2)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_dk(B_LU = '3301010399',A_P_OL = 5)
#' osi_nut_p_dk(B_LU = c('3301010399','3301029800'),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate excess index in Denmark derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_dk <- function(B_LU, A_P_OL, unitcheck = TRUE) {
  
  # add visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='DK']
  
  # get argument length
  arg.length <- max(length(B_LU),length(A_P_OL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('DK',arg.length),
                           B_LU = B_LU,
                           A_P_OL = A_P_OL),
               fname = 'osi_nut_p_dk',
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
  dt[, value := osi_evaluate_logistic(A_P_OL, b = -0.02403355, x0 = 202.32313601,v =   0.13527924)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_ee(A_P_M3 = 45,A_SOM_LOI = 3, B_LU='testcrop1')
#' 
#' @return 
#' The phosphorus excess index in Estonia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_ee <- function(A_P_M3,A_SOM_LOI,B_LU = NA_character_, unitcheck = TRUE) {
  
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
  
  # get max length of input variables
  arg.length <- max(length(A_P_M3),length(A_SOM_LOI),length(B_LU))
  
  # check inputs (no check on crop type due to missing crop codes)
  osi_checkvar(parm = list(A_P_M3 = A_P_M3,
                           A_SOM_LOI = A_SOM_LOI),
               fname = 'osi_nut_p_ee',
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
  dt[,value := osi_evaluate_logistic(x = A_P_M3, b = -0.01966403, x0 = 199.66773872, v= 0.36047007)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphate excess index in Greece
#' 
#' This function calculates the phosphate excess index. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg P/kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_el(B_LU = '3301061299',A_P_OL = 5)
#' osi_nut_p_el(B_LU = c('3301061299','3301000000'),A_P_OL = c(5,15))
#' 
#' @return 
#' The phosphate availability index in Greece derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_el <- function(B_LU, A_P_OL, unitcheck = TRUE) {
  
  # add visual bindings
  osi_country = crop_code = crop_cat1 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='EL']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_OL))
  
  # check inputs (crop code is not yet in osi_crops, so no check)
  osi_checkvar(parm = list(A_P_OL = A_P_OL),
               fname = 'osi_nut_p_el',
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
  dt[, value := OBIC::evaluate_logistic(A_P_OL, b = -5.169509e-02, x0 = 2.001029e+02, v = 5.130838e-04)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphate excess index in Spain
#' 
#' This function calculates the phosphate excess. 
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
#' osi_nut_p_es(B_LU = '3301010301',A_CLAY_MI = 5,A_SAND_MI = 25,A_P_OL = 5)
#' osi_nut_p_es(B_LU = c('3301010901','3301010500'),A_CLAY_MI = c(5,10),
#' A_SAND_MI = c(50,50),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate excess index in Spain derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_es <- function(B_LU, A_CLAY_MI,A_SAND_MI,A_P_OL, unitcheck = TRUE) {
  
  # add visual bindings
  osi_country = crop_code = crop_cat1 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='ES']
  
  # get max length of input variables
  arg.length <- max(length(B_LU),length(A_CLAY_MI),length(A_SAND_MI),length(A_P_OL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('ES',arg.length),
                           B_LU = B_LU,
                           A_CLAY_MI = A_CLAY_MI,
                           A_SAND_MI = A_SAND_MI,
                           A_P_OL = A_P_OL),
               fname = 'osi_nut_p_es',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
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
  
  # assess P excess for sandy soils (Arenoso)
  dt[A_CLAY_MI <= 15 & A_SAND_MI > 50, value := osi_evaluate_logistic(A_P_OL, b = -0.1369895, x0 = 22.7738155, v=  1.5927931 )]
  
  # assess P excess for loamy? soils (Franco)
  dt[A_CLAY_MI <= 15 & A_SAND_MI <=50 , value := osi_evaluate_logistic(A_P_OL, b = -0.07218576, x0 = 43.60115997, v =  1.62711859)]
  
  # assess P excess for clayey soils (Arcilloso)
  dt[A_CLAY_MI > 15, value := osi_evaluate_logistic(A_P_OL, b = -0.05414748, x0 = 58.11857292, v=  1.62796595)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param A_P_AAA (numeric) The exchangeable P-content of the soil measured via ammonium acetate extraction
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_fi(B_LU = '4010', B_TEXTURE_USDA = 'Si',
#' A_P_AAA = 45,A_C_OF=1.5)
#' 
#' @return 
#' The phosphorus excess index in Finland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_fi <- function(B_LU, B_TEXTURE_USDA, A_P_AAA,A_C_OF = 0.5, unitcheck = TRUE) {
  
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
  dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_e_p']
  
  # get the max length of input variables
  arg.length <- max(length(B_LU),length(B_TEXTURE_USDA),length(A_P_AAA),length(A_C_OF))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('FI',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_USDA = B_TEXTURE_USDA,
                           A_C_OF = A_C_OF,
                           A_P_AAA = A_P_AAA),
               fname = 'osi_nut_p_fi',
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
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = 'B_SOILTYPE_AGR',
              by.y = 'osi_threshold_soilcat',
              all.x = TRUE)
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = A_P_AAA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
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
osi_nut_p_fr <- function(B_LU, A_P_OL,A_PH_WA = NA_real_, unitcheck = TRUE) {
  
  # set visual bindings
  value = osi_country = osi_indicator = id = crop_cat1 = B_SOILTYPE_AGR = NULL
  crop_code = crop_p = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # Load in the interal datasets
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FR']
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FR' & osi_indicator =='i_e_p']
  
  # soil types
  dt.soiltype <- as.data.table(euosi::osi_soiltype)
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_OL),length(A_PH_WA))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('FR',arg.length),
                           B_LU = B_LU,
                           A_PH_WA = A_PH_WA,
                           A_P_OL = A_P_OL),
               fname = 'osi_nut_p_fr',
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
  
  # estimate agricultural soil type
  dt[, B_SOILTYPE_AGR := fifelse(A_PH_WA > 8,'craie','general')]
  dt[is.na(B_SOILTYPE_AGR) & is.na(A_PH_WA), B_SOILTYPE_AGR := 'general']
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = c('B_SOILTYPE_AGR', 'crop_p'),
              by.y = c('osi_threshold_soilcat','osi_threshold_cropcat'),
              all.x = TRUE)
  
  # estimate OSI score
  dt[,value := osi_evaluate_logistic(x = A_P_OL, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_hu(B_LU = 'testcrop1',A_P_AL = 45,A_CACO3_IF = 5,A_CLAY_MI = 5,A_SOM_LOI = 5)
#' 
#' @return 
#' The phosphorus excess index in Hungary estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_hu <- function(A_SOM_LOI,A_CLAY_MI,A_CACO3_IF,A_P_AL,B_LU = NA_character_, unitcheck = TRUE) {
  
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
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,
                           A_CLAY_MI = A_CLAY_MI,
                           A_CACO3_IF = A_CACO3_IF,
                           A_P_AL = A_P_AL),
               fname = 'osi_nut_p_hu',
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_ie(B_LU = 'testcrop1',A_P_OL = 5)
#' osi_nut_p_ie(B_LU = c('testcrop1','testcrop2'),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate excess index in Ireland derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_ie <- function(B_LU, A_P_OL, unitcheck = TRUE) {
  
  # add visual binding
  cropcat1 = NULL
  
  # length of inputs
  arg.length <- max(length(B_LU),length(A_P_OL))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_P_OL = A_P_OL),
               fname = 'osi_nut_p_ie',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # temporary fix since crop codes are unknown
  dt[grepl('grass',B_LU),cropcat1 := 'grassland']
  dt[!grepl('grass',B_LU),cropcat1 := 'arable']
  
  # P index derived following P-Olsen.
  # evaluation soil P status for grasslands
  dt[cropcat1 =='grassland', value := OBIC::evaluate_logistic(A_P_OL, b = -0.1624248,x0 = 19.3785353,v =  1.6269463)]
  
  # evaluation soil P status for other crops
  dt[cropcat1 == 'arable', value := OBIC::evaluate_logistic(A_P_OL, b = -0.1299494,x0 = 24.2190050,v=  1.6273494)]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_it(B_LU = '3301061299',A_P_OL = 5)
#' osi_nut_p_it(B_LU = c('3301061299','3301000000'),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate excess index in Italy derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_it <- function(B_LU, A_P_OL, unitcheck = TRUE) {
  
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
               fname = 'osi_nut_p_it',
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
  dt[, value := osi_evaluate_logistic(A_P_OL, b = -0.1299494, x0 = 24.2190050, v= 1.6273494)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_lv(B_LU ='testcrop1',A_P_DL = 45,B_TEXTURE_USDA = 'Sa')
#' 
#' @return 
#' The phosphorus excess index in Latvia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_lv <- function(A_P_DL,B_TEXTURE_USDA, B_LU = NA_character_, unitcheck = TRUE) {
  
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
  arg.length <- max(length(A_P_DL),length(B_TEXTURE_USDA),length(B_LU))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(B_TEXTURE_USDA = B_TEXTURE_USDA,
                           A_P_DL = A_P_DL),
               fname = 'osi_nut_p_lv',
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_lt(B_LU = 'testcrop1', A_P_AL = 45,A_SOM_LOI = 4.5)
#' 
#' @return 
#' The phosphorus excess index in Lithuania estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_lt <- function(A_P_AL,A_SOM_LOI,B_LU = NA_character_, unitcheck = TRUE) {
  
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
  arg.length <- max(length(A_P_AL),length(A_SOM_LOI),length(B_LU))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,
                           A_P_AL = A_P_AL),
               fname = 'osi_nut_p_lt',
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
#' @param B_SOILTYPE_AGR (character) The soil type in a particular region
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2
#' @param A_P_WA (numeric) The P-content of the soil extracted with water
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_nl(B_LU = '265', A_P_AL = 45, A_P_CC = 2.5, B_SOILTYPE_AGR = 'dekzand')
#' osi_nut_p_nl(B_LU = c('265','1019'),A_P_AL = c(35,54),
#' A_P_CC = c(2.5,4.5), A_P_WA = c(35,65), B_SOILTYPE_AGR = rep('dekzand',2))
#' 
#' @return 
#' The phosphate excess index in the Netherlands estimated from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_nl <- function(B_LU, A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_,
                         B_SOILTYPE_AGR = NA_real_, unitcheck = TRUE) {
  
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
               fname = 'osi_nut_p_nl',
               na_allowed = TRUE,
               unitcheck = unitcheck)
  
  # Collect the data into a table, convert to Dutch units
  # assume 3.5% organic matter for the calculation of bulk density
  dt <- data.table(id = 1:arg.length,
                   A_P_AL = A_P_AL * 2.29 * 0.1,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA *(1 / (0.02525 * 3.5 + 0.6541))/0.43646,
                   B_LU = B_LU,
                   value = NA_real_)
  
  dt <- merge(dt,dt.crops,by.x = 'B_LU', by.y = 'crop_code',all.x=TRUE)
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # Calculate the phosphate excess for grass (PBI)
  dt[grepl("gras",crop_cat1), value := pmax(0,log(A_P_CC) * (-0.0114 * A_P_AL + 2.5) + 0.0251 * A_P_CC + 2)]
  
  # Calculate the phosphate excess for maize (PBI)
  dt[grepl("maize",crop_cat1), value := A_P_CC + 0.05 * pmin(37,(A_P_AL / A_P_CC))]
  
  # calculate the P-excess for arable systems, normalized to a scale with maximum around 6
  dt[grepl("arable|cropland|permanent",crop_cat1), value := A_P_WA * 0.1]
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = value, b = -0.3248758, x0 =  9.6880541, v =  1.6272915 )]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_no(B_LU = 'testcrop1', A_P_AL = 50)
#' 
#' @return 
#' The phosphorus excess index in Norway estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_no <- function(A_P_AL,B_LU = NA_character_, unitcheck = TRUE) {
  
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
  arg.length <- max(length(A_P_AL),length(B_LU))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_P_AL = A_P_AL),
               fname = 'osi_nut_p_no',
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
  dt[,value := osi_evaluate_logistic(x = A_P_AL, b = -0.01912416, x0 = 206.32851686, v=   0.34388620)]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_pl(B_LU = 'testcrop1',A_P_DL = 45)
#' 
#' @return 
#' The phosphorus excess index in Poland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_pl <- function(A_P_DL,B_LU = NA_character_, unitcheck = TRUE) {
  
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
  arg.length <- max(length(A_P_DL),length(B_LU))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_P_DL = A_P_DL),
               fname = 'osi_nut_p_pl',
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
  dt[,value := osi_evaluate_logistic(x = A_P_DL, b = -0.02283399, x0 = 151.86202214, v =   0.78388489)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphate excess index in Portugal
#' 
#' This function calculates the phosphate excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg P/kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_pt(B_LU = '3301061299',A_P_OL = 5)
#' osi_nut_p_pt(B_LU = c('3301061299','3301000000'),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Portugal derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_pt <- function(B_LU, A_P_OL, unitcheck = TRUE) {
  
  # add visual bindings
  osi_country = crop_code = crop_cat1 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='PT']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_OL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('PT',arg.length),
                           B_LU = B_LU,
                           A_P_AL = A_P_OL),
               fname = 'osi_nut_p_pt',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_P_OL = A_P_OL * 2.29,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # P index derived following P-Olsen.
  
  # evaluation soil P status for grasslands and croplands
  dt[, value := OBIC::evaluate_logistic(A_P_OL, b = -0.04188751 , x0 = 77.36111528 , v = 1.74093923)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphate excess index index in Romenia
#' 
#' This function calculates the phosphate excess index. 
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
osi_nut_p_ro <- function(B_LU, A_P_AL, unitcheck = TRUE) {
  
  # add visual binding
  cropcat1 = NULL
  
  # length of inputs
  arg.length <- max(length(B_LU),length(A_P_AL))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_P_AL = A_P_AL),
               fname = 'osi_nut_p_ro',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_P_AL = A_P_AL,
                   value = NA_real_)
  
  # P index derived following P-AL
  
  # evaluation soil P status
  # https://icpa.ro/site_vechi/documente/coduri/Planuri_de_fertilizare.pdf
  dt[, value := OBIC::evaluate_logistic(A_P_AL, b = -1.781695 , x0 = 56.737743, v = 69.979745)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphate excess index in Sweden
#' 
#' This function calculates the phosphate excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate (mg P / kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_se(B_LU = '3301061299',A_P_AL = 5)
#' osi_nut_p_se(B_LU = c('3301061299','3301000000'),A_P_AL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate excess index in Sweden derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_se <- function(B_LU, A_P_AL, unitcheck = TRUE) {
  
  # add visual binding
  crop_cat1 = osi_country = . = crop_code = crop_cat2 = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='SE']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_AL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('SE',arg.length),
                           B_LU = B_LU,
                           A_P_AL = A_P_AL),
               fname = 'osi_nut_p_se',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
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
  dt[grepl('maize|cere|whea',crop_cat2), value := osi_evaluate_logistic(A_P_AL, b = -0.03374637, x0 =  93.47462, v= 1.636168)]
  
  # evaluation soil P status II for hostvete
  dt[, value := osi_evaluate_logistic(A_P_AL,b =-0.07080519, x0 = 200.73869, v= 1.209206e-05)]
  
  # evaluation soil P status III for oil crops
  dt[grepl('oilcrops',crop_cat2), value := osi_evaluate_logistic(A_P_AL, b = -0.03374637, x0 =  93.47462,  v= 1.636168)]
  
  # evaluation soil P status IVA for potato and sugar beet
  dt[grepl('potato|beet',crop_cat2), value := osi_evaluate_logistic(A_P_AL, b = -0.01969912,x0= 158.94441,v= 1.606657)]
  
  # evaluation soil P status III for maize and cereals and other crops as well
  dt[grepl('arable|other|vegeta|fodd|legum|grass|fruit',crop_cat2), value := osi_evaluate_logistic(A_P_AL, b = -0.03374637, x0 =  93.47462, v= 1.636168)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_sk(B_LU = '3301010901', A_P_M3 = 45,B_TEXTURE_HYPRES = 'C')
#' 
#' @return 
#' The phosphorus excess index in Slovak Republic estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_sk <- function(B_LU, B_TEXTURE_HYPRES,A_P_M3, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='SK']
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(B_TEXTURE_HYPRES),length(A_P_M3))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('SK',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                           A_P_M3 = A_P_M3),
               fname = 'osi_nut_p_sk',
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
  dt[B_TEXTURE_HYPRES %in% c('C'),value := osi_evaluate_logistic(x = A_P_M3,b= -0.01624901,x0 = 202.99049001, v =   1.80519106 )]
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),value := osi_evaluate_logistic(x = A_P_M3,b= -0.01731495,x0 = 188.56710994, v =   1.76801999 )]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),value := osi_evaluate_logistic(x = A_P_M3, b= -0.01225165,x0 = 351.92210058, v =   0.24005529)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_sl(B_LU = 'testcrop1',A_P_AL = 45)
#' 
#' @return 
#' The phosphorus excess index in Slovenia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_sl <- function(A_P_AL,B_LU = NA_character_, unitcheck = TRUE) {
  
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
  arg.length <- max(length(A_P_AL),length(B_LU))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_P_AL = A_P_AL),
               fname = 'osi_nut_p_sl',
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
  dt[,value := osi_evaluate_logistic(x = A_P_AL, b = -0.0262186, x0 = 122.4380883, v =   1.7036038 )]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_nut_p_uk(B_LU = 'testcrop1',A_SOM_LOI = 3,A_P_OL = 5)
#' osi_nut_p_uk(B_LU = c('testcrop1','testcrop2'),A_SOM_LOI = c(3,4),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate excess index in United Kingdom derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_nut_p_uk <- function(B_LU, A_SOM_LOI,A_P_OL, unitcheck = TRUE) {
  
  # add visual bindings
  crop_name = . = crop_cat1 = BDS = NULL
  
  # crop properties
  # dt.crops <- as.data.table(euosi::osi_crops)
  
  # get max length of inputs
  arg.length <- max(length(A_P_OL),length(A_SOM_LOI),length(B_LU))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,
                           A_P_OL = A_P_OL),
               fname = 'osi_nut_p_uk',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
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
