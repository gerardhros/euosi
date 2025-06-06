#' Calculate the pH index (wrapper function)
#' 
#' This function calculates the pH index for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_CA_CO_PO (numeric) The calcium occupation of the CEC (\%)
#' @param A_MG_CO_PO (numeric) The magnesium occupation of the CEC (\%)
#' @param A_K_CO_PO (numeric) The potassium occupation of the CEC (\%)
#' @param A_NA_CO_PO (numeric) The sodium occupation of the CEC (\%)
#' @param A_PH_WA (numeric) The pH measured in h2o
#' @param A_PH_CC (numeric) The pH measured in cacl2 
#' @param A_PH_KCL (numeric) The pH measured in KCl
#' @param B_COUNTRY (character) The country code
#' 
#' @import data.table
#' 
#' @return
#' The index to evaluate the soil pH 
#' 
#' @export
osi_c_ph <- function(B_LU, 
                     B_SOILTYPE_AGR = NA_character_, A_CLAY_MI= NA_real_, A_SAND_MI = NA_real_,
                     A_SOM_LOI = NA_real_, A_C_OF = NA_real_,
                     A_CA_CO_PO = NA_real_, A_MG_CO_PO = NA_real_,A_K_CO_PO = NA_real_, A_NA_CO_PO = NA_real_,
                     A_PH_WA = NA_real_, A_PH_CC= NA_real_, A_PH_KCL= NA_real_,
                     B_COUNTRY) {
  
  # add visual bindings
  value = B_TEXTURE_USDA = A_SILT_MI = B_TEXTURE_HYPRES = NULL
  B_TEXTURE_BE = B_TEXTURE_GEPPA = id = NULL
  
  # desired length of inputs
  arg.length <- max(length(B_LU), 
                    length (B_SOILTYPE_AGR), length(A_CLAY_MI),length(A_SAND_MI),
                    length(A_SOM_LOI),length(A_C_OF),
                    length(A_PH_CC), length(A_MG_CO_PO),length(A_K_CO_PO), length(A_NA_CO_PO), 
                    length(A_CA_CO_PO), length(A_PH_KCL),length(A_PH_WA), 
                    length(B_COUNTRY))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_CLAY_MI=A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_CA_CO_PO = A_CA_CO_PO,
                   A_MG_CO_PO = A_MG_CO_PO,
                   A_K_CO_PO = A_K_CO_PO,
                   A_NA_CO_PO = A_NA_CO_PO,
                   A_PH_CC = A_PH_CC,
                   A_PH_WA = A_PH_WA,
                   A_PH_KCL = A_PH_KCL,
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
  dt[is.na(A_PH_KCL), A_PH_KCL := osi_conv_ph(element='A_PH_KCL',A_PH_CC = A_PH_CC,A_PH_WA = A_PH_WA)]
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 2]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 * 0.5]
  
  # evaluate OSI index for pH
  
  # Austria (AT), Belgium (BE), Switzerland (CH), Czech Republic (CZ), Germany (DE)
  dt[B_COUNTRY == 'AT', value := osi_c_ph_at(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,A_PH_CC = A_PH_CC)]
  dt[B_COUNTRY == 'BE', value := osi_c_ph_be(B_LU = B_LU, B_TEXTURE_BE = B_TEXTURE_BE, A_PH_KCL = A_PH_KCL)]
  dt[B_COUNTRY == 'CH', value := osi_c_ph_ch(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA, 
                                             A_CA_CO_PO = A_CA_CO_PO, A_MG_CO_PO = A_MG_CO_PO, 
                                             A_K_CO_PO = A_K_CO_PO, A_NA_CO_PO = A_NA_CO_PO)]
  dt[B_COUNTRY == 'CZ', value := NA_real_]
  dt[B_COUNTRY == 'DE', value := osi_c_ph_de(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_C_OF = A_C_OF, 
                                             A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_PH_CC = A_PH_CC)]
  
  # Denmark (DK), Estonia (EE), Spain (ES),France (FR), Finland (FI) 
  dt[B_COUNTRY == 'DK', value := NA_real_]
  dt[B_COUNTRY == 'EE', value := NA_real_]
  dt[B_COUNTRY == 'ES', value := NA_real_]
  dt[B_COUNTRY == 'FR', value := osi_c_ph_fr(B_LU = B_LU,B_TEXTURE_GEPPA = B_TEXTURE_GEPPA,A_PH_WA = A_PH_WA)]
  dt[B_COUNTRY == 'FI', value := osi_c_ph_fi(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_PH_WA = A_PH_WA, A_C_OF = A_C_OF)]
  
  # Hungary (HU), Ireland (IE), Italy (IT), Latvia (LV), Lithuania (LT)
  dt[B_COUNTRY == 'HU', value := NA_real_]
  dt[B_COUNTRY == 'IE', value := osi_c_ph_ie(B_LU = B_LU, A_PH_WA = A_PH_WA, A_SOM_LOI = A_SOM_LOI)]
  dt[B_COUNTRY == 'IT', value := NA_real_]
  dt[B_COUNTRY == 'LV', value := NA_real_]
  dt[B_COUNTRY == 'LT', value := NA_real_]
  
  # the Netherlands (NL), Norway (NO),  Sweden (SE), Slovak Republic (SK), Slovenia (SL)
  dt[B_COUNTRY == 'NL', value := osi_c_ph_nl(ID = id, B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                                             A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_CC = A_PH_CC)]
  dt[B_COUNTRY == 'NO', value := NA_real_]
  dt[B_COUNTRY == 'SE', value := osi_c_ph_se(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_PH_WA = A_PH_WA)]
  dt[B_COUNTRY == 'SK', value := NA_real_]
  dt[B_COUNTRY == 'SL', value := NA_real_]
  
  # Poland (PL), United Kingdom (UK)
  dt[B_COUNTRY == 'PL', value := NA_real_]
  dt[B_COUNTRY == 'UK', value := osi_c_ph_uk(B_LU = B_LU, A_PH_WA = A_PH_WA, A_SOM_LOI = A_SOM_LOI)]
  
  # select the output variable
  value <- dt[,value]
  
  # return the OSI score
  return(value)
  
}

#' Calculate the pH index in Austria
#' 
#' This function calculates the soil acidity
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_PH_CC (numeric) The pH measured in cacl2
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_ph_at(A_PH_CC = 47,B_TEXTURE_HYPRES = 'C',B_LU='grass')
#' 
#' @return 
#' The pH index in Austria, depending on land use and soil type. A numeric value.
#' 
#' @export
osi_c_ph_at <- function(A_PH_CC,B_TEXTURE_HYPRES,B_LU = NA_character_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL

  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # parameters
  # dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']

  # get max length of input
  arg.length <- max(length(A_PH_CC), length(B_TEXTURE_HYPRES),length(B_LU))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_PH_CC = A_PH_CC,
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
  
  # https://ooe.lko.at/das-abc-der-d%C3%BCngung-teil-3-kalk+2400+3417995
  
  # calculate the OSI score light textured soils
  dt[B_TEXTURE_HYPRES %in% c('C'),
     value := osi_evaluate_logistic(x = A_PH_CC, b= 10.6100040,x0 = 4.5990197,v = 0.4430063)]
  # calculate the OSI score medium texture soils
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_PH_CC, b= 9.3631126,x0 = 5.0327400,v = 0.3688407)]
  # calculate the OSI score for heavy textured soils
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_PH_CC, b= 8.8363357,x0 = 5.5183950,v = 0.4419161)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}
#' Calculate the pH index for Belgium 
#' 
#' This function evaluates the pH index in Belgium
#' 
#' @param B_LU (character) The crop type
#' @param B_TEXTURE_BE (character) The soil texture according to Belgium classification system
#' @param A_PH_KCL (numeric) The pH measured in KCl
#' 
#' @import data.table
#' 
#' @return 
#' The pH index in Belgium estimated from pH in KCL, the textural class and the crop type
#' 
#' @export
osi_c_ph_be <- function(B_LU, B_TEXTURE_BE, A_PH_KCL) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = B_SOILTYPE_AGR = NULL
  crop_code = crop_k = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='BE']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'BE' & osi_indicator =='i_c_ph']
  
  # get the max length of inputs
  arg.length <- max(length(B_LU),length(B_TEXTURE_BE), length(A_PH_KCL))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_BE = B_TEXTURE_BE,
                   A_PH_KCL = A_PH_KCL,
                   B_SOILTYPE_AGR = NA_real_,
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
  dt[,value := osi_evaluate_logistic(x = A_PH_KCL, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}
#' Calculate the pH index for Switzerland 
#' 
#' This function evaluates the pH index in Switzerland
#' 
#' @param B_LU (character) The crop type
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_CA_CO_PO (numeric) The calcium occupation of the CEC (\%)
#' @param A_MG_CO_PO (numeric) The magnesium occupation of the CEC (\%)
#' @param A_K_CO_PO (numeric) The potassium occupation of the CEC (\%)
#' @param A_NA_CO_PO (numeric) The sodium occupation of the CEC (\%)
#' @param A_PH_WA (numeric) The pH measured in h2o
#'  
#' @import data.table
#' 
#' @return 
#' The pH index in Switzerland estimated from the base saturation with Ca, Mg and K. For soils with pH water below 5.9 the soil is extracted with HCL+H2SO4. Soils with higher pH are extracted with Bariumchloride.
#' 
#' @export
osi_c_ph_ch <- function(B_LU, A_CLAY_MI = NA_real_,A_PH_WA = NA_real_,
                        A_CA_CO_PO = NA_real_, A_MG_CO_PO= NA_real_, A_K_CO_PO= NA_real_,A_NA_CO_PO= NA_real_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = BS = crop_k = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='CH']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)

  # get max length of input variables
  arg.length <- max(length(B_LU),length(A_CLAY_MI),length(A_PH_WA),
                    length(A_CA_CO_PO),length(A_MG_CO_PO),length(A_K_CO_PO),
                    length(A_NA_CO_PO))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   A_CA_CO_PO = A_CA_CO_PO, 
                   A_MG_CO_PO = A_MG_CO_PO, 
                   A_K_CO_PO = A_K_CO_PO,
                   A_NA_CO_PO = A_NA_CO_PO,
                   value = NA_real_)
  
  # calculate base saturation
  dt[, BS:= A_CA_CO_PO + A_MG_CO_PO + A_K_CO_PO + A_NA_CO_PO]
  # 
  # # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU', 
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  # 
  # # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = c('B_SOILTYPE_AGR', 'crop_cat1'),
  #             by.y = c('osi_threshold_soilcat','osi_threshold_cropcat'),
  #             all.x = TRUE)
  
  # derive the OSI score for the Base Saturation
  dt[,value := osi_evaluate_logistic(x = BS, b= 0.12208837 ,x0 = 0.04485426 ,v = 0.00213294 )]
  
  # If BS not available, ue pH-water (for soils < 30%  clay)
  dt[is.na(value) & A_CLAY_MI <= 30, value := osi_evaluate_logistic(x = A_PH_WA, b= 2.5556578,x0 = -1.0511862,v = 0.0000001)]
  dt[is.na(value) & A_CLAY_MI > 30, value := osi_evaluate_logistic(x = A_PH_WA, b= 2.4248367,x0 = -0.7972011,v = 0.0000003)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the pH index for soils in Germany
#' 
#' This function calculates the soil pH. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_C_OF (numeric) The carbon content of the soil layer (g/ kg)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_PH_CC (numeric) The soil acidity measures with CaCl2 method.
#' 
#' @import data.table
#' 
#' @return 
#' The pH index in Germany derived for grassland and arable crops. A numeric value.
#' 
#' @export
osi_c_ph_de <- function(B_LU,A_SOM_LOI, A_C_OF, A_CLAY_MI,A_SAND_MI, A_PH_CC) {
  
  # add visual bindings
  A_SILT_MI = B_LU_CAT = stype = id = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='DE']
  
  # get max length of input
  arg.length <- max(length(B_LU),length(A_SOM_LOI),length(A_C_OF),
                    length(A_CLAY_MI), length(A_SAND_MI), length(A_PH_CC))
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_C_OF= A_C_OF,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU', 
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # add crop type for now
  dt[B_LU_CAT := 'arable']
  
  # add soil type
  dt[A_SAND_MI >= 85 & A_SILT_MI <= 25 & A_CLAY_MI <= 5 & A_C_OF < 150, stype := "BG1"]
  dt[A_SAND_MI >= 42 & A_SAND_MI <= 95 & A_SILT_MI <= 40 & A_CLAY_MI <= 17 & A_C_OF < 150,  stype :="BG2"]
  dt[A_SAND_MI >= 33 & A_SAND_MI <= 83 & A_SILT_MI <= 50 & A_CLAY_MI >= 8 & A_CLAY_MI <= 25 & A_C_OF < 150,  stype :="BG3"]
  dt[A_SAND_MI <= 75 & A_SILT_MI <= 100 & A_CLAY_MI <= 35 & A_C_OF < 150,  stype :="BG4"]
  dt[A_SAND_MI <= 65 & A_SILT_MI <= 75 & A_CLAY_MI >= 25 & A_CLAY_MI <= 100 & A_C_OF < 150, stype := "BG5"]
  dt[ A_C_OF >= 150,  stype := "BG6"]
  
  # evaluate A_PH_CC for arable soils
  dt.bld.os1 <- dt[B_LU_CAT == 'arable' & A_SOM_LOI <= 4]
  dt.bld.os1[stype=='BG1', value := osi_evaluate_logistic(A_PH_CC, b = 3.195609, x0 = 0.6121391, v = 0.0000020)]
  dt.bld.os1[stype=='BG2', value := osi_evaluate_logistic(A_PH_CC, b = 2.380364, x0 = -0.8092676, v = 0.0000009)]
  dt.bld.os1[stype=='BG3', value := osi_evaluate_logistic(A_PH_CC, b = 2.213768, x0 = -1.3790779, v = 0.0000004)]
  dt.bld.os1[stype=='BG4', value := osi_evaluate_logistic(A_PH_CC, b = 2.409068, x0 = -0.5001904, v = 0.0000006)]
  dt.bld.os1[stype=='BG5', value := osi_evaluate_logistic(A_PH_CC, b = 2.729031, x0 = 2.1575810, v = 0.0000881)]
  
  dt.bld.os2 <- dt[B_LU_CAT == 'arable' & A_SOM_LOI > 4 & A_SOM_LOI <= 8]
  dt.bld.os2[stype=='BG1', value := osi_evaluate_logistic(A_PH_CC, b = 2.312291, x0 = -2.2454588, v = 0.0000002)]
  dt.bld.os2[stype=='BG2', value := osi_evaluate_logistic(A_PH_CC, b = 3.284422, x0 = 0.3346049, v = 0.0000005)]
  dt.bld.os2[stype=='BG3', value := osi_evaluate_logistic(A_PH_CC, b = 1.870397, x0 = -2.5234952, v = 0.0000009)]
  dt.bld.os2[stype=='BG4', value := osi_evaluate_logistic(A_PH_CC, b = 1.954006, x0 = -2.9304223, v = 0.0000001)]
  dt.bld.os2[stype=='BG5', value := osi_evaluate_logistic(A_PH_CC, b = 3.033481, x0 = 1.9002489, v = 0.0000522)]
  
  dt.bld.os3 <- dt[B_LU_CAT == 'arable' & A_SOM_LOI > 8 & A_SOM_LOI <= 15]
  dt.bld.os3[stype=='BG1', value := osi_evaluate_logistic(A_PH_CC, b = 2.877503, x0 = -0.7768501, v = 8.0e-07)]
  dt.bld.os3[stype=='BG2', value := osi_evaluate_logistic(A_PH_CC, b = 2.388151, x0 = -1.8557106, v = 4.0e-07)]
  dt.bld.os3[stype=='BG3', value := osi_evaluate_logistic(A_PH_CC, b = 3.373189, x0 = 0.6575093, v = 2.2e-06)]
  dt.bld.os3[stype=='BG4', value := osi_evaluate_logistic(A_PH_CC, b = 2.282828, x0 = -1.8574349, v = 3.0e-07)]
  dt.bld.os3[stype=='BG5', value := osi_evaluate_logistic(A_PH_CC, b = 2.006280, x0 = -2.1570047, v = 9.0e-07)]
  
  dt.bld.os4 <- dt[B_LU_CAT == 'arable' & A_SOM_LOI > 15 & A_SOM_LOI <= 30]
  dt.bld.os4[stype=='BG1', value := osi_evaluate_logistic(A_PH_CC, b = 4.513283, x0 = 3.2138234, v = 0.0787507)]
  dt.bld.os4[stype=='BG2', value := osi_evaluate_logistic(A_PH_CC, b = 2.772481, x0 = -0.8968812, v = 0.0000016)]
  dt.bld.os4[stype=='BG3', value := osi_evaluate_logistic(A_PH_CC, b = 2.518304, x0 = -1.4907428, v = 0.0000009)]
  dt.bld.os4[stype=='BG4', value := osi_evaluate_logistic(A_PH_CC, b = 3.029233, x0 = 0.3359194, v = 0.0000071)]
  dt.bld.os4[stype=='BG5', value := osi_evaluate_logistic(A_PH_CC, b = 2.492763, x0 = -1.0614226, v = 0.0000016)]
  
  dt.bld.os5 <- dt[B_LU_CAT == 'arable' & A_SOM_LOI > 30]
  dt.bld.os5[,value := osi_evaluate_logistic(A_PH_CC, b = 3.9498965 , x0 = -0.3671917, v = 0.0000002)]
  
  # evaluate A_PH_CC for grassland soils
  dt.gld.os1 <- dt[B_LU_CAT == 'grassland' & A_SOM_LOI <= 15]
  dt.gld.os1[stype=='BG1', value := osi_evaluate_logistic(A_PH_CC, b = 2.208178, x0 = -2.480642, v = 5e-7)]
  dt.gld.os1[stype=='BG2', value := osi_evaluate_logistic(A_PH_CC, b = 2.465608, x0 = -1.222992, v = 7e-7)]
  dt.gld.os1[stype=='BG3', value := osi_evaluate_logistic(A_PH_CC, b = 2.408763, x0 = -1.879384, v = 1e-7)]
  dt.gld.os1[stype=='BG4', value := osi_evaluate_logistic(A_PH_CC, b = 1.993325, x0 = -2.536739, v = 3e-7)]
  dt.gld.os1[stype=='BG5', value := osi_evaluate_logistic(A_PH_CC, b = 1.271445, x0 = -6.183446, v = 8e-7)]
  
  dt.gld.os2 <- dt[B_LU_CAT == 'grassland' & A_SOM_LOI > 15 & A_SOM_LOI <= 30]
  dt.gld.os2[stype=='BG1', value := osi_evaluate_logistic(A_PH_CC, b = 4.513283, x0 = 3.2138234, v = 0.0787507)]
  dt.gld.os2[stype=='BG2', value := osi_evaluate_logistic(A_PH_CC, b = 2.772481, x0 = -0.8968812, v = 0.0000016)]
  dt.gld.os2[stype=='BG3', value := osi_evaluate_logistic(A_PH_CC, b = 2.525533, x0 = -1.4618408, v = 0.0000007)]
  dt.gld.os2[stype=='BG4', value := osi_evaluate_logistic(A_PH_CC, b = 2.454728, x0 = -1.2590135, v = 0.0000011)]
  dt.gld.os2[stype=='BG5', value := osi_evaluate_logistic(A_PH_CC, b = 3.031534, x0 = 1.0386808, v =  0.0000436)]
   
  dt.gld.os3 <- dt[B_LU_CAT == 'grassland' & A_SOM_LOI > 30]
  dt.gld.os3[,value := osi_evaluate_logistic(A_PH_CC, b = 3.9498965 , x0 = -0.3671917, v = 0.0000002)]
  
  # no recommendation for others
  dt.other <- dt[!B_LU_CAT %in% c('grassland','arable')]
  
  # rbind all OSI scores
  dt.fin <- rbind(dt.bld.os1,
                  dt.bld.os2,
                  dt.bld.os3,
                  dt.bld.os4,
                  dt.bld.os5,
                  dt.gld.os1,
                  dt.gld.os2,
                  dt.gld.os3,
                  dt.other
                  )
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
}

#' Calculate the pH index for France 
#' 
#' This function evaluates the pH index in France
#' 
#' @param B_LU (character) The crop type
#' @param B_TEXTURE_GEPPA (character) The soil texture class in a particular region. 
#' @param A_PH_WA (numeric) The pH measured in H2O
#' 
#' @import data.table
#' 
#' @return 
#' The pH index in France estimated from pH in water, the textural class and the crop type
#' 
#' @export
osi_c_ph_fr <- function(B_LU,B_TEXTURE_GEPPA, A_PH_WA) {
  
  # set visual bindings
  osi_c_ph_fr = osi_country = osi_indicator = id = crop_cat1 = NULL
  cat_crop_ph = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = B_SOILTYPE_AGR = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # Load in the parameter set (to set min and max, to be done later)
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # load in thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='FR' & osi_indicator=='i_c_ph']
  
  # Check length of desired input
  arg.length <- max(length(A_PH_WA),length(B_TEXTURE_GEPPA),length(B_LU))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_subset(B_TEXTURE_GEPPA, choices =c("L","LL","Ls","Lsa","La","LAS","AA","A","As","Als","Al","Sa","Sal","S","SS","Sl"), empty.ok = FALSE)
  checkmate::assert_numeric(A_PH_WA, lower = 0, upper = 14, any.missing = TRUE, len = arg.length)
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_GEPPA = B_TEXTURE_GEPPA,
                   A_PH_WA = A_PH_WA,
                   B_SOILTYPE_AGR = NA_character_,
                   value = NA_real_)
  
  # recategorise soil type to allow merging with internal table
  dt[grepl('^L$|^Lsa$|^LAS$|^La$|^LL$|^Ls$',B_TEXTURE_GEPPA),B_SOILTYPE_AGR := 'loam']
  dt[grepl('^SS$|^Sl$|^S$|^Sal$|^Sa$',B_TEXTURE_GEPPA),B_SOILTYPE_AGR := 'sand']
  dt[is.na(B_SOILTYPE_AGR),B_SOILTYPE_AGR := 'other']
  
  # merge subset on crop class
  dt.sb <- dt[B_LU %in% c('BTN','BVF')][,cat_crop_ph := 'sugar beet']
  dt.sb <- merge(dt.sb,
                 dt.thresholds,
                 by.x = 'cat_crop_ph',
                 by.y = 'osi_threshold_cropcat',
                 all.x = TRUE)
  
  
  # merge subset on soil texture class
  dt.other <- dt[!B_LU %in% c('BTN','BVF')]
  dt.other <- merge(dt.other,
                    dt.thresholds,
                    by.x = 'B_SOILTYPE_AGR',
                    by.y = 'osi_threshold_soilcat',
                    all.x = TRUE)
  
  # combine both again
  dt <- rbind(dt.sb[,.(id,A_PH_WA,osi_st_c1, osi_st_c2, osi_st_c3)],
              dt.other[,.(id,A_PH_WA,osi_st_c1, osi_st_c2, osi_st_c3)])
  
  # convert to the OSI score
  dt[, value := evaluate_logistic(x = A_PH_WA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select and return value
  value <- dt[,value]
  
  return(value)
  
}



#' Calculate the pH index for Finland
#' 
#' This function evaluates the pH index in Finland
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_PH_WA (numeric) The pH values determined via water extract
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_ph_fi(B_LU = 'SOJ', B_TEXTURE_USDA = 'Si',A_PH_WA = 4.5)
#' 
#' @return 
#' The pH index in Finland estimated from pH water. A numeric value.
#' 
#' @export
osi_c_ph_fi <- function(B_LU, B_TEXTURE_USDA, A_PH_WA,A_C_OF = 0) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  B_SOILTYPE_AGR = crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FI']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_ph']
  
  # get the max length of inputs
  arg.length <- max(length(B_LU),length(B_TEXTURE_USDA), length(A_PH_WA),length(A_C_OF))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA = B_TEXTURE_USDA,
                   B_SOILTYPE_AGR = NA_character_,
                   A_PH_WA = A_PH_WA,
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
  dt[,value := osi_evaluate_logistic(x = A_PH_WA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the pH index in Ireland
#' 
#' This function calculates the pH index 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_SOM_LOI (numeric) The organic matter content of soil in percentage
#' @param A_PH_WA (numeric) The pH measured in water
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_ph_ie(B_LU = '265',A_PH_WA = 5, A_SOM_LOI = 3.5)
#' osi_c_ph_ie(B_LU = c('265','1019'),A_PH_WA = c(3.5,5.5),A_SOM_LOI = c(3.5,6.5))
#' 
#' @return 
#' The pH index in Ireland. A numeric value.
#' 
#' @export
osi_c_ph_ie <- function(B_LU, A_PH_WA,A_SOM_LOI) {

  # add visual bindings
  osi_country = . = crop_code = crop_cat1 = crop_name = id = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='IE']
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = as.character(B_LU),
                   A_SOM_LOI = A_SOM_LOI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_name)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # pH evaluation for peat soils
  dt[A_SOM_LOI > 20, value := osi_evaluate_logistic(A_PH_WA, b = 8.2990603 , x0 = 4.8541180 , v = 0.1105874)]
  
  # pH evaluation
  dt[grepl('potato',tolower(crop_name)), 
     value := osi_evaluate_logistic(A_PH_WA, b = 5.364616, x0 = 2.668736, v = 2.112456e-06)]
  dt[grepl('grass',tolower(crop_name)), 
     value := osi_evaluate_logistic(A_PH_WA, b = 3.70689744, x0 = 4.31512440, v = 0.03378904)]
  dt[grepl('cereal|wheat|maiz|corn|tritic|barley',tolower(crop_name)), value := osi_evaluate_logistic(A_PH_WA, b = 3.639040494, x0 = 3.910131651, v = 0.008373893 )]
  dt[grepl('clover|beet|beans|peas|rape',tolower(crop_name)), 
     value := osi_evaluate_logistic(A_PH_WA, b = 2.51825519, x0 = 3.90417201, v = 0.01634118  )]
  dt[is.na(value),
     value := osi_evaluate_logistic(A_PH_WA, b = 2.48634015 , x0 = 3.93503547, v = 0.01855978)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value
  value <- dt[, value]
  
  # return
  return(value)
}

#' Calculate the distance for target for soil pH in view of the BLN production function
#'
#' This functions evaluates the difference between the measured pH and the optimal pH according to the Bemestingsadvies
#'
#' @param ID (character) A field id
#' @param B_LU (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of soil in percentage
#' @param A_CLAY_MI (numeric) The percentage of clay (\%) 
#' @param A_PH_CC (numeric) The pH-CaCl2 of the soil
#'
#' @references Handboek Bodem en Bemesting tabel 5.1, 5.2 en 5.3
#'
#' @import data.table
#' @import OBIC
#'
#' @return
#' The osi indicator for the soil pH
#'
#' @export
osi_c_ph_nl <- function(ID,B_LU, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC) {
  
  # add visual bindings
  D_CP_STARCH = FIELD_ID = D_CP_POTATO = D_CP_SUGARBEET = D_CP_GRASS = D_CP_MAIS = D_CP_OTHER = D_CP_RUST = D_CP_RUSTDEEP = NULL
  D_PH_DELTA = i_c_ph = . = oid = osi_country = NULL
  osi_soil_cat1 = crop_code = B_LU_BRP = NULL
  
  # Check inputs
  arg.length <- max(length(A_PH_CC), length(B_SOILTYPE_AGR), length(A_SOM_LOI), length(A_CLAY_MI),
                    length(B_LU))
  checkmate::assert_numeric(A_PH_CC, lower = 2, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices =euosi::osi_soiltype[osi_country=='NL',osi_soil_cat1])
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(euosi::osi_crops[osi_country=='NL',crop_code]), empty.ok = FALSE)
  
  # Collect information in table
  dt <- data.table(FIELD_ID = ID,
                   oid = 1:arg.length,
                   B_LU_BRP = as.numeric(B_LU),
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC
                   )
  
  # Calculate the crop rotation fraction
  dt[, D_CP_STARCH := OBIC::calc_rotation_fraction(FIELD_ID, B_LU_BRP, crop = "starch")]
  dt[, D_CP_POTATO := OBIC::calc_rotation_fraction(FIELD_ID, B_LU_BRP, crop = "potato")]
  dt[, D_CP_SUGARBEET := OBIC::calc_rotation_fraction(FIELD_ID, B_LU_BRP, crop = "sugarbeet")]
  dt[, D_CP_GRASS := OBIC::calc_rotation_fraction(FIELD_ID, B_LU_BRP, crop = "grass")]
  dt[, D_CP_MAIS := OBIC::calc_rotation_fraction(FIELD_ID, B_LU_BRP, crop = "mais")]
  dt[, D_CP_OTHER := OBIC::calc_rotation_fraction(FIELD_ID, B_LU_BRP, crop = "other")]
  dt[, D_CP_RUST := OBIC::calc_rotation_fraction(FIELD_ID, B_LU_BRP, crop = "rustgewas")]
  dt[, D_CP_RUSTDEEP := OBIC::calc_rotation_fraction(FIELD_ID, B_LU_BRP, crop = "rustgewasdiep")]
  
  # filter on one line per field
  dt[,year := 1:.N,by='FIELD_ID']
  dt2 <- dt[year==1]
  
  # calculate the distance to optimum pH
  dt2[, D_PH_DELTA := OBIC::calc_ph_delta(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI,
                                          A_CLAY_MI, A_PH_CC, D_CP_STARCH,
                                          D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS,
                                          D_CP_MAIS, D_CP_OTHER)]
  
  # merge with original dt
  dt <- merge(dt,dt2[,.(FIELD_ID,D_PH_DELTA)],by = 'FIELD_ID',all.x=TRUE)
  
  # evaluate the distance to target for pH indicator
  dt[, i_c_ph := 1 - osi_evaluate_logistic(x = D_PH_DELTA, b = 9, x0 = 0.3, v = 0.4, increasing = TRUE)]
  
  # sort again
  setorder(dt,oid)
  
  # extract the pH indicator
  value <- dt[,i_c_ph]
  
  # return value
  return(value)
  
}

#' Calculate the pH index in Sweden
#' 
#' This function calculates the pH index 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_SOM_LOI (numeric) The organic matter content of soil in percentage
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_PH_WA (numeric) The pH measured in water
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_ph_se(B_LU = 265,A_PH_WA = 5,A_CLAY_MI = 15, A_SOM_LOI = 2)
#' osi_c_ph_se(B_LU = c(265,1019),A_PH_WA = c(3.5,5.5), A_CLAY_MI = c(5,15),A_SOM_LOI = c(2,4))
#' 
#' @return 
#' The pH index in Sweden. A numeric value.
#' 
#' @export
osi_c_ph_se <- function(B_LU, A_SOM_LOI,A_CLAY_MI,A_PH_WA) {
  
  # add visual bindings
  id = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # get max length of inputs
  arg.length <- max(length(B_LU), length(A_SOM_LOI),length(A_CLAY_MI),length(A_PH_WA))
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI=A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1,crop_name)],
  #             by.x = 'B_LU', 
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # pH evaluation for peat soils
  dt[A_SOM_LOI > 20, value := osi_evaluate_logistic(A_PH_WA, b = 4.538515 , x0 = 1.518938 , v = 1.412605e-07)]
  
  # evaluation for SOM <6%
  dt[A_SOM_LOI <= 6 & A_CLAY_MI <= 5, 
     value := osi_evaluate_logistic(A_PH_WA, b = 8.8363357, x0 = 5.5183950, v = 0.4419161)]
  dt[A_SOM_LOI <= 6 & A_CLAY_MI > 5 & A_CLAY_MI <= 25, 
     value := osi_evaluate_logistic(A_PH_WA, b = 7.052878824, x0 = 5.108970970, v = 0.007202975)]
  dt[A_SOM_LOI <= 6 & A_CLAY_MI > 25, 
     value := osi_evaluate_logistic(A_PH_WA, b = 7.8120295, x0 = 5.9273987, v = 0.3275014)]
  
  # evaluaton for SOM 6-12%
  dt[A_SOM_LOI > 6 & A_SOM_LOI <= 12 & A_CLAY_MI <= 5, 
     value := osi_evaluate_logistic(A_PH_WA, b = 6.929731, x0 = 3.054091, v = 9.544093e-08)]
  dt[A_SOM_LOI > 6 & A_SOM_LOI <= 12 & A_CLAY_MI > 5 & A_CLAY_MI <= 25, 
     value := osi_evaluate_logistic(A_PH_WA, b = 8.8363357, x0 = 5.5183950, v = 0.4419161)]
  dt[A_SOM_LOI > 6 & A_SOM_LOI <= 12 & A_CLAY_MI > 25, 
     value := osi_evaluate_logistic(A_PH_WA, b = 4.122796, x0 = 1.719736, v = 4.590169e-07)]
  
  # evaluaton for SOM 12-20%
  dt[A_SOM_LOI > 6 & A_SOM_LOI <= 12 & A_CLAY_MI <= 5, 
     value := osi_evaluate_logistic(A_PH_WA, b = 9.3631126 , x0 = 5.0327400 , v = 0.3688407)]
  dt[A_SOM_LOI > 6 & A_SOM_LOI <= 12 & A_CLAY_MI > 5 & A_CLAY_MI <= 25, 
     value := osi_evaluate_logistic(A_PH_WA, b = 8.2118736723 , x0 = 4.1043449126 , v = 0.0008085829 )]
  dt[A_SOM_LOI > 6 & A_SOM_LOI <= 12 & A_CLAY_MI > 25, 
     value := osi_evaluate_logistic(A_PH_WA, b = 9.4672526, x0 = 5.1501833 , v = 0.4411129 )]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value
  value <- dt[, value]
  
  # return
  return(value)
}

#' Calculate the pH index in United Kingdom
#' 
#' This function calculates the pH index 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_SOM_LOI (numeric) The organic matter content of soil in percentage
#' @param A_PH_WA (numeric) The pH measured in water
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_ph_uk(B_LU = '265',A_PH_WA = 5, A_SOM_LOI = 4)
#' osi_c_ph_uk(B_LU = c('265','1019'),A_PH_WA = c(3.5,5.5), A_SOM_LOI = c(3.5,4))
#' 
#' @return 
#' The pH index in UK A numeric value.
#' 
#' @export
osi_c_ph_uk <- function(B_LU, A_PH_WA,A_SOM_LOI) {

  # add visual bindings
  crop_code = crop_cat1 = crop_name = id = . = osi_country = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='UK']
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_PH_WA),length(A_SOM_LOI))
  
  # internal data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU = as.character(B_LU),
                   A_SOM_LOI = A_SOM_LOI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_name)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # pH evaluation for peat soils
  dt[A_SOM_LOI > 20 & crop_cat1=='arable' , 
     value := osi_evaluate_logistic(A_PH_WA, b = 9.7710662  , x0 = 5.3773918  , v = 0.5782954)]
  dt[A_SOM_LOI > 20 & crop_cat1=='grassland' , 
     value := osi_evaluate_logistic(A_PH_WA, b = 10.0036781, x0 = 4.8744711, v = 0.4417105)]
  
  # pH evaluation
  dt[crop_cat1=='arable', 
     value := osi_evaluate_logistic(A_PH_WA, b = 7.5257616 , x0 = 5.8837702 , v = 0.2576445)]
  dt[crop_cat1=='grassland', 
     value := osi_evaluate_logistic(A_PH_WA, b = 8.8395672 , x0 = 5.5185430 , v = 0.4422152)]
  
  # generic evaluation
  dt[A_SOM_LOI > 20 & is.na(value), 
     value := osi_evaluate_logistic(A_PH_WA, b = 9.7710662  , x0 = 5.3773918  , v = 0.5782954)]
  dt[A_SOM_LOI <= 20 & is.na(value), 
     value := osi_evaluate_logistic(A_PH_WA, b = 7.5257616 , x0 = 5.8837702 , v = 0.2576445)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value
  value <- dt[, value]
  
  # return
  return(value)
}

