#' Calculate the K excess index (wrapper function)
#' 
#' This function calculates the potassium excess for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_AER_FR (character) An agroeconomic region in France
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_PH_WA (numeric) The acidity of the soil, measured in water
#' @param A_CACO3_IF (numeric) The percentage of carbonated lime (\%) 
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analyzed via Cobalt-hexamine extraction
#' @param A_K_AAA (numeric) The exchangeable K-content of the soil measured via ammonium acetate extraction 
#' @param A_K_AL (numeric) The exchangeable K-content of the soil measured via Ammonium Lactate extracton (mg K/ kg)
#' @param A_K_AN (numeric) The K-content of the soil extracted with ammonium nitrate (mg K /kg)
#' @param A_K_CAL (numeric) The exchangeable K-content of the soil measured via Calcium Ammonium Lactate (mg K/ kg)
#' @param A_K_CC (numeric) The plant available potassium, extracted with 0.01M CaCl2 (mg / kg), 
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_K_DL (numeric) The exchangeable K-content of the soil measured via Double Lactate extraction (mg K/ kg)
#' @param A_K_M3 (numeric) The exchangeable K-content of the soil measured via Mehlich 3 extracton (mg K/ kg)
#' @param A_K_NaAAA (numeric) The K-content of the soil extracted with Morgan's solution, sodium acetate acetic acid (mg/ kg)
#' @param A_K_WA (numeric) The exchangeable K-content of the soil measured via water extracton (mg K/ kg)
#' @param B_COUNTRY (character) The country code
#' @param pwarning (boolean) Option to print a warning rather than error (stop) message for input checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_k(B_LU = '265', B_SOILTYPE_AGR = 'dekzand',A_SOM_LOI = 4, 
#' A_CLAY_MI = 11,A_SAND = 24, A_PH_CC = 5.4, A_CEC_CO = 125, 
#' A_K_AAA=346, A_K_CO_PO = 8.5, A_K_CC = 145,B_COUNTRY = 'NL')
#' 
#' @return
#' The capacity of the soil to supply and buffer potassium, evaluated given an optimum threshold for yield. If the value is exceeding this threshold, then the efficiency of fertilizers decline. A numeric value.
#' 
#' @export
osi_nut_k <- function(B_LU, B_SOILTYPE_AGR = NA_character_,B_AER_FR = NA_character_,
                            A_SOM_LOI = NA_real_, A_C_OF = NA_real_, 
                            A_CLAY_MI = NA_real_,A_SAND_MI = NA_real_,
                            A_PH_CC = NA_real_, A_PH_WA = NA_real_,A_CACO3_IF = NA_real_,
                            A_CEC_CO = NA_real_, 
                            A_K_AAA = NA_real_,A_K_AL = NA_real_,A_K_AN = NA_real_,A_K_CAL = NA_real_,A_K_CC = NA_real_,
                            A_K_CO_PO = NA_real_,A_K_DL = NA_real_,A_K_M3 = NA_real_,A_K_NaAAA = NA_real_,A_K_WA = NA_real_,
                            B_COUNTRY, pwarning = FALSE) {
  
  # add visual bindings
  i_c_k = B_TEXTURE_USDA = B_TEXTURE_HYPRES = B_TEXTURE_BE = B_TEXTURE_GEPPA = A_SILT_MI = NULL
  
  # desired length of inputs
  arg.length <- max(length(B_LU),length(B_SOILTYPE_AGR),length(B_AER_FR),
                    length(A_SOM_LOI),length(A_C_OF),
                    length(A_CLAY_MI),length(A_SAND_MI),
                    length(A_PH_CC), length(A_PH_WA), length(A_CACO3_IF), length(A_CEC_CO),
                    length(A_K_AAA),length(A_K_AL),length(A_K_AN),length(A_K_CAL),
                    length(A_K_CC),length(A_K_CO_PO),length(A_K_DL),length(A_K_M3),
                    length(A_K_NaAAA),length(A_K_WA),length(B_COUNTRY))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_AER_FR = B_AER_FR,
                   B_COUNTRY = B_COUNTRY,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = pmax(0,100 - A_CLAY_MI - A_SAND_MI),
                   A_PH_CC = A_PH_CC,
                   A_PH_WA = A_PH_WA,
                   A_CACO3_IF = A_CACO3_IF,
                   A_CEC_CO = A_CEC_CO,
                   A_K_AAA = A_K_AAA,
                   A_K_AL = A_K_AL,
                   A_K_AN = A_K_AN,
                   A_K_CAL = A_K_CAL,
                   A_K_CC = A_K_CC,
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_DL = A_K_DL,
                   A_K_M3 = A_K_M3,
                   A_K_NaAAA = A_K_NaAAA,
                   A_K_WA = A_K_WA,
                   value = NA_real_
  )
  
  # check required inputs
  osi_checkvar(parm = list(B_COUNTRY = dt$B_COUNTRY, 
                           B_LU = dt$B_LU,
                           B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
                           A_CLAY_MI = dt$A_CLAY_MI,
                           A_SAND_MI = dt$A_SAND_MI,
                           A_SILT_MI = dt$A_SILT_MI,
                           A_SOM_LOI = dt$A_SOM_LOI,
                           A_C_OF = dt$A_C_OF,
                           A_CEC_CO = dt$A_CEC_CO,
                           A_PH_CC = dt$A_PH_CC,
                           A_CACO3_IF = dt$A_CACO3_IF,
                           A_K_AAA = dt$A_K_AAA
                           ),
               fname='osi_nut_k',
               na_allowed = TRUE,
               unitcheck = TRUE,
               pwarning = pwarning)
  
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
  
  # pedotransfer of Helling (1964) when CEC is missing
  dt[is.na(A_CEC_CO), A_CEC_CO := (0.44 * A_PH_WA + 3)* A_CLAY_MI + (5.1 * A_PH_WA - 5.9) * A_C_OF * 0.1]
  
  # estimate extrable soil K pools when not available
  dt[,A_K_AL := osi_conv_potassium(element='A_K_AL',A_K_AAA = A_K_AAA)]
  dt[,A_K_AN := osi_conv_potassium(element='A_K_AN',A_K_AAA = A_K_AAA)]
  dt[,A_K_CAL:= osi_conv_potassium(element='A_K_CAL',A_K_AAA = A_K_AAA)]
  dt[,A_K_CC := osi_conv_potassium(element='A_K_CC',A_K_AAA = A_K_AAA)]
  dt[,A_K_CO_PO := osi_conv_potassium(element='A_K_CO_PO',A_K_AAA = A_K_AAA,A_CEC_CO=A_CEC_CO,A_PH_CC = A_PH_CC)]
  dt[,A_K_DL := osi_conv_potassium(element='A_K_DL',A_K_AAA = A_K_AAA,A_PH_CC = A_PH_CC)]
  dt[,A_K_M3 := osi_conv_potassium(element='A_K_M3',A_K_AAA = A_K_AAA,A_PH_CC = A_PH_CC)]
  dt[,A_K_NaAAA := osi_conv_potassium(element='A_K_NaAAA',A_K_AAA = A_K_AAA)]
  dt[,A_K_WA := osi_conv_potassium(element='A_K_WA',A_K_AAA = A_K_AAA)]
  
  #check required calculated inputs
  osi_checkvar(parm = list(B_TEXTURE_USDA = dt$B_TEXTURE_USDA, 
                           B_TEXTURE_HYPRES = dt$B_TEXTURE_HYPRES,
                           B_TEXTURE_BE = dt$B_TEXTURE_BE,
                           B_TEXTURE_GEPPA = dt$B_TEXTURE_GEPPA,
                           A_PH_WA = dt$A_PH_WA,
                           A_SOM_LOI = dt$A_SOM_LOI,
                           A_C_OF = dt$A_C_OF,
                           A_CEC_CO = dt$A_CEC_CO,
                           A_K_CC = dt$A_K_CC,
                           A_K_CO_PO = dt$A_K_CO_PO,
                           A_K_AL = dt$A_K_AL,
                           A_K_AN = dt$A_K_AN,
                           A_K_CAL = dt$A_K_CAL,
                           A_K_DL = dt$A_K_DL,
                           A_K_M3 = dt$A_K_M3,
                           A_K_NaAAA = dt$A_K_NaAAA,
                           A_K_WA = dt$A_K_WA),
               fname='oci_nut_k',
               unitcheck = TRUE,
               pwarning = pwarning)
  
  # calculate the OSI score per country
  
  # Austria (AT), Belgium (BE), Switzerland (CH), Czech Republic (CZ), Germany (DE)
  dt[B_COUNTRY == 'AT', value := osi_nut_k_at(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,A_K_CAL = A_K_CAL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'BE', value := osi_nut_k_be(B_LU = B_LU, B_TEXTURE_BE = B_TEXTURE_BE,A_K_AAA  = A_K_AAA, unitcheck = FALSE)]
  dt[B_COUNTRY == 'CH', value := osi_nut_k_ch(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_K_AAA = A_K_AAA, unitcheck = FALSE)]
  dt[B_COUNTRY == 'CZ', value := osi_nut_k_cz(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,A_K_M3 = A_K_M3, unitcheck = FALSE)]
  dt[B_COUNTRY == 'DE', value := osi_nut_k_de(B_LU = B_LU, A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_K_CAL = A_K_CAL, unitcheck = FALSE)]
  
  # Denmark (DK), Estonia (EE), Greece (EL), Spain (ES),France (FR), Finland (FI) 
  dt[B_COUNTRY == 'DK', value := osi_nut_k_dk(B_LU = B_LU, A_K_AL = A_K_AL)]
  dt[B_COUNTRY == 'EE', value := osi_nut_k_ee(B_LU = B_LU,B_TEXTURE_USDA = B_TEXTURE_USDA,A_K_M3 = A_K_M3, unitcheck = FALSE)]
  dt[B_COUNTRY == 'EL', value := osi_nut_k_el(B_LU = B_LU, A_K_AAA = A_K_AAA, unitcheck = FALSE)]
  dt[B_COUNTRY == 'ES', value := osi_nut_k_es(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,A_K_AAA = A_K_AAA, unitcheck = FALSE)]
  dt[B_COUNTRY == 'FR', value := osi_nut_k_fr(B_LU = B_LU, B_TEXTURE_GEPPA  = B_TEXTURE_GEPPA, A_PH_WA = A_PH_WA,
                                                    B_AER_FR = B_AER_FR, A_K_AAA = A_K_AAA, unitcheck = FALSE)]
  dt[B_COUNTRY == 'FI', value := osi_nut_k_fi(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_K_AAA = A_K_AAA, A_C_OF = A_C_OF, unitcheck = FALSE)]
  
  # Hungary (HU), Ireland (IE), Italy (IT), Latvia (LV), Lithuania (LT)
  dt[B_COUNTRY == 'HU', value := osi_nut_k_hu(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_CACO3_IF = A_CACO3_IF,A_K_AL = A_K_AL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'IE', value := osi_nut_k_ie(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI,A_K_NaAAA = A_K_NaAAA, unitcheck = FALSE)]
  dt[B_COUNTRY == 'IT', value := osi_nut_k_it(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_K_AAA = A_K_AAA, unitcheck = FALSE)]
  dt[B_COUNTRY == 'LV', value := osi_nut_k_lv(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_K_DL = A_K_DL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'LT', value := osi_nut_k_lt(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_K_AL = A_K_AL, unitcheck = FALSE)]
  
  # the Netherlands (NL), Norway (NO),  Sweden (SE), Slovak Republic (SK), Slovenia (SL)
  dt[B_COUNTRY == 'NL', value := osi_nut_k_nl(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                                    A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,
                                                    A_PH_CC = A_PH_CC, A_CEC_CO = A_CEC_CO, 
                                                    A_K_CO_PO = A_K_CO_PO, A_K_CC = A_K_CC, unitcheck = FALSE)]
  
  dt[B_COUNTRY == 'NO', value := osi_nut_k_no(B_LU = B_LU, A_K_AL = A_K_AL,A_CLAY_MI = A_CLAY_MI, unitcheck = FALSE)]
  dt[B_COUNTRY == 'SE', value := osi_nut_k_se(B_LU = B_LU, A_K_AL = A_K_AL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'SK', value := osi_nut_k_sk(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_K_M3 = A_K_M3, unitcheck = FALSE)]
  dt[B_COUNTRY == 'SL', value := osi_nut_k_sl(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_K_AL = A_K_AL, unitcheck = FALSE)]
  
  # Poland (PL), Portugal (PT) United Kingdom (UK)
  dt[B_COUNTRY == 'PL', value := osi_nut_k_pl(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_K_DL = A_K_DL, unitcheck = FALSE)]
  dt[B_COUNTRY == 'PT', value := osi_nut_k_pt(B_LU = B_LU, A_K_AAA = A_K_AAA, unitcheck = FALSE)]
  dt[B_COUNTRY == 'RO', value := osi_nut_k_ro(B_LU = B_LU, A_K_AL = A_K_AL,B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, unitcheck = FALSE)]
  dt[B_COUNTRY == 'UK', value := osi_nut_k_uk(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_K_AN = A_K_AN, unitcheck = FALSE)]
  
  # select the output variable
  value <- dt[,value]
  
  # return the OSI score
  return(value)
  
}

#' Calculate the potassium excess index in Austria
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_CAL (numeric) The exchangeable K-content of the soil measured via Calcium Ammonium Lactate (mg K/ kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_at(B_LU = '3301000000',A_K_CAL = 47,B_TEXTURE_HYPRES = 'C')
#' 
#' @return 
#' The potassium excess index in Austria estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_nut_k_at <- function(A_K_CAL,B_TEXTURE_HYPRES,B_LU = NA_character_, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='AT']
  
  # get max length of input variables
  arg.length <- max(length(A_K_CAL),length(B_TEXTURE_HYPRES),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('AT',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                           A_K_CAL = A_K_CAL),
               fname = 'oci_nut_k_at',
               unitcheck = unitcheck)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_K_CAL = A_K_CAL,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)

  # calculate the OSI score light textured soils
  dt[B_TEXTURE_HYPRES %in% c('C'),
     value := osi_evaluate_logistic(x = A_K_CAL, b = -0.01212, x0 = 300.44, v = 0.6033)]
  # calculate the OSI score medium texture soils
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_K_CAL, b = -0.01375, x0 = 238.37, v = 1.78)]
  # calculate the OSI score for heavy textured soils
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_CAL, b = -0.0115, x0 = 286.2, v = 1.796)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium excess index in Belgium
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_BE (character) The soil texture according to Belgium classification system
#' @param A_K_AAA (numeric) The exchangeable K-content of the soil measured via ammonium acetate extraction
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_be(B_LU = '8410',B_TEXTURE_BE ='S', A_K_AAA = 45)
#' 
#' @return 
#' The potassium excess index in Belgium estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_nut_k_be <- function(B_LU, B_TEXTURE_BE, A_K_AAA = NA_real_, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = crop_k = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  B_SOILTYPE_AGR = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='BE']
 
  # get max length of input arguments
  arg.length <- max(length(B_LU),length(B_TEXTURE_BE),length(A_K_AAA))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('BE',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_BE = B_TEXTURE_BE,
                           A_K_AAA = A_K_AAA),
               fname = 'oci_nut_k_be',
               unitcheck = unitcheck)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_BE = B_TEXTURE_BE,
                   A_K_AAA = A_K_AAA,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = A_K_AAA, b = -0.01082, x0 = 301.78, v = 1.7673)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium excess index in Switzerland
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_K_AAA (numeric) The exchangeable K-content of the soil measured via acid ammonium acetate extraction (mg K/ kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_ch(B_LU = '8410',A_K_AAA = 50, A_CLAY_MI = 4.5)
#' 
#' @return 
#' The potassium excess index in Switzerland estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_nut_k_ch <- function(A_K_AAA,A_CLAY_MI,B_LU = NA_character_, unitcheck = TRUE) {
  
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
  arg.length <- max(length(A_K_AAA),length(A_CLAY_MI),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(A_CLAY_MI = A_CLAY_MI,
                           A_K_AAA = A_K_AAA),
               fname = 'osi_nut_k_ch',
               unitcheck = unitcheck)
  
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
  dt[A_CLAY_MI <= 10, value := osi_evaluate_logistic(x = A_K_AAA, b = -0.00839, x0 = 372.26, v = 1.5966)]
  dt[A_CLAY_MI > 10 & A_CLAY_MI <= 20, value := osi_evaluate_logistic(x = A_K_AAA, b = -0.00921, x0 = 336.12, v = 1.5588)]
  dt[A_CLAY_MI > 20 & A_CLAY_MI <= 30, value := osi_evaluate_logistic(x = A_K_AAA, b = -0.01134, x0 = 271.69, v = 1.5407)]
  dt[A_CLAY_MI > 30 & A_CLAY_MI <= 40, value := osi_evaluate_logistic(x = A_K_AAA, b = -0.01263, x0 = 242.27, v = 1.5093)]
  dt[A_CLAY_MI > 40, value := osi_evaluate_logistic(x = A_K_AAA, b = -0.02664, x0 = 115.49, v = 1.5325)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium excess index in Czech Republic
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_M3 (numeric) The exchangeable K-content of the soil measured via Mehlich 3 extracton (mg K/ kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_cz(B_LU = '3301000000', A_K_M3 = 81, B_TEXTURE_HYPRES='C')
#' 
#' @return 
#' The potassium excess index in Czech Republic estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_nut_k_cz <- function(A_K_M3,B_TEXTURE_HYPRES,B_LU = NA_character_, unitcheck = TRUE) {
  
  # # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='CZ']
  
  # get max length of input variables
  arg.length <- max(length(B_LU),length(B_TEXTURE_HYPRES),length(A_K_M3))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('CZ',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                           A_K_M3 = A_K_M3),
               fname = 'osi_nut_k_cz',
               unitcheck = unitcheck)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES=B_TEXTURE_HYPRES,
                   A_K_M3 = A_K_M3,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # convert to the OSI score
  dt[B_TEXTURE_HYPRES %in% c('C'),
     value := osi_evaluate_logistic(x = A_K_M3, b = -0.00982, x0 = 335.84, v = 1.8029)]
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_K_M3, b = -0.00925, x0 = 356.48, v = 1.7986)]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_M3, b = -0.10697, x0 = 224.35, v = 58.0264)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium excess index in Germany
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_C_OF (numeric) The carbon content of the soil layer (g/ kg)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_K_CAL (numeric) The potassium content extracted with calcium ammonium lactate (g / kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @return 
#' The potassium excess index in Germany derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_nut_k_de <- function(B_LU, A_C_OF, A_CLAY_MI,A_SAND_MI, A_K_CAL, unitcheck = TRUE) {
  
  # add visual bindings
  A_SILT_MI = A_K_CAL2 = stype = B_LU_CAT = NULL
  crop_code = crop_cat1 = osi_country = . = id = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='DE']
  
  # get max length of input variables
  arg.length <- max(length(B_LU),length(A_C_OF),length(A_CLAY_MI),length(A_SAND_MI),length(A_K_CAL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('DE',arg.length),
                           B_LU = B_LU,
                           A_C_OF = A_C_OF,
                           A_CLAY_MI = A_CLAY_MI,
                           A_SAND_MI = A_SAND_MI,
                           A_K_CAL = A_K_CAL),
               fname = 'osi_nut_k_de',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_C_OF= A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_K_CAL = A_K_CAL* 0.1, # in mg K/100g,
                   A_K_CAL2 = A_K_CAL * ((1 / (0.02525 * (A_C_OF*2*0.1) + 0.6541))/10), #in mg K/100ml
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # add soil type
  dt[A_SAND_MI >= 85 & A_SILT_MI <= 25 & A_CLAY_MI <= 5 & A_C_OF < 150, stype := "BG1"]
  dt[A_SAND_MI >= 42 & A_SAND_MI <= 95 & A_SILT_MI <= 40 & A_CLAY_MI <= 17 & A_C_OF < 150,  stype :="BG2"]
  dt[A_SAND_MI >= 33 & A_SAND_MI <= 83 & A_SILT_MI <= 50 & A_CLAY_MI >= 8 & A_CLAY_MI <= 25 & A_C_OF < 150,  stype :="BG3"]
  dt[A_SAND_MI <= 75 & A_SILT_MI <= 100 & A_CLAY_MI <= 35 & A_C_OF < 150,  stype :="BG4"]
  dt[A_SAND_MI <= 65 & A_SILT_MI <= 75 & A_CLAY_MI >= 25 & A_CLAY_MI <= 100 & A_C_OF < 150, stype := "BG5"]
  dt[ A_C_OF >= 150,  stype := "BG6"]
  
  # evaluate A_K_AAA for arable soils
  dt[stype=='BG1' & crop_cat1 %in% c('arable','maize','permanent'), value := osi_evaluate_logistic(A_K_CAL, b = -0.10391, x0 = 31.43, v = 1.7684)]
  dt[stype=='BG2' & crop_cat1 %in% c('arable','maize','permanent'), value := osi_evaluate_logistic(A_K_CAL, b = -0.08953, x0 = 36.28, v = 1.7474)]
  dt[stype=='BG3' & crop_cat1 %in% c('arable','maize','permanent'), value := osi_evaluate_logistic(A_K_CAL, b = -0.07638, x0 = 42.55, v = 1.7511)]
  dt[stype=='BG4' & crop_cat1 %in% c('arable','maize','permanent'), value := osi_evaluate_logistic(A_K_CAL, b = -0.07024, x0 = 46.9, v = 1.7988)]
  dt[stype=='BG5' & crop_cat1 %in% c('arable','maize','permanent'), value := osi_evaluate_logistic(A_K_CAL, b = -0.05202, x0 = 63.81, v = 1.8263)]
  dt[stype=='BG6' & crop_cat1 %in% c('arable','maize','permanent'), value := osi_evaluate_logistic(A_K_CAL2, b = -0.07217, x0 = 45.45, v = 1.7838)]
  
  # evaluate A_K_AAA for grassland soils
  dt[stype=='BG1' & crop_cat1 %in% c('grassland'), value := osi_evaluate_logistic(A_K_CAL, b = -0.19519, x0 = 17.78, v = 0.7916)]
  dt[stype=='BG2' & crop_cat1 %in% c('grassland'), value := osi_evaluate_logistic(A_K_CAL, b = -0.21371, x0 = 43.48, v = 0.0062)]
  dt[stype=='BG3' & crop_cat1 %in% c('grassland'), value := osi_evaluate_logistic(A_K_CAL, b = -0.16233, x0 = 45.88, v = 0.0296)]
  dt[stype=='BG4' & crop_cat1 %in% c('grassland'), value := osi_evaluate_logistic(A_K_CAL, b = -0.1343, x0 = 28.03, v = 2.2603)]
  dt[stype=='BG5' & crop_cat1 %in% c('grassland'), value := osi_evaluate_logistic(A_K_CAL, b = -0.12803, x0 = 29.48, v = 2.269)]
  dt[stype=='BG6' & crop_cat1 %in% c('grassland'), value := osi_evaluate_logistic(A_K_CAL2, b = -0.1343, x0 = 28.03, v = 2.2603)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value and return
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the potassium excess index in Denmark
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_K_AL (numeric) The K-content of the soil extracted with ammonium lactate (mg K / kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_dk(B_LU = '3301000000',A_K_AL = 5)
#' osi_nut_k_dk(B_LU = c('3301000000','3301061299'),A_K_AL = c(3.5,5.5))
#' 
#' @return 
#' The potassium excess index in Denmark derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_nut_k_dk <- function(B_LU, A_K_AL, unitcheck = TRUE) {
  
  # add visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='DK']
  
  # length of arguments
  arg.length <- max(length(B_LU),length(A_K_AL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('DK',arg.length),
                           B_LU = B_LU,
                           A_K_AL = A_K_AL),
               fname = 'osi_nut_k_dk',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_K_AL = A_K_AL,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # evaluation soil K status, only threshold at optimum level is given (IFS, Ristimaki et al. (2007))
  dt[, value := OBIC::evaluate_logistic(A_K_AL, b = -0.01074, x0 = 293.55, v = 1.6325)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value 
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the potassium excess index in Estonia
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param A_K_M3 (numeric) The exchangeable K-content of the soil measured via Mehlich 3 extracton (mg P/ kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_ee(B_LU = 'testcrop',A_K_M3 = 45,B_TEXTURE_USDA = 'clay')
#' 
#' @return 
#' The potassium excess index in Estonia estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_nut_k_ee <- function(A_K_M3,B_TEXTURE_USDA,B_LU = NA_character_, unitcheck = TRUE) {
  
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
  arg.length <- max(length(A_K_M3),length(B_TEXTURE_USDA),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_TEXTURE_USDA = B_TEXTURE_USDA,
                           A_K_M3 = A_K_M3),
               fname = 'osi_nut_k_ee',
               unitcheck = unitcheck)
  
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
  dt[B_TEXTURE_USDA == 'Sa',value := osi_evaluate_logistic(x = A_K_M3, b = -0.02363, x0 = 139.21, v = 1.7949)]
  dt[B_TEXTURE_USDA %in% c('LoSa'),value := osi_evaluate_logistic(x = A_K_M3, b = -0.01573, x0 = 205.59, v = 1.7329)]
  dt[B_TEXTURE_USDA %in% c('SaLo'),value := osi_evaluate_logistic(x = A_K_M3, b = -0.0146, x0 = 225.33, v = 1.7932)]
  dt[B_TEXTURE_USDA %in% c('Lo','SaCl','SaCL'),
     value := osi_evaluate_logistic(x = A_K_M3, b = -0.01124, x0 = 289.69, v = 1.7577)]
  dt[B_TEXTURE_USDA %in% c('Cl','ClLo','SiCL','SiClLo','SiLo','Si','SiCl'),
     value := osi_evaluate_logistic(x = A_K_M3, b = -0.00836, x0 = 397.81, v = 1.8353)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium excess index in Greece
#' 
#' This function calculates the potassium excess index. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_K_AAA (numeric) The K-content of the soil extracted with ammonium acetate (mg K/kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_el(B_LU = '3301061299',A_K_AAA = 50)
#' osi_nut_k_el(B_LU = c('3301061299','3301000000'),A_K_AAA = c(50,150))
#' 
#' @return 
#' The potassium excess index in Greece derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_nut_k_el <- function(B_LU, A_K_AAA, unitcheck = TRUE) {
  
  # add visual bindings
  osi_country = crop_code = crop_cat1 = . = id =NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='EL']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_K_AAA))
  
  # check inputs (crop code is not yet in osi_crops, so no check)
  osi_checkvar(parm = list(A_K_AAA = A_K_AAA),
               fname = 'osi_nut_k_el',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_K_AAA = A_K_AAA,
                   value = NA_real_)
  
  # merge crop properties
  # dt <- merge(dt,
  #             dt.crops[,.(crop_code,crop_cat1)],
  #             by.x = 'B_LU',
  #             by.y = 'crop_code',
  #             all.x=TRUE)
  
  # evaluation soil K status for grasslands and croplands
  # source ChatGPT, PhD thesis from https://ikee.lib.auth.gr/record/292420/files/GRI-2017-19713.pdf
  dt[, value := osi_evaluate_logistic(A_K_AAA, b = -0.00632, x0 = 526.88, v = 1.8346)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the potassium excess index in Spain
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_AAA (numeric) The K-content of the soil extracted with ammoninium acetate (mg K/kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_es(B_LU = '3301000000',A_K_AAA = 5,B_TEXTURE_HYPRES='C')
#' 
#' @return 
#' The potassium excess index in Spain derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_nut_k_es <- function(B_LU, B_TEXTURE_HYPRES,A_K_AAA, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='ES']
  
  # get max length of input variables
  arg.length <- max(length(A_K_AAA),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('ES',arg.length),
                           B_LU = B_LU,
                           A_K_AAA = A_K_AAA),
               fname = 'osi_nut_k_es',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_K_AAA = A_K_AAA,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # franco = loam (M) ; arena = sand (C); silt = limoso (MF); arcilla = clay (F to VF)
  
  # evaluate the OSI score for arenose
  dt[B_TEXTURE_HYPRES %in% c('C'),
     value := osi_evaluate_logistic(x = A_K_AAA, b = -0.0085, x0 = 367.44, v = 1.5965)]
  # evaluate the OSI score for franco
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_K_AAA, b = -0.00621, x0 = 500.78, v = 1.5808)]
  # evaluate the OSI score for arcilloso
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_AAA, b = -0.00491, x0 = 633.26, v = 1.5787)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the potassium excess index in Finland
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_K_AAA (numeric) The exchangeable K-content of the soil measured via ammonium acetate extraction
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_fi(B_LU = '1110', B_TEXTURE_USDA = 'Si',A_K_AAA = 45,A_C_OF = 15)
#' 
#' @return 
#' The potassium excess index in Finland estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_nut_k_fi <- function(B_LU, B_TEXTURE_USDA, A_K_AAA,A_C_OF = 0.5, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = B_SOILTYPE_AGR = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = osi_threshold_region = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FI']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_k']
  
  # get max length of input
  arg.length <- max(length(B_LU),length(B_TEXTURE_USDA),length(A_K_AAA),length(A_C_OF))
  
  # repeat A_C_OF if only one default is given
  if(length(A_C_OF)==1 & arg.length > 1){A_C_OF <- rep(A_C_OF,arg.length)}
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('FI',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_USDA = B_TEXTURE_USDA,
                           A_C_OF = A_C_OF,
                           A_K_AAA = A_K_AAA),
               fname = 'osi_nut_k_fi',
               unitcheck = unitcheck)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA = B_TEXTURE_USDA,
                   B_SOILTYPE_AGR = NA_character_,
                   A_K_AAA = A_K_AAA,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # set agricultural soiltype
  dt[A_C_OF > 200, B_SOILTYPE_AGR := 'organic']
  dt[grepl('^Cl$|^SiCL$|^SaCL$|^SiCl$',B_TEXTURE_USDA), B_SOILTYPE_AGR := 'clay']  
  dt[grepl('^ClLo$|^SiClLo$|^Lo$|^SiLo$|^LoSa$|^Si$|^SaClLo$',B_TEXTURE_USDA), B_SOILTYPE_AGR := 'loam']
  dt[is.na(B_SOILTYPE_AGR),B_SOILTYPE_AGR := 'sand']
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = 'B_SOILTYPE_AGR',
              by.y = 'osi_threshold_soilcat',
              all.x = TRUE)
  
  # convert to the OSI score
  dt[B_SOILTYPE_AGR=='clay',value := osi_evaluate_logistic(x = A_K_AAA,b = -0.00573, x0 = 552.32, v = 1.6538)]
  dt[B_SOILTYPE_AGR=='loam',value := osi_evaluate_logistic(x = A_K_AAA,b = -0.01811, x0 = 159.28, v = 6.5885)]
  dt[B_SOILTYPE_AGR=='sand',value := osi_evaluate_logistic(x = A_K_AAA,b = -0.01139, x0 = 277.91, v = 1.6516)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium excess index in France
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_GEPPA (character) The soil texture class in a particular region. 
#' @param B_SOILTYPE_AGR (character) The agricultural soil type classification
#' @param B_AER_FR (character) An agroeconomic region in France. Optional argument.
#' @param A_K_AAA (numeric) The exchangeable K-content of the soil measured via ammonium acetate extraction
#' @param A_PH_WA (numeric) The pH measured in water.
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_fr(B_LU = 'SOJ', A_K_AAA = 45,B_TEXTURE_GEPPA ='As',
#' B_SOILTYPE_AGR = 'limons battants', B_AER_FR = 'nord picardie')
#' 
#' @details
#' The function has two optional arguments soil type (B_SOILTYPE_AGR) and agricultural region (B_AER_FR). When these are unknown, then the soil type is estimated based on the pH value. Threshold values are then generalized for calcareous and non-calcareous soils.
#' 
#' @return 
#' The potassium excess index in France estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_nut_k_fr <- function(B_LU, A_K_AAA, B_TEXTURE_GEPPA = NA_character_, B_SOILTYPE_AGR = NA_character_, 
                         B_AER_FR = NA_character_, A_PH_WA = NA_real_, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = osi_threshold_cropcat = osi_threshold_region = NULL
  crop_code = crop_k = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FR']

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
  arg.length <- max(length(B_LU),length(B_TEXTURE_GEPPA),length(B_SOILTYPE_AGR),
                    length(B_AER_FR),length(A_K_AAA))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('FR',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_GEPPA = B_TEXTURE_GEPPA,
                           A_K_AAA = A_K_AAA),
               fname = 'osi_nut_k_fr',
               unitcheck = unitcheck)
  
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
                   B_TEXTURE_GEPPA = B_TEXTURE_GEPPA,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_AER_FR = B_AER_FR,
                   A_K_AAA = A_K_AAA,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_k)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # merge thresholds
  if(sum(is.na(B_AER_FR))>0){scol <- 'B_TEXTURE_GEPPA'} else {scol <- 'B_SOILTYPE_AGR'}
  dt <- merge(dt,
              dt.thresholds,
              by.x = c(scol,'B_AER_FR', 'crop_k'),
              by.y = c('osi_threshold_soilcat','osi_threshold_region','osi_threshold_cropcat'),
              all.x = TRUE)
  
  # convert to the OSI score (to be update for each individual optie)
  dt[crop_k =='moderate',value := osi_evaluate_logistic(x = A_K_AAA,b = -0.02221, x0 = 148.42, v = 1.8025 )]
  dt[crop_k =='high',value := osi_evaluate_logistic(x = A_K_AAA, b = -0.01012, x0 = 300.95, v = 0.8066)]
  dt[crop_k =='low',value := osi_evaluate_logistic(x = A_K_AAA,b = -0.02602, x0 = 112.04, v = 1.1065 )]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}


#' Calculate the potassium excess index in Hungary
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (character) The crop code
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_CACO3_IF (numeric) the percentage of CaCO3 (\%)
#' @param A_K_AL (numeric) The exchangeable K-content of the soil measured via Ammonium Lactate extracton (mg K/ kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_hu(B_LU = 'testcrop',A_K_AL = 45,A_CACO3_IF = 5,A_CLAY_MI = 5,A_SOM_LOI = 5)
#' 
#' @return 
#' The potassium excess index in Hungary estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_nut_k_hu <- function(A_SOM_LOI,A_CLAY_MI,A_CACO3_IF,A_K_AL,B_LU = NA_character_, unitcheck = TRUE) {
  
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
  arg.length <- max(length(A_K_AL),length(A_SOM_LOI),length(A_CLAY_MI),length(A_CACO3_IF),length(B_LU))
  
  # check inputs (no check on B_LU since crops codes are not in osi_crop)
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,
                           A_CLAY_MI = A_CLAY_MI,
                           A_CACO3_IF = A_CACO3_IF,
                           A_K_AL = A_K_AL),
               fname = 'osi_nut_k_hu',
               unitcheck = unitcheck)
  
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
     value := osi_evaluate_logistic(x = A_K_AL, b = -0.0117, x0 = 280.48, v = 1.785 )]
  dt[A_SOM_LOI > 4 & A_CACO3_IF > 1,
     value := osi_evaluate_logistic(x = A_K_AL, b = -0.00977, x0 = 337.5, v = 1.8026 )]
  
  # derive the OSI score for brown forest soil (proxied by clay)
  dt[A_CLAY_MI > 20 & A_SOM_LOI <= 4 & A_CACO3_IF <= 1,
     value := osi_evaluate_logistic(x = A_K_AL, b = -0.0115, x0 = 300.93, v = 0.8736)]
  dt[A_CLAY_MI > 20 & A_SOM_LOI <= 4 & A_CACO3_IF > 1,
     value := osi_evaluate_logistic(x = A_K_AL, b = -0.01121, x0 = 294.66, v = 1.8132)]
  
  # derive the OSI score for sandy soil (proxied by clay)
  dt[A_CLAY_MI <= 20 & A_SOM_LOI <= 4 & A_CACO3_IF <= 1,
     value := osi_evaluate_logistic(x = A_K_AL, b = -0.01883, x0 = 174.03, v = 1.781)]
  dt[A_CLAY_MI <= 20 & A_SOM_LOI <= 4 & A_CACO3_IF > 1,
     value := osi_evaluate_logistic(x = A_K_AL, b = -0.01467, x0 = 231.89, v = 1.9134)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium excess index in Ireland
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_K_NaAAA (numeric) The K-content of the soil extracted with Morgan's solution, sodium acetate acetic acid (mg/ kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_ie(B_LU = 'testcrop',A_SOM_LOI = 2,A_K_NaAAA = 5)
#' osi_nut_k_ie(B_LU = c('testcrop','testcrop2'),A_SOM_LOI = c(2,4),A_K_NaAAA = c(3.5,5.5))
#' 
#' @return 
#' The potassium excess index in Ireland derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_nut_k_ie <- function(B_LU, A_SOM_LOI,A_K_NaAAA, unitcheck = TRUE) {
  
  # add visual bindings
  BDS = id = NULL
  
  # check inputs
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,
                           A_K_NaAAA = A_K_NaAAA),
               fname = 'osi_nut_k_ie',
               unitcheck = unitcheck)
  
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
  dt[A_SOM_LOI <= 20, value := osi_evaluate_logistic(A_K_NaAAA, b = -0.01106, x0 = 285.18, v = 1.637)]
  dt[A_SOM_LOI > 20, value := osi_evaluate_logistic(A_K_NaAAA, b = -0.00578, x0 = 540.88, v = 1.6)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the potassium excess index in Italy
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_AAA (numeric) The K-content of the soil extracted with ammoninium acetate (mg K/kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_it(B_LU = '3301000000',A_K_AAA = 5,B_TEXTURE_HYPRES='C')
#' osi_nut_k_it(B_LU = c('3301000000','3301010101'),
#' A_K_AAA = c(3.5,5.5),B_TEXTURE_HYPRES=c('C','C'))
#' 
#' @return 
#' The potassium excess index in Italy derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_nut_k_it <- function(B_LU, B_TEXTURE_HYPRES,A_K_AAA, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = . = crop_code = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='IT']
  
  # get max length of input data
  arg.length <- max(length(B_LU),length(B_TEXTURE_HYPRES),length(A_K_AAA))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('IT',arg.length),
                           B_LU = B_LU,
                           A_K_AAA = A_K_AAA),
               fname = 'osi_nut_k_it',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_K_AAA = A_K_AAA,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # franco = loam (M) ; arena = sand (C); silt = limoso (MF); arcilla = clay (F to VF)
  
  # evaluate the OSI score for terreni sabbiosi
  dt[B_TEXTURE_HYPRES %in% c('C'),
     value := osi_evaluate_logistic(x = A_K_AAA, b = -0.01624, x0 = 203.08, v = 1.8034)]
  # evaluate the OSI score for terreni medio impasto
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_K_AAA, b = -0.01423, x0 = 235.34, v = 1.8587 )]
  # evaluate the OSI score for terreni argillosi e limos
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_AAA, b = -0.00977, x0 = 332.32, v = 1.7515 )]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the potassium excess index in Latvia
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (character) The crop code
#' @param A_K_DL (numeric) The exchangeable K-content of the soil measured via Double Lactate extraction (mg K/ kg)
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_lv(B_LU = 'testcrop',A_K_DL = 45,B_TEXTURE_USDA='Sa')
#' 
#' @return 
#' The potassium excess index in Latvia estimated from extractable potassium A numeric value.
#' 
#' @export
osi_nut_k_lv <- function(A_K_DL,B_TEXTURE_USDA, B_LU = NA_character_, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  #crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # get max length of input variables
  arg.length <- max(length(A_K_DL),length(B_TEXTURE_USDA),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_TEXTURE_USDA = B_TEXTURE_USDA,
                           A_K_DL = A_K_DL),
               fname = 'osi_nut_k_lv',
               unitcheck = unitcheck)
  
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
  dt[B_TEXTURE_USDA == 'Sa',
     value := osi_evaluate_logistic(x = A_K_DL, -0.02732, x0 = 117.62, v = 1.7064)]
  dt[B_TEXTURE_USDA %in% c('LoSa','SaLo'),
     value := osi_evaluate_logistic(x = A_K_DL, b = -0.02077, x0 = 154.34, v = 1.6996)]
  dt[B_TEXTURE_USDA %in% c('Lo','SaCl','SaCL'),
     value := osi_evaluate_logistic(x = A_K_DL, b = -0.01828, x0 = 175.18, v = 1.6953)]
  dt[B_TEXTURE_USDA %in% c('Cl','ClLo', 'SiCL','SiClLo', 'SiLo','Si','SiCl'),
     value := osi_evaluate_logistic(x = A_K_DL,b = -0.01675, x0 = 192.05, v = 1.7127)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium excess index in Lithuania
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (character) The crop code
#' @param A_K_AL (numeric) The exchangeable K-content of the soil measured via Ammonium Lactate extraction (mg P/ kg)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_lt(A_K_AL = 45,A_SOM_LOI= 1.5)
#' 
#' @return 
#' The potassium excess index in Lithuania estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_nut_k_lt <- function(A_K_AL,A_SOM_LOI,B_LU = NA_character_, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # get max length of input variables
  arg.length <- max(length(A_K_AL),length(A_SOM_LOI),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,
                           A_K_AL = A_K_AL),
               fname = 'osi_nut_k_lt',
               unitcheck = unitcheck)
  
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
  dt[A_SOM_LOI <= 20,value := osi_evaluate_logistic(x = A_K_AL, b = -0.01612, x0 = 198.9, v = 1.6999)]
  dt[A_SOM_LOI > 20,value := osi_evaluate_logistic(x = A_K_AL, b = -0.00912, x0 = 359.16, v = 1.7785)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}



#' Calculate the K excess index for the Netherlands 
#' 
#' This function calculates the K excess of a soil, using the agronomic index used in the Netherlands.
#' 
#' @param B_LU (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analyzed via Cobalt-hexamine extraction
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_K_CC (numeric) The plant available potassium, extracted with 0.01M CaCl2 (mg / kg), 
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_nl(B_LU = '265', B_SOILTYPE_AGR = 'dekzand',
#' A_SOM_LOI = 4, A_CLAY_MI = 11,A_PH_CC = 5.4, A_CEC_CO = 125, 
#' A_K_CO_PO = 8.5, A_K_CC = 145)
#' osi_nut_k_nl('265', 'dekzand',4, 11,5.4,  125,8.5, 145)
#' osi_nut_k_nl(c('265','1019'), rep('dekzand',2),c(4,6), c(11,14),
#' c(5.4,5.6),  c(125,145),c(8.5,3.5), c(145,180))
#' 
#' @return
#' The capacity of the soil to supply and buffer potassium, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_nut_k_nl <- function(B_LU, B_SOILTYPE_AGR,A_SOM_LOI, A_CLAY_MI,A_PH_CC, 
                               A_CEC_CO, A_K_CO_PO, A_K_CC, unitcheck = TRUE) {
  
  # add visual bindings
  id = crop_category = soiltype.n = crop_code = soiltype = NULL
  b = cF = kindex1 = kindex2 = A_PH_KCL = A_K_CO = NULL
  osi_country = osi_indicator = crop_cat1 = osi_soil_cat1 = osi_soil_cat2 = value = NULL
  osi_threshold_cropcat = osi_threshold_soilcat = i_c_k = osi_st_c1 = osi_st_c2 = osi_st_c3 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'NL']
  dt.crops[, crop_code := as.character(crop_code)]
  
  dt.soils <- as.data.table(euosi::osi_soiltype)
  dt.soils <- dt.soils[osi_country == 'NL']
  
  # Load in the thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'NL' & osi_indicator == 'i_c_k']
  checkmate::assert_data_table(dt.thresholds,max.rows = 6, min.rows = 6)
  
  # convert B_LU to integer
  B_LU <- as.character(B_LU)
  
  # Check inputs
  arg.length <- max(length(A_PH_CC), length(A_SOM_LOI), length(A_CEC_CO), length(A_K_CO_PO), 
                    length(A_K_CC), length(A_CLAY_MI), length(B_SOILTYPE_AGR), length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('NL',arg.length),
                           B_LU = B_LU,
                           B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                           A_CLAY_MI = A_CLAY_MI,
                           A_SOM_LOI = A_SOM_LOI,
                           A_K_CC = A_K_CC,
                           A_PH_CC = A_PH_CC,
                           A_CEC_CO = A_CEC_CO,
                           A_K_CO_PO = A_K_CO_PO),
               fname = 'osi_nut_k_nl',
               unitcheck = unitcheck)
  
  
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
                   value = NA_real_)
  
  # merge with crop and soil classification tables
  dt <- merge(dt, dt.crops[, list(crop_code, crop_cat1)], 
              by.x = "B_LU", by.y = "crop_code", all.x = TRUE)
  dt <- merge(dt, dt.soils[, list(osi_soil_cat1, osi_soil_cat2)], 
              by.x = "B_SOILTYPE_AGR", by.y = "osi_soil_cat1",all.x = TRUE)
  
  # Calculate the K excess for grassland (CBGV, 2019)
  dt.grass <- dt[crop_cat1 == 'grassland']
  
  # add K-index where CEC is maximized at 400 mmol+ / kg
  dt.grass[A_CEC_CO > 400, A_CEC_CO := 400]
  dt.grass[,value := 4 - exp(-0.08551 * A_K_CC + 0.5264 * log(A_K_CC) - 0.001607 * A_CEC_CO + 
                               0.1275 * log(A_CEC_CO) + 0.010836 * A_K_CC * log(A_CEC_CO))]
  
  # Calculate the K excess for maize (CBGV, 2019)
  dt.maize <- dt[crop_cat1 == 'maize']
  dt.maize[,value := (1 - (120 - A_K_CC) / 120) * 2.5]
  
  # Calculate the K excess for arable crops (Ros & Bussink, 2011)
  dt.arable <- dt[crop_cat1 %in% c('arable','cropland','permanent')]
  
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
  
  # Calculate the K excess for nature
  dt.nature <- dt[crop_cat1 %in% c('nature','forest','other')]
  dt.nature[,value := NA_real_]
  
  # score the K index given threshold for agronomic production / product quality
  
  # subset
  dths <- dt.thresholds[osi_threshold_cropcat == 'grassland']
  
  # evaluate grassland
  dt.grass[, i_c_k := osi_evaluate_logistic(value, b = -0.91486, x0 = 4.36, v = 2.454)]
  
  # subset and evaluate for maize
  dths <- dt.thresholds[osi_threshold_cropcat == 'maize']
  dt.maize[, i_c_k := osi_evaluate_logistic(value, b = -0.91486, x0 = 4.36, v = 2.454)]
  
  # subset and evaluate for arable sandy soils
  dths <- dt.thresholds[osi_threshold_cropcat == 'arable' & osi_threshold_soilcat == 'sand']
  dt.arable[grepl('zand|dal',B_SOILTYPE_AGR), i_c_k := osi_evaluate_logistic(value,b = -0.08125, x0 = 38.74, v = 1.6288)]
  
  # subset and evaluate for arable peat soils
  dths <- dt.thresholds[osi_threshold_cropcat == 'arable' & osi_threshold_soilcat == 'peat']
  dt.arable[grepl('veen',B_SOILTYPE_AGR), i_c_k := osi_evaluate_logistic(value, b = -0.08125, x0 = 38.74, v = 1.6288)]
  
  # subset and evaluate for arable clay soils
  dths <- dt.thresholds[osi_threshold_cropcat == 'arable' & osi_threshold_soilcat == 'clay']
  dt.arable[grepl('klei',B_SOILTYPE_AGR), i_c_k := osi_evaluate_logistic(value, b = -0.09995, x0 = 32.87, v = 1.7907)]
  
  # subset and evaluate for arable loess soils
  dths <- dt.thresholds[osi_threshold_cropcat == 'arable' & osi_threshold_soilcat == 'loess']
  dt.arable[grepl('loess',B_SOILTYPE_AGR), i_c_k := osi_evaluate_logistic(value, b = -0.09995, x0 = 32.87, v = 1.7907)]
  
  # evaluate nature soils
  dt.nature[, i_c_k := NA_real_]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.arable,dt.grass, dt.maize,dt.nature), fill = TRUE)
  setorder(dt, id)
  
  # select the output variable
  out <- dt[,i_c_k]
  
  # return the OSI score
  return(out)
}


#' Calculate the potassium excess index in Norway
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_K_AL (numeric) The K-content of the soil extracted with ammonium lactate (mg K / kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_no(B_LU = 'testcrop1',A_K_AL = 5,A_CLAY_MI=5)
#' osi_nut_k_no(B_LU = c('testcrop1','testcrop2'),A_K_AL = c(3.5,5.5),A_CLAY_MI=c(3,5))
#' 
#' @return 
#' The potassium excess index in Norway derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_nut_k_no <- function(B_LU, A_K_AL,A_CLAY_MI, unitcheck = TRUE) {
  
  # add visual bindings
  id = NULL
  
  #get max length of inputs
  arg.length <- max(length(B_LU),length(A_K_AL),length(A_CLAY_MI))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_K_AL = A_K_AL,
                           A_CLAY_MI = A_CLAY_MI),
               fname = 'osi_nut_k_no',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_K_AL = A_K_AL,
                   value = NA_real_)
  
  # evaluation soil K status
  # https://www.nibio.no/tema/jord/gjodslingshandbok/korreksjonstabeller/kaliumkorreksjon-til-eng
  # https://www.nibio.no/tema/jord/gjodslingshandbok/korreksjonstabeller/kalium--korn-oljevekster-potet-og-gronnsaker
  dt[, value := OBIC::evaluate_logistic(A_K_AL,b = -0.01291, x0 = 250.2, v = 1.7261 )]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value 
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the potassium excess index in Poland
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_DL (numeric) The exchangeable K-content of the soil measured via ammonium double lactate extracton (mg K/ kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_pl(B_LU = 'testcrop1',A_K_DL = 45,B_TEXTURE_HYPRES='C')
#' 
#' @return 
#' The potassium excess index in Poland estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_nut_k_pl <- function(A_K_DL,B_TEXTURE_HYPRES,B_LU = NA_character_, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = osi_threshold_region = id = crop_cat1 = NULL
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='PO']
  
  # parameters
  # dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
  # get max length of input variables
  arg.length <- max(length(A_K_DL),length(B_TEXTURE_HYPRES),length(B_LU))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                           A_K_DL = A_K_DL),
               fname = 'osi_nut_k_pl',
               unitcheck = unitcheck)
  
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
     value := osi_evaluate_logistic(x = A_K_DL, b= -0.02113, x0 = 149.6, v = 1.6449)]
  # calculate the OSI score light textured soils
  dt[B_TEXTURE_HYPRES %in% c('MF'),
     value := osi_evaluate_logistic(x = A_K_DL, b = -0.01207, x0 = 300.45, v = 0.5917)]
  # calculate the OSI score medium texture soils
  dt[B_TEXTURE_HYPRES %in% c('M'),
     value := osi_evaluate_logistic(x = A_K_DL, b = -0.01493, x0 = 219.47, v = 1.7819)]
  # calculate the OSI score for heavy textured soils
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_DL, b = -0.01342, x0 = 249.39, v = 1.8557)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium excess index index in Portugal
#' 
#' This function calculates the potassium excess index. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_K_AAA (numeric) The K-content of the soil extracted with acid ammonium acetate (mg K / kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_pt(B_LU = '3301061299',A_K_AAA = 50)
#' osi_nut_k_pt(B_LU =  c('3301061299','3301000000'),A_K_AAA = c(35,55))
#' 
#' @return 
#' The potassium availability index in Portugal derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_nut_k_pt <- function(B_LU, A_K_AAA, unitcheck = TRUE) {
  
  # add visual binding
  crop_cat1 = osi_country = . = crop_code = crop_cat2 = id = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='PT']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_K_AAA))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('PT',arg.length),
                           B_LU = B_LU,
                           A_K_AAA = A_K_AAA),
               fname = 'osi_nut_k_pt',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_K_AAA = A_K_AAA * 1.205, # unit is mg K2O/ kg in fertilizer recommendation
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # evaluation soil K status
  dt[, value := osi_evaluate_logistic(A_K_AAA, b = -0.02732, x0 = 117.62, v = 1.7064)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the potassium excess index in Sweden
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_K_AL (numeric) The K-content of the soil extracted with ammonium lactate (mg K / kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_se(B_LU = '3301061299',A_K_AL = 5)
#' osi_nut_k_se(B_LU =  c('3301061299','3301000000'),A_K_AL = c(3.5,5.5))
#' 
#' @return 
#' The potassium excess index in Sweden derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_nut_k_se <- function(B_LU, A_K_AL, unitcheck = TRUE) {
  
  # add visual binding
  crop_cat1 = osi_country = . = crop_code = crop_cat2 = id = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='SE']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_K_AL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('SE',arg.length),
                           B_LU = B_LU,
                           A_K_AL = A_K_AL),
               fname = 'osi_nut_k_se',
               unitcheck = unitcheck)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_K_AL = A_K_AL,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_cat2)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # evaluation soil K status
  dt[, value := OBIC::evaluate_logistic(A_K_AL,b = -0.01225, x0 = 300.51, v = 0.5419)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate thepotassium excess index in Romenia
#' 
#' This function calculates the potassium excess index 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_AL (numeric) The exchangeable K-content of the soil measured via ammonium lactate extracton (mg K/ kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_ro(B_LU = 'testcrop1',A_K_AL = 45,B_TEXTURE_HYPRES='M')
#' 
#' @return 
#' The potassium excess index in Romenia estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_nut_k_ro <- function(A_K_AL,B_TEXTURE_HYPRES,B_LU = NA_character_, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  
  # get max length of input variables
  arg.length <- max(length(B_LU),length(B_TEXTURE_HYPRES),length(A_K_AL))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                           A_K_AL = A_K_AL),
               fname = 'osi_nut_k_ro',
               unitcheck = unitcheck)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_K_AL = A_K_AL,
                   value = NA_real_)
  
  # evaluate the OSI score for Light sandy and loamy soils, Lower values for sandy soils, higher values for loamy-sandy soils 
  dt[B_TEXTURE_HYPRES %in% c('C'), value := osi_evaluate_logistic(x = A_K_AL, b = -0.01287, x0 = 244.16, v = 1.6206)]
  dt[B_TEXTURE_HYPRES %in% c('M'), value := osi_evaluate_logistic(x = A_K_AL, b = -0.12669, x0 = 301.21, v = 26.4311)]
  
  # evaluate the OSI score for Medium clayey-sandy and loamy soils. Averaged conditions.
  dt[B_TEXTURE_HYPRES %in% c('MF'), value := osi_evaluate_logistic(x = A_K_AL, b = -0.23822, x0 = 262.04, v = 43.2289)]
  
  # evaluate the OSI score for Heavy clayey-sandy and loamy soils. Averaged conditions.  
  dt[B_TEXTURE_HYPRES %in% c('F','VF'), value := osi_evaluate_logistic(x = A_K_AL, b = -0.14281, x0 = 317.5, v = 31.3842)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium excess index in  Slovak Republic
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_M3 (numeric) The exchangeable K-content of the soil measured via Mehlich 3 extracton (mg K/ kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_sk(B_LU = '3301010901', A_K_M3 = 45,B_TEXTURE_HYPRES='C')
#' 
#' @return 
#' The potassium excess index in Slovak Republic estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_nut_k_sk <- function(B_LU, B_TEXTURE_HYPRES,A_K_M3, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='SK']
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(B_TEXTURE_HYPRES),length(A_K_M3))
  
  # get max length of input variables
  arg.length <- max(length(B_LU),length(B_TEXTURE_HYPRES),length(A_K_M3))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('SK',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                           A_K_M3 = A_K_M3),
               fname = 'osi_nut_k_sk',
               unitcheck = unitcheck)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_K_M3 = A_K_M3,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # convert to the OSI score
  dt[B_TEXTURE_HYPRES %in% c('C'),
     value := osi_evaluate_logistic(x = A_K_M3, b = -0.01002, x0 = 326.58, v = 1.7743)]
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_K_M3, b = -0.00797, x0 = 415.86, v = 1.8222 )]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_M3, b = -0.00615, x0 = 539.04, v = 1.8201)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium excess index in Slovenia
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_K_AL (numeric) The exchangeable K-content of the soil measured via ammoniuml lactate extracton (mg K/ kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_sl(B_LU = 'testcrop1',A_K_AL = 45 ,B_TEXTURE_HYPRES = 'C')
#' 
#' @return 
#' The potassium excess index in Slovenia estimated from extractable potassium. A numeric value.
#' 
#' @export
osi_nut_k_sl <- function(A_K_AL,B_TEXTURE_HYPRES,B_LU = NA_character_, unitcheck = TRUE) {
  
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
  arg.length <- max(length(A_K_AL),length(B_TEXTURE_HYPRES),length(B_LU))
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                           A_K_AL = A_K_AL),
               fname = 'osi_nut_k_sl',
               unitcheck = unitcheck)
  
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
     value := osi_evaluate_logistic(x = A_K_AL, b = -0.18567, x0 = 135.58, v = 69.2425)]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_K_AL, b = -0.00797, x0 = 406.63, v = 1.7338)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the potassium excess index in United Kingdom
#' 
#' This function calculates the potassium excess. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_K_AN (numeric) The K-content of the soil extracted with ammonium nitrate (mg K /kg)
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'   
#' @import data.table
#' 
#' @examples 
#' osi_nut_k_uk(B_LU = 'testcrop1',A_SOM_LOI=3,A_K_AN = 50)
#' osi_nut_k_uk(B_LU = c('testcrop1','testcrop2'),A_SOM_LOI = c(3,5),A_K_AN = c(35,55))
#' 
#' @return 
#' The potassium excess index in United Kingdom derived from extractable soil K fractions. A numeric value.
#' 
#' @export
osi_nut_k_uk <- function(B_LU, A_SOM_LOI,A_K_AN, unitcheck = TRUE) {
  
  # add visual bindings
  crop_name = . = crop_cat1 = osi_country = BDS = NULL
  
  # crop properties
  #dt.crops <- as.data.table(euosi::osi_crops)
  #dt.crops <- dt.crops[osi_country=='UK']
  
  # check inputs (not for B_LU since these are not in osi_crops)
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,
                           A_K_AN = A_K_AN),
               fname = 'osi_nut_k_uk',
               unitcheck = unitcheck)
  
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_K_AN = A_K_AN,
                   value = NA_real_)
  
  # merge with crop
  # dt <- merge(dt,
  #             dt.crops[,.(B_LU, crop_name, crop_cat1)],
  #             by = 'B_LU',
  #             all.x = TRUE)
  
  # temporary fix
  dt[,crop_name := B_LU]
  
  # convert from mg / kg to mg / liter sample volume
  dt[, BDS := (1/(0.02525 * A_SOM_LOI + 0.6541))]
  dt[, A_K_AN := A_K_AN * BDS]
  
  # optimum value is index 2 for all land uses
  dt[, value := osi_evaluate_logistic(A_K_AN, b = -0.00922, x0 = 341.81, v = 1.6337)]
  
  # optimum value for vegatables
  dt[grepl('vegetab|cabbag|leek|carrot|sprout|celery|onion|potato|lettu',tolower(crop_name)), 
     value := osi_evaluate_logistic(A_K_AN, b = -0.00853, x0 = 366.63, v = 1.6052)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}
