#' Calculate the magnesium availability index (wrapper function)
#' 
#' This function calculates the magnesium availability for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_CLAY_MI (numeric) is the clay content (\%)
#' @param A_SAND_MI (numeric) is the sand content (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_CEC_CO (numeric) is the Cation exhange capacity in mmol+/kg 
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CACO3_IF (numeric) the percentage of CaCO3 (\%)
#' @param A_MG_AAA (numeric) is the exchangeable Mg concentration (mg/kg)
#' @param A_MG_AL (numeric) The exchangeable Mg-content of the soil measured via Ammonium Lactate extraction (mg Mg/ kg)
#' @param A_MG_AN (numeric) The Mg-content of the soil extracted with ammonium nitrate (mg Mg /kg)
#' @param A_MG_CC (numeric) The plant available content of Mg in the soil (mg  Mg per kg) extracted by 0.01M CaCl2
#' @param A_MG_CO_PO (numeric) The exchangeable Mg-content of the soil measured via Cohex extracton, percentage occupation at CEC (\%)
#' @param A_MG_DL (numeric) The exchangeable Mg-content of the soil measured via Double Lactate extraction (mg Mg/ kg)
#' @param A_MG_KCL (numeric) The plant available potassium, extracted with KCL (mg per kg)
#' @param A_MG_M3 (numeric) The exchangeable Mg-content of the soil measured via Mehlich 3 extracton (mg Mg/ kg)
#' @param A_MG_NaAAA (numeric) The Mg-content of the soil extracted with Morgan's solution, sodium acetate acetic acid (mg/ kg)
#' @param A_K_AAA (numeric) The exchangeable K-content of the soil measured via ammonium acetate extraction 
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_K_CC (numeric) The plant available potassium, extracted with 0.01M CaCl2 (mg per kg)
#' @param B_COUNTRY (character) The country code
#'  
#' @import data.table
#' 
#' @return
#' The capacity of the soil to supply and buffer magnesium, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_c_magnesium <- function(B_LU, B_SOILTYPE_AGR = NA_character_,
                            A_CLAY_MI = NA_real_,A_SAND_MI = NA_real_,
                            A_SOM_LOI = NA_real_, A_C_OF = NA_real_, A_CEC_CO = NA_real_,
                            A_PH_CC = NA_real_, A_CACO3_IF = NA_real_,
                            A_MG_AAA = NA_real_,A_MG_AL = NA_real_, A_MG_AN = NA_real_,A_MG_CC = NA_real_,
                            A_MG_CO_PO = NA_real_, A_MG_DL = NA_real_,A_MG_KCL = NA_real_,A_MG_M3 = NA_real_,
                            A_MG_NaAAA = NA_real_,
                            A_K_AAA = NA_real_,A_K_CO_PO = NA_real_,A_K_CC = NA_real_,
                            B_COUNTRY) {
  
  # add visual bindings
  B_TEXTURE_USDA = A_SILT_MI = B_TEXTURE_GEPPA = B_TEXTURE_HYPRES = B_TEXTURE_BE = NULL
  A_PH_WA = A_PH_KCL = NULL
  
  # desired length of inputs
  arg.length <- max(length(B_LU),length(B_SOILTYPE_AGR), length(A_CLAY_MI),length(A_SAND_MI),
                    length(A_SOM_LOI),length(A_C_OF),length(A_CEC_CO), 
                    length(A_PH_CC), length(A_CACO3_IF),
                    length(A_MG_AAA),length(A_MG_AL),length(A_MG_AN),length(A_MG_CC),
                    length(A_MG_CO_PO),length(A_MG_DL),length(A_MG_KCL),length(A_MG_M3),
                    length(A_MG_NaAAA),
                    length(A_K_CO_PO),length(A_K_CC),length(A_K_AAA),
                    length(B_COUNTRY))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = pmax(0,100 - A_CLAY_MI - A_SAND_MI),
                   B_COUNTRY = B_COUNTRY,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_CEC_CO = A_CEC_CO,
                   A_PH_CC = A_PH_CC,
                   A_PH_WA = NA_real_,
                   A_PH_KCL = NA_real_,
                   A_CACO3_IF = A_CACO3_IF,
                   A_MG_AAA = A_MG_AAA,
                   A_MG_AL = A_MG_AL,
                   A_MG_AN = A_MG_AN,
                   A_MG_CC = A_MG_CC,
                   A_MG_CO_PO = A_MG_CO_PO,
                   A_MG_DL = A_MG_DL,
                   A_MG_KCL = A_MG_KCL,
                   A_MG_M3 = A_MG_M3,
                   A_MG_NaAAA = A_MG_NaAAA,
                   A_K_AAA = A_K_AAA,
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_CC = A_K_CC,
                   value = NA_real_
  )
  
  # check required inputs
  osi_checkvar(parm = list(B_COUNTRY = dt$B_COUNTRY, B_LU = dt$B_LU,
                           B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
                           A_CLAY_MI = dt$A_CLAY_MI,
                           A_SAND_MI = dt$A_SAND_MI,
                           A_SILT_MI = dt$A_SILT_MI,
                           A_SOM_LOI = dt$A_SOM_LOI,
                           A_C_OF = dt$A_C_OF,
                           A_CEC_CO = dt$A_CEC_CO,
                           A_PH_CC = dt$A_PH_CC,
                           A_CACO3_IF = dt$A_CACO3_IF,
                           A_MG_AAA = dt$A_MG_AAA,
                           A_K_AAA = dt$A_K_AAA,
                           A_K_CO_PO = dt$A_K_CO_PO,
                           A_K_CC = dt$A_K_CC),
               fname='oci_c_magnesium',
               na_allowed = TRUE)
  
  # estimate texture information
  dt[,B_TEXTURE_USDA := osi_get_TEXTURE_USDA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_HYPRES := osi_get_TEXTURE_HYPRES(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_BE := osi_get_TEXTURE_BE(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_GEPPA := osi_get_TEXTURE_GEPPA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  
  # estimate missing soil properties (from defaults in LUCAS), and use 
  dt[is.na(A_PH_WA) & !is.na(A_PH_CC), A_PH_WA := osi_conv_ph(element='A_PH_WA',A_PH_CC = A_PH_CC)]
  dt[!is.na(A_PH_WA) & is.na(A_PH_CC), A_PH_CC := osi_conv_ph(element='A_PH_CC',A_PH_WA = A_PH_WA)]
  dt[is.na(A_PH_KCL), A_PH_KCL := osi_conv_ph(element='A_PH_KCL',A_PH_WA = A_PH_WA,A_PH_CC = A_PH_CC)]
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 2]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 * 0.5]
  
  # pedotransfer of Helling (1964) when CEC is missing
  dt[is.na(A_CEC_CO), A_CEC_CO := (0.44 * A_PH_WA + 3)* A_CLAY_MI + (5.1 * A_PH_WA - 5.9) * A_C_OF * 0.1]
  
  # estimate K paramters needed for Dutch advice
  dt[,A_K_CC := osi_conv_potassium(element='A_K_CC',A_K_AAA = A_K_AAA)]
  dt[,A_K_CO_PO := osi_conv_potassium(element='A_K_CO_PO',A_K_AAA = A_K_AAA,A_CEC_CO=A_CEC_CO,A_PH_CC = A_PH_CC)]
  
  # estimate extrable soil Mg pools when not available
  dt[,A_MG_AL := osi_conv_magnesium(element='A_MG_AL',A_MG_AAA = A_MG_AAA)]
  dt[,A_MG_AN := osi_conv_magnesium(element='A_MG_AN',A_MG_AAA = A_MG_AAA)]
  dt[,A_MG_CC := osi_conv_magnesium(element='A_MG_CC',A_MG_AAA = A_MG_AAA)]
  dt[,A_MG_CO_PO := osi_conv_magnesium(element='A_MG_CO_PO',A_MG_AAA = A_MG_AAA, A_CEC_CO = A_CEC_CO)]
  dt[,A_MG_DL := osi_conv_magnesium(element='A_MG_DL',A_MG_AAA = A_MG_AAA)]
  dt[,A_MG_KCL := osi_conv_magnesium(element='A_MG_KCL',A_MG_AAA = A_MG_AAA)]
  dt[,A_MG_M3 := osi_conv_magnesium(element='A_MG_M3',A_MG_AAA = A_MG_AAA)]
  dt[,A_MG_NaAAA := osi_conv_magnesium(element='A_MG_NaAAA',A_MG_AAA = A_MG_AAA)]
  
  #check required calculated inputs
  osi_checkvar(parm = list(B_TEXTURE_USDA = dt$B_TEXTURE_USDA, 
                           B_TEXTURE_HYPRES = dt$B_TEXTURE_HYPRES,
                           B_TEXTURE_BE = dt$B_TEXTURE_BE,
                           B_TEXTURE_GEPPA = dt$B_TEXTURE_GEPPA,
                           A_PH_WA = dt$A_PH_WA,
                           A_PH_KCL = dt$A_PH_KCL,
                           A_SOM_LOI = dt$A_SOM_LOI,
                           A_C_OF = dt$A_C_OF,
                           A_CEC_CO = dt$A_CEC_CO,
                           A_K_CC = dt$A_K_CC,
                           A_K_CO_PO = dt$A_K_CO_PO,
                           A_MG_AL = dt$A_MG_AL,
                           A_MG_AN = dt$A_MG_AN,
                           A_MG_CC = dt$A_MG_CC,
                           A_MG_CO_PO = dt$A_MG_CO_PO,
                           A_MG_KCL = dt$A_MG_KCL,
                           A_MG_DL = dt$A_MG_DL,
                           A_MG_M3 = dt$A_MG_M3,
                           A_MG_NaAAA = dt$A_MG_NaAAA
                           ),
               fname='oci_c_magnesium')
  
  # calculate the open soil index score for magnesium availability
  
  # Austria (AT), Belgium (BE), Switzerland (CH), Czech Republic (CZ), Germany (DE)
  dt[B_COUNTRY == 'AT', value := osi_c_magnesium_at(B_LU=B_LU,B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_MG_CC = A_MG_CC)]
  dt[B_COUNTRY == 'BE', value := osi_c_magnesium_be(B_LU = B_LU, A_MG_CC = A_MG_CC)]
  dt[B_COUNTRY == 'CH', value := osi_c_magnesium_ch(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_MG_AAA = A_MG_AAA)]
  dt[B_COUNTRY == 'CZ', value := oci_c_magnesium_cz(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,A_MG_M3 = A_MG_M3)]
  dt[B_COUNTRY == 'DE', value := osi_c_magnesium_de(B_LU = B_LU, A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_MG_CC = A_MG_CC)]
  
  # Denmark (DK), Estonia (EE), Greece (EL), Spain (ES),France (FR), Finland (FI) 
  dt[B_COUNTRY == 'DK', value := osi_c_magnesium_dk(B_LU = B_LU, A_MG_AL = A_MG_AL)]
  dt[B_COUNTRY == 'EE', value := oci_c_magnesium_ee(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA,A_MG_M3 = A_MG_M3)]
  dt[B_COUNTRY == 'EL', value := NA_real_]
  dt[B_COUNTRY == 'ES', value := osi_c_magnesium_es(B_LU = B_LU,A_MG_CO_PO = A_MG_CO_PO)]
  dt[B_COUNTRY == 'FR', value := osi_c_magnesium_fr(B_LU = B_LU,A_CLAY_MI = A_CLAY_MI,A_CEC_CO = A_CEC_CO, 
                                                    A_MG_AAA = A_MG_AAA,A_CACO3_IF = A_CACO3_IF)]
  dt[B_COUNTRY == 'FI', value := osi_c_magnesium_fi(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_MG_AAA = A_MG_AAA,A_C_OF = A_C_OF)]
  
  # Hungary (HU), Ireland (IE), Italy (IT), Latvia (LV), Lithuania (LT)
  dt[B_COUNTRY == 'HU', value := osi_c_magnesium_hu(B_LU = B_LU, B_TEXTURE_USDA = B_TEXTURE_USDA, A_MG_KCL = A_MG_KCL)]
  dt[B_COUNTRY == 'IE', value := osi_c_magnesium_ie(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI, A_MG_NaAAA = A_MG_NaAAA)]
  dt[B_COUNTRY == 'IT', value := osi_c_magnesium_it(B_LU = B_LU, A_MG_CO_PO = A_MG_CO_PO, A_K_CO_PO = A_K_CO_PO)]
  dt[B_COUNTRY == 'LV', value := osi_c_magnesium_lv(B_LU = B_LU,B_TEXTURE_USDA = B_TEXTURE_USDA, A_MG_DL = A_MG_DL)]
  dt[B_COUNTRY == 'LT', value := osi_c_magnesium_lt(B_LU = B_LU,A_PH_KCL = A_PH_KCL, A_MG_AL = A_MG_AL)]
  
  # the Netherlands (NL), Norway (NO),  Sweden (SE), Slovak Republic (SK), Slovenia (SL)
  dt[B_COUNTRY == 'NL', value := osi_c_magnesium_nl(B_LU = B_LU,B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                                    A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI,
                                                    A_PH_CC = A_PH_CC, A_CEC_CO = A_CEC_CO,
                                                    A_K_CO_PO = A_K_CO_PO,A_MG_CC = A_MG_CC,A_K_CC = A_K_CC)]
  dt[B_COUNTRY == 'NO', value := osi_c_magnesium_no(B_LU = B_LU, A_MG_AL = A_MG_AL)]
  dt[B_COUNTRY == 'SE', value := osi_c_magnesium_se(B_LU = B_LU, A_MG_AL = A_MG_AL)]
  dt[B_COUNTRY == 'SK', value := osi_c_magnesium_sk(B_LU = B_LU,B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,A_MG_M3 = A_MG_M3)]
  dt[B_COUNTRY == 'SL', value := osi_c_magnesium_sl(B_LU = B_LU,B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,A_MG_AL = A_MG_AL)]
  
  # Poland (PL), Portugal, and United Kingdom (UK)
  dt[B_COUNTRY == 'PL', value := osi_c_magnesium_pl(B_LU = B_LU,B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,A_MG_CC = A_MG_CC)]
  dt[B_COUNTRY == 'PT', value := osi_c_magnesium_pt(B_LU = B_LU, A_MG_AAA = A_MG_AAA)]
  dt[B_COUNTRY == 'RO', value := osi_c_magnesium_ro(B_LU = B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES, A_CEC_CO = A_CEC_CO,
                                                    A_MG_CC = A_MG_CC, A_MG_CO_PO = A_MG_CO_PO,A_K_CO_PO = A_K_CO_PO, A_PH_WA = 5)]
  dt[B_COUNTRY == 'UK', value := osi_c_magnesium_uk(B_LU = B_LU,A_SOM_LOI = A_SOM_LOI,A_MG_AN = A_MG_AN)]
  
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
#' osi_c_magnesium_at(B_LU = '3301000000',A_MG_CC = 47,B_TEXTURE_HYPRES = 'C')
#' 
#' @return 
#' The magnesium availability index in Austria estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_at <- function(B_LU, A_MG_CC,B_TEXTURE_HYPRES) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='AT']
  
  # get max length of input variables
  arg.length <- max(length(A_MG_CC),length(B_TEXTURE_HYPRES),length(B_LU))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_MG_CC = A_MG_CC,
                   value = NA_real_)
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('AT',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                           A_MG_CC = A_MG_CC),
               fname = 'osi_c_magnesium_at')
  
  # merge crop properties
  dt <- merge(dt,
             dt.crops[,.(crop_code,crop_cat1)],
             by.x = 'B_LU', 
             by.y = 'crop_code',
             all.x=TRUE)
  
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

#' Calculate the magnesium availability index in Belgium
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_MG_CC (numeric) The exchangeable Mg-content of the soil measured via calcium chloride extracton (mg Mg/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_be(B_LU = '8410',A_MG_CC = 45)
#' 
#' @return 
#' The magnesium availability index in Belgium estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_be <- function(B_LU,A_MG_CC) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = rop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='BE']
  
  # get max length of input variables
  arg.length <- max(length(A_MG_CC),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep("BE",arg.length),
                           B_LU = B_LU,
                           A_MG_CC = A_MG_CC),
               fname = 'osi_c_magnesium_be')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_MG_CC = A_MG_CC,
                   value = NA_real_)
  
  # calculate the OSI score 
  # according to ChatGPT uses Flanders the recommendation of the Netherlands with optimum around 45 mg /kg
  # https://lv.vlaanderen.be/sites/default/files/attachments/praktijkgids-bemesting-meststoffen-groenbedekkers_1.pdf
  # for NL is information derived from handboekbodemenbemesting.nl
  dt[,value := evaluate_logistic(A_MG_CC, b = 0.1717292, x0 = 19.5341702, v = 1.0640583)]
 
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
#' osi_c_magnesium_ch(B_LU = '8410', A_MG_AAA = 50,A_CLAY_MI=15)
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
  
  # get max length of input variables
  arg.length <- max(length(A_MG_AAA),length(A_CLAY_MI),length(B_LU))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_MG_AAA = A_MG_AAA,
                   value = NA_real_)
  
  # check inputs
  osi_checkvar(parm = list(A_CLAY_MI = A_CLAY_MI,
                           A_MG_AAA = A_MG_AAA),
               fname = 'osi_c_magnesium_ch')
  
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
#' oci_c_magnesium_cz(B_LU = '3301000000',A_MG_M3 = 81,B_TEXTURE_HYPRES='C')
#' 
#' @return 
#' The magnesium availability index in Czech Republic estimated from extractable magnesium. A numeric value.
#' 
#' @export
oci_c_magnesium_cz <- function(A_MG_M3,B_TEXTURE_HYPRES,B_LU = NA_character_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='CZ']
  
  # get max length of input variables
  arg.length <- max(length(A_MG_M3),length(B_TEXTURE_HYPRES),length(B_LU))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_MG_M3 = A_MG_M3,
                   value = NA_real_)
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('CZ',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                           A_MG_M3 = A_MG_M3),
               fname = 'osi_c_magnesium_cz')
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)

  # convert to the OSI score
  dt[B_TEXTURE_HYPRES %in% c('C'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.04947471,x0 = 3.29991394 ,v = 0.01033346)]
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.051319601,x0 = 2.593526993 ,v = 0.002350958)]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.02996501,x0 = 87.56290295  ,v = 0.14816731)]
  
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
#' @param A_MG_CC (numeric) The magnesium content extracted with CaCl2 (g / kg)
#' 
#' @import data.table
#' 
#' @return 
#' The magnesium availability index in Germany derived from extractable soil Mg fractions. A numeric value.
#' 
#' @export
osi_c_magnesium_de <- function(B_LU, A_C_OF, A_CLAY_MI,A_SAND_MI, A_MG_CC) {
  
  # add visual bindings
  A_SILT_MI = stype = NULL
  
  # get max length of input variables
  arg.length <- max(length(B_LU),length(A_C_OF),length(A_CLAY_MI),length(A_SAND_MI),length(A_MG_CC))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('DE',arg.length),
                           B_LU = B_LU,
                           A_C_OF = A_C_OF,
                           A_CLAY_MI = A_CLAY_MI,
                           A_SAND_MI = A_SAND_MI,
                           A_MG_CC = A_MG_CC),
               fname = 'osi_c_magnesium_de')
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_C_OF= A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_MG_CC = A_MG_CC,
                   value = NA_real_)
  
  # add soil type
  dt[A_SAND_MI >= 85 & A_SILT_MI <= 25 & A_CLAY_MI <= 5 & A_C_OF < 150, stype := "BG1"]
  dt[A_SAND_MI >= 42 & A_SAND_MI <= 95 & A_SILT_MI <= 40 & A_CLAY_MI <= 17 & A_C_OF < 150,  stype :="BG2"]
  dt[A_SAND_MI >= 33 & A_SAND_MI <= 83 & A_SILT_MI <= 50 & A_CLAY_MI >= 8 & A_CLAY_MI <= 25 & A_C_OF < 150,  stype :="BG3"]
  dt[A_SAND_MI <= 75 & A_SILT_MI <= 100 & A_CLAY_MI <= 35 & A_C_OF < 150,  stype :="BG4"]
  dt[A_SAND_MI <= 65 & A_SILT_MI <= 75 & A_CLAY_MI >= 25 & A_CLAY_MI <= 100 & A_C_OF < 150, stype := "BG5"]
  dt[ A_C_OF >= 150,  stype := "BG6"]
  
  # evaluate A_MG_CC for arable an grassland soils
  dt[stype=='BG1', value := osi_evaluate_logistic(A_MG_CC, b = 0.17394558, x0 = 1.97132628, v = 0.01974615)]
  dt[stype=='BG2', value := osi_evaluate_logistic(A_MG_CC, b = 0.131007506, x0 = -8.898667288, v = 0.005444774)]
  dt[stype=='BG3', value := osi_evaluate_logistic(A_MG_CC, b = 0.105975715, x0 = -13.136695181, v = 0.004774989)]
  dt[stype=='BG4', value := osi_evaluate_logistic(A_MG_CC, b = 0.07756033, x0 = 3.04955824, v = 0.02572424)]
  dt[stype=='BG5', value := osi_evaluate_logistic(A_MG_CC, b = 0.06074047, x0 = 3.55690767, v = 0.02687046)]
  dt[stype=='BG6', value := osi_evaluate_logistic(A_MG_CC, b = 0.17394558, x0 = 1.97132628, v = 0.01974615)]
  
  # select value and return
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the magnesium availability index in Denmark
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_MG_AL (numeric) The Mg-content of the soil extracted with ammonium lactate (mg Mg / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_dk(B_LU = '3301000000',A_MG_AL = 5)
#' osi_c_magnesium_dk(B_LU = c('3301000000','3301061299'),A_MG_AL = c(3.5,5.5))
#' 
#' @return 
#' The magnesium availability index in Denmark derived from extractable soil Mg fractions. A numeric value.
#' 
#' @export
osi_c_magnesium_dk <- function(B_LU, A_MG_AL) {
  
  # length of arguments
  arg.length <- max(length(B_LU),length(A_MG_AL))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('DK',arg.length),
                           B_LU = B_LU,
                           A_MG_AL = A_MG_AL),
               fname = 'osi_c_magnesium_dk')
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_MG_AL = A_MG_AL,
                   value = NA_real_)
  
  # evaluation soil Mg status, only threshold at optimum level is given (IFS, Ristimaki et al. (2007))
  dt[, value := OBIC::evaluate_logistic(A_MG_AL, b = 0.07792559, x0 = 0.53507550    , v = 0.08751223  )]
  
  # select value 
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
#' oci_c_magnesium_ee(B_LU = 'testcrop',A_MG_M3 = 45,B_TEXTURE_USDA = 'clay')
#' 
#' @return 
#' The magnesium availability index in Estonia estimated from extractable magnesium. A numeric value.
#' 
#' @export
oci_c_magnesium_ee <- function(A_MG_M3,B_TEXTURE_USDA,B_LU = NA_character_) {
  
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
  arg.length <- max(length(A_MG_M3),length(B_TEXTURE_USDA),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_TEXTURE_USDA = B_TEXTURE_USDA,
                           A_MG_M3 = A_MG_M3),
               fname = 'osi_c_magnesium_ee')
  
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
  dt[B_TEXTURE_USDA %in%  c('sand','Sa'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.13379398,x0 = 2.32860029,v = 0.01118338)]
  dt[B_TEXTURE_USDA %in% c('loamy sand','LoSa'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.09055787,x0 = 2.54738930,v = 0.02382157)]
  dt[B_TEXTURE_USDA %in% c('sandy loam','SaLo'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.091975296,x0 = 2.951269556,v = 0.008792147)]
  dt[B_TEXTURE_USDA %in% c('loam','sandy clay','sandy clay loam','Lo','SaCl','SaCL'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.06050444,x0 = 2.83249967,v = 0.01937565)]
  dt[B_TEXTURE_USDA %in% c('clay','clay loam','silty clay','silty clay loam','silt loam','silt',
                           'Cl','ClLo','SiCl','SiCL','SiClLo','SiLo','Si'),
     value := osi_evaluate_logistic(x = A_MG_M3, b= 0.04557622,x0 = 0.55322883,v = 0.01921304)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the magnesium availability index in Spain
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_MG_CO_PO (numeric) The exchangeable Mg-content of the soil measured via Cohex extracton, percentage occupation at CEC (\%)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_es(B_LU = '3301000000' , A_MG_CO_PO = 4.5)
#' 
#' @return 
#' The magnesium availability index in Spain estimated from the magnesium occupation at CEC. A numeric value.
#' 
#' @export
osi_c_magnesium_es <- function(B_LU,A_MG_CO_PO) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='ES']
  
  # get max length of input variables
  arg.length <- max(length(A_MG_CO_PO),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('ES',arg.length),
                           B_LU = B_LU,
                           A_MG_CO_PO = A_MG_CO_PO),
               fname = 'osi_c_magnesium_es')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_MG_CO_PO = A_MG_CO_PO,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = A_MG_CO_PO, b= 0.25752413 ,x0 = -8.39867874,v = 0.03108629)]
 
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the magnesium availability index in Finland
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_MG_AAA (numeric) The exchangeable Mg-content of the soil measured via ammonium acetate extraction (mg Mg / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_fi(B_LU = '1110', B_TEXTURE_USDA = 'Si',A_MG_AAA = 45)
#' 
#' @return 
#' The magnesium availability index in Finland estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_fi <- function(B_LU, B_TEXTURE_USDA, A_MG_AAA,A_C_OF = 0.5) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = B_SOILTYPE_AGR = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FI']
  
  # get max length of input data
  arg.length <- max(length(B_LU),length(B_TEXTURE_USDA), length(A_MG_AAA),
                    length(A_C_OF))
  
  # repeat A_C_OF if only one default is given
  if(length(A_C_OF)==1 & arg.length > 1){A_C_OF <- rep(A_C_OF,arg.length)}
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('FI',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_USDA = B_TEXTURE_USDA,
                           A_C_OF = A_C_OF,
                           A_MG_AAA = A_MG_AAA),
               fname = 'osi_c_magnesium_fi')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA = B_TEXTURE_USDA,
                   B_SOILTYPE_AGR = NA_character_,
                   A_MG_AAA = A_MG_AAA,
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
  
  # evaluate the OSI score for Mg status, only threshold at optimum level is given (IFS, Ristimaki et al. (2007))
  dt[B_SOILTYPE_AGR %in% c('sand','organic'), value := osi_evaluate_logistic(x = A_MG_AAA, b= 0.04239632 ,x0 = 0.53855312 ,v = 0.14653214 )]
  dt[B_SOILTYPE_AGR == 'loam', value := osi_evaluate_logistic(x = A_MG_AAA, b= 0.04239632 ,x0 = 0.53855312 ,v = 0.14653214 )]
  dt[B_SOILTYPE_AGR == 'clay', value := osi_evaluate_logistic(x = A_MG_AAA, b= 0.02578294  ,x0 = 0.55427721  ,v = 0.14196990  )]
  
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
#' @param A_MG_AAA (numeric) The extractable Mg content in the soil (mg/kg)
#' 
#' @import data.table
#'  
#' @return 
#' The magnesium availability index in France estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_fr <- function(B_LU,A_CLAY_MI, A_CEC_CO, A_CACO3_IF, A_MG_AAA) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  soil_cat_mg = osi_st_c1 = osi_st_c2 = osi_st_c3 = NULL
  
  # Load in the crop datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # load and subset thresholds for situation in France
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='FR' & osi_indicator=='i_c_mg']
  checkmate::assert_data_table(dt.thresholds,max.rows = 4)
  
  # Check length of desired input
  arg.length <- max(length(A_CLAY_MI),length(A_CEC_CO),length(A_CACO3_IF),
                    length(A_MG_AAA))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('FR',arg.length),
                           B_LU = B_LU,
                           A_CLAY_MI = A_CLAY_MI,
                           A_CEC_CO = A_CEC_CO,
                           A_CACO3_IF = A_CACO3_IF,
                           A_MG_AAA = A_MG_AAA),
               fname = 'osi_c_magnesium_fr')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_MG_AAA = A_MG_AAA,
                   A_CLAY_MI = A_CLAY_MI,
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
  dt[,value := osi_evaluate_logistic(x = A_MG_AAA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
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
#' osi_c_magnesium_hu(B_LU = 'testcrop',A_MG_KCL = 45,B_TEXTURE_USDA = 'loam')
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
  
  # get max length of input variables
  arg.length <- max(length(A_MG_KCL),length(B_TEXTURE_USDA),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_TEXTURE_USDA = B_TEXTURE_USDA,
                           A_MG_KCL = A_MG_KCL),
               fname = 'osi_c_magnesium_hu')
  
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
  dt[B_TEXTURE_USDA %in%  c('sand','Sa'),
     value := osi_evaluate_logistic(x = A_MG_KCL, b= 0.1560195 ,x0 = 2.8715883 ,v = 0.1279928 )]
  dt[B_TEXTURE_USDA %in% c('loamy sand','sandy loam','LoSa','SaLo'),
     value := osi_evaluate_logistic(x = A_MG_KCL, b= 0.1102304 ,x0 = 10.2981516  ,v = 0.2085848 )]
  dt[B_TEXTURE_USDA %in% c('loam','sandy clay','sandy clay loam','Lo','SaCl','SaCL'),
     value := osi_evaluate_logistic(x = A_MG_KCL,b= 0.05181633 ,x0 = 2.39759172 ,v = 2.39759172 )]
  dt[B_TEXTURE_USDA %in% c('clay','clay loam','silty clay','silty clay loam','silt loam','silt',
                           'Cl','ClLo','SiCl','SiCL','SiClLo','SiLo','Si'),
     value := osi_evaluate_logistic(x = A_MG_KCL, b= 0.05181633 ,x0 = 2.39759172 ,v = 2.39759172 )]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the magnesium availability index in Italy
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_MG_CO_PO (numeric) The exchangeable Mg-content of the soil measured via Cohex extracton, percentage occupation at CEC (\%)
#' @param A_K_CO_PO (numeric) The exchangeable K-content of the soil measured via Cohex extracton, percentage occupation at CEC (\%)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_it(B_LU = '3301000000',A_MG_CO_PO = 4.5,A_K_CO_PO = 3)
#' 
#' @return 
#' The magnesium availability index in Italy estimated from the Mg extracted soil pool, expressed as occupation of the CEC. A numeric value.
#' 
#' @export
osi_c_magnesium_it <- function(B_LU,A_MG_CO_PO,A_K_CO_PO) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  fr_mg_k = v1 = v2 = . = crop_code = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='IT']
  
  # get max length of input data
  arg.length <- max(length(B_LU),length(A_MG_CO_PO),length(A_K_CO_PO))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('IT',arg.length),
                           B_LU = B_LU,
                           A_MG_CO_PO = A_MG_CO_PO,
                           A_K_CO_PO = A_K_CO_PO),
               fname = 'osi_c_magnesium_it')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_MG_CO_PO = A_MG_CO_PO,
                   A_K_CO_PO = A_K_CO_PO,
                   v1 = NA_real_,
                   v2 = NA_real_,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # add ratio Mg over K (molar or mass??)
  dt[, fr_mg_k := A_MG_CO_PO / A_K_CO_PO]
  
  # calculate the OSI score for Mg occupation
  dt[,v1 := osi_evaluate_logistic(x = A_MG_CO_PO, b= 1.329855 ,x0 = 1.796094 ,v = 0.629399)]
  
  # calculate the OSI score for Mg K ratio
  dt[,v2 := osi_evaluate_logistic(x = fr_mg_k, b= 1.34913097 ,x0 = -2.11085890 ,v = 0.01403961 )]
  
  # get lowest score
  dt[, value := pmin(v1,v2,na.rm=T)]
  
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
#' osi_c_magnesium_ie(B_LU = 'testcrop',A_SOM_LOI = 2,A_MG_NaAAA = 5)
#' osi_c_magnesium_ie(B_LU = c('testcrop','testcrop2'),A_SOM_LOI = c(2,4),A_MG_NaAAA = c(3.5,5.5))
#' 
#' @return 
#' The magnesium availability index in Ireland derived from extractable soil Mg fractions. A numeric value.
#' 
#' @export
osi_c_magnesium_ie <- function(B_LU, A_SOM_LOI,A_MG_NaAAA) {
  
  # add visual bindings
  BDS = NULL
  
  # check inputs
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,
                           A_MG_NaAAA = A_MG_NaAAA),
               fname = 'osi_c_magnesium_ie')
  
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
#' osi_c_magnesium_lv(B_LU = 'testcrop',A_MG_DL = 45,B_TEXTURE_USDA='sand')
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
  
  # get max length of input variables
  arg.length <- max(length(A_MG_DL),length(B_TEXTURE_USDA),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_TEXTURE_USDA = B_TEXTURE_USDA,
                           A_MG_DL = A_MG_DL),
               fname = 'osi_c_magnesium_lv')
  
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
  dt[B_TEXTURE_USDA %in%  c('sand','Sa'),
     value := osi_evaluate_logistic(x = A_MG_DL, b= 0.06556918,x0 = -27.38667996,v = 0.01409843)]
  dt[B_TEXTURE_USDA %in% c('loamy sand','sandy loam','LoSa','SaLo'),
     value := osi_evaluate_logistic(x = A_MG_DL, b= 0.06208305,x0 = 3.09989977,v = 0.07585618)]
  dt[B_TEXTURE_USDA %in% c('loam','sandy clay','sandy clay loam','Lo','SaCl','SaCL'),
     value := osi_evaluate_logistic(x = A_MG_DL, b= 0.04707890,x0 = 2.05827044,v = 0.07764412)]
  dt[B_TEXTURE_USDA %in% c('clay','clay loam','silty clay','silty clay loam','silt loam','silt',
                           'Cl','ClLo','SiCl','SiCL','SiClLo','SiLo','Si'),
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
  
  # get max length of input variables
  arg.length <- max(length(A_MG_AL),length(A_PH_KCL),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(A_PH_KCL = A_PH_KCL,
                           A_MG_AL = A_MG_AL),
               fname = 'osi_c_magnesium_lt')
  
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
#' osi_c_magnesium_nl(B_LU = '265', B_SOILTYPE_AGR = 'dekzand',
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
  id = crop_code = soiltype = osi_country = soiltype.n = crop_category = NULL
  crop_cat1 = osi_soil_cat1 = osi_soil_cat2 = NULL
  
  # Load in the datasets for soil and crop types
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'NL']

  dt.soils <- as.data.table(euosi::osi_soiltype)
  dt.soils <- dt.soils[osi_country == 'NL']
  
  # Check inputs
  arg.length <- max(length(A_MG_CC), length(A_PH_CC), length(A_SOM_LOI), length(A_CEC_CO), 
                    length(A_K_CO_PO), length(A_CLAY_MI), length(B_SOILTYPE_AGR), length(B_LU))
   
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('NL',arg.length),
                           B_LU = B_LU,
                           B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                           A_MG_CC = A_MG_CC,
                           A_PH_CC = A_PH_CC,
                           A_CEC_CO = A_CEC_CO,
                           A_K_CC = A_K_CC,
                           A_K_CO_PO = A_K_CO_PO,
                           A_SOM_LOI = A_SOM_LOI,
                           A_CLAY_MI = A_CLAY_MI                           ),
               fname = 'osi_c_magnesium_nl')
  
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
  dt <- merge(dt, dt.crops[, list(crop_code, crop_cat1)], 
              by.x = "B_LU_BRP", by.y = "crop_code", all.x = TRUE)
  dt <- merge(dt, dt.soils[, list(osi_soil_cat1, osi_soil_cat2)], 
              by.x = "B_SOILTYPE_AGR", by.y = "osi_soil_cat1",all.x = TRUE)
  
  # Calculate the Mg availability for arable land
  dt.arable <- dt[crop_cat1 == "arable"]
  dt.arable[,D_MG := A_MG_CC]
  
  # Calculate the Mg availability for maize land
  dt.maize <- dt[crop_cat1 == "maize"]
  dt.maize[,D_MG := A_MG_CC]
  
  # Calculate Mg availability for grassland on sandy and loamy soils
  dt.grass.sand <- dt[crop_cat1 == "grassland" & grepl('zand|loess|dalgrond',B_SOILTYPE_AGR)]
  dt.grass.sand[,D_MG := A_MG_CC]
  
  # Calculate Mg availability for grassland on clay and peat soils
  dt.grass.other <- dt[crop_cat1 == "grassland" & grepl('klei|veen',B_SOILTYPE_AGR)]
  
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
  dt.nature <- dt[crop_cat1 %in%  c("nature","permanent","forest","other")]
  dt.nature[,D_MG := 0]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass.sand,dt.grass.other, dt.arable,dt.maize,dt.nature), fill = TRUE)
  
  # avoid values below -1
  dt[value < -1, value := -1]
  
  # setorder
  setorder(dt, id)
  
  # convert to indicator score
  dt[crop_cat1 %in% c("arable","maize"),value := evaluate_logistic(D_MG, b = 0.206, x0 = 45, v = 2.39)]
  dt[crop_cat1 == "grassland" & grepl('zand|loess|dalgrond',B_SOILTYPE_AGR),value := evaluate_logistic(D_MG, b = 0.075, x0 = 80, v = 2)]
  dt[crop_cat1 == "grassland" & grepl('klei|veen',B_SOILTYPE_AGR), value := evaluate_logistic(D_MG, b = 0.15, x0 = 75, v = 1)]
  dt[crop_cat1 %in% c("nature","other","forest","permanent"), value := 1]
 
  # select and return OSI indicator
  value <- dt[, value]
  
  return(value)
}

#' Calculate the magnesium availability index in Norway
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_MG_AL (numeric) The exchangeable Mg-content of the soil measured via ammonium lactate (mg K/kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_no(B_LU = '8410',A_MG_AL = 45)
#' 
#' @return 
#' The magnesium availability index in Norway estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_no <- function(B_LU,A_MG_AL) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = rop_code = . = NULL
  
  # crop data
  #dt.crops <- as.data.table(euosi::osi_crops)
  #dt.crops <- dt.crops[osi_country=='BE']
  
  # get max length of input variables
  arg.length <- max(length(A_MG_AL),length(B_LU))
  
  # check inputs (not yet crop since not available in osi_crops)
  osi_checkvar(parm = list(A_MG_AL = A_MG_AL),
               fname = 'osi_c_magnesium_no')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_MG_AL = A_MG_AL,
                   value = NA_real_)
  
  # calculate the OSI score 
  # according to ChatGPT optimum is around 25-45 mg/kg
  # https://www.nlr.no/kunnskap/fagartikler/grovfor/ostlandet/magnesium
  # https://www.statsforvalteren.no/siteassets/fm-rogaland/bilder-fmro/landbruk/skogbruk/dokumenter/lonnsomme-juletrar-122014-publisert-versjon-2b-small.pdf
  dt[,value := evaluate_logistic(A_MG_AL, b = 0.2080099, x0 = 27.7202550, v = 1.9831178)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
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
  
  # get max length of input variables
  arg.length <- max(length(A_MG_CC),length(B_TEXTURE_HYPRES),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                           A_MG_CC = A_MG_CC),
               fname = 'osi_c_magnesium_pl')
  
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

#' Calculate the magnesium availability index in Portugal
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_MG_AAA (numeric) The exchangeable Mg-content of the soil measured via ammonium acetate extraction (mg Mg / kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_pt(B_LU = '3301010901',A_MG_AAA = 45)
#' 
#' @return 
#' The magnesium availability index in Portugal estimated from the Mg extracted soil pool. A numeric value.
#' 
#' @export
osi_c_magnesium_pt <- function(B_LU,A_MG_AAA) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='PT']
  
  # get max length of input variables
  arg.length <- max(length(A_MG_AAA),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('PT',arg.length),
                           B_LU = B_LU,
                           A_MG_AAA = A_MG_AAA),
               fname = 'osi_c_magnesium_pt')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_MG_AAA = A_MG_AAA,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = A_MG_AAA, b= 0.08883167 ,x0 = -14.53907863,v = 0.00880924 )]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the magnesium availability index in Romenia
#' 
#' This function calculates the magnesium availability index 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_CEC_CO (numeric) is the Cation exhange capacity in mmol+/kg 
#' @param A_MG_CC (numeric) The plant available content of Mg in the soil (mg  Mg per kg) extracted by 0.01M CaCl2
#' @param A_MG_CO_PO (numeric) The exchangeable Mg-content of the soil measured via Cohex extracton, percentage occupation at CEC (\%)
#' @param A_K_CO_PO (numeric) The exchangeable K-content of the soil measured via Cohex extracton, percentage occupation at CEC (\%)
#' @param A_PH_WA (numeric) The pH measured in water
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_ro(B_LU = 'testcrop1',B_TEXTURE_HYPRES ='M', A_CEC_CO = 140,
#' A_MG_CC = 60, A_MG_CO_PO = 8,A_K_CO_PO = 12, A_PH_WA = 5)
#' 
#' @return 
#' The Mg index in Romenia. A numeric value.
#' 
#' @export
osi_c_magnesium_ro <- function(B_LU, B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,A_CEC_CO, A_MG_CC,A_MG_CO_PO,A_K_CO_PO,A_PH_WA) {
  
  # add visual bindings
  crop_code = v1 = v2 = crop_cat1 = crop_name = id = . = osi_country = NULL
  ICMG = A_K_CO = A_MG_CO = NULL
  
  # crop data
  #dt.crops <- as.data.table(euosi::osi_crops)
  #dt.crops <- dt.crops[osi_country=='UK']
  
  # get max length of inputs
  arg.length <- max(length(B_LU),length(A_PH_WA),length(A_MG_CC),
                    length(A_CEC_CO),length(A_MG_CO_PO),length(A_K_CO_PO))
  
  # check inputs
  osi_checkvar(parm = list(A_PH_WA = A_PH_WA,
                           A_CEC_CO = A_CEC_CO,
                           A_MG_CO_PO = A_MG_CO_PO,
                           A_K_CO_PO = A_K_CO_PO,
                           A_MG_CC = A_MG_CC),
               fname = 'osi_c_magnesium_ro')
  
  # internal data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU = as.character(B_LU),
                   A_MG_CC = A_MG_CC,
                   A_CEC_CO = A_CEC_CO,
                   A_MG_CO_PO = A_MG_CO_PO,
                   A_K_CO_PO = A_K_CO_PO,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # convert exchnageable cations to mg/kg
  dt[,A_MG_CO := A_CEC_CO * A_MG_CO_PO * 24.305 * 0.5]
  dt[,A_K_CO := A_CEC_CO * A_K_CO_PO * 39.098]
  
  # estimate the ICMg ratio conrom te Borlan (în Lăcătuşu, 2000)
  dt[, ICMG := A_MG_CO  * (1.1 ^ (-0.555 * (A_PH_WA - 4))) / A_K_CO] 
  
  # https://agil.ase.ro/wp-content/uploads/2025/02/3_Monitorizarea_caltatii_mediului.pdf
  # https://www.icpa.ro/documente/coduri/Evaluarea_continutului_de_nutrienti_din_sol.pdf
  
  # convert MG availability to the OSI score
  dt[B_TEXTURE_HYPRES %in% c('C'),
     v1 := osi_evaluate_logistic(x = A_MG_CC, b= 0.08905224, x0 = 2.84842622,v = 0.25877337)]
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),
     v1 := osi_evaluate_logistic(x = A_MG_CC, b= 0.1088578, x0 = 34.9119749,v = 1.3515142)]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),
     v1 := osi_evaluate_logistic(x = A_MG_CC, b= 0.07968356, x0 = 66.91305622,v = 1.97228574 )]
  
  # asses the K-Mg index as well
  dt[, v2 := osi_evaluate_logistic(ICMG, b = 7.042036, x0 = -1.619893, v = 3.578662e-06)]
  
  # select or combine both indices
  dt[is.na(v2), value := v1]
  dt[!is.na(v2), value := (cf_ind_importance(v1)*v1 + cf_ind_importance(v2) * v2)/
                          (cf_ind_importance(v1) + cf_ind_importance(v2))]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select value
  value <- dt[, value]
  
  # return
  return(value)
}

#' Calculate the magnesium availability index in Sweden
#' 
#' This function calculates the magnesium availability. 
#' 
#' @param B_LU (character) The crop code
#' @param A_MG_AL (numeric) The exchangeable Mg-content of the soil measured via ammonium lactate (mg K/kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_se(B_LU = '3301010901',A_MG_AL = 45)
#' 
#' @return 
#' The magnesium availability index in Sweden estimated from the Mg extracted soil pool. A numeric value.
#' 
#' @export
osi_c_magnesium_se <- function(B_LU,A_MG_AL) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='SE']
  
  # get max length of input variables
  arg.length <- max(length(A_MG_AL),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('SE',arg.length),
                           B_LU = B_LU,
                           A_MG_AL = A_MG_AL),
               fname = 'osi_c_magnesium_se')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_MG_AL = A_MG_AL,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = A_MG_AL, b= 0.923539575,x0 = -2.901659957,v = 0.004982924)]
  
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
#' @param A_MG_M3 (numeric) The exchangeable Mg-content of the soil measured via Mehlich 3 extracton (mg Mg/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_magnesium_sk(B_LU ='3301010901',A_MG_M3 = 45,B_TEXTURE_HYPRES='C')
#' 
#' @return 
#' The magnesium availability index in Slovak Republic estimated from extractable magnesium. A numeric value.
#' 
#' @export
osi_c_magnesium_sk <- function(B_LU,B_TEXTURE_HYPRES,A_MG_M3) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='SK']
  
  # get max length of input variables
  arg.length <- max(length(A_MG_M3),length(B_TEXTURE_HYPRES),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_COUNTRY = rep('SK',arg.length),
                           B_LU = B_LU,
                           B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                           A_MG_M3 = A_MG_M3),
               fname = 'osi_c_magnesium_sk')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_MG_M3 = A_MG_M3,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x=TRUE)
  
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
#' osi_c_magnesium_sl(A_MG_AL = 45,B_TEXTURE_HYPRES='C')
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
  
  # get max length of input variables
  arg.length <- max(length(A_MG_AL),length(B_TEXTURE_HYPRES),length(B_LU))
  
  # check inputs
  osi_checkvar(parm = list(B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                           A_MG_AL = A_MG_AL),
               fname = 'osi_c_magnesium_sl')
  
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
#' osi_c_magnesium_uk(B_LU = 'testcrop1',A_SOM_LOI=3,A_MG_AN = 50)
#' osi_c_magnesium_uk(B_LU = c('testcrop1','testcrop2'),A_SOM_LOI = c(3,5),A_MG_AN = c(35,55))
#' 
#' @return 
#' The magnesium availability index in United Kingdom derived from extractable soil Mg fractions. A numeric value.
#' 
#' @export
osi_c_magnesium_uk <- function(B_LU, A_SOM_LOI,A_MG_AN) {
  
  # add visual bindings
  crop_cat1 = BDS = . = crop_name = NULL
  
  # crop properties
  # dt.crops <- as.data.table(euosi::osi_crops)
  
  # check inputs
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,
                           A_MG_AN = A_MG_AN),
               fname = 'osi_c_magnesium_uk')
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_MG_AN = A_MG_AN,
                   value = NA_real_)
  
  # merge with crop
  # dt <- merge(dt,
  #             dt.crops[,.(B_LU, crop_name, crop_cat1)],
  #             by = 'B_LU',
  #             all.x = TRUE)
  
  # convert from mg / kg to mg / liter sample volume
  dt[, BDS := (1/(0.02525 * A_SOM_LOI + 0.6541))]
  dt[, A_MG_AN := A_MG_AN * BDS]
  
  # optimum value is index 2 for all land uses
  dt[, value := osi_evaluate_logistic(A_MG_AN, b = 0.05557028 , x0 = -18.14423092, v = 0.07984178)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}