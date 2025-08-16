#' Calculate the Zinc availability index for agricultural soils (wrapper function)
#' 
#' This function calculates the zinc availability for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_CLAY_MI (numeric) The clay content (\%)
#' @param A_SAND_MI (numeric) The sand content (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_C_OF (numeric) The carbon content of the soil layer (g/ kg)
#' @param A_PH_WA (numeric) The acidity of the soil, measured in water (-)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-) 
#' @param A_ZN_EDTA (numeric) The plant available content of Zn in the soil (mg Zn per kg) extracted by EDTA 
#' @param A_ZN_CC (numeric) The plant available content of Zn in the soil (ug  Zn per kg) extracted by 0.01M CaCl2
#' @param A_ZN_RT (numeric) The total Zn-content of the soil via XRF or Dumas (mg Zn/kg)
#' @param B_COUNTRY (character) The country code
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_zinc(B_LU = 'SOJ', A_CLAY_MI = 45, A_SAND_MI = 15,A_ZN_EDTA = 45, A_PH_WA = 6.5,
#' A_PH_CC = NA, A_ZN_CC = NA, A_ZN_RT = 51, B_COUNTRY='FR')
#' 
#' @return
#' The capacity of the soil to supply and buffer zinc, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_c_zinc <- function(B_LU, A_CLAY_MI = NA_real_,A_SAND_MI = NA_real_,A_C_OF = NA_real_,
                       A_SOM_LOI = NA_real_,A_PH_WA = NA_real_,A_PH_CC = NA_real_,A_ZN_RT = NA_real_,
                       A_ZN_EDTA = NA_real_,A_ZN_CC = NA_real_, B_COUNTRY) {
  
  # add visual bindings
  value = A_SILT_MI = A_ZN_AAA = NULL
  
  # desired length of inputs
  arg.length <- max(length(B_LU), length(A_CLAY_MI), length(A_SAND_MI),length(A_C_OF),
                    length(A_SOM_LOI), length(A_PH_WA), length(A_PH_CC),length(A_ZN_RT), 
                    length(A_ZN_EDTA), length(A_ZN_CC))
  
  # check inputs
  osi_checkvar(parm = list(B_LU = B_LU,
                           B_COUNTRY = B_COUNTRY,
                           A_CLAY_MI = A_CLAY_MI,
                           A_SAND_MI = A_SAND_MI,
                           A_SOM_LOI = A_SOM_LOI,
                           A_PH_CC = A_PH_CC,
                           A_ZN_RT = A_ZN_RT),
               fname ='osi_c_zinc')
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = pmax(0,100 - A_CLAY_MI - A_SAND_MI),
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   B_COUNTRY = B_COUNTRY,
                   A_PH_WA = A_PH_WA,
                   A_PH_CC = A_PH_CC,
                   A_ZN_RT = A_ZN_RT,
                   A_ZN_EDTA = A_ZN_EDTA,
                   A_ZN_CC = A_ZN_CC,
                   value = NA_real_)
  
  # estimate missing soil properties
  dt[is.na(A_PH_WA) & !is.na(A_PH_CC), A_PH_WA := osi_conv_ph(element='A_PH_WA',A_PH_CC = A_PH_CC)]
  dt[!is.na(A_PH_WA) & is.na(A_PH_CC), A_PH_CC := osi_conv_ph(element='A_PH_CC',A_PH_WA = A_PH_WA)]
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 2]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 * 0.5]
  
  # temporary fix: estimate missing A_ZN_CC (https://doi.org/10.1080/00103629509369501)
  dt[!is.na(A_ZN_RT) & is.na(A_ZN_CC), A_ZN_CC := osi_conv_zinc(element = 'A_ZN_CC',A_SOM_LOI = A_SOM_LOI, A_ZN_RT = A_ZN_RT,A_PH_CC = A_PH_CC)]
  dt[!is.na(A_ZN_RT) & is.na(A_ZN_EDTA), A_ZN_EDTA := osi_conv_zinc(element = 'A_ZN_EDTA',A_SOM_LOI = A_SOM_LOI, A_ZN_RT = A_ZN_RT,A_PH_CC = A_PH_CC)]
  dt[!is.na(A_ZN_RT) & is.na(A_ZN_AAA), A_ZN_AAA := osi_conv_zinc(element = 'A_ZN_AAA',A_SOM_LOI = A_SOM_LOI, A_ZN_RT = A_ZN_RT,A_PH_CC = A_PH_CC)]
  
  # check updated soil properties
  osi_checkvar(parm = list(A_SOM_LOI = dt$A_SOM_LOI,
                           A_C_OF = dt$A_C_OF,
                           A_PH_WA = dt$A_PH_WA,
                           A_PH_CC = dt$A_PH_CC,
                           A_ZN_CC = dt$A_ZN_CC,
                           A_ZN_EDTA = dt$A_ZN_EDTA),
               fname ='osi_c_zinc')
  
  # calculate the open soil index score for Zinc availability 
  
  # Austria (AT), Belgium (BE), Switzerland (CH), Czech Republic (CZ), Germany (DE)
  dt[B_COUNTRY == 'AT', value := NA_real_]
  dt[B_COUNTRY == 'BE', value := NA_real_]
  dt[B_COUNTRY == 'CH', value := NA_real_]
  dt[B_COUNTRY == 'CZ', value := NA_real_]
  dt[B_COUNTRY == 'DE', value := osi_c_zinc_de(B_LU = B_LU,A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_ZN_EDTA = A_ZN_EDTA)]
  
  # Denmark (DK), Estonia (EE), Spain (ES),France (FR), Finland (FI) 
  dt[B_COUNTRY == 'DK', value := NA_real_]
  dt[B_COUNTRY == 'EE', value := NA_real_]
  dt[B_COUNTRY == 'ES', value := NA_real_]
  dt[B_COUNTRY == 'FR', value := osi_c_zinc_fr(B_LU = B_LU,A_PH_WA = A_PH_WA,A_ZN_EDTA = A_ZN_EDTA)]
  dt[B_COUNTRY == 'FI', value := NA_real_]
  
  # Hungary (HU), Ireland (IE), Italy (IT), Latvia (LV), Lithuania (LT)
  dt[B_COUNTRY == 'HU', value := NA_real_]
  dt[B_COUNTRY == 'IE', value := osi_c_zinc_ie(B_LU = B_LU,A_SOM_LOI = A_SOM_LOI, A_PH_WA = A_PH_WA, A_ZN_EDTA = A_ZN_EDTA)]
  dt[B_COUNTRY == 'IT', value := NA_real_]
  dt[B_COUNTRY == 'LV', value := NA_real_]
  dt[B_COUNTRY == 'LT', value := NA_real_]
  
  # the Netherlands (NL), Norway (NO),  Sweden (SE), Slovak Republic (SK), Slovenia (SL)
  dt[B_COUNTRY == 'NL', value := osi_c_zinc_nl(B_LU = B_LU,A_PH_CC = A_PH_CC,A_ZN_CC = A_ZN_CC)]
  dt[B_COUNTRY == 'NO', value := NA_real_]
  dt[B_COUNTRY == 'SE', value := NA_real_]
  dt[B_COUNTRY == 'SK', value := NA_real_]
  dt[B_COUNTRY == 'SL', value := NA_real_]
  
  # Poland (PL), Portugal (PT), Romania (RO), United Kingdom (UK)
  dt[B_COUNTRY == 'PL', value := NA_real_]
  dt[B_COUNTRY == 'PT', value := osi_c_zinc_pt(B_LU = B_LU, A_ZN_AAA = A_ZN_AAA, A_PH_CC = A_PH_CC)]
  dt[B_COUNTRY == 'RO', value := osi_c_zinc_ro(B_LU = B_LU, A_ZN_EDTA = A_ZN_EDTA, A_P_AL = A_P_AL,A_PH_WA = A_PH_WA)]
  dt[B_COUNTRY == 'UK', value := osi_c_zinc_uk(B_LU = B_LU,A_PH_WA = A_PH_WA, A_ZN_EDTA = A_ZN_EDTA)]
  
  # select the output variable
  out <- dt[,value]
  
  # return the OSI score
  return(out)
  
}

#' Calculate the zinc availability index in Germany
#' 
#' This function calculates the zinc availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_C_OF (numeric) The carbon content of the soil layer (g/ kg)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_ZN_EDTA (numeric) Zn content measured in EDTA (mg / kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_zinc_de(B_LU = '3301061299',A_C_OF=25, A_CLAY_MI=5,A_SAND_MI=15,A_ZN_EDTA = 50)
#'  
#' @return 
#' The zinc availability index in Germany derived from extractable soil Zn fractions. A numeric value.
#' 
#' @export
osi_c_zinc_de <- function(B_LU, A_C_OF, A_CLAY_MI,A_SAND_MI,A_ZN_EDTA) {

  # add visual bindings
  stype = . = crop_name = crop_code = crop_cat1 = osi_country = A_SILT_MI = NULL
  
  # crop properties
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='DE']
  
  # argument length
  arg.length <- max(length(B_LU),length(A_CLAY_MI),length(A_C_OF),
                    length(A_SAND_MI),length(A_ZN_EDTA))
  
  # check inputs
  osi_checkvar(parm = list(B_LU = B_LU,
                           B_COUNTRY = rep('DE',arg.length),
                           A_CLAY_MI = A_CLAY_MI,
                           A_SAND_MI = A_SAND_MI,
                           A_C_OF = A_C_OF,
                           A_ZN_EDTA = A_ZN_EDTA),
               fname = 'osi_c_zinc_de')
  
  # internal data.table
  dt <- data.table(id = 1: arg.length,
                   B_LU = B_LU,
                   A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_ZN_EDTA = A_ZN_EDTA,
                   value = NA_real_)
  
  # add soil type
  dt[A_SAND_MI >= 85 & A_SILT_MI <= 25 & A_CLAY_MI <= 5 & A_C_OF < 150, stype := "BG1"]
  dt[A_SAND_MI >= 42 & A_SAND_MI <= 95 & A_SILT_MI <= 40 & A_CLAY_MI <= 17 & A_C_OF < 150,  stype :="BG2"]
  dt[A_SAND_MI >= 33 & A_SAND_MI <= 83 & A_SILT_MI <= 50 & A_CLAY_MI >= 8 & A_CLAY_MI <= 25 & A_C_OF < 150,  stype :="BG3"]
  dt[A_SAND_MI <= 75 & A_SILT_MI <= 100 & A_CLAY_MI <= 35 & A_C_OF < 150,  stype :="BG4"]
  dt[A_SAND_MI <= 65 & A_SILT_MI <= 75 & A_CLAY_MI >= 25 & A_CLAY_MI <= 100 & A_C_OF < 150, stype := "BG5"]
  dt[ A_C_OF >= 150,  stype := "BG6"]
  
  # merge with crop
  dt <- merge(dt,
              dt.crops[,.(crop_code, crop_name, crop_cat1)],
              by.x = 'B_LU',
              by.y = 'crop_code',
              all.x = TRUE)
  
  # evaluate A_B_HW for arable soils
  dt[stype %in% c('BG1','BG2') & crop_cat1=='arable', value := osi_evaluate_logistic(A_ZN_EDTA, b = 3.0751854532, x0 = -2.1435147033, v = 0.0001590648)]
  dt[stype %in% c('BG3','BG4','BG5') & crop_cat1=='arable', value := osi_evaluate_logistic(A_ZN_EDTA, b = 5.213396, x0 = 1.492806 , v = 1.886530)]
  dt[stype=='BG6' & crop_cat1=='arable', value := osi_evaluate_logistic(A_ZN_EDTA, b = 5.492159925  , x0 = -0.463183003, v = 0.009234246)]
  
  # evalute A_B_HW for grassland (no richtwerte existieren)
  dt[crop_cat1 != 'arable', value := 1]
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
}


#' Calculate the Zn availability index for agricultural soils in France 
#' 
#' This function calculates the Zn availability of a soil, using the agronomic index used in France.
#' 
#' @param B_LU  (character) crop type 
#' @param A_PH_WA (numeric) pH measured in water (-)
#' @param A_ZN_EDTA (numeric) Zn content measured in EDTA (mg / kg)
#'
#' @import data.table
#' 
#' @examples 
#' osi_c_zinc_fr(B_LU = 'SOJ', A_ZN_EDTA = 45, A_PH_WA = 6.5)
#' 
#' @return 
#' The zinc availability index in France estimated from extractable zinc and pH measured in water, a numeric value.
#' 
#' @export
osi_c_zinc_fr <- function(B_LU, A_PH_WA, A_ZN_EDTA) {
  
  # set visual bindings
  value = osi_country = osi_indicator = id = crop_cat1 = NULL
  osi_crops = cat_zn = osi_st_c1 = osi_st_c2 = osi_st_c3 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # load in thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='FR' & osi_indicator=='i_c_zn']
  checkmate::assert_data_table(dt.thresholds,max.rows = 2)
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_ZN_EDTA),length(A_PH_WA))
  
  # check inputs
  osi_checkvar(parm = list(B_LU = B_LU,
                           B_COUNTRY = rep('FR',arg.length),
                           A_PH_WA = A_PH_WA,
                           A_ZN_EDTA = A_ZN_EDTA),
               fname = 'osi_c_zinc_fr')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_ZN_EDTA = A_ZN_EDTA,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # add category based in pH-water
  dt[, cat_zn := ifelse(A_PH_WA <= 6.2,'acid', 'alkaline')]
  
  # merge with threshold
  dt <- merge(dt,
              dt.thresholds,
              by.x = 'cat_zn',
              by.y = 'osi_threshold_soilcat',
              all.x = TRUE)
  
  # convert to the OSI score only for in and mais soils: DLN, MID, MIE and MIS crops
  dt[, value := osi_evaluate_logistic(A_ZN_EDTA,b = osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  dt[!B_LU %in% c('DLN','MID','MIE','MIS','3301010600','3301010699','3301090400'), value := 1]

  # Sort the input in correct order
  setorder(dt, id)
  
  # select and return OSI indicator
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the Zn availability index for agricultural soils in Ireland 
#' 
#' This function calculates the Zn availability of a soil, using the agronomic index used in Ireland.
#' 
#' @param B_LU  (character) crop type 
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_PH_WA (numeric) pH measured in water (-)
#' @param A_ZN_EDTA (numeric) Zn content measured in EDTA (mg / kg)
#'
#' @import data.table
#' 
#' @examples 
#' osi_c_zinc_ie(B_LU = 'testcrop', A_SOM_LOI = 4, A_ZN_EDTA = 45, A_PH_WA = 6.5)
#' 
#' @return 
#' The zinc availability index in Ireland estimated from extractable zinc and pH measured in water, a numeric value.
#' 
#' @export
osi_c_zinc_ie <- function(B_LU,A_SOM_LOI, A_PH_WA, A_ZN_EDTA) {
  
  # set visual bindings
  value = osi_country = osi_indicator = id = crop_cat1 = NULL
  BD = osi_crops = cat_zn = osi_st_c1 = osi_st_c2 = osi_st_c3 = NULL
  
  # Load in the datasets
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country == 'IE']
  
  # Load in the parameter set (to set min and max, to be done later)
  # dt.parms <- as.data.table(euosi::osi_parms)
  
  # load in thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country=='UK' & osi_indicator=='i_c_zn']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_ZN_EDTA),length(A_PH_WA),length(A_SOM_LOI))
  
  # check inputs
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,
                           A_PH_WA = A_PH_WA,
                           A_ZN_EDTA = A_ZN_EDTA),
               fname = 'osi_c_zinc_ie')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_ZN_EDTA = A_ZN_EDTA,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # merge with threshold
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = 'cat_zn',
  #             by.y = 'osi_threshold_soilcat',
  #             all.x = TRUE)
  
  # convert from mg / kg to mg / liter sample volume
  dt[, BD := (1/(0.02525 * A_SOM_LOI + 0.6541))]
  dt[,A_ZN_EDTA := A_ZN_EDTA * BD]
  
  # set OSI score
  dt[, value := osi_evaluate_logistic(A_ZN_EDTA,b = 8.719122,x0 = -0.9514568,v =  1.134101e-05)]
  
  # increase risk at high pH
  dt[A_PH_WA > 7, value := value * 0.8]
  
  # Sort the input in correct order
  setorder(dt, id)
  
  # select and return OSI indicator
  value <- dt[, value]
  
  return(value)
  
}
#' Calculate the Zn availability index for the Netherlands
#' 
#' This function calculates the availability of Zn for plant uptake
#' 
#' @param B_LU (numeric) The crop code from the BRP
#' @param A_PH_CC (numeric) The acidity of the soil, determined in 0.01M CaCl2 (-)
#' @param A_ZN_CC The plant available Zn content, extracted with 0.01M CaCl2 (mg / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_zinc_nl(B_LU = '265', A_ZN_CC = 45, A_PH_CC = 6.5)
#' 
#' @return 
#' The function of the soil to supply zinc (a numeric value).
#' 
#' @export
osi_c_zinc_nl <- function(B_LU, A_PH_CC, A_ZN_CC) {
  
  # set visual bindings
  id = crop_code = soiltype = soiltype.n = crop_n = crop_category = D_ZN = NULL
  osi_country = osi_indicator = crop_cat1 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'NL']
  
  # load and subset thresholds for situation in NL
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='NL' & osi_indicator=='i_c_zn']
  
  # Check input
  arg.length <- max(length(B_LU), length(A_ZN_CC), length(A_PH_CC))
  
  # check inputs
  osi_checkvar(parm = list(B_LU = B_LU,
                           B_COUNTRY = rep('NL',arg.length),
                           A_PH_CC = A_PH_CC,
                           A_ZN_CC = A_ZN_CC),
               fname = 'osi_c_zinc_nl')
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = as.character(B_LU),
                   A_PH_CC = A_PH_CC,
                   A_ZN_CC = A_ZN_CC,
                   D_ZN = NA_real_,
                   value = NA_real_
                  )
  
  # merge properties form crop category and soil type
  dt <- merge(dt, 
              dt.crops[, list(crop_code, crop_cat1)],
              by.x = "B_LU", by.y = "crop_code")

  # Calculate Zn-availability
  dt[crop_cat1 =='arable', D_ZN := 10^(0.88 + 0.56 * log10(A_ZN_CC*0.001) + 0.13 * A_PH_CC)]
  dt[crop_cat1 =='maize', D_ZN := 10^(0.88 + 0.56 * log10(A_ZN_CC*0.001) + 0.13 * A_PH_CC)]
  dt[crop_cat1 %in% c('nature','forest','other','permanent'), D_ZN := 0]
  dt[crop_cat1 =='grassland', D_ZN := 10^(-1.04 + 0.67 * log10(A_ZN_CC*0.001) + 0.5 * A_PH_CC)]
  
  # Too high values for Zn-availability are prevented
  dt[D_ZN > 250, D_ZN := 250]
  
  # convert to OSI score
  dt[, value := osi_evaluate_parabolic(D_ZN,x.top = dt.thresholds$osi_st_c1)]
  
  # Sort the input in correct order
  setorder(dt, id)
  
  # select and return OSI indicator
  value <- dt[, value]
  
  return(value)
}

#' Calculate the Zn availability index for Portugal
#' 
#' This function calculates the availability of Zn for plant uptake
#' 
#' @param B_LU (numeric) A unique crop code
#' @param A_ZN_AAA The plant available Zn content, extracted with ammonium acetate (mg Zn / kg)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-) 
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_zinc_pt(B_LU = '3301061299', A_ZN_AAA = 45,A_PH_CC = 4.5)
#' 
#' @return 
#' The function of the soil to supply zinc (a numeric value).
#' 
#' @export
osi_c_zinc_pt <- function(B_LU, A_ZN_AAA,A_PH_CC) {
  
  # set visual bindings
  id = crop_code = soiltype = soiltype.n = crop_n = crop_category = D_ZN = NULL
  osi_country = osi_indicator = crop_cat1 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'PT']
  
  # Check input
  arg.length <- max(length(B_LU), length(A_ZN_AAA))
  
  # check inputs
  osi_checkvar(parm = list(B_LU = B_LU,
                           B_COUNTRY = rep('PT',arg.length),
                           A_PH_CC = A_PH_CC,
                           A_ZN_AAA = A_ZN_AAA),
               fname = 'osi_c_zinc_pt')
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = as.character(B_LU),
                   A_PH_CC = A_PH_CC,
                   A_ZN_AAA = A_ZN_AAA,
                   value = NA_real_ )
  
  # merge properties form crop category and soil type
  dt <- merge(dt, 
              dt.crops[, list(crop_code, crop_cat1)],
              by.x = "B_LU", 
              by.y = "crop_code",
              all.x=TRUE)
  
  # convert to OSI score
  dt[, value := osi_evaluate_logistic(A_ZN_AAA,b=9.34426, x0 = -1.074980e-03,v = 3.284924e-05)]
  
  # increase risk at high pH
  dt[A_PH_CC > 7, value := value * 0.8]
  
  # Sort the input in correct order
  setorder(dt, id)
  
  # select and return OSI indicator
  value <- dt[, value]
  
  return(value)
}

#' Calculate the Zn availability index for agricultural soils in Romenia 
#' 
#' This function calculates the Zn availability of a soil, using the agronomic index used in RO.
#' 
#' @param B_LU  (character) crop type 
#' @param A_PH_WA (numeric) pH measured in water (-)
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate (mg P / kg)
#' @param A_ZN_EDTA (numeric) Zn content measured in EDTA, pH 8.6, with ammonium carbonate (mg / kg)
#'
#' @import data.table
#' 
#' @examples 
#' osi_c_zinc_ro(B_LU = 'testcrop', A_ZN_EDTA = 45, A_P_AL = 45,A_PH_WA = 6.5)
#' 
#' @return 
#' The zinc availability index in United Kingdom estimated from extractable zinc and pH measured in water, a numeric value.
#' 
#' @export
osi_c_zinc_ro <- function(B_LU, A_PH_WA, A_P_AL, A_ZN_EDTA) {
  
  # set visual bindings
  value = osi_country = osi_indicator = id = crop_cat1 = . = osi_crops = v1 = v2 = NULL
  
  # Load in the datasets
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country == 'RO']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_PH_WA),length(A_P_AL), length(A_ZN_EDTA))
  
  # check inputs
  osi_checkvar(parm = list(A_PH_WA = A_PH_WA,
                           A_P_AL = A_P_AL,
                           A_ZN_EDTA = A_ZN_EDTA),
               fname = 'osi_c_zinc_ro')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_ZN_EDTA = A_ZN_EDTA,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # retreive proxies of Borlan et al. (1975, 1982)
  # https://www.icpa.ro/documente/coduri/Evaluarea_continutului_de_nutrienti_din_sol.pdf
  dt[A_PH_WA <= 8, fr := 1.3 * A_PH_WA - 0.11 * A_PH_WA^2 - 2.82]
  dt[A_PH_WA > 8, fr:= 1.3 * A_PH_WA - 0.11 * A_PH_WA^2 -2.282 + (A_PH_WA - 8) * (0.05 * A_PH_WA)]
  dt[, irpm := 90 - 10 * A_PH_WA / A_P_AL]
  dt[, iczn := A_ZN_EDTA * fr * 100 / A_P_AL]
  
  # Zn deficiency occurs in soils where the IRPM and ICZn values are lower than 0.384 and 1.7, respectively.
  
  # set OSI score
  dt[, v1 := osi_evaluate_logistic(irpm,b = 32.7925540, x0 = 0.3531964, v =  3.6784009 )]
  dt[, v2 := osi_evaluate_logistic(iczn,b = 13.3831928, x0 = 0.5762774, v =  1.0695647)]
  
  # combine both risksindicators
  dt[!is.na(v1) & is.na(v2), value := v1]
  dt[is.na(v1) & !is.na(v2), value := v2]
  dt[!is.na(v1) & !is.na(v2), value := (cf_ind_importance(v1)*v1 + cf_ind_importance(v2) * v2)/
                                       (cf_ind_importance(v1) + cf_ind_importance(v2))]
  
  # Sort the input in correct order
  setorder(dt, id)
  
  # select and return OSI indicator
  value <- dt[, value]
  
  return(value)
  
}
#' Calculate the Zn availability index for agricultural soils in United Kingdom 
#' 
#' This function calculates the Zn availability of a soil, using the agronomic index used in UK.
#' 
#' @param B_LU  (character) crop type 
#' @param A_PH_WA (numeric) pH measured in water (-)
#' @param A_ZN_EDTA (numeric) Zn content measured in EDTA (mg / kg)
#'
#' @import data.table
#' 
#' @examples 
#' osi_c_zinc_uk(B_LU = 'testcrop', A_ZN_EDTA = 45, A_PH_WA = 6.5)
#' 
#' @return 
#' The zinc availability index in United Kingdom estimated from extractable zinc and pH measured in water, a numeric value.
#' 
#' @export
osi_c_zinc_uk <- function(B_LU, A_PH_WA, A_ZN_EDTA) {
  
  # set visual bindings
  value = osi_country = osi_indicator = id = crop_cat1 = NULL
  osi_crops = cat_zn = osi_st_c1 = osi_st_c2 = osi_st_c3 = NULL
  
  # Load in the datasets
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country == 'UK']
  
  # load in thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country=='UK' & osi_indicator=='i_c_zn']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_ZN_EDTA),length(A_PH_WA))
  
  # check inputs
  osi_checkvar(parm = list(A_PH_WA = A_PH_WA,
                           A_ZN_EDTA = A_ZN_EDTA),
               fname = 'osi_c_zinc_uk')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_ZN_EDTA = A_ZN_EDTA,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # set OSI score with optimum around 1.5 (possible deficiency) and 0.5 (probably deficiency)
  dt[, value := osi_evaluate_logistic(A_ZN_EDTA,b = 8.719122,x0 = -0.9514568,v =  1.134101e-05)]

  # increase risk at high pH
  dt[A_PH_WA > 6.2, value := value * 0.8]
  
  # Sort the input in correct order
  setorder(dt, id)
  
  # select and return OSI indicator
  value <- dt[, value]
  
  return(value)
  
}