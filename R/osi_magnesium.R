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
  
  # Calculate the Mg availability for arable land -----
  dt.arable <- dt[crop_category == "akkerbouw"]
  dt.arable[,D_MG := A_MG_CC]
  
  # Calculate the Mg availability for maize land -----
  dt.maize <- dt[crop_category == "mais"]
  dt.maize[,D_MG := A_MG_CC]
  
  # Calculate Mg availability for grassland on sandy and loamy soils -----
  dt.grass.sand <- dt[crop_category == "grasland" & grepl('zand|loess|dalgrond',B_SOILTYPE_AGR)]
  dt.grass.sand[,D_MG := A_MG_CC]
  
  # Calculate Mg availability for grassland on clay and peat soils ----- 
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
