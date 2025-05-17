#' Calculate the pH index (wrapper function)
#' 
#' This function calculates the pH index for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_TEXTURE_USDA (character) The USDA textural class
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
osi_c_ph <- function(B_LU, B_TEXTURE_USDA= NA_real_, A_PH_WA = NA_real_, A_PH_CC= NA_real_, A_PH_KCL= NA_real_,B_COUNTRY) {
  
  # add visual bindings
  value = NULL
  
  # desired length of inputs
  arg.length <- max(length(A_PH_CC), length(A_PH_KCL),length(A_PH_WA), length(B_LU),
                    length(B_TEXTURE_USDA),length(B_LU),length(B_COUNTRY))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA=B_TEXTURE_USDA,
                   A_PH_CC = A_PH_CC,
                   A_PH_WA = A_PH_WA,
                   A_PH_KCL = A_PH_KCL,
                   value = NA_real_  
                   )
  
  # calculate the open soil index score for pH per country
  dt[B_COUNTRY == 'FR', value := osi_c_ph_fr(B_LU = B_LU,
                                             B_TEXTURE_USDA = B_TEXTURE_USDA,
                                             A_PH_WA = A_PH_WA)]
  
  # select the output variable
  value <- dt[,value]
  
  # return the OSI score
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
  
  # crop data
  # dt.crops <- as.data.table(euosi::osi_crops)
  # dt.crops <- dt.crops[osi_country=='DE']
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
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
#' @param B_TEXTURE_USDA (numeric) The soil textural class
#' @param A_PH_WA (numeric) The pH measured in H2O
#' 
#' @import data.table
#' 
#' @return 
#' The pH index in France estimated from pH in water, the textural class and the crop type
#' 
#' @export
osi_c_ph_fr <- function(B_LU,B_TEXTURE_USDA, A_PH_WA) {
  
  # set visual bindings
  osi_c_ph_fr = osi_country = osi_indicator = id = crop_cat1 = NULL
  cat_crop_ph = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # Load in the parameter set (to set min and max, to be done later)
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # load in thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='FR' & osi_indicator=='i_c_ph']
  
  # Check length of desired input
  arg.length <- max(length(A_PH_WA),length(B_TEXTURE_USDA),length(B_LU))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_PH_WA, lower = 0, upper = 14, any.missing = TRUE, len = arg.length)
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA = B_TEXTURE_USDA,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # merge subset on crop class
  dt.sb <- dt[B_LU %in% c('BTN','BVF')][,cat_crop_ph := 'sugar beet']
  dt.sb <- merge(dt.sb,
                 dt.thresholds,
                 by.x = 'cat_crop_ph',
                 by.y = 'osi_threshold_cropcat',
                 all.x = TRUE)
  
  # merge subset on soil texture class
  dt.other <- dt[B_LU %in% c('BTN','BVF')]
  dt.other <- merge(dt.other,
                    dt.thresholds,
                    by.x = 'B_TEXTURE_USDA',
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
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = crop_k = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='BE']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'BE' & osi_indicator =='i_c_ph']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_BE = B_TEXTURE_BE,
                   A_PH_KCL = A_PH_KCL,
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
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FI']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_ph']
  
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
#' osi_c_ph_ie(B_LU = 265,A_PH_WA = 5)
#' osi_c_ph_ie(B_LU = c(265,1019),A_PH_WA = c(3.5,5.5))
#' 
#' @return 
#' The pH index in Ireland. A numeric value.
#' 
#' @export
osi_c_ph_ie <- function(B_LU, A_PH_WA,A_SOM_LOI) {
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='IE']
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_P_WA = A_P_WA,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_name)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # pH evaluation for peat soils
  dt[A_SOM_LOI > 20, value := osi_evaluate_logistic(A_P_WA, b = 8.2990603 , x0 = 4.8541180 , v = 0.1105874)]
  
  # pH evaluation
  dt[grepl('potato',tolower(crop_name)), value := osi_evaluate_logistic(A_P_WA, b = 5.364616, x0 = 2.668736, v = 2.112456e-06)]
  dt[grepl('grass',tolower(crop_name)), value := osi_evaluate_logistic(A_P_WA, b = 3.70689744, x0 = 4.31512440, v = 0.03378904)]
  dt[grepl('cereal|wheat|maiz|corn|tritic|barley',tolower(crop_name)), value := osi_evaluate_logistic(A_P_WA, b = 3.639040494, x0 = 3.910131651, v = 0.008373893 )]
  dt[grepl('clover|beet|beans|peas|rape',tolower(crop_name)), value := osi_evaluate_logistic(A_P_WA, b = 2.51825519, x0 = 3.90417201, v = 0.01634118  )]
  dt[is.na(value),value := osi_evaluate_logistic(A_P_WA, b = 2.48634015 , x0 = 3.93503547, v = 0.01855978)]
  
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
#' @param B_LU_BRP (numeric) The crop code from the BRP
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
  D_PH_DELTA = i_c_ph = . = oid = NULL
  
  # Check inputs
  arg.length <- max(length(A_PH_CC), length(B_SOILTYPE_AGR), length(A_SOM_LOI), length(A_CLAY_MI),
                    length(B_LU))
  checkmate::assert_numeric(A_PH_CC, lower = 2, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices =euosi::osi_soiltype[osi_country=='NL',osi_soil_cat1])
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(euosi::osi_crops[osi_country=='NL',crop_code]), empty.ok = FALSE)
  
  # Collect information in table
  dt <- data.table(FIELD_ID = ID,
                   oid = 1:arg.length,
                   B_LU_BRP = B_LU_BRP,
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
