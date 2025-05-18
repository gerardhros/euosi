#' Calculate the boron availability index (wrapper function)
#' 
#' This function calculates the boron availability for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_B_HW (numeric) The plant available content of B in the soil (mg  Cu per kg) extracted by hot water
#' @param A_CLAY_MI (numeric) The clay content (\%)
#' @param B_COUNTRY (character) The country code
#' 
#' @import data.table
#' 
#' @return
#' The capacity of the soil to supply and buffer boron, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_c_boron <- function(B_LU,A_CLAY_MI,A_B_HW, B_COUNTRY) {
  
  # add visual bindings
  i_c_bo = NULL
  
  # desired length of inputs
  arg.length <- max(length(A_CLAY_MI), length(A_B_HW), length(B_LU))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI = A_CLAY_MI,
                   A_B_HW = A_B_HW,
                   B_LU = B_LU,
                   value = NA_real_
                   )
  
  # calculate the open soil index score for magnesium availability per country
  dt[B_COUNTRY == 'FR', value := osi_c_boron_fr(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI,A_B_HW = A_B_HW)]
  
  # select the output variable
  value <- dt[,value]
  
  # return the OSI score
  return(value)
  
}

#' Calculate the boron availability index in Switzerland
#' 
#' This function calculates the boron availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_B_HW (numeric) The plant available content of B in the soil (mg B per kg) extracted by hot water 
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_boron_ch(B_LU = 265,A_B_HW = 50)
#' osi_c_boron_ch(B_LU = c(265,1019),A_B_HW = c(35,55))
#' 
#' @return 
#' The boron availability index in Switzerland derived from extractable soil B fractions. A numeric value.
#' 
#' @export
osi_c_boron_ch <- function(B_LU, A_B_HW) {
  
  # crop properties
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_B_HW = A_B_HW,
                   value = NA_real_)
  
  # merge with crop
  dt <- merge(dt,
              dt.crops[,.(B_LU, crop_name, crop_cat1)],
              by = 'B_LU',
              all.x = TRUE)
  
  # evaluate risk based OSI
  dt[,value := osi_evaluate_logistic(x = A_B_HW, b = 3.12505457, x0 = -0.24198833, v = 0.03220845)]
  
  # set risk cases for specific cases
  dt[, senscrop := fifelse(grepl('ruben|raps|sonnen|sellerie|reben|kernobst|steinobst',tolower(crop_name)),'yes','no')]
  
  # OSI score only for bron sensitive crops, others no issue
  dt[senscrop=='no',value := pmin(1,2 * value)]
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the boron availability index in Germany
#' 
#' This function calculates the boron availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_C_OF (numeric) The carbon content of the soil layer (g/ kg)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_B_HW (numeric) The plant available content of B in the soil (mg B per kg) extracted by hot water 
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_boron_de(B_LU = 265,A_C_OF=25, A_CLAY_MI=5,A_SAND_MI=15,A_PH_CC = 4,A_B_HW = 50)
#'  
#' @return 
#' The boron availability index in Germany derived from extractable soil B fractions. A numeric value.
#' 
#' @export
osi_c_boron_de <- function(B_LU, A_C_OF, A_CLAY_MI,A_SAND_MI,A_PH_CC,A_B_HW) {
  
  # crop properties
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_C_OF= A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_CC = A_PH_CC,
                   A_B_HW = A_B_HW,
                   value = NA_real_)
  
  # add soil type
  dt.ge[A_SAND_MI >= 85 & A_SILT_MI <= 25 & A_CLAY_MI <= 5 & A_C_OF < 150, stype := "BG1"]
  dt.ge[A_SAND_MI >= 42 & A_SAND_MI <= 95 & A_SILT_MI <= 40 & A_CLAY_MI <= 17 & A_C_OF < 150,  stype :="BG2"]
  dt.ge[A_SAND_MI >= 33 & A_SAND_MI <= 83 & A_SILT_MI <= 50 & A_CLAY_MI >= 8 & A_CLAY_MI <= 25 & A_C_OF < 150,  stype :="BG3"]
  dt.ge[A_SAND_MI <= 75 & A_SILT_MI <= 100 & A_CLAY_MI <= 35 & A_C_OF < 150,  stype :="BG4"]
  dt.ge[A_SAND_MI <= 65 & A_SILT_MI <= 75 & A_CLAY_MI >= 25 & A_CLAY_MI <= 100 & A_C_OF < 150, stype := "BG5"]
  dt.ge[ A_C_OF >= 150,  stype := "BG6"]
  
  # merge with crop
  dt <- merge(dt,
              dt.crops[,.(B_LU, crop_name, crop_cat1)],
              by = 'B_LU',
              all.x = TRUE)
  
  # evaluate A_B_HW for arable soils
  dt[stype=='BG1' & crop_cat1=='arable', value := osi_evaluate_logistic(A_B_HW, b = 8.341249, x0 = -1.551311, v = 1.144485e-06)]
  dt[stype=='BG2' & crop_cat1=='arable', value := osi_evaluate_logistic(A_B_HW, b = 26.0637369, x0 = 0.1613828  , v = 0.7724603 )]
  dt[stype=='BG3' & crop_cat1=='arable', value := osi_evaluate_logistic(A_B_HW, b = 14.316871145, x0 = -0.261544761, v = 0.001656784 )]
  dt[stype=='BG4' & crop_cat1=='arable', value := osi_evaluate_logistic(A_B_HW, b = 5.394509, x0 = -2.323976, v = 1.053392e-06)]
  dt[stype=='BG5' & crop_cat1=='arable', value := osi_evaluate_logistic(A_B_HW, b = 5.394509, x0 = -2.323976, v = 1.053392e-06)]
  dt[stype=='BG6' & crop_cat1=='arable', value := osi_evaluate_logistic(A_B_HW, b = 16.953310497 , x0 = -0.225954413  , v = 0.003793037 )]
  
  # evalute A_B_HW for grassland (no richtwerte existieren)
  dt[crop_cat1 != 'arable', value := 1]
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the boron availability index in Ireland
#' 
#' This function calculates the boron availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_B_HW (numeric) The plant available content of B in the soil (mg B per kg) extracted by hot water 
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_boron_ie(B_LU = 265,A_B_HW = 50)
#' osi_c_boron_ie(B_LU = c(265,1019),A_B_HW = c(35,55))
#' 
#' @return 
#' The boron availability index in Ireland derived from extractable soil B fractions. A numeric value.
#' 
#' @export
osi_c_boron_ie <- function(B_LU, A_B_HW) {
  
  # crop properties
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_B_HW = A_B_HW,
                   value = NA_real_)
  
  # merge with crop
  dt <- merge(dt,
              dt.crops[,.(B_LU, crop_name, crop_cat1)],
              by = 'B_LU',
              all.x = TRUE)
  
  # evaluate risk based OSI
  dt[,value := osi_evaluate_logistic(x = A_B_HW, b = 3.7254881, x0 = 0.3723286 , v = 0.4226000 )]
  
  # set risk cases for specific cases
  dt[, senscrop := fifelse(grepl('swedes|turnip|rape|beet|mangel|celery|carrot|brassica|radish|cabbage|cauliflo|broccol|sprout',tolower(crop_name)),'yes','no')]
  
  # OSI score only for bron sensitive crops, others no issue
  dt[senscrop=='no',value := 1]
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the B availability index for agricultural soils in France 
#' 
#' This function calculates the B availability of a soil, using the agronomic index used in France
#' 
#' @param B_LU (character) The crop type
#' @param A_CLAY_MI (numeric) The clay content (\%)
#' @param A_B_HW (numeric) The plant available content of B in the soil (mg B per kg) extracted by hot water 
#'
#' @import data.table
#' 
#' @return 
#' The boron availability index in France estimated from extractable boron, clay, A numeric value.
#' 
#' @export
osi_c_boron_fr <- function(B_LU,A_CLAY_MI, A_B_HW) {
  
  # set visual bindings
  i_c_bo = osi_country = osi_indicator = id = crop_cat1 = NULL
  soil_cat_bo = osi_st_c1 = osi_st_c2 = osi_st_c3 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # Load in parms dataset (to be used later for upper and lowe rlimits)
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # load the threshold values
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='FR' & osi_indicator=='i_c_b']
  
  # Check length of desired input
  arg.length <- max(length(A_CLAY_MI),length(A_B_HW),length(B_LU))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_B_HW, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_data_table(dt.thresholds,max.rows = 2)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_B_HW = A_B_HW,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # set soil class for merging with threshold
  dt[, soil_cat_bo := fifelse(A_CLAY_MI > 50,'clay','other')]
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = 'soil_cat_bo',
              by.y = 'osi_threshold_soilcat',
              all.x = TRUE)
  
  # convert to the OSI score
  dt[, value := evaluate_logistic(x = A_B_HW, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # exlcude other crops that a subset
  dt[!B_LU %in% c('DFV','TRN','FVL'), value := 1]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select and return value
  value <- dt[,value]
  
  return(value)
  
}

#' Calculate the B availability index for agricultural soils in the Netherlands 
#' 
#' This function calculates the B availability of a soil, using the agronomic index used in France
#' 
#' @param B_LU (character) The crop type
#' @param A_CLAY_MI (numeric) The clay content (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_B_HW (numeric) The plant available content of B in the soil (mg B per kg) extracted by hot water 
#'
#' @import data.table
#' 
#' @return 
#' The boron availability index in the Netherlands estimated from hot water extractable boron, and clay, A numeric value.
#' 
#' @export
osi_c_boron_nl <- function(B_LU,A_CLAY_MI, A_SOM_LOI,A_B_HW) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  soil_cat_bo = osi_st_c1 = osi_st_c2 = osi_st_c3 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'NL']
  
  # Load in parms dataset (to be used later for upper and lowe rlimits)
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # load the threshold values
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='NL' & osi_indicator=='i_c_b']
  
  # Check length of desired input
  arg.length <- max(length(A_CLAY_MI),length(A_B_HW),length(B_LU))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_B_HW, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_data_table(dt.thresholds,max.rows = 2)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_B_HW = A_B_HW,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # merge with crop
  dt <- merge(dt,
              dt.crop[,.(B_LU, crop_cat1)],
              by = 'B_LU',
              all.x = TRUE)
  
  
  
  # set soil class for merging with threshold
  dt[, soil_cat_bo := fifelse(A_CLAY_MI > 50,'clay','other')]
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = 'soil_cat_bo',
              by.y = 'osi_threshold_soilcat',
              all.x = TRUE)

  #add B recommendation
  dt <- data.table(A_B_HWA = seq(0,1,0.05))
  dt[,value := 0]
  dt[A_B_HWA < 0.2,value := 0.4]
  dt[A_B_HWA >= 0.2 & A_B_HWA < 0.3, value := 0.3]
  dt[A_B_HWA >= 0.3 & A_B_HWA < 0.35, value := 0.2]
  plot(I(0.4-value)~A_B_HWA,data=dt,type='l')
  a = seq(0,1,0.05)
  lines(a, OBIC::evaluate_logistic(a, b = 4.2, x0 = 1.2, v = 1.2),col='blue')
    
  # convert to the OSI score
  dt[, value := evaluate_logistic(x = A_B_HW, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # exlcude other crops that a subset
  dt[!B_LU %in% c('DFV','TRN','FVL'), value := 1]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select and return value
  value <- dt[,value]
  
  return(value)
  
}

#' Calculate the boron availability index in United Kingdom
#' 
#' This function calculates the boron availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_TEXTURE_HYPRES (character) The soil texture according to HYPRES classification system
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_B_HW (numeric) The plant available content of B in the soil (mg B per kg) extracted by hot water 
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_boron_uk(B_LU = 265,B_TEXTURE_HYPRES='C',A_SOM_LOI=3,A_PH_CC = 4,A_B_HW = 50)
#' osi_c_boron_uk(B_LU = c(265,1019),B_TEXTURE_HYPRES = c('C','F'),A_SOM_LOI = c(3,3),A_PH_CC = c(4,6),A_B_HW = c(35,55))
#' 
#' @return 
#' The boron availability index in UK derived from extractable soil B fractions. A numeric value.
#' 
#' @export
osi_c_boron_uk <- function(B_LU, B_TEXTURE_HYPRES,A_SOM_LOI,A_PH_CC,A_B_HW) {
  
  # crop properties
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   B_TEXTURE_HYPRES = B_TEXTURE_HYPRES,
                   A_PH_CC = A_PH_CC,
                   A_SOM_LOI = A_SOM_LOI,
                   A_B_HW = A_B_HW,
                   value = NA_real_)
  
  # merge with crop
  dt <- merge(dt,
              dt.crops[,.(B_LU, crop_name, crop_cat1)],
              by = 'B_LU',
              all.x = TRUE)
  
  # convert from mg / kg to mg / liter sample volume
  dt[, BDS := (1/(0.02525 * A_SOM_LOI + 0.6541))]
  dt[,A_B_HW := A_B_HW * BDS]
  
  # evaluate risk based on optimum value of 0.8 mg B per liter dry soil
  dt[,value := osi_evaluate_logistic(x = A_B_HW, b = 29.1874386, x0 = 0.6872707, v = 5.1035358)]
  
  # set risk cases for specific cases
  dt[, senscrop := fifelse(grepl('beet|carrot|brassica|radish|cabbage|cauliflo|broccol|sprout',tolower(crop_name)),'yes','no')]
  dt[B_TEXTURE_HYPRES %in% c('C') & A_PH_WA > 6.5 & senscrop=='yes',value := value * 0.5]
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
}


