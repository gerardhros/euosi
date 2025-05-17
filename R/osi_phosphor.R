#' Calculate the phosphate availability index (wrapper function)
#' 
#' This function calculates the phosphate availability for all European countries (if available). 
#' 
#' @param B_LU (character) The crop code
#' @param B_SOILTYPE_AGR (character) The soil type in a particular region
#' @param B_AER_FR (character) An agroeconomic region in France
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2
#' @param A_P_WA (numeric) The P-content of the soil extracted with water
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg/kg)
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_COUNTRY (character) The country code
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_posphor(B_LU = 265, A_P_AL = 45, A_P_CC = 2.5,B_COUNTRY='NL')
#' osi_c_posphor(B_LU = 1019,A_P_AL = 54,A_P_CC = 4.5, A_P_WA = 65,B_COUNTRY ='NL')
#' 
#' @return 
#' The phosphate availability index in the Netherlands estimated from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_posphor <- function(B_LU, 
                          B_SOILTYPE_AGR = NA_character_,B_AER_FR = NA_character_, A_P_OL = NA_real_,
                          A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_, B_COUNTRY) {
  
  # add visual bindings
  A_P_OL = value = id = . = NULL
  
  # note that qualitative checks on the inputs are done by the country specific functions
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_AL),length(A_P_CC),length(A_P_WA),
                    length(B_AER_FR), length(B_SOILTYPE_AGR),length(B_COUNTRY))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                   B_AER_FR = B_AER_FR,
                   A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA,
                   A_P_OL = A_P_OL,
                   B_COUNTRY = B_COUNTRY,
                   value = NA_real_
                   )
  
  # calculate the open soil index score for phosphor availability in the Netherlands
  dt[B_COUNTRY == 'NL', value := osi_c_posphor_nl(B_LU = B_LU, A_P_AL = A_P_AL, A_P_CC = A_P_CC, A_P_WA = A_P_WA)]
  
  # calculate the open soil index score for phosphor availability in the Netherlands
  dt[B_COUNTRY == 'FR', value := osi_c_posphor_fr(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR, B_AER_FR = B_AER_FR, A_P_OL= A_P_OL)]
  
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
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_at(A_P_CAL = 47)
#' 
#' @return 
#' The phosphorus availability index in Austria estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_posphor_at <- function(A_P_CAL,B_LU = NA_character_) {
  
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
                   A_P_CAL = A_P_CAL,
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
  dt[,value := osi_evaluate_logistic(x = A_P_CAL, b= 0.138491,x0 = 2.81405015,v = 0.01965865)]
  
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
#' @param A_P_AL (numeric) The exchangeable P-content of the soil measured via ammonium lactate extraction
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_be(B_LU = 'SOJ', A_P_AL = 45)
#' 
#' @return 
#' The phosphorus availability index in Belgium estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_posphor_be <- function(B_LU, A_P_AL) {
  
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
  dt.thresholds <- dt.thresholds[osi_country == 'BE' & osi_indicator =='i_c_p']
  
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
  dt[,value := osi_evaluate_logistic(x = A_P_AL, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
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
#' @param A_P_AAA (numeric) The exchangeable P-content of the soil measured via Ammonium Lactate (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_ch(A_P_AAA = 50)
#' 
#' @return 
#' The phosphorus availability index in Norway estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_posphor_ch <- function(A_P_AAA,B_LU = NA_character_) {
  
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
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_cz(A_P_M3 = 81)
#' 
#' @return 
#' The phosphorus availability index in Czech Republic estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_posphor_cz <- function(A_P_M3,B_LU = NA_character_) {
  
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
                   A_P_M3 = A_P_M3,
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
  dt[,value := osi_evaluate_logistic(x = A_P_M3, b= 0.0890545,x0 = 4.429710417,v = 0.007972486)]
  
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
#' @param A_P_CAL (numeric) The P-content of the soil extracted with ammonium lactate(mg P2O5 / 100g)
#' @param A_P_DL (numeric) The P-content of the soil extracted with double lactate (mg P / kg)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_posphor_de(B_LU = 265, A_P_AL = 45,A_P_DL = 5)
#' osi_c_posphor_de(B_LU = c(265,1019),A_P_AL = c(35,54),A_P_DL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Germany stimated from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_posphor_de <- function(B_LU, A_SOM_LOI,A_P_CAL = NA_real_, A_P_DL = NA_real_) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_SOM_LOI= A_SOM_LOI,
                   A_P_CAL = A_P_CAL,
                   A_P_DL = A_P_DL,
                   value1 = NA_real_,
                   value2 = NA_real_,
                   value = NA_real_)
  
  # evaluation conform VDLUFA for cropland and soil types
  dt[!is.na(A_P_CAL), value1 := OBIC::evaluate_logistic(A_P_CAL, b = 0.2711, x0 = -5.9449, v = 0.0239)]
  
  # adjust for peat soils
  dt[!is.na(A_P_CAL) & A_SOM_LOI > 20, value1 := OBIC::evaluate_logistic(A_P_CAL, b = 0.1743, x0 = 2.92395, v = 0.096079)]
  
  # evaluation conform VDLUFA for cropland and soil types
  dt[!is.na(A_P_DL), value2 := OBIC::evaluate_logistic(A_P_DL, b = 0.5357, x0 = -4.03796, v = 0.01856)]
  
  # set value
  dt[,value := fifelse(!is.na(A_P_CAL),value1,value2)]
  
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
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_posphor_dk(B_LU = 265,A_P_OL = 5)
#' osi_c_posphor_dk(B_LU = c(265,1019),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Denmark derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_posphor_dk <- function(B_LU, A_P_OL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # evaluation P-Olsen for cropland and soil types
  dt[, value := OBIC::evaluate_logistic(A_P_OL, b = 0.226612, x0 = 30.137321,v = 1.247315)]
  
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
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_ee(A_P_M3 = 45)
#' 
#' @return 
#' The phosphorus availability index in Estonia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_posphor_ee <- function(A_P_M3,A_SOM_LOI,B_LU = NA_character_) {
  
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
  dt[A_SOM_LOI <= 2,value := osi_evaluate_logistic(x = A_P_M3, b= 0.1078429,x0 = -17.16723,v = 0.0153443)]
  dt[A_SOM_LOI > 2,value := osi_evaluate_logistic(x = A_P_M3, b= 0.173065,x0 = -17.18668,v = 0.0264163)]
  
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
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg/kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_posphor_es(B_LU = 265,A_CLAY_MI = 5,A_P_OL = 5)
#' osi_c_posphor_es(B_LU = c(265,1019),A_CLAY_MI = c(5,10),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Spain derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_posphor_es <- function(B_LU, A_CLAY_MI,A_P_OL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # assess P availability for sandy soils (Arenoso)
  dt[A_CLAY_MI < 15 & A_SAND_MI > 50, value := OBIC::evaluate_logistic(A_P_OL, b = 0.47947, x0 = -1.94363, v = 0.074075)]
  
  # assess P availability for loamy? soils (Franco)
  dt[A_CLAY_MI < 15 & A_SAND_MI <=50 , value := OBIC::evaluate_logistic(A_P_OL, b = 0.27155, x0 = 2.81733, v = 0.154671)]
  
  # assess P availability for clayey soils (Arcilloso)
  dt[A_CLAY_MI > 15, value := OBIC::evaluate_logistic(A_P_OL, b = 0.20196, x0 = 2.87602, v = 0.133171)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}

#' Calculate the phosphate availability index in France
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_SOILTYPE_AGR (character) The soil type in a particular region. Optional.
#' @param B_AER_FR (character) An agroeconomic region in France. Optional.
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen
#' @param A_PH_WA (numeric) The pH measured in water.
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_posphor_fr(B_LU = 'SOJ', A_P_OL = 45, 
#' B_SOILTYPE_AGR = 'limons battants', B_AER_FR = 'nord-picardie')
#' 
#' @details
#' The function has two optional arguments soil type (B_SOILTYPE_AGR) and agricultural region (B_AER_FR). When these are unknown, then the soil type is estimated based on the pH value. Threshold values are then generalized for calcareous and non-calcareous soils.
#' 
#' @return 
#' The phosphate availability index in France estimated from extractable soil P Olsen (a numeric value). 
#' 
#' @export
osi_c_posphor_fr <- function(B_LU, A_P_OL,B_SOILTYPE_AGR = NA_character_, B_AER_FR = NA_character_, A_PH_WA = NA_real_) {
  
  # set visual bindings
  value = osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = crop_p = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # Load in the interal datasets
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FR']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FR' & osi_indicator =='i_c_p']
  
  # filter the thresholds when no B_AER_FR is given
  if(sum(is.na(B_AER_FR))>0){
    dt.thresholds <- dt.thresholds[is.na(osi_threshold_region)]
  } else {
    dt.thresholds <- dt.thresholds[!is.na(osi_threshold_region)]
  }
  
  # soil types
  dt.soiltype <- as.data.table(euosi::osi_soiltype)
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_OL),length(B_SOILTYPE_AGR), length(B_AER_FR))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(dt.soiltype$osi_soil_cat1), empty.ok = FALSE)
  checkmate::assert_character(B_AER_FR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_AER_FR, choices = unique(dt.thresholds$osi_threshold_region), empty.ok = FALSE)
  checkmate::assert_numeric(A_P_OL, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_AER_FR = B_AER_FR,
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
  dt[is.na(B_SOILTYPE_AGR), B_SOILTYPE_AGR := fifelse(A_PH_WA > 8,'craie','general')]
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = c('B_SOILTYPE_AGR','B_AER_FR', 'crop_p'),
              by.y = c('osi_threshold_soilcat','osi_threshold_region','osi_threshold_cropcat'),
              all.x = TRUE)
  
  # estimate OSI score
  dt[,value := osi_evaluate_logistic(x = A_P_OL, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the phosphorus availability index in Finland
#' 
#' This function calculates the phosphorus availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_P_AAA (numeric) The exchangeable P-content of the soil measured via ammonium acetate extraction
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_fi(B_LU = 'SOJ', B_TEXTURE_USDA = 'Si',A_P_AAA = 45)
#' 
#' @return 
#' The phosphorus availability index in Finland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_posphor_fi <- function(B_LU, B_TEXTURE_USDA, A_P_AAA,A_C_OF = 0) {
  
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
  dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_c_p']
  
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
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_hu(A_P_AL = 45,A_CACO3_IF = 5,A_CLAY_MI = 5,A_SOM_LOI = 5)
#' 
#' @return 
#' The phosphorus availability index in Hungary estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_posphor_hu <- function(A_SOM_LOI,A_CLAY_MI,A_CACO3_IF,A_P_AL,B_LU = NA_character_) {
  
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
  dt[A_SOM_LOI > 4 & A_CACO3_IF <= 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.068946,x0 = 3.064834,v = 0.0349155)]
  dt[A_SOM_LOI > 4 & A_CACO3_IF > 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.07114313,x0 = 3.6741042,v = 0.016646)]
  
  # derive the OSI score for brown forest soil (proxied by clay)
  dt[A_CLAY_MI > 20 & A_SOM_LOI <= 4 & A_CACO3_IF <= 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.08938405,x0 = -17.8025024,v = 0.006430696)]
  dt[A_CLAY_MI > 20 & A_SOM_LOI <= 4 & A_CACO3_IF > 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.0919443,x0 = 1.95992725,v = 0.01361575)]
  
  # derive the OSI score for sandy soil (proxied by clay)
  dt[A_CLAY_MI <= 20 & A_SOM_LOI <= 4 & A_CACO3_IF <= 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.08938405,x0 = -17.80250244,v = 0.006430696)]
  dt[A_CLAY_MI <= 20 & A_SOM_LOI <= 4 & A_CACO3_IF > 1,value := osi_evaluate_logistic(x = A_P_AL, b= 0.09148938,x0 = 5.716193495,v = 0.007882485)]
  
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
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_posphor_ie(B_LU = 265,A_P_OL = 5)
#' osi_c_posphor_ie(B_LU = c(265,1019),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Ireland derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_posphor_ie <- function(B_LU, A_P_OL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # P index derived following P-Olsen.
  # evaluation soil P status for grasslands
  dt[, value := OBIC::evaluate_logistic(A_P_OL, b = 0.6560111, x0 = 3.44709, v = 0.588379)]
  
  # evaluation soil P status for other crops
  dt[, value := OBIC::evaluate_logistic(A_P_OL, b = 0.50194, x0 = 3.91821, v = 0.5799892)]
  
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
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_posphor_it(B_LU = 265,A_P_OL = 5)
#' osi_c_posphor_it(B_LU = c(265,1019),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Italy derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_posphor_it <- function(B_LU, A_P_OL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # evaluation P-Olsen for cropland and soil types
  dt[, value := OBIC::evaluate_logistic(A_P_OL, b = 0.43987, x0 = -5.7314, v = 0.011909)]
  
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
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_lv(A_P_DL = 45,B_TEXTURE_USDA='sand')
#' 
#' @return 
#' The phosphorus availability index in Latvia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_posphor_lv <- function(A_P_DL,B_TEXTURE_USDA, B_LU = NA_character_) {
  
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
  dt[B_TEXTURE_USDA == 'sand',value := osi_evaluate_logistic(x = A_P_DL, b= 0.249934,x0 = 2.907279,v = 0.070707)]
  dt[B_TEXTURE_USDA %in% c('loamy sand',
                           'sandy loam'),value := osi_evaluate_logistic(x = A_P_DL, b= 0.211789,x0 = 2.937473,v = 0.050529)]
  dt[B_TEXTURE_USDA %in% c('loam','sandy clay',
                           'sandy clay loam'),value := osi_evaluate_logistic(x = A_P_DL, b= 0.17789728,x0 = -7.742856,v = 0.007900254)]
  dt[B_TEXTURE_USDA %in% c('clay','clay loam',
                           'silty clay','silty clay loam',
                           'silt loam','silt'),value := osi_evaluate_logistic(x = A_P_DL, b= 0.1513621,x0 = -9.9252899,v = 0.00760908)]
  
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
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_lt(A_P_AL = 45)
#' 
#' @return 
#' The phosphorus availability index in Lithuania estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_posphor_lt <- function(A_P_AL,A_SOM_LOI,B_LU = NA_character_) {
  
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
  dt[A_SOM_LOI <= 20,value := osi_evaluate_logistic(x = A_P_AL, b= 0.115141,x0 = -13.66102,v = 0.0084859)]
  dt[A_SOM_LOI > 20,value := osi_evaluate_logistic(x = A_P_AL, b= 0.1014425,x0 = -10.704884,v = 0.00674754)]
  
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
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2
#' @param A_P_WA (numeric) The P-content of the soil extracted with water
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_posphor_nl(B_LU = 265, A_P_AL = 45, A_P_CC = 2.5)
#' osi_c_posphor_nl(B_LU = c(265,1019),A_P_AL = c(35,54),A_P_CC = c(2.5,4.5), A_P_WA = c(35,65))
#' 
#' @return 
#' The phosphate availability index in the Netherlands estimated from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_posphor_nl <- function(B_LU, A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = crop_code = NULL
  
  # convert B_LU to integer
  B_LU <- as.integer(B_LU)
  
  # Load in the crops data set and the parms dataset
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country =='NL']
  dt.crops[, crop_code := as.integer(crop_code)]
  
  # select parms (to check min and max, to be done later)
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # subset thresholds to Dutch situation for phosphorus
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='NL' & osi_indicator=='i_c_p']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_AL),length(A_P_CC),length(A_P_WA))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_numeric(A_P_AL, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_P_CC, lower = 0.1, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_P_WA, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  
  # check that there is only 1 scoring function for P
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA,
                   B_LU = B_LU,
                   value = NA_real_
                  )
  
  dt <- merge(dt,dt.crops,by.x = 'B_LU', by.y = 'crop_code',all.x=TRUE)
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # Calculate the phosphate availability for grass (PBI)
  dt[grepl("gras",crop_cat1), value := pmax(0,log(A_P_CC) * (-0.0114 * A_P_AL + 2.5) + 0.0251 * A_P_CC + 2)]
  
  # Calculate the phosphate availability for maize (PBI)
  dt[grepl("maize",crop_cat1), value := A_P_CC + 0.05 * (A_P_AL / A_P_CC)]
  
  # calculate the P-availability for arable systems, normalized to a scale with maximum around 6
  dt[grepl("arable",crop_cat1), value := A_P_WA * 0.1]
  
  # calculate the P-availability for nature 
  dt[grepl("nature",crop_cat1), value := 0]
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = value, b= dt.thresholds$osi_st_c1,x0 = dt.thresholds$osi_st_c2,v = dt.thresholds$osi_st_c3)]
  
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
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_no(A_P_AL = 50)
#' 
#' @return 
#' The phosphorus availability index in Norway estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_posphor_no <- function(A_P_AL,B_LU = NA_character_) {
  
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

#' Calculate the phosphate availability index in Sweden
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate (mg P / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_posphor_se(B_LU = 265,A_P_AL = 5)
#' osi_c_posphor_se(B_LU = c(265,1019),A_P_AL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in Sweden derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_posphor_se <- function(B_LU, A_P_AL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_AL = A_P_AL,
                   value = NA_real_)
  
  # evaluation soil P status III for maize and cereals
  dt[, value := OBIC::evaluate_logistic(A_P_AL, b = 0.126197, x0 = 14.6487, v = 0.46202)]
  
  # evaluation soil P status II for hostvete
  dt[, value := OBIC::evaluate_logistic(A_P_AL, b = 0.60458, x0 = 2.8517965, v = 0.0256494)]
  
  # evaluation soil P status III for oil crops
  dt[, value := OBIC::evaluate_logistic(A_P_AL, b = 0.126197, x0 = 14.6487, v = 0.46202)]
  
  # evaluation soil P status IVA for potato and sugar beet
  dt[, value := OBIC::evaluate_logistic(A_P_AL, b = 0.0695783, x0 = -27.867195, v = 0.0163328)]
  
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
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_sk(A_P_M3 = 45)
#' 
#' @return 
#' The phosphorus availability index in Slovak Republic estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_posphor_sk <- function(B_TEXTURE_HYPRES,A_P_M3,B_LU = NA_character_) {
  
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
                   A_P_M3 = A_P_M3,
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
  dt[B_TEXTURE_HYPRES %in% c('C'),value := osi_evaluate_logistic(x = A_P_M3, b= 0.07743388,x0 = -1.23994065,v = 0.00401143)]
  dt[B_TEXTURE_HYPRES %in% c('MF','M'),value := osi_evaluate_logistic(x = A_P_M3, b= 0.07683386,x0 = -7.05760285,v = 0.00574734)]
  dt[B_TEXTURE_HYPRES %in% c('F','VF'),value := osi_evaluate_logistic(x = A_P_M3, b= 0.089515,x0 = 3.0679272,v = 0.01673862)]
  
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
#' @param A_P_AL (numeric) The exchangeable P-content of the soil measured via Mehlich 3 extracton (mg P/ kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_sl(A_P_AL = 45)
#' 
#' @return 
#' The phosphorus availability index in Slovenia estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_posphor_sl <- function(A_P_AL,B_LU = NA_character_) {
  
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
  dt[,value := osi_evaluate_logistic(x = A_P_AL, b= 0.1080206,x0 = -11.5603261,v = 0.00766528)]
  
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
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_phosphorus_pl(A_P_DL = 45)
#' 
#' @return 
#' The phosphorus availability index in Poland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_c_posphor_pl <- function(A_P_DL,B_LU = NA_character_) {
  
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
  dt[,value := osi_evaluate_logistic(x = A_P_DL, b= 0.1199736,x0 = -12.565624,v = 0.0072809)]
  
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
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg/kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_posphor_uk(B_LU = 265,A_P_OL = 5)
#' osi_c_posphor_uk(B_LU = c(265,1019),A_P_OL = c(3.5,5.5))
#' 
#' @return 
#' The phosphate availability index in United Kingdom derived from extractable soil P fractions. A numeric value.
#' 
#' @export
osi_c_posphor_uk <- function(B_LU, A_P_OL) {
  
  # internal data.table
  dt <- data.table(id = 1: length(B_LU),
                   B_LU = B_LU,
                   A_P_OL = A_P_OL,
                   value = NA_real_)
  
  # P index derived following P-Olsen.
  # optimum value is index 2 for all land uses except vegetables (index 3)
  # evaluation P-Olsen for cropland and soil types
  dt[, value := OBIC::evaluate_logistic(A_P_OL, b = 0.3111, x0 = 2.77424, v = 0.043408)]
  
  # assess soil P status for vegetables
  dt[, value := OBIC::evaluate_logistic(A_P_OL, b = 0.2864, x0 = 2.78737, v = 0.055188)]
  
  # select value and return
  value <- dt[,value]
  return(value)
}















