#' Calculate the phosphorus excess index in Finland
#' 
#' This function calculates the phosphorus excess index 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_P_AA (numeric) The exchangeable P-content of the soil measured via ammonium acetate extraction
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_posphor_fi(B_LU = 'SOJ', B_TEXTURE_USDA = 'Si',A_P_AA = 45)
#' 
#' @return 
#' The phosphorus excess index in Finland estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_fi <- function(B_LU, B_TEXTURE_USDA, A_P_AA,A_C_OF = 0) {
  
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
  dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_e_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA = B_TEXTURE_USDA,
                   B_SOILTYPE_AGR = NA_character_,
                   A_P_AA = A_P_AA,
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
  dt[,value := osi_evaluate_logistic(x = A_P_AA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
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
#' 
#' @import data.table
#' 
#' @examples 
#' osi_nut_posphor_be(B_LU = 'SOJ', A_P_AL = 45)
#' 
#' @return 
#' The phosphorus excess index in Belgium estimated from extractable phosphorus. A numeric value.
#' 
#' @export
osi_nut_p_be <- function(B_LU, A_P_AL) {
  
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
  dt.thresholds <- dt.thresholds[osi_country == 'BE' & osi_indicator =='i_e_p']
  
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


#' Calculate the phosphorus excess index in France
#' 
#' This function calculates the phosphorus excess index 
#' 
#' @param B_LU (character) The crop code
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen
#' @param A_PH_WA (numeric) The pH measured in water.
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
osi_nut_p_fr <- function(B_LU, A_P_OL,A_PH_WA = NA_real_) {
  
  # set visual bindings
  value = osi_country = osi_indicator = id = crop_cat1 = B_SOILTYPE_AGR = NULL
  crop_code = crop_p = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # Load in the interal datasets
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FR']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FR' & osi_indicator =='i_e_p']
  
  # soil types
  dt.soiltype <- as.data.table(euosi::osi_soiltype)
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_OL))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_P_OL, lower = 1, upper = 250, any.missing = TRUE, len = arg.length)
  
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
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = c('B_SOILTYPE_AGR', 'crop_p'),
              by.y = c('osi_threshold_soilcat','osi_threshold_cropcat'),
              all.x = TRUE)
  
  # estimate OSI score
  dt[,value := osi_evaluate_logistic(x = A_P_OL, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}