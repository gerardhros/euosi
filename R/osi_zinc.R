#' Calculate the Zinc availability index for agricultural soils (wrapper function)
#' 
#' This function calculates the zinc availability for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_PH_WA (numeric) The acidity of the soil, measured in water (-)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-) 
#' @param A_ZN_EDTA (numeric) The plant available content of Zn in the soil (mg Zn per kg) extracted by EDTA 
#' @param A_ZN_CC (numeric) The plant available content of Zn in the soil (mg  Zn per kg) extracted by 0.01M CaCl2
#' @param B_COUNTRY (character) The country code
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_zinc(B_LU = 'SOJ', A_ZN_EDTA = 45, A_PH_WA = 6.5,
#' A_PH_CC = NA, A_ZN_CC = NA, B_COUNTRY='FR')
#' 
#' @return
#' The capacity of the soil to supply and buffer zinc, evaluated given an optimum threshold for yield. A numeric value.
#' 
#' @export
osi_c_zinc <- function(B_LU, A_PH_WA = NA_real_,A_PH_CC = NA_real_,A_ZN_EDTA = NA_real_,A_ZN_CC = NA_real_, B_COUNTRY) {
  
  # add visual bindings
  value = NULL
  
  # desired length of inputs
  arg.length <- max(length(B_LU), length(A_PH_WA), length(A_ZN_EDTA), length(A_PH_CC),length(A_PH_CC))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_COUNTRY = B_COUNTRY,
                   A_PH_WA = A_PH_WA,
                   A_PH_CC = A_PH_CC,
                   A_ZN_EDTA = A_ZN_EDTA,
                   A_ZN_CC = A_ZN_CC,
                   value = NA_real_
  )
  
  # calculate the open soil index score for magnesium availability for France
  dt[B_COUNTRY == 'FR', value := osi_c_zinc_fr(B_LU = B_LU,A_PH_WA = A_PH_WA,A_ZN_EDTA = A_ZN_EDTA)]
  
  # calculate the open soil index score for magnesium availability for the Netherlands
  dt[B_COUNTRY == 'NL', value := osi_c_zinc_nl(B_LU = B_LU,A_PH_CC = A_PH_CC,A_ZN_CC = A_ZN_CC)]
  
  # select the output variable
  out <- dt[,value]
  
  # return the OSI score
  return(out)
  
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
  
  # Load in the parameter set (to set min and max, to be done later)
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # load in thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='FR' & osi_indicator=='i_c_zn']
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_ZN_EDTA),length(A_PH_WA))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_ZN_EDTA, lower = 0.001, upper = 100, any.missing = TRUE, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 11, any.missing = TRUE, len = arg.length)
  
  # check that there are two scoring function for Zinc
  checkmate::assert_data_table(dt.thresholds,max.rows = 2)
  
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
  dt[!B_LU %in% c('DLN','MID','MIE','MIS'), value := 1]

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
#' osi_c_zinc_nl(B_LU = 265, A_ZN_CC = 45, A_PH_CC = 6.5)
#' 
#' @return 
#' The function of the soil to supply zinc (a numeric value).
#' 
#' @export
osi_c_zinc_nl <- function(B_LU, A_PH_CC, A_ZN_CC) {
  
  # set visual bindings
  id = crop_code = soiltype = soiltype.n = crop_n = crop_category = D_ZN = NULL
  osi_country = osi_indicator = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)

  # load and subset thresholds for situation in NL
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='NL' & osi_indicator=='i_c_zn']
  
  # Check input
  arg.length <- max(length(B_LU), length(A_ZN_CC), length(A_PH_CC))
  checkmate::assert_numeric(A_ZN_CC, lower = 5, upper = 50000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(crops.obic$crop_code), empty.ok = FALSE)

  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_PH_CC = A_PH_CC,
                   A_ZN_CC = A_ZN_CC,
                   D_ZN = NA_real_,
                   value = NA_real_
                  )
  
  # merge properties form crop category and soil type
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU", by.y = "crop_code")

  # Calculate Zn-availability
  dt[crop_category =='akkerbouw', D_ZN := 10^(0.88 + 0.56 * log10(A_ZN_CC*0.001) + 0.13 * A_PH_CC)]
  dt[crop_category =='mais', D_ZN := 10^(0.88 + 0.56 * log10(A_ZN_CC*0.001) + 0.13 * A_PH_CC)]
  dt[crop_category =='natuur', D_ZN := 0]
  dt[crop_category =='grasland', D_ZN := 10^(-1.04 + 0.67 * log10(A_ZN_CC*0.001) + 0.5 * A_PH_CC)]
  
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
