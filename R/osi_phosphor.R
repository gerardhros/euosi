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

#' Calculate the phosphate availability index in France
#' 
#' This function calculates the phosphate availability. 
#' 
#' @param B_LU (character) The crop code
#' @param B_SOILTYPE_AGR (character) The soil type in a particular region
#' @param B_AER_FR (character) An agroeconomic region in France
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_posphor_fr(B_LU = 'SOJ', A_P_OL = 45, 
#' B_SOILTYPE_AGR = 'limons battants', B_AER_FR = 'nord-picardie')
#' 
#' @return 
#' The phosphate availability index in France estimated from extractable soil P Olsen (a numeric value).
#' 
#' @export
osi_c_posphor_fr <- function(B_LU, B_SOILTYPE_AGR, B_AER_FR, A_P_OL = NA_real_) {
  
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
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_p)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
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
