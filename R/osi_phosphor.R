#' Calculate the phosphate availability index (wrapper function)
#' 
#' This function calculates the phosphate availability for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2
#' @param A_P_WA (numeric) The P-content of the soil extracted with water
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
osi_c_posphor <- function(B_LU, A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_, B_COUNTRY) {
  
  # add visual bindings
  i_c_p = NULL
  
  # note that qualitative checks on the inputs are done by the country specific functions
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_P_AL),length(A_P_CC),length(A_P_WA),length(B_COUNTRY))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA,
                   B_LU = B_LU,
                   B_COUNTRY = B_COUNTRY,
                   value = NA_real_
  )
  
  # calculate the open soil index score for phosphor availability
  dt[B_COUNTRY == 'NL', i_c_p := osi_c_posphor_nl(B_LU, A_P_AL = A_P_AL, A_P_CC = A_P_CC, A_P_WA = A_P_WA)]
  
  # select the output variable
  out <- dt[,i_c_p]
  
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
  i_c_p = osi_country = osi_indicator = id = crop_cat1 = NULL
  
  # Load in the crops data set and the parms dataset
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.parms <- as.data.table(euosi::osi_parms)
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  
  # subset thresholds to Dutch situation for phosphorus
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
  dt[,i_c_p := osi_evaluate_logistic(x = value, b= dt.thresholds$osi_st_c1,x0 = dt.thresholds$osi_st_c2,v = dt.thresholds$osi_st_c3)]
  
  # return value
  value <- dt[, i_c_p]
  
  return(value)
  
}

