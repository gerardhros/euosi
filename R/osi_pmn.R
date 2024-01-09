#' Calculate the index for the microbial biological activity (wrapper function)
#' 
#' This function assesses the microbial biological activity (of microbes and fungi) via the Potentially Mineralizable N pool, also called PMN (or SoilLife by Eurofins in the past) for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_N_PMN (numeric) The potentially mineralizable N pool (mg N / kg soil) 
#' @param B_COUNTRY (character) The country code
#'  
#' @import data.table
#' @import OBIC
#' 
#' @examples 
#' osi_b_pmn(B_LU = 256, B_SOILTYPE_AGR = 'dekzand', A_N_PMN = 125, B_COUNTRY = 'NL')
#' osi_b_pmn(c(256,1027), c('dekzand','rivierklei'), c(125,45),c('NL','NL'))
#'
#' @return 
#' the normalized potentially mineralizable Nitrogen pool (mg N / kg), a numeric value, converted to an OSI score.
#' 
#' @export
osi_b_pmn <- function(B_LU, B_SOILTYPE_AGR,A_N_PMN, B_COUNTRY) {
  
  # add visual bindings
  i_b_pmn = NULL
  
  # note that qualitative checks on the inputs are done by the country specific functions
  
  # Check length of desired input
  arg.length <- max(length(A_N_PMN), length(B_LU), length(B_SOILTYPE_AGR),length(B_COUNTRY))
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   A_N_PMN = A_N_PMN,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_COUNTRY = B_COUNTRY,
                   value = NA_real_
                  )
  
  # calculate the open soil index score for phosphor availability
  dt[B_COUNTRY == 'NL', i_b_pmn := osi_b_pmn_nl(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR, A_N_PMN = A_N_PMN)]
  
  # select the output variable
  out <- dt[,i_b_pmn]
  
  # return the OSI score
  return(out)
  
}

#' Calculate the index for the microbial biological activity in the Netherlands
#' 
#' This function assesses the microbial biological activity (of microbes and fungi) via the Potentially Mineralizable N pool, also called PMN (or SoilLife by Eurofins in the past).
#' 
#' @param B_LU (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_N_PMN (numeric) The potentially mineralizable N pool (mg N / kg soil) 
#' 
#' @import data.table
#' @import OBIC
#' 
#' @examples 
#' osi_b_pmn_nl(B_LU = 256, B_SOILTYPE_AGR = 'dekzand', A_N_PMN = 125)
#' osi_b_pmn_nl(c(256,1027), c('dekzand','rivierklei'), c(125,45))
#'
#' @return 
#' the normalized potentially mineralizable Nitrogen pool (mg N / kg), a numeric value, converted to an OSI score.
#' 
#' @export
osi_b_pmn_nl <- function(B_LU, B_SOILTYPE_AGR,A_N_PMN) {
  
  # add visual bindings
  osi_country = osi_indicator = crop_code = NULL
  
  # Load in the crops data set and the parms dataset
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country =='NL']
  dt.crops[, crop_code := as.integer(crop_code)]
  
  # load and subset thresholds to Dutch situation for PMN
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='NL' & osi_indicator=='i_b_pmn']
  
  # check length and of arguments
  arg.length <- max(length(A_N_PMN), length(B_LU), length(B_SOILTYPE_AGR))
  checkmate::assert_numeric(A_N_PMN, lower = 0, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(euosi::osi_soiltype$osi_soil_cat1), empty.ok = FALSE)
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # calculate the PMN value for the Netherlands using the Dutch OBIC
  value <- OBIC::calc_pmn(B_LU_BRP = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR, A_N_PMN = A_N_PMN)

  # convert to OSI score
  value <- osi_evaluate_logistic(x = value,
                                 b = dt.thresholds$osi_st_c1, 
                                 x0 = dt.thresholds$osi_st_c2,
                                 v = dt.thresholds$osi_st_c3)
  # return value
  return(value)
}

