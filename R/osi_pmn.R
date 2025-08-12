#' Calculate the index for the microbial biological activity (wrapper function)
#' 
#' This function assesses the microbial biological activity (of microbes and fungi) via the Potentially Mineralizable N pool, also called PMN (or SoilLife by Eurofins in the past) for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param A_N_PMN (numeric) The potentially mineralizable N pool (mg N / kg soil) 
#' @param B_COUNTRY (character) The country code
#'  
#' @import data.table
#' @import OBIC
#' 
#' @examples 
#' osi_b_pmn(B_LU = 256, B_SOILTYPE_AGR = 'dekzand', A_N_PMN = 125, B_COUNTRY = 'NL')
#' osi_b_pmn(B_LU = c(256,1027), B_SOILTYPE_AGR = c('dekzand','rivierklei'), 
#' A_N_PMN = c(125,45),B_COUNTRY = c('NL','NL'))
#'
#' @return 
#' the normalized potentially mineralizable Nitrogen pool (mg N / kg), a numeric value, converted to an OSI score.
#' 
#' @export
osi_b_pmn <- function(B_LU, B_SOILTYPE_AGR,A_CLAY_MI = NA_real_, A_N_PMN = NA_real_, A_N_RT = NA_real_, B_COUNTRY) {
  
  # add visual bindings

  # note that qualitative checks on the inputs are done by the country specific functions
  
  # Check length of desired input
  arg.length <- max(length(A_N_PMN), length(B_LU), length(B_SOILTYPE_AGR),length(B_COUNTRY),length(A_N_RT))
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_CLAY_MI = A_CLAY_MI,
                   A_N_RT = A_N_RT,
                   A_N_PMN = A_N_PMN,
                   B_COUNTRY = B_COUNTRY,
                   value = NA_real_
                  )
  
  # estimate A_N_PMN (mg N / kg) when missing
  dt[is.na(A_N_PMN) & !is.na(A_N_RT), A_N_PMN := exp(-3.440931 + 1.1012449 * log(A_N_RT) - 0.055858 * log(A_CLAY_MI))]
  
  # calculate the open soil index score for phosphor availability
  dt[B_COUNTRY == 'NL', value := osi_b_pmn_nl(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR, A_N_PMN = A_N_PMN)]
  
  # calculate PMN for other countries
  dt[B_COUNTRY != 'NL', value := osi_b_pmn_eu(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_N_RT = A_N_RT)]
  
  # select the output variable
  value <- dt[,value]
  
  # return the OSI score
  return(value)
  
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
  osi_country = osi_indicator = crop_code = osi_b_pmn = NULL
  crop_cat1 = cf = id = NULL
  
  # Load in the crops data set and the parms dataset
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country =='NL']
  
  # load and subset thresholds to Dutch situation for PMN
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='NL' & osi_indicator=='i_b_pmn']

  # ensure input as character
  B_LU = as.character(B_LU)
  
  # check length and of arguments
  arg.length <- max(length(A_N_PMN), length(B_LU), length(B_SOILTYPE_AGR))
  checkmate::assert_numeric(A_N_PMN, lower = 0, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(euosi::osi_soiltype$osi_soil_cat1), empty.ok = FALSE)
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # make internal data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU = as.character(B_LU),
                   A_N_PMN = A_N_PMN,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   value = NA_real_
                   )
  
  # merge with crop_category  
  dt <- merge(dt, 
              dt.crops[, list(crop_code = as.character(crop_code), crop_cat1)], 
              by.x = "B_LU", 
              by.y = "crop_code", 
              all.x = TRUE)
  
  # OBIC normalization of measured data on sector and soil averaged BOBI measurements
  dt[crop_cat1 %in% c('maize','grassland') & grepl('klei|loess',B_SOILTYPE_AGR), cf := 137 / 120]
  dt[crop_cat1 %in% c('maize','grassland') & grepl('veen',B_SOILTYPE_AGR), cf := 1]
  dt[crop_cat1 %in% c('maize','grassland') & grepl('zand|dal',B_SOILTYPE_AGR), cf := 84 / 89]
  dt[crop_cat1 %in% 'arable' & grepl('zand|dal',B_SOILTYPE_AGR), cf := 37 / 45]
  dt[crop_cat1 %in% 'arable' & grepl('klei|loess',B_SOILTYPE_AGR), cf := 22 / 42]
  
  # Calculate PMN index for soil fertility
  dt[, value := A_N_PMN * cf]
  
  # PMN values above 500 are very unlikely, so maximize the PMN-index
  dt[value > 500, value := 500]
  
  # convert to OSI score
  dt[, osi_b_pmn := osi_evaluate_logistic(x = value,
                                          b = dt.thresholds$osi_st_c1, 
                                          x0 = dt.thresholds$osi_st_c2,
                                          v = dt.thresholds$osi_st_c3)]
  
  # setorderid
  setorder(dt,id)
  
  # get value
  value <- dt[,osi_b_pmn]
  
  # return value
  return(value)
}

#' Calculate the index for the microbial biological activity across the EU
#' 
#' This function assesses the microbial biological activity (of microbes and fungi) via the Potentially Mineralizable N pool, also called PMN (or SoilLife by Eurofins in the past).
#' 
#' @param B_LU (numeric) The crop code
#' @param A_CLAY_MI (numeric) The aclay content of the soil (\%)
#' @param A_N_RT (numeric) The total N content of the soil(mg N / kg soil) 
#' 
#' @import data.table
#' @import OBIC
#' 
#' @examples 
#' osi_b_pmn_eu(B_LU = 256, A_CLAY_MI = 4.5, A_N_RT = 1250)
#' osi_b_pmn_eu(c(256,1027),c(4,48), c(3125,1450))
#'
#' @return 
#' the normalized potentially mineralizable Nitrogen pool (mg N / kg), a numeric value, converted to an OSI score.
#' 
#' @export
osi_b_pmn_eu <- function(B_LU, A_N_RT, A_CLAY_MI) {
  
  # add visual bindings
  osi_country = osi_indicator = PMN = NULL
  
  # Load in the crops data set and the parms dataset
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # load and subset thresholds to Dutch situation for PMN
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='NL' & osi_indicator=='i_b_pmn']
  
  # check length and of arguments
  arg.length <- max(length(A_N_RT), length(B_LU))

  # make internal table
  arg.length <- max(length(B_LU), length(A_N_RT), length(A_CLAY_MI))
  
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_N_RT = A_N_RT
                   )
  
  # predict PMN (based on Dutch emperical relationship)
  # built on the large dataset of Dutch soils (R2 = 0.79, N=109.146 samples)
  dt[, PMN := exp(-3.440931 + 1.1012449 * log(A_N_RT) - 0.055858 * log(A_CLAY_MI))]

  # convert to OSI score
  dt[, value := osi_evaluate_logistic(x = PMN,b = 0.2, x0 = 20,v = 1.2)]
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
}

