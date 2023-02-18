#' Calculate the soil nitrogen supplying capacity (wrapper function)
#' 
#' This function calculates the nitrogen supplying capacity for soils in Europe
#'  
#' @param B_LU (numeric) The crop code from the BRP 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param B_COUNTRY (character) The country code
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen(B_LU = 256, B_SOILTYPE_AGR = 'dekzand',A_N_RT = 2500, 
#' A_SOM_LOI = 4.5, B_COUNTRY = 'NL')
#' osi_c_nitrogen(1019,'dekzand',4.5,2315,'NL')
#'
#' @return 
#' the normalized potentially mineralizable Nitrogen pool (mg N / kg), a numeric value, converted to an OSI score.
#' 
#' @export
osi_c_nitrogen <- function(B_LU, B_SOILTYPE_AGR,A_SOM_LOI,A_N_RT, B_COUNTRY) {
  
  # add visual bindings
  i_c_n = NULL
  
  # note that qualitative checks on the inputs are done by the country specific functions
  
  # Check length of desired input
  arg.length <- max(length(A_N_RT), length(A_SOM_LOI),length(B_LU), length(B_SOILTYPE_AGR),length(B_COUNTRY))
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_COUNTRY = B_COUNTRY,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT, 
                   A_CN_FR = A_SOM_LOI * 10 * 0.5 * 1000/ A_N_RT,
                   value = NA_real_
  )
  
  # calculate the open soil index score for phosphor availability
  dt[B_COUNTRY == 'NL', i_c_n := osi_c_nitrogen_nl(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                                                   A_SOM_LOI = A_SOM_LOI,A_N_RT = A_N_RT)]
  
  # select the output variable
  out <- dt[,i_c_n]
  
  # return the OSI score
  return(out)
  
}

#' Calculate the soil nitrogen supplying capacity in the Netherlands
#' 
#' This function calculates the NLV (nitrogen producing capacity) for the soil
#' 
#' @param B_LU (numeric) The crop code from the BRP 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_nl(B_LU = 256, B_SOILTYPE_AGR = 'dekzand',A_SOM_LOI = 4.5,A_N_RT = 2500)
#' osi_c_nitrogen_nl(1019,'dekzand',5.5,2315)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_nl <- function(B_LU, B_SOILTYPE_AGR,A_SOM_LOI,A_N_RT) {
  
  # add visual bindings
  osi_country = osi_indicator = crop_code = crop_cat1 = osi_threshold_cropcat = NULL
  A_CN_FR = D_BDS = D_RD = D_OC = D_GA = id = value = i_c_n = osi_st_c1 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'NL']
  
  # load and subset thresholds to Dutch situation for PMN
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='NL' & osi_indicator=='i_c_n']
  
  # check length and of arguments
  arg.length <- max(length(A_N_RT), length(A_SOM_LOI),length(B_LU), length(B_SOILTYPE_AGR),length(A_CN_FR))
  checkmate::assert_numeric(A_N_RT, lower = 0.1, upper = 30000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(euosi::osi_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(euosi::osi_soiltype$osi_soil_cat1), empty.ok = FALSE)
  checkmate::assert_data_table(dt.thresholds,max.rows = 2,min.rows = 2)
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_SOM_LOI * 10 * 0.5 * 1000/ A_N_RT, 
                   value = NA_real_
                  )

  # merge with crop_category  
  dt <- merge(dt, dt.crops[, list(crop_code, crop_cat1)], 
              by.x = "B_LU", by.y = "crop_code", all.x = TRUE)
  
  # calculate derivative supporting soil properties
  dt[, D_BDS := OBIC::calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI)]
  dt[, D_RD := OBIC::calc_root_depth(B_LU_BRP = B_LU)]
  dt[, D_OC := OBIC::calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
  dt[, D_GA := OBIC::calc_grass_age(id, B_LU_BRP = B_LU)]
  
  # calculate the N supplying capacity for the Netherlands using the Dutch OBIC
  dt[,value := OBIC::calc_nlv(B_LU_BRP = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                          A_N_RT = A_N_RT, A_CN_FR = A_CN_FR, D_OC = D_OC, D_BDS = D_BDS, 
                          D_GA = D_GA)]
  
  # convert to OSI score
  
    # subset and evaluate for arable soils
    dths <- dt.thresholds[osi_threshold_cropcat == 'arable']
    dt[grepl('arable|maize',crop_cat1), i_c_n := osi_evaluate_parabolic(value, x.top = dths[,osi_st_c1])]
 
    # subset and evaluate for grassland soils
    dths <- dt.thresholds[osi_threshold_cropcat == 'grassland']
    dt[grepl('grassland',crop_cat1), i_c_n := osi_evaluate_parabolic(value, x.top = dths[,osi_st_c1])]
    
    # set OSI score for others  
    dt[grepl('nature',crop_cat1), i_c_n := 1]
    
  # select output variable
  out <- dt[,i_c_n]
  
  # return value
  return(out)
}