#' Calculate a biodiversity index
#' 
#' This function calculates the biodiversity index bsed on soil organic matter and pH.
#' 
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_PH_WA (numeric) The pH measured in h2o
#' 
#' @import data.table
#' 
#' @examples 
#' osi_biodiversity(A_SOM_LOI = 3,A_PH_WA = 4.5)
#' 
#' @return 
#' The biodiversity index. A numeric value.
#' 
#' @export
osi_biodiversity <- function(A_SOM_LOI, A_PH_WA) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_indicator %in% c('i_b_ph','i_b_som')]
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # evaluate OSI score for pH condition supporting biodiversity
  dtt.ph <- dt.thresholds[osi_indicator == 'i_b_ph']
  dt[, bio_ph := osi_evaluate_logistic(x = A_PH_WA, b = dtt.ph$osi_st_c1, x0 = dtt.ph$osi_st_c2, v = dtt.ph$osi_st_c3)]
  
  # evaluate OSI score for SOM condition supporting biodiversity
  dtt.som <- dt.thresholds[osi_indicator == 'i_b_som']
  dt[, bio_som := osi_evaluate_logistic(x = A_SOM_LOI, b = dtt.som$osi_st_c1, x0 = dtt.som$osi_st_c2, v = dtt.som$osi_st_c3)]
  
  # convert to the OSI score
  dt[,value := (bio_ph + bio_som) * 0.5]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}