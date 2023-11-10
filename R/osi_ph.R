#' Calculate the pH index (wrapper function)
#' 
#' This function calculates the pH index for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param A_PH_WA (numeric) The pH measured in h2o
#' @param A_PH_CC (numeric) The pH measured in cacl2 
#' @param A_PH_KCL (numeric) The pH measured in KCl
#' @param B_TEXTURE_USDA (character) The USDA textural class 

#' 
#' @import data.table
#' 
#' @examples 

#' @return
#' The index to evaluate the soil pH 
#' 
#' @export
osi_c_ph <- function(A_PH_WA,A_PH_CC, A_PH_KCL,B_LU,B_TEXTURE_USDA) {
  
  # add visual bindings
  i_c_ph = NULL
  
  # desired length of inputs
  arg.length <- max(length(A_PH_CC), length(A_PH_KCL),length(A_PH_WA), length(B_LU),length(B_TEXTURE_USDA),length(B_LU))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   A_PH_CC = A_PH_CC,
                   A_PH_WA = A_PH_WA,
                   A_PH_KCL = A_PH_KCL,
                   B_LU = B_LU,
                   B_TEXTURE_USDA=B_TEXTURE_USDA,
                   value = NA_real_
  )
  
  # calculate the open soil index score for pH per country
  dt[B_COUNTRY == 'FR', i_c_ph := osi_c_boron_fr(B_TEXTURE_USDA = B_TEXTURE_USDA,B_LU = B_LU,
                                                 A_PH_WA = A_PH_WA)]
  
  # select the output variable
  out <- dt[,i_c_ph]
  
  # return the OSI score
  return(out)
  
}


#' 
#' #' Calculate the pH index for France 
#' 
#' This function evaluates the pH index in France
#' 
#' @param A_PH_WA (numeric) The pH measured in H2O
#' @param B_TEXTURE_USDA (numeric) The soil textural class
#' @param B_LU (character) The crop type
#'
#' @import data.table
#' 
#' @examples 
#' 
#' 
#' @return 
#' The pH index in France estimated from pH in water, the textural class and the crop type
#' 
#' @export

osi_c_ph_fr <- function(B_TEXTURE_USDA, A_PH_WA, B_LU) {
  
  # set visual bindings
  osi_c_ph_fr = osi_country = osi_indicator = id = crop_cat1 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # Load in the threshold data set and the parms dataset
  dt.parms <- as.data.table(euosi::osi_parms)
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  
  # subset thresholds to French situation for Magnesium
  soil_cat_ph <- B_TEXTURE_USDA
  dt.thresholds <- ifelse(B_LU=='BTN'|B_LU=='BVF', dt.thresholds[dt.thresholds$osi_country=='FR' & dt.thresholds$osi_indicator=='i_c_ph' & 
                                   dt.thresholds$osi_threshold_cropcat=='sugar beet',],
                          dt.thresholds[dt.thresholds$osi_country=='FR' & dt.thresholds$osi_indicator=='i_c_ph' & 
                                          dt.thresholds$osi_threshold_soilcat==soil_cat_ph,])
                          
  
  
  # Check length of desired input
  arg.length <- max(length(A_PH_WA),length(B_TEXTURE_USDA),length(B_LU))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_numeric(A_PH_WA, lower = 0, upper = 14, any.missing = TRUE, len = arg.length)

  # check that there are only 1 scoring function for K
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   A_PH_WA = A_PH_WA,B_TEXTURE_USDA=B_TEXTURE_USDA,B_LU=B_LU,
                   value = A_PH_WA)
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # convert to the OSI score
  i_c_ph <-  osi::evaluate_logistic(x = dt$value, b= dt.thresholds$osi_st_c1,x0 = dt.thresholds$osi_st_c2,v = dt.thresholds$osi_st_c3)
  
  return(value)
  
}

#' 