#' Calculate the pH index (wrapper function)
#' 
#' This function calculates the pH index for all European countries (if available). 
#' 
#' @param B_LU (numeric) The crop code
#' @param B_TEXTURE_USDA (character) The USDA textural class
#' @param A_PH_WA (numeric) The pH measured in h2o
#' @param A_PH_CC (numeric) The pH measured in cacl2 
#' @param A_PH_KCL (numeric) The pH measured in KCl
#' @param B_COUNTRY (character) The country code
#' 
#' @import data.table
#' 
#' @return
#' The index to evaluate the soil pH 
#' 
#' @export
osi_c_ph <- function(B_LU, B_TEXTURE_USDA= NA_real_, A_PH_WA = NA_real_, A_PH_CC= NA_real_, A_PH_KCL= NA_real_,B_COUNTRY) {
  
  # add visual bindings
  value = NULL
  
  # desired length of inputs
  arg.length <- max(length(A_PH_CC), length(A_PH_KCL),length(A_PH_WA), length(B_LU),
                    length(B_TEXTURE_USDA),length(B_LU),length(B_COUNTRY))
  
  # Collect the data in an internal data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA=B_TEXTURE_USDA,
                   A_PH_CC = A_PH_CC,
                   A_PH_WA = A_PH_WA,
                   A_PH_KCL = A_PH_KCL,
                   value = NA_real_  
                   )
  
  # calculate the open soil index score for pH per country
  dt[B_COUNTRY == 'FR', value := osi_c_ph_fr(B_LU = B_LU,
                                             B_TEXTURE_USDA = B_TEXTURE_USDA,
                                             A_PH_WA = A_PH_WA)]
  
  # select the output variable
  value <- dt[,value]
  
  # return the OSI score
  return(value)
  
}


#' Calculate the pH index for France 
#' 
#' This function evaluates the pH index in France
#' 
#' @param B_LU (character) The crop type
#' @param B_TEXTURE_USDA (numeric) The soil textural class
#' @param A_PH_WA (numeric) The pH measured in H2O
#' 
#' @import data.table
#' 
#' @return 
#' The pH index in France estimated from pH in water, the textural class and the crop type
#' 
#' @export
osi_c_ph_fr <- function(B_LU,B_TEXTURE_USDA, A_PH_WA) {
  
  # set visual bindings
  osi_c_ph_fr = osi_country = osi_indicator = id = crop_cat1 = NULL
  cat_crop_ph = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # Load in the parameter set (to set min and max, to be done later)
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # load in thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='FR' & osi_indicator=='i_c_ph']
  
  # Check length of desired input
  arg.length <- max(length(A_PH_WA),length(B_TEXTURE_USDA),length(B_LU))
  
  # check the values (update the limits later via dt.parms)
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_PH_WA, lower = 0, upper = 14, any.missing = TRUE, len = arg.length)
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_TEXTURE_USDA = B_TEXTURE_USDA,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # merge subset on crop class
  dt.sb <- dt[B_LU %in% c('BTN','BVF')][,cat_crop_ph := 'sugar beet']
  dt.sb <- merge(dt.sb,
                 dt.thresholds,
                 by.x = 'cat_crop_ph',
                 by.y = 'osi_threshold_cropcat',
                 all.x = TRUE)
  
  # merge subset on soil texture class
  dt.other <- dt[B_LU %in% c('BTN','BVF')]
  dt.other <- merge(dt.other,
                    dt.thresholds,
                    by.x = 'B_TEXTURE_USDA',
                    by.y = 'osi_threshold_soilcat',
                    all.x = TRUE)
  
  # combine both again
  dt <- rbind(dt.sb[,.(id,A_PH_WA,osi_st_c1, osi_st_c2, osi_st_c3)],
              dt.other[,.(id,A_PH_WA,osi_st_c1, osi_st_c2, osi_st_c3)])
  
  # convert to the OSI score
  dt[, value := evaluate_logistic(x = A_PH_WA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select and return value
  value <- dt[,value]
  
  return(value)
  
}

#' 