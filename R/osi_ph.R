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
osi_c_ph_fr <- function(B_LU,A_CLAY_MI, A_SAND_MI, A_SILT_MI, A_PH_WA) {
  
  # set visual bindings
  osi_c_ph_fr = osi_country = osi_indicator = id = crop_cat1 = NULL
  cat_crop_ph = B_TEXTURE_GEPPA = B_TEXTURE_GEPPA2 = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL

  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # Load in the parameter set (to set min and max, to be done later)
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # load in thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='FR' & osi_indicator=='i_c_ph']
  
  # calculate the French texture
  tri.data <- as.data.frame(cbind(A_CLAY_MI,A_SAND_MI,A_SILT_MI))
  names(tri.data) <-C('CLAY','SAND','SILT')
  TT.data <- as.data.frame(soiltexture::TT.points.in.classes(tri.data2015,"FR.GEPPA.TT",text.tol=1))
  B_TEXTURE_GEPPA <- factor(names(TT.data2015)[1+max.col(TT.data2015)],ordered=TRUE)
  B_TEXTURE_GEPPA2 <- ifelse(B_TEXTURE_GEPPA=='L'|B_TEXTURE_GEPPA=='Lsa'|B_TEXTURE_GEPPA=='LAS'|B_TEXTURE_GEPPA=='La'|B_TEXTURE_GEPPA=='LL','loam',
                             ifelse(B_TEXTURE_GEPPA=='SS'|B_TEXTURE_GEPPA=='Sl'|B_TEXTURE_GEPPA=='S'|B_TEXTURE_GEPPA=='L'|B_TEXTURE_GEPPA=='Sal'|B_TEXTURE_GEPPA=='Sa','sand','other'))
  
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
                   B_TEXTURE_GEPPA2 = B_TEXTURE_GEPPA2,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # merge 
  dt.sb <- merge(dt,
                 dt.thresholds,
                 by=c("crop_ph" = "osi_threshold_cropcat" , "B_SOILTYPE_AGR2" = "osi_threshold_soilcat"))
  
   # convert to the OSI score
  dt[, value := evaluate_logistic(x = A_PH_WA, b= osi_st_c1,x0 = osi_st_c2,v = osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # select and return value
  value <- dt[,value]
  
  return(value)
  
}

#' 