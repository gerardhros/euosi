#' Calculate the water erosion index using RUSLE approach 
#' 
#' This function calculates the water erosion index 
#' 
#' @param B_LU (character) The crop code
#' @param B_TEXTURE_USDA (character) The soil texture according to USDA classification system
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_erosion(B_LU = 'SOJ',B_TEXTURE_USDA = 'Cl', A_SOM_LOI = 3.5,A_CLAY_MI = 5,A_SAND_MI = 15)
#' 
#' @return 
#' The water erosion index. A numeric value.
#' 
#' @export
osi_erosion <- function(B_LU, B_TEXTURE_USDA, A_SOM_LOI,A_CLAY_MI,A_SAND_MI) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  #dt.crops <- dt.crops[osi_country=='BE']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  #dt.thresholds <- as.data.table(euosi::osi_thresholds)
  #dt.thresholds <- dt.thresholds[osi_country == 'BE' & osi_indicator =='i_e_p']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI= 100 - A_CLAY_MI - A_SAND_MI,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_c)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = 'crop_cat1',
  #             by.y = 'osi_threshold_cropcat',
  #             all.x = TRUE)
  
  # estimate texture class
  dt[,B_TEXTURE_USDA := osi_get_TEXTURE_USDA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  
  # calculate soil permeability class based on soil texture
  dt[B_TEXTURE_USDA=='Sa', perm := 1]
  dt[B_TEXTURE_USDA=='SaL', perm := 2]
  dt[B_TEXTURE_USDA %in% c('L','SiL'), perm := 3]
  dt[B_TEXTURE_USDA %in% c('SaCL','CL'), perm := 3]
  dt[B_TEXTURE_USDA=='SaC', perm := 5]
  dt[B_TEXTURE_USDA=='C', perm := 6]
  
  # calculate the textural factor m
  dt[, m :=(A_SILT_MI+0.2 * A_SAND_MI) * (100 - A_CLAY_MI)]
  
  # set soil structure (BS) => check Elise
  dt[, B_S := 3]
  
  # calculate soil erodibility
  dt[, A_SOM_LOI2 := pmin(4, A_SOM_LOI)]
  dt[, k := ((2.1*10^-4*(m^(1.14))*(12-(A_SOM_LOI2)))+(3.25*(B_S-2))+(2.5*(perm-3)))/759.3]
  
  # set two unknown parameters => ask Elise
  dt[, B_RE := 1]
  dt[, B_LS := 1]
  
  # calculate totl erosion loss based on rusle
  dt[,ero := pmax(0,B_RE * k * B_LS * crop_c * 0.95)]
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = ero, b = -0.79825,x0 =	4.5, v = 1)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}
