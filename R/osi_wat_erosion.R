#' Calculate the water erosion index using RUSLE approach 
#' 
#' This function calculates the water erosion index 
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param B_COUNTRY (character) The country code
#'  
#' @import data.table
#' 
#' @examples 
#' osi_erosion(B_LU = '265',A_SOM_LOI = 3.5,A_CLAY_MI = 5,A_SAND_MI = 15,B_COUNTRY='NL')
#' 
#' @return 
#' The water erosion index. A numeric value.
#' 
#' @export
osi_erosion <- function(B_LU, A_SOM_LOI,A_CLAY_MI,A_SAND_MI,B_COUNTRY) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  B_TEXTURE_USDA = A_SILT_MI = perm = m = B_S = A_SOM_LOI2 = NULL
  k = B_RE = B_LS = ero = crop_c = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # get max lengthof inputs
  arg.length <- max(length(B_LU), length(A_SOM_LOI),length(A_CLAY_MI),length(A_SAND_MI),
                    length(B_COUNTRY))
  
  # add checks on input
  osi_checkvar(parm = list(B_LU = B_LU,
                           B_COUNTRY = B_COUNTRY,
                           A_SAND_MI = A_SAND_MI,
                           A_CLAY_MI = A_CLAY_MI,
                           A_SOM_LOI = A_SOM_LOI),
               fname ='osi_erosion')
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI= 100 - A_CLAY_MI - A_SAND_MI,
                   B_COUNTRY =B_COUNTRY,
                   value = NA_real_)
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(osi_country, crop_code,crop_cat1,crop_c)],
              by.x = c('B_LU','B_COUNTRY') ,
              by.y = c('crop_code','osi_country'),
              all.x=TRUE)
  
  # estimate texture class
  dt[,B_TEXTURE_USDA := osi_get_TEXTURE_USDA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  
  # calculate soil permeability class based on soil texture
  dt[B_TEXTURE_USDA =='Sa', perm := 1]
  dt[B_TEXTURE_USDA %in% c('SaL','SaLo'), perm := 2]
  dt[B_TEXTURE_USDA %in% c('L','SiL','SiLo','Lo','LoSa'), perm := 3]
  dt[B_TEXTURE_USDA %in% c('SaCL','CL','ClLo','SiClLo','SiCL','Si'), perm := 3]
  dt[B_TEXTURE_USDA %in% c('SaC'), perm := 5]
  dt[B_TEXTURE_USDA %in% c('C','Cl'), perm := 6]
  
  # calculate the textural factor m
  dt[, m :=(A_SILT_MI+0.2 * A_SAND_MI) * (100 - A_CLAY_MI)]
  
  # set soil structure (BS) => check Elise
  dt[, B_S := 3]
  
  # set soil erosion cover managemetn factor (C-factor) when missing
  dt[is.na(crop_c), crop_c := 0.23]
  
  # calculate soil erodibility
  dt[, A_SOM_LOI2 := pmin(4, A_SOM_LOI)]
  dt[, k := ((2.1*10^-4*(m^(1.14))*(12-(A_SOM_LOI2)))+(3.25*(B_S-2))+(2.5*(perm-3)))/759.3]
  
  # set two unknown parameters => ask Elise
  dt[, B_RE := 1]
  dt[, B_LS := 1]
  
  # calculate total erosion loss based on rusle
  dt[,ero := pmax(0,B_RE * k * B_LS * crop_c * 0.95)]
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = ero, b = -0.79825,x0 =	4.5, v = 1)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}
