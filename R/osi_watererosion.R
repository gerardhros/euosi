#' Calculate indicator for water erodibility (sheet and rill erosion)
#'
#' This function calculates the risk for water erodibility of soils, based on the RUSLE equation using
#' soil erodibility calculated based on soil data (https://doi.org/10.1016/j.scitotenv.2014.02.010) equation 1
#' rainfall erosivity B_RE extracted from map (https://doi.org/10.1016/j.scitotenv.2015.01.008)
#' C factor based on https://doi.org/10.1016/j.still.2021.105155 taken if possible for 3 years before actual year.
#' LS slope _LS length and steepness extracted from map (https://doi.org/10.3390/geosciences5020117)
#' P practices is set to 1 (no information)
#' 
#' @param B_LU (numeric) The crop code of current crop
#' @param B_LU_a2 (numeric) The crop code of crop 2 seasons before
#' @param B_LU_a1 (numeric) The crop code of crop 1 season before
#' @param B_LU_p1 (numeric) The crop code of crop 1 season before
#' @param B_LU_p2 (numeric) The crop code of crop 2 seasons before 
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SAND_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The soil organic matter content (\%)
#' @param B_s (character) The soil structural class from the EU soil database (\-)
#' @param B_TEXTURE_USDA (character) The soil texture class according to the USDA classification (\-)
#' @param B_RE (numeric) The rainfall erosivity extracted spatially
#' @param B_LS (numeric) The slope length and steepness factor extracted spatially 

#'
#' @examples 

#' @return 
#' The water erosion without considering management practices. A numeric value.

osi_p_wae <- function(B_LU,A_CLAY_MI,A_SILT_MI,A_SAND_MI,A_SOM_LOI,B_s,B_LU_a2,B_LU_a1,B_LU_p1,B_LU_p2,B_RE,B_LS) {
  
  # add visual bindings
  id = crop_code = crop_cat1 = k = s = p = NULL
  
  # ensure B_LU is character
  B_LU <- as.character(B_LU)
  B_LU_a1 <- as.character(B_LU)
  B_LU_a2 <- as.character(B_LU)
  B_LU_p1 <- as.character(B_LU)
  B_LU_p2 <- as.character(B_LU)
  B_s <- as.character(B_s)
  B_TEXTURE_USDA <- as.character(B_TEXTURE_USDA)
  
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_CLAY_MI),length(A_SAND_MI),LENGTH(A_SOM_LOI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(B_RE, lower = 0, upper = 5000, any.missing = FALSE)
  checkmate::assert_numeric(B_LS, lower = 0, upper = 100, any.missing = FALSE)
  
  checkmate::assert_character(B_TEXTURE_USDA, any.missing = FALSE, min.len = 1, len = arg.length)
  
  checkmate::assert_character(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  
  checkmate::assert_character(B_LU_a1, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_a1, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  
  checkmate::assert_character(B_LU_a2, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_a2, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  
  checkmate::assert_character(B_LU_p1, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_p1, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  
  checkmate::assert_character(B_LU_p2, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_p2, choices = unique(dt.crops$crop_code), empty.ok = FALSE)
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_LU_a1 = B_LU_a1,
                   B_LU_a2 = B_LU_a2,
                   B_LU_p1 = B_LU_p1,
                   B_LU_p2 = B_LU_p2,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   B_TEXTURE_USDA = B_TEXTURE_USDA,
                   B_s = B_s,
                   B_RE = B_RE,
                   B_LS = B_LS,
                   value = NA_real_
  )
  
  # Change B_s into number
  dt[s := ifelse(dt$B_s=='G',1,ifelse(dt$B_s=='N',2,ifelse(dt$B_s=='P',3,ifelse(dt$B_s=='H',4,NA))))]
  
  # Define soil permeability class based on soil textural class
  dt[p := ifelse(dt$B_TEXTURE_USDA=='sand',1,ifelse(dt$B_TEXTURE_USDA=='loamy sand'|dt$B_TEXTURE_USDA=='sandy loam',2,
                                                    ifelse(dt$B_TEXTURE_USDA=='loam'|dt$B_TEXTURE_USDA=='silty loam',3,
                                                           ifelse(dt$B_TEXTURE_USDA=='sandy clay loam'|dt$B_TEXTURE_USDA=='clay loam',4,
                                                                  ifelse(dt$B_TEXTURE_USDA=='silty clay loam'|dt$B_TEXTURE_USDA=='sand clay',5,
                                                                         ifelse(dt$B_TEXTURE_USDA=='silty clay'|dt$B_TEXTURE_USDA=='clay',6,NA))))))]
  # Calculate the textural factor
  dt[m := (A_SILT_MI+(0.2*A_SAND_MI))*(100-A_CLAY_MI)]
  
  # Calculate soil erodibility
  dt[,k := ((2.1*10^-4*m*(10-A_SOM_LOI))+(3.25*(s-2))+(2.5*(p-3))/100)*0.1317]
  
  
  # add crop names and crop_c values
  dt <- merge(dt, dt.crops[, list(crop_code, crop_c)], by.x = "B_LU", by.y = "crop_code",all.x = TRUE)
  dt$crop_c_a2 <- dt.crops[dt.crops$crop_code==B_LU_a2]$crop_c
  dt$crop_c_a1 <- dt.crops[dt.crops$crop_code==B_LU_a1]$crop_c
  dt$crop_c_p1 <- dt.crops[dt.crops$crop_code==B_LU_p1]$crop_c
  dt$crop_c_p2 <- dt.crops[dt.crops$crop_code==B_LU_p2]$crop_c
  
  #When crop = NA, crop_c = empty string; when crop_c = NA, NA
  dt[dt=='']<-NA
  
  
  # calculate C factor
  plus <- function(x) {
    if(all(is.na(x))){
      c(x[0],NA)} else {
        sum(x,na.rm = TRUE)}
  }
  dt[,c := plus(c(crop_c + crop_c_a2 + crop_c_a2 + crop_c_p1 + crop_c_p2))]
  
  # calculate erosion
  dt[,waer := B_RE * B_LS * c * k * 1]
  dt[,value := osi_evaluate_logistic(x = waer, b= -1.99564,x0 = 1,v = 1)]
  
  # return
  return(value)
  
}
  
  
  

  
  
