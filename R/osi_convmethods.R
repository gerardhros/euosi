#' Estimate soil pH values (-)
#' 
#' @param element (character) the method requested to be calculated
#' @param A_PH_KCL (numeric) soil pH determined in 1M KCl extract
#' @param A_PH_CC (numeric) soil pH determined in 0.01m CaCl2 extract
#' @param A_PH_WA (numeric) soil pH determined in water
#' 
#' @export 
osi_conv_ph <- function(element, A_PH_KCL = NA_real_,A_PH_CC = NA_real_, A_PH_WA = NA_real_){
  
  # check inputs
  checkmate::assert_numeric(A_N_RT, lower = 0.1, upper = 30000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 3000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CN_FR, lower = 5, upper = 25, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(element,choices = c('A_SOM_LOI','A_C_OF','A_N_RT','A_CN_FR'),empty.ok = FALSE)
  
  # make internal table with inputs
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_CN_FR)
  
  # estimate SOM properties from other measurements
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 2]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 * 0.5]
  dt[is.na(A_C_OF) & !is.na(A_N_RT) & !is.na(A_CN_FR), A_C_OF := A_N_RT * A_CN_FR * 0.001]
  dt[is.na(A_N_RT) & !is.na(A_C_OF) & !is.na(A_CN_FR), A_N_RT := A_C_OF * 1000 / A_CN_FR]
  dt[is.na(A_CN_FR) & !is.na(A_C_OF) & !is.na(A_N_RT), A_CN_FR := A_C_OF * 1000 / A_N_RT]
  
  # select the reqestred property
  value <- dt[,get(element)]
  
  # return value
  return(value)
  
}

#' Calculate A_B_HW
#' 
#' This function calculates the hot water extractable B content from A_B_CC (mg / kg).
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil. Options: duinzand, dekzand, zeeklei, rivierklei, maasklei, dalgrond, moerige_klei, veen en loess
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_B_CC (numeric) The extractable boron content of the soil (ug / kg), measured in a 0.01M CaCl2 extract
#' @param A_PH_CC (numeric) The pH of the soil, measured in a 0.01M CaCl2 extract (-)
#' 
#' @import data.table
#' 
#' @references Van Rotterdam & Bussink (2017) and de Haas et al. (2004)
#'
#' @export
osi_conv_hwb <- function(B_SOILTYPE_AGR, A_SOM_LOI = NA_real_, A_B_CC= NA_real_, A_PH_CC= NA_real_){
  
  # initialize global variables
  value1 = value2 = value3 = id = NULL
  
  # number of soils
  arg.length <- max(length(A_B_CC),length(A_SOM_LOI),length(B_SOILTYPE_AGR),length(A_PH_CC))
  
  # make local copy of inputs into data.table
  dt <- data.table(id = 1:arg.length,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_B_CC = A_B_CC * 0.001,
                   A_PH_CC = A_PH_CC,
                   value1 = NA_real_,
                   value2 = NA_real_,
                   value3 = NA_real_,
                   value = NA_real_)
  
  # estimate A_B_HW from A_SOM_LOI or A_B_CC using equations from Rotterdam & Bussink (2017), NMI project 1323
  dt[grepl('duin|dal|zand',B_SOILTYPE_AGR),value1 := 0.0305 * A_SOM_LOI + 0.2038]
  dt[grepl('klei|loss|loes',B_SOILTYPE_AGR) & A_PH_CC > 6,value1 := 0.2731 * A_SOM_LOI]
  dt[grepl('klei|loss|loes',B_SOILTYPE_AGR) & A_PH_CC <= 6,value1 := 0.1829 * A_SOM_LOI]
  dt[grepl('duin|dal|zand',B_SOILTYPE_AGR),value2 := 2.80599 * A_B_CC + 0.00745]
  dt[grepl('klei|loss|loes|veen',B_SOILTYPE_AGR),value2 := 4.38902 * A_B_CC - 0.10114]
  
  # estimate A_B_HW from A_B_CC using equations from De Haas et al. (2014)
  dt[grepl('duin',B_SOILTYPE_AGR), value3 := 0.0983 + 2.3256 * A_B_CC]
  dt[grepl('dekzand',B_SOILTYPE_AGR), value3 := 0.0983 + 2.3256 * A_B_CC - 0.383]
  dt[grepl('dalgrond',B_SOILTYPE_AGR), value3 := 0.0983 + 2.3256 * A_B_CC + 0.0179]
  dt[grepl('klei|loss|loes|veen',B_SOILTYPE_AGR), value3 := 0.1795 + 2.356 * A_B_CC]
  
  # estimate mean value from three regression equations
  dt[,value := mean(c(value1, value2, value3),na.rm = TRUE),by=id]
  
  # extract output variable
  value <- dt[,value]
  
  # return value
  return(value)
  
}

#' Estimate soil organic matter and nutrient values (-)
#' 
#' @param element (character) the method requested to be calculated
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param A_C_OF (numeric) The organic carbon content of the soil in g N / kg
#' @param A_CN_FR (numeric) The carbon to nitrogen ratio
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' 
#' @export 
osi_conv_ph <- function(element, A_SOM_LOI = NA_real_,A_C_OF = NA_real_, 
                        A_N_RT = NA_real_,A_CN_FR = NA_real_){
  
  # check inputs
  checkmate::assert_numeric(A_PH_KCL, lower = 3, upper = 10, any.missing = TRUE)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 10, any.missing = TRUE)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 10, any.missing = TRUE)
  checkmate::assert_subset(element,choices = c('A_PH_CC','A_PH_KCL','A_PH_WA'),empty.ok = FALSE)
  
  # make internal table with inputs
  dt <- data.table(A_PH_KCL = A_PH_KCL,
                   A_PH_CC = A_PH_CC,
                   A_PH_WA = A_PH_WA)
  
  # estimate pH from other measurements
  dt[is.na(A_PH_CC), A_PH_CC := A_PH_KCL * 0.9288 + 0.5262]
  dt[is.na(A_PH_WA), A_PH_WA := 2.23 + 0.777 * A_PH_KCL]
  dt[is.na(A_PH_KCL), A_PH_KCL := (A_PH_WA - 2.23) / 0.777]
  dt[is.na(A_PH_KCL), A_PH_KCL := (A_PH_CC - 0.5262) / 0.9288]
  
  # select the reqestred pH
  value <- dt[,get(element)]
  
  # return value
  return(value)
  
}

#' Calculate potential mineralizable N for agricultural soils
#' default values are taken from global ISRIC database
#' 
#' @param method (character) Method id that should be used for the conversion
#' @param A_N_RT (numeric)  Soil organic N content (mg N / kg) of top soil
#' @param A_CLAY_MI (numeric) Clay content (\%)
#' @param med_PMN (numeric) Regional median value of PMN (mg N / kg)
#' @param med_NRT (numeric) Regional median value of soil organic N content (mg N / kg)
#' @param med_CLAY (numeric) Regional median value of clay content (\%)
#' 
#' @import data.table
#' 
#' @export
osi_conv_npmn <- function(A_N_RT, A_CLAY_MI, med_PMN = 51.9, med_NRT = 1425, med_CLAY = 26){
  
  # initialize
  PMN = med_PMN_pred = cor_rg = NULL
  
  # Check soil inputs
  arg.length <- max(length(A_N_RT),length(A_CLAY_MI))
  checkmate::assert_subset(method,choices = c('M1'),empty.ok = FALSE)
  checkmate::assert_numeric(A_N_RT, lower = 0.1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(med_PMN, lower = 0.1, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(med_NRT, lower = 0.1, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(med_CLAY, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  
  # Collect data in a table
  # to avoid NaN from clay = 0, it it slightly increased
  dt <- data.table(id = 1:arg.length,
                   A_N_RT = A_N_RT,
                   A_CLAY_MI = A_CLAY_MI + 0.001,
                   med_PMN = med_PMN,
                   med_NRT = med_NRT,
                   med_CLAY = med_CLAY,
                   value = NA_real_)
  
  # Coefficient values of regression model to estimate PMN (mg N / kg), 
  # built on the large dataset of Dutch soils (R2 = 0.79, N=109.146 samples)
  b0 <- -3.440931;  b1 <- 1.1012449;  b2 <- 0.055858
  
  # predict PMN (based on Dutch emperical relationship)
  dt[, PMN := exp(b0 + b1 * log(A_N_RT) - b2 * log(A_CLAY_MI))]
  
  # Calculate correction factor for regional variation
  dt[, med_PMN_pred := exp(b0 + b1 * log(med_NRT) - b2 * log(med_CLAY))]
  dt[, cor_rg := med_PMN / med_PMN_pred]
  
  # adjust predicted PMN with regional correction
  dt[, value := PMN * cor_rg]
  
  # extract value
  value <- dt[, value]
  
  # return value
  return(value)
}