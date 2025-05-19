#' Calculate a carbon index
#' 
#' This function calculates a soil organic carbon index.
#' 
#' @param B_LU (numeric) The crop code
#' @param B_BGZ (factor) an European region-id used for carbon analyses 
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_carbon(B_LU,A_C_OF, B_BGZ,A_CLAY_MI=5,A_SAND_MI=25)
#' 
#' @return 
#' The carbon index. A numeric value.
#' 
#' @export
osi_carbon <- function(B_LU,A_C_OF, B_BGZ,A_CLAY_MI,A_SAND_MI) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)

  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_indicator == 'i_c_oc' & osi_country == 'EU']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_BGZ = B_BGZ,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = pmax(0,100 - A_CLAY_MI - A_SAND_MI),
                   A_C_OF = A_C_OF,
                   value = NA_real_)
  
  # estimate texture HYPRES
  dt[,B_TEXTURE_HYPRES := osi_get_TEXTURE_HYPRES(A_CLAY_MI,A_SILT_MI,A_SAND_MI,type=='code')]
  
  # merge with crop code
  dt <- merge(dt,
              dt.crops,
              by.x = 'B_LU', 
              by.y = 'crop_code',all.x=TRUE)
  
  # merge with threshold
  dt <- merge(dt,
              dt.thresholds,
              by.x = c('B_BGZ','B_TEXTURE_HYPRES','crop_cat1'))
  
  # SOC content is divided by regional "other"
  dt[,otratio := A_C_OF / osi_other]
  
  # evaluate OSI score
  dt[, value := osi_evaluate_logistic(otratio, b=osi_st_c1,x0=	osi_st_c2,v=osi_st_c3)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}