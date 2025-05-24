#' Calculate indicator for plant available water
#'
#' This function calculates the plant available water index.
#' 
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content of the soil (g / kg)
#'
#' @import data.table  
#' @import OBIC
#'
#' @examples 
#' osi_p_paw(A_CLAY_MI = 4.5, A_SAND_MI = 23, A_SILT_MI = 72.5,A_C_OF = 23)
#' 
#' @export
osi_p_paw <- function(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_C_OF) {
  
  # add visual bindings
  osi_country = osi_indicator = NULL
  mc_fc = mc_wp = mc_paw = NULL
  
  # Load in the thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'EU' & osi_indicator == 'i_p_paw']
  
  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_SAND_MI),length(A_SILT_MI), length(A_C_OF))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_data_table(dt.thresholds,max.rows = 1, min.rows = 1)
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_LOAM_MI = (A_CLAY_MI + A_SILT_MI),
                   A_C_OF = A_C_OF,
                   value = NA_real_)
  
  # estimate moisture content at Field Capacity
  dt[, mc_fc := 0.2449 - 0.1887 * (1/((A_C_OF*10^-3*100)+1)) + 
                0.004527 * A_CLAY_MI + 0.001535 * A_SILT_MI + 
                0.001442 * A_SILT_MI * (1/((A_C_OF*10^-3*100)+1)) - 
                0.00005110 * A_SILT_MI * A_CLAY_MI + 
                0.0008676 * A_CLAY_MI * (1/((A_C_OF*10^-3*100)+1))]
  
  # estimate moisture content af Wilting Point
  dt[,mc_wp := 0.09878 + 0.002127* A_CLAY_MI - 
               0.0008366 * A_SILT_MI - 0.07670 *(1/((A_C_OF*10^-3*100)+1)) + 
               0.00003853 * A_SILT_MI * A_CLAY_MI + 
               0.002330 * A_CLAY_MI * (1/((A_C_OF*10^-3*100)+1)) + 
               0.0009498 * A_SILT_MI * (1/((A_C_OF*10^-3*100)+1))]
  
  # PAW is difference between field capacity and WP, and convert to mm 
  dt[, mc_paw := (mc_fc - mc_wp) * 0.2 * 1000]
  
  # evaluate PAW  
  dt[, value := osi_evaluate_logistic(x = mc_paw,
                                      b = dt.thresholds$osi_st_c1,
                                      x0 = dt.thresholds$osi_st_c2,
                                      v = dt.thresholds$osi_st_c3)]
  
  # return selected PAW
  value <- dt[, value]
  
  # return
  return(value)
  
}
