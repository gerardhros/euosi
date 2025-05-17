#' Calculate crumbleability index in the topsoil for all EU countries
#'
#' This function calculates the crumbleability index
#' 
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_PH_CC (numeric) The pH measured in CaCl2
#'
#' @import data.table  
#' @import OBIC
#'
#' @examples 
#' 
#' @return 
#' The function returns the crumbleability index
#' 
#' @export
osi_p_crumbleability <- function(A_CLAY_MI,A_SOM_LOI,A_PH_CC) {
  
  # Add visual bindings
  id = NULL
  
  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_SOM_LOI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
 
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # make lookup table
  df.lookup <- data.table(A_CLAY_MI = c(4, 10, 17, 24, 30, 40, 100),
                          value.A_CLAY_MI = c(10, 9, 8, 6.5, 5, 3.5, 1),
                          cor.A_SOM_LOI = c(0, 0.06, 0.09, 0.12, 0.25, 0.35, 0.46),
                          cor.A_PH_CC = c(0, 0, 0.15, 0.3, 0.7, 1, 1.5)
                         )
  
  # derive interpolation functions
  fun.A_CLAY_MI <- approxfun(x = df.lookup$A_CLAY_MI, y = df.lookup$value.A_CLAY_MI, rule = 2)
  fun.A_SOM_LOI <- approxfun(x = df.lookup$A_CLAY_MI, y = df.lookup$cor.A_SOM_LOI, rule = 2)
  fun.A_PH_CC <- approxfun(x = df.lookup$A_CLAY_MI, y = df.lookup$cor.A_PH_CC, rule = 2)
  
  # Calculate value.A_CLAY_MI, and corrections for SOM and pH
  dt[, value.A_CLAY_MI := fun.A_CLAY_MI(A_CLAY_MI)]
  dt[, cor.A_SOM_LOI := fun.A_SOM_LOI(A_CLAY_MI)]
  dt[A_PH_CC <= 7, cor.A_PH_CC := fun.A_PH_CC(A_CLAY_MI)]
  dt[A_PH_CC >7, cor.A_PH_CC := 0]
  
  # Calculate the crumbleability value
  dt[,crvalue := value.A_CLAY_MI + cor.A_SOM_LOI * A_SOM_LOI - cor.A_PH_CC * pmax(0, 7 - A_PH_CC)]
  
  # Limit the value to 1 - 10
  dt[,crvalue := pmin(10,pmax(1,crvalue))]
  
  # merge with evaluation crumbleability table from OBIC
  dt <- merge(dt, 
              OBIC::eval.crumbleability, 
              by=c("crop_crumbleability" = "crop_group" )
              )
  
  # evaluate 
  dt[crvalue > lower, value := 0.5 + 0.5 * (crvalue - lower)/(upper - lower)]
  dt[crvalue <= lower, value := 0.5 * crvalue / lower]
  dt[, value := pmin(1,pmax(0,value))]
 
  # return selected crumbleability index
  value <- dt[, value]
  
  # return
  return(value)
  
}






