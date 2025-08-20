#' Calculate the bulk density
#' 
#' This pedotransfer function calculates the bulk density of the soil based on texture and organic matter
#' 
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param pwarning (boolean) Option to print a warning rather than error (stop) message for input checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples
#' osi_p_density(A_SOM_LOI = 6.5, A_CLAY_MI = 28)
#' osi_p_density(A_SOM_LOI = 3.5, A_CLAY_MI = 2)
#' osi_p_density(A_SOM_LOI = c(3.5,8.5),A_CLAY_MI = c(2,28))
#' 
#' @return 
#' The bulk density of an arable soil (kg / m3) evaluated given the maximum density that limit the root penetration.
#' 
#' @export
osi_p_density <- function(A_SOM_LOI, A_CLAY_MI, pwarning = FALSE) {
  
  # set visual bindings
  dens.sand = dens.clay = cf = density = crit1 = NULL
  osi_indicator = osi_country = NULL
  
  # Load in the thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'EU' & osi_indicator == 'i_p_dens']
  checkmate::assert_data_table(dt.thresholds,max.rows = 1)
  
  # Check input
  osi_checkvar(parm = list(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI), 
               fname = 'osi_p_density',
               pwarning = pwarning)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_
                   )
  
  # calculate soil texture dependent density
  dt[, dens.sand := (1 / (0.02525 * A_SOM_LOI + 0.6541)) * 1000]
  dt[, dens.clay :=  (0.00000067*A_SOM_LOI^4 - 0.00007792*A_SOM_LOI^3 + 0.00314712*A_SOM_LOI^2 - 0.06039523*A_SOM_LOI + 1.33932206) * 1000]
    
  # fraction clay correction
  dt[, cf := pmin(1, A_CLAY_MI/25)]
    
  # clay dependent density
  dt[, density := cf * dens.clay + (1-cf) * dens.sand]
  
  # calculate critical soil density limiting root penetration (Van den Akker et al., 2020)
  dt[,crit1 := pmin(1.6,1.75 - 0.009 * A_CLAY_MI)*1000]
  
  # calculate the open soil index socre
  dt[,value := osi_evaluate_logistic(x = density, 
                                     b = dt.thresholds$osi_st_c1, 
                                     x0 = crit1, 
                                     v = dt.thresholds$osi_st_c3, 
                                     increasing = FALSE)]

  # return value
  value <- dt[, value]
  
  return(value)
  
}
