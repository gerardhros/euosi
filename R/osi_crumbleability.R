#' Calculate crumbleability index in the topsoil for all EU countries
#'
#' This function calculates the crumbleability index
#' 
#' @param B_LU (numeric) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_PH_CC (numeric) The pH measured in CaCl2
#' @param B_COUNTRY (character) The country code
#'
#' @import data.table  
#' @import OBIC
#'
#' @importFrom stats approxfun
#'
#' @examples 
#' osi_p_crumbleability(B_LU = '256', A_CLAY_MI = 5, A_SOM_LOI = 3.5, A_PH_CC = 5.5, B_COUNTRY = 'NL')
#' 
#' @return 
#' The function returns the crumbleability index
#' 
#' @export
osi_p_crumbleability <- function(B_LU,A_CLAY_MI,A_SOM_LOI,A_PH_CC,B_COUNTRY) {
  
  # Add visual bindings
  id = . = crop_code = osi_country = crop_crumbleability = NULL
  value.A_CLAY_MI = cor.A_PH_CC = cor.A_SOM_LOI = crvalue = lower = upper = NULL
  
  # load internal table
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # length of inputs
  arg.length <- max(length(B_LU),length(A_CLAY_MI), length(A_SOM_LOI),length(A_PH_CC),
                    length(B_COUNTRY))
  
  # check required inputs
  osi_checkvar(parm = list(B_COUNTRY = B_COUNTRY, B_LU = B_LU,
                           A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,
                           A_PH_CC = A_PH_CC),
               fname = 'osi_p_crumbleability')
   
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_PH_CC = A_PH_CC,
                   B_COUNTRY = B_COUNTRY,
                   value = NA_real_)
  
  # merge with crop table
  dt <- merge(dt,
              dt.crops[,.(crop_code,osi_country,crop_crumbleability)],
              by.x =c('B_LU','B_COUNTRY'),
              by.y = c('crop_code','osi_country'),
              all.x=TRUE)
  
  # for crops having no crumbleability factor assume its similar to cereals
  dt[is.na(crop_crumbleability), crop_crumbleability := 2]
  
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
              by.x = 'crop_crumbleability',
              by.y = 'crop_group',
              all.x=TRUE
              )
  
  # evaluate 
  dt[crvalue > lower, value := 0.5 + 0.5 * (crvalue - lower)/(upper - lower)]
  dt[crvalue <= lower, value := 0.5 * crvalue / lower]
  dt[, value := pmin(1,pmax(0,value))]
 
  # sort dt
  setorder(dt,id)
  
  # return selected crumbleability index
  value <- dt[, value]
  
  # return
  return(value)
  
}






