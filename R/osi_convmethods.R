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