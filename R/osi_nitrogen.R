#' Calculate the soil nitrogen supplying capacity (wrapper function)
#' 
#' This function calculates the nitrogen supplying capacity for soils in Europe
#'  
#' @param B_LU (numeric) The crop code from the BRP 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_CACO3_IF (numeric) The percentage of carbonated lime (\%) 
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#' @param B_COUNTRY (character) The country code
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen(B_LU = 256, B_SOILTYPE_AGR = 'dekzand',A_N_RT = 2500, 
#' A_CLAY_MI = 11, A_SAND_MI = 3,A_C_OF = NA,A_CACO3_IF = NA,
#' A_SOM_LOI = 4.5, B_COUNTRY = 'NL')
#'
#' @return 
#' the capacity of soils to supply  nitorgen, converted to an OSI score.
#' 
#' @export
osi_c_nitrogen <- function(B_LU, B_SOILTYPE_AGR = NA_character_,A_CLAY_MI = NA_real_,A_SAND_MI = NA_real_,A_SOM_LOI = NA_real_,A_C_OF = NA_real_,
                           A_N_RT, A_CACO3_IF = NA_real_, B_COUNTRY) {
  
  # add visual bindings
  A_CN_FR = value = id = NULL
  
  # note that qualitative checks on the inputs are done by the country specific functions
  
  # Check length of desired input
  arg.length <- max(length(A_N_RT), length(A_SOM_LOI),length(B_LU), length(B_SOILTYPE_AGR),length(B_COUNTRY))
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_COUNTRY = B_COUNTRY,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT, 
                   value = NA_real_  
                   )
  
  # estimate missing properties (if applicable)
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 2]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 * 0.5]
  dt[, A_CN_FR := A_C_OF * 1000/ A_N_RT]
  
  # Austria (AT), Belgium (BE), Switzerland (CH), Czech Republic (CZ), Germany (DE)
  dt[B_COUNTRY == 'AT', value := osi_c_nitrogen_at(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'BE', value := osi_c_nitrogen_be(B_LU = B_LU, A_N_RT = A_N_RT, A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_CACO3_IF = A_CACO3_IF)]
  dt[B_COUNTRY == 'CH', value := osi_c_nitrogen_ch(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'CZ', value := osi_c_nitrogen_cz(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'DE', value := osi_c_nitrogen_de(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  
  # Denmark (DK), Estonia (EE), Spain (ES),France (FR), Finland (FI) 
  dt[B_COUNTRY == 'DK', value := osi_c_nitrogen_dk(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'EE', value := osi_c_nitrogen_ee(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'ES', value := osi_c_nitrogen_es(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'FR', value := osi_c_nitrogen_fr(B_LU = B_LU,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,
                                                   A_C_OF = A_C_OF,A_N_RT = A_N_RT, A_CACO3_IF = A_CACO3_IF)]
  dt[B_COUNTRY == 'FI', value := osi_c_nitrogen_fi(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  
  # Hungary (HU), Ireland (IE), Italy (IT), Latvia (LV), Lithuania (LT)
  dt[B_COUNTRY == 'HU', value := osi_c_nitrogen_hu(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'IE', value := osi_c_nitrogen_ie(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'IT', value := osi_c_nitrogen_it(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'LV', value := osi_c_nitrogen_lv(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'LT', value := osi_c_nitrogen_lt(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  
  # the Netherlands (NL), Norway (NO),  Sweden (SE), Slovak Republic (SK), Slovenia (SL)
  dt[B_COUNTRY == 'NL', value := osi_c_nitrogen_nl(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                                                   A_SOM_LOI = A_SOM_LOI,A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'NO', value := osi_c_nitrogen_no(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'SE', value := osi_c_nitrogen_se(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'SK', value := osi_c_nitrogen_sk(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'SL', value := osi_c_nitrogen_sl(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  
  # Poland (PL), United Kingdom (UK)
  dt[B_COUNTRY == 'PL', value := osi_c_nitrogen_pl(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  dt[B_COUNTRY == 'UK', value := osi_c_nitrogen_uk(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, A_N_RT = A_N_RT)]
  
  # sort the internal table on id
  setorder(dt,id)
  
  # select the output variable
  out <- dt[,value]
  
  # return the OSI score
  return(out)
  
}

#' Calculate the soil nitrogen supplying capacity in Austria
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_at(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_at <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}

#' Calculate the soil nitrogen supplying capacity in Belgium
#' 
#' This function calculates the NSC (nitrogen supply capacity) for the soil
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_CACO3_IF (numeric) The percentage of carbonated lime (\%) 
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_be(B_LU = '772',A_N_RT = 1200, A_C_OF = 25, A_CLAY_MI = 3.5, A_SAND_MI = 15,A_CACO3_IF = 0.8)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_be <- function(B_LU, A_N_RT, A_C_OF, A_CLAY_MI, A_SAND_MI,A_CACO3_IF) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = crop_k = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='BE']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FR' & osi_indicator =='i_c_n']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   A_CACO_IF = A_CACO_IF,
                   value = NA_real_)
  
  # estimate bulk density
  dt[, D_BDS := 0.80806 + 0.823844*exp(0.0578*0.1*A_C_OF) + 0.0014065 * A_SAND_MI - (0.0010299 * A_CLAY_MI)]
  
  # estimate helper variables
  dt[,D_NHA := A_N_RT * 0.2 * D_BDS * 10000 * 1000 * 10^-6]
  dt[,D_NSC := (22/((12+A_CLAY_MI)*(545+A_CACO_IF))) * D_NHA * 21.35 * 0.33]
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # merge thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = c('crop_cat1'),
              by.y = c('osi_threshold_cropcat'),
              all.x = TRUE)
  
  # convert to the OSI score
  dt[,value := osi_evaluate_parabolic(x = D_NSC, x.top = osi_st_c1)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}
#' Calculate the soil nitrogen supplying capacity in Switzerland
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_ch(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_ch <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}

#' Calculate the soil nitrogen supplying capacity in Czech Republic
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_cz(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_cz <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}

#' Calculate the soil nitrogen supplying capacity in Germany
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_de(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_de <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}

#' Calculate the soil nitrogen supplying capacity in Denmark
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_dk(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_dk <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}

#' Calculate the soil nitrogen supplying capacity in Estonia
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_ee(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_ee <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}

#' Calculate the soil nitrogen supplying capacity in Spain
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_es(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_es <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}


#' Calculate the soil nitrogen supplying capacity in Finland
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_fi(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_fi <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}

#' Calculate the soil nitrogen supplying capacity in France
#' 
#' This function calculates the NSC (nitrogen supply capacity) for the soil
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_CACO3_IF (numeric) The percentage of carbonated lime (\%) 
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_fr(B_LU = 'CML', A_CLAY_MI = 15, A_SAND_MI = 20, 
#' A_C_OF = 45,A_N_RT = 2500,A_CACO3_IF = 0)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_fr <- function(B_LU,A_CLAY_MI,A_SAND_MI,A_C_OF,A_N_RT, A_CACO3_IF) {
  
  # add visual bindings
  osi_country = osi_indicator = NULL
  D_BDS = id = value = crop_code = crop_cat1 = D_NHA = D_NSC = osi_st_c1 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # load and subset thresholds for situation in France
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='FR' & osi_indicator=='i_c_n']
  
  # check length and of arguments
  arg.length <- max(length(A_N_RT), length(A_C_OF),length(B_LU), length(A_CACO3_IF),length(A_SAND_MI))
  checkmate::assert_numeric(A_N_RT, lower = 0.1, upper = 30000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 3000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_LU,dt.crops$crop_code)
  checkmate::assert_data_table(dt.thresholds,max.rows = 2,min.rows = 2)
  
  # Collect data in an internal table
  dt <- data.table(id = 1 : arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_CACO3_IF = A_CACO3_IF,
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_
  )
  
  # merge with crop_category (arable or grassland)
  dt <- merge(dt, 
              dt.crops[, list(crop_code, crop_cat1)], 
              by.x = "B_LU", 
              by.y = "crop_code", 
              all.x = TRUE)
  
  # calculate derivative supporting soil properties: bulk density and N pool (kg N / ha)
  dt[, D_BDS := 0.80806 + (0.823844*exp(0.0578*0.1*A_C_OF)) + (0.0014065 * A_SAND_MI) - (0.0010299 * A_CLAY_MI)] 
  dt[, D_NHA := A_N_RT * 0.2 * D_BDS * 10000 * 1000 * 10^-6]  
  
  # calculate the N supplying capacity for France (kg N/ha/yr)
  dt[, D_NSC := ((22/((12+A_CLAY_MI)*(545+A_CACO3_IF))) * D_NHA)*21.35 * 0.33]
  
  # convert to OSI score
  
  # merge with thresholds
  dt <- merge(dt,
              dt.thresholds,
              by.x = 'crop_cat1',
              by.y ='osi_threshold_cropcat',
              all.x = TRUE
  )
  
  # subset and evaluate for arable or grassland soils depending on crop category
  dt[, value := osi_evaluate_parabolic(D_NSC, x.top = osi_st_c1)]
  
  # ensure that value is between 0 and 1
  dt[, value := pmax(0,pmin(1,value))]
  
  # setorder dt
  setorder(dt,id)
  
  # select output variable
  out <- dt[,value]
  
  # return value
  return(out)
}

#' Calculate the soil nitrogen supplying capacity in Hungary
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_hu(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_hu <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}


#' Calculate the soil nitrogen supplying capacity in Ireland
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_ie(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_ie <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}

#' Calculate the soil nitrogen supplying capacity in Italy
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_it(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_it <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}

#' Calculate the soil nitrogen supplying capacity in Latvia
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_lv(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_lv <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}


#' Calculate the soil nitrogen supplying capacity in Lithuania
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_lt(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_lt <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}

#' Calculate the soil nitrogen supplying capacity in the Netherlands
#' 
#' This function calculates the NLV (nitrogen producing capacity) for the soil
#' 
#' @param B_LU (numeric) The crop code from the BRP 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_nl(B_LU = 256, B_SOILTYPE_AGR = 'dekzand',A_SOM_LOI = 4.5,A_N_RT = 2500)
#' osi_c_nitrogen_nl(1019,'dekzand',5.5,2315)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_nl <- function(B_LU, B_SOILTYPE_AGR,A_SOM_LOI,A_N_RT) {
  
  # add visual bindings
  osi_country = osi_indicator = crop_code = crop_cat1 = osi_threshold_cropcat = NULL
  A_CN_FR = D_BDS = D_RD = D_OC = D_GA = id = value = osi_st_c1 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'NL']
  
  # load and subset thresholds to Dutch situation for PMN
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='NL' & osi_indicator=='i_c_n']
  
  # convert B_LU to numeric if the input is character
  B_LU = as.integer(B_LU)
  
  # check length and of arguments
  arg.length <- max(length(A_N_RT), length(A_SOM_LOI),length(B_LU), length(B_SOILTYPE_AGR))
  checkmate::assert_numeric(A_N_RT, lower = 0.1, upper = 30000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU, choices = as.integer(unique(dt.crops$crop_code)), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(euosi::osi_soiltype$osi_soil_cat1), empty.ok = FALSE)
  checkmate::assert_data_table(dt.thresholds,max.rows = 2,min.rows = 2)
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_SOM_LOI * 10 * 0.5 * 1000/ A_N_RT, 
                   value = NA_real_
  )
  
  # merge with crop_category  
  dt <- merge(dt, 
              dt.crops[, list(crop_code = as.integer(crop_code), crop_cat1)], 
              by.x = "B_LU", 
              by.y = "crop_code", 
              all.x = TRUE)
  
  # calculate derivative supporting soil properties
  dt[, D_BDS := OBIC::calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI)]
  dt[, D_RD := OBIC::calc_root_depth(B_LU_BRP = B_LU)]
  dt[, D_OC := OBIC::calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
  dt[, D_GA := OBIC::calc_grass_age(id, B_LU_BRP = B_LU)]
  
  # calculate the N supplying capacity for the Netherlands using the Dutch OBIC
  dt[,value := OBIC::calc_nlv(B_LU_BRP = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                              A_N_RT = A_N_RT, A_CN_FR = A_CN_FR, D_OC = D_OC, D_BDS = D_BDS, 
                              D_GA = D_GA)]
  
  # convert to OSI score
  
  # subset and evaluate for arable soils
  dths <- dt.thresholds[osi_threshold_cropcat == 'arable']
  dt[grepl('arable|maize',crop_cat1), value := osi_evaluate_parabolic(value, x.top = dths[,osi_st_c1])]
  
  # subset and evaluate for grassland soils
  dths <- dt.thresholds[osi_threshold_cropcat == 'grassland']
  dt[grepl('grassland',crop_cat1), value := osi_evaluate_parabolic(value, x.top = dths[,osi_st_c1])]
  
  # set OSI score for others  
  dt[grepl('nature',crop_cat1), value := 1]
  
  # select output variable
  out <- dt[,value]
  
  # return value
  return(out)
}

#' Calculate the soil nitrogen supplying capacity in Norway
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_no(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_no <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}

#' Calculate the soil nitrogen supplying capacity in Poland
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_pl(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_pl <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}

#' Calculate the soil nitrogen supplying capacity in Sweden
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_se(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_se <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}


#' Calculate the soil nitrogen supplying capacity in Slovak Republic
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_sk(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_sk <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}


#' Calculate the soil nitrogen supplying capacity in Slovena
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_sl(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_sl <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}


#' Calculate the soil nitrogen supplying capacity in United Kingdom
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_uk(BLU= 2515, A_CLAY_MI = 25, A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_uk <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # run default NSC calculation
  dt[, value := os_c_nitrogen_default(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, S_SAND_MI = A_SAND_MI,
                                      A_SOM_LOI = A_SOM_LOI, A_N_RT = A_N_RT)]
  
  # select calculated NSC
  value <- dt[,value]
  
  # return
  return(value)
}



#' Calculate the soil nitrogen supplying capacity in X
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_tmp
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_eu <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_) {
  
  # add visual bindings
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # load and subset thresholds 
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  
  # check length and of arguments
  arg.length <- max(length(B_LU),length(A_CLAY_MI),length(A_SAND_MI),length(A_SOM_LOI),length(A_C_OF),length(A_N_RT))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_N_RT, lower = 0.1, upper = 30000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   value = NA_real_
  )
  
  # estimate texture information
  # dt[,B_TEXTURE_USDA := osi_get_TEXTURE_USDA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  # dt[,B_TEXTURE_HYPRES := osi_get_TEXTURE_HYPRES(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  # dt[,B_TEXTURE_BE := osi_get_TEXTURE_BE(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  # dt[,B_TEXTURE_GEPPA := osi_get_TEXTURE_GEPPA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  
  # estimate missing soil properties (from defaults in LUCAS)
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 2]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 * 0.5]
  dt[,A_CN_FR : A_C_OF * 1000 / A_N_RT ]
  
  # estimate bulk density via pedotransfer function
  dt[, BDS := (1/(0.02525 * A_SOM_LOI + 0.6541)) * 1000]
  
  # set annual decomposition rate of 2%
  dt[, arate := 0.02]
  
  # estimate N supply for top 30 cm soil layer, set at max at 400 kg / yr
  dt[, NSC := pmin(400,(100 * 100 * 0.3 * B_DS) * A_N_RT * arate * 0.001 * 0.001)]
  
  # convert to OSI score, optimum is 125 kg N / ha / yr
  dt[, value := osi_evaluate_logistic(x = NSC, b = 0.04856090, x0 = -17.79554596,v= 0.08588367)]
  
  # select output variable
  out <- dt[,value]
  
  # return value
  return(out)
}


