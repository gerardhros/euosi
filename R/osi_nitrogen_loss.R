#' Calculate the risk for nitrogen leaching in Belgium
#' 
#' This function calculates the N leaching risks for the soil
#' 
#' @param B_LU (character) The crop code
#' @param B_PREC_SUM (numeric) Total potential precipitation in summer (mm)
#' @param B_PREC_WIN (numeric) Total potential precipitation in winter (mm)
#' @param B_PET_SUM (numeric) Total potential evapotranspiration in summer (mm)
#' @param B_PET_WIN (numeric) Total potential evapotranspiration in winter (mm)
#' @param B_TEMP_SUM (numeric) Mean winter temperature (degrees Celcius)
#' @param B_TEMP_WIN (numeric) Mean winter temperature (degrees Celcius)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_CACO3_IF (numeric) The percentage of carbonated lime (\%) 
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_gw_nleach_be(B_LU = '772',A_N_RT = 1200, A_C_OF = 25, A_CLAY_MI = 3.5, A_SAND_MI = 15,A_CACO3_IF = 0.8)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_gw_nleach_be <- function(B_LU, A_N_RT, A_C_OF, A_CLAY_MI, A_SAND_MI,A_CACO3_IF,
                             B_PREC_SUM,B_PREC_WIN, B_PET_SUM,B_PET_WIN,
                             B_TEMP_SUM,B_TEMP_WIN) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = crop_k = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='BE']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  #dt.thresholds <- as.data.table(euosi::osi_thresholds)
  #dt.thresholds <- dt.thresholds[osi_country == 'FR' & osi_indicator =='i_c_k']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_PREC_SUM = B_PREC_SUM,
                   B_PREC_WIN = B_PREC_WIN, 
                   B_PET_SUM = B_PET_SUM,
                   B_PET_WIN = B_PET_WIN,
                   B_TEMP_SUM = B_TEMP_SUM,
                   B_TEMP_WIN = B_TEMP_WIN,
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
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = c('crop_cat1'),
  #             by.y = c('osi_threshold_cropcat'),
  #             all.x = TRUE)
  
  # calculate N surplus
  dt[crop_cat1 == 'grassland', NS := pmax(0,D_NSC - 140)]
  dt[!crop_cat1 == 'grassland', NS := pmax(0,D_NSC - 100)]
  
  # calculate fle max
  dt[A_C_OF/10 > 20, flemax := 0.20]
  dt[grepl('^Sa$|^SaL$',B_TEXTURE_USDA), flemax := 1.0]
  dt[grepl('^CL$|^L$|^SiL$|^SaCL$',B_TEXTURE_USDA),flexmax := 0.75]
  dt[grepl('^C$|^SaC$',B_TEXTURE_USDA),flexmax := 0.5]
  
  # calculate flu
  dt[crop_cat1 %in% c('grassland','forest'), flu := 0.85]
  dt[crop_cat1 %in% c('arable'), flu := 1.0]
  
  # calculate PS
  dt[crop_s == 'summer',B_PS := B_PREC_SUM - abs(B_PET_SUM)]
  dt[crop_s == 'winter',B_PS := B_PREC_WIN - abs(B_PET_WIN)]
  
  # calculate fp
  dt[flemax == 0.2 & B_PS <= 50,fp := 0.25]
  dt[flemax == 0.2 & B_PS > 50 & B_PS <= 100, fp := 1 + (B_PS - 100)*0.015]
  dt[flemax == 0.2 & B_PS > 100 & B_PS <= 300, fp := 1]
  dt[flemax == 0.2 & B_PS > 300 & B_PS <= 400, fp := 1 - (B_PS - 300)*0.005]
  dt[flemax == 0.2 & B_PS > 400, fp := 0.5]
  
  dt[flemax == 0.5 & B_PS <= 50,fp := 0.25]
  dt[flemax == 0.5 & B_PS > 50 & B_PS <= 100, fp := 1 + (B_PS - 100)*0.015]
  dt[flemax == 0.5 & B_PS > 100 & B_PS <= 300, fp := 1]
  dt[flemax == 0.5 & B_PS > 300 & B_PS <= 400, fp := 1 - (B_PS - 300)*0.005]
  dt[flemax == 0.5 & B_PS > 400, fp := 0.5]

  dt[flemax == 1 & B_PS <= 50,fp := 0.25]
  dt[flemax == 1 & B_PS > 50 & B_PS <= 100, fp := 1 + (B_PS - 100)*0.003]
  dt[flemax == 1 & B_PS > 100 & B_PS <= 300, fp := 1]
  dt[flemax == 1 & B_PS > 300, fp := 1] 
  
  dt[flemax == 0.75 & B_PS <= 50,fp := 0.25]
  dt[flemax == 0.75 & B_PS > 50 & B_PS <= 300, fp := 1 + (B_PS - 300)*0.003]
  dt[flemax == 0.75 & B_PS > 300, fp := 1]   

  # calculate temperature
  dt[crop_s=='summer', B_TEMP := B_TEMP_SUM]
  dt[crop_s=='winter', B_TEMP := B_TEMP_WIN]

  # calculate ft
  dt[B_TEMP <=5, ft := 1]
  dt[B_TEMP > 5 & B_TEMP <= 15, ft := 0.75]
  dt[B_TEMP > 15, ft := 0.5]

  # calculate fc
  dt[A_C_OF * 0.1 <= 1, fc := 1.00]
  dt[A_C_OF * 0.1 > 1 & A_C_OF * 0.1 <= 2, fc := 0.90]
  dt[A_C_OF * 0.1 > 2 & A_C_OF * 0.1 <= 5, fc := 0.75]
  dt[A_C_OF * 0.1 > 5, fc := 0.5]
  
  # calculate N loss
  dt[,nloss := flemax * flu * NS * pmin(fp,ft,fc)]
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = nloss, b = -0.79255, x0 = 2.5, v=1)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}

#' Calculate the risk for nitrogen leaching in France
#' 
#' This function calculates the N leaching risks for the soil
#' 
#' @param B_LU (character) The crop code
#' @param B_PREC_SUM (numeric) Total potential precipitation in summer (mm)
#' @param B_PREC_WIN (numeric) Total potential precipitation in winter (mm)
#' @param B_PET_SUM (numeric) Total potential evapotranspiration in summer (mm)
#' @param B_PET_WIN (numeric) Total potential evapotranspiration in winter (mm)
#' @param B_TEMP_SUM (numeric) Mean winter temperature (degrees Celcius)
#' @param B_TEMP_WIN (numeric) Mean winter temperature (degrees Celcius)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_CACO3_IF (numeric) The percentage of carbonated lime (\%) 
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_gw_nleach_fr(B_LU = '772',A_N_RT = 1200, A_C_OF = 25, A_CLAY_MI = 3.5, A_SAND_MI = 15,A_CACO3_IF = 0.8)
#' 
#' @return 
#' The risk of nitrogen leaching. A numeric value, converted to a OSI score.
#' 
#' @export
osi_gw_nleach_fr <- function(B_LU, A_N_RT, A_C_OF, A_CLAY_MI, A_SAND_MI,A_CACO3_IF,
                             B_PREC_SUM,B_PREC_WIN, B_PET_SUM,B_PET_WIN,
                             B_TEMP_SUM,B_TEMP_WIN
                             ) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FR']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)
  # dt.thresholds <- dt.thresholds[osi_country == 'FR' & osi_indicator =='i_c_n']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_PREC_SUM = B_PREC_SUM,
                   B_PREC_WIN = B_PREC_WIN, 
                   B_PET_SUM = B_PET_SUM,
                   B_PET_WIN = B_PET_WIN,
                   B_TEMP_SUM = B_TEMP_SUM,
                   B_TEMP_WIN = B_TEMP_WIN,
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
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = c('crop_cat1'),
  #             by.y = c('osi_threshold_cropcat'),
  #             all.x = TRUE)

  # calculate N surplus
  dt[crop_cat1 == 'grassland', NS := pmax(0,D_NSC - 140)]
  dt[!crop_cat1 == 'grassland', NS := pmax(0,D_NSC - 100)]
  
  # calculate fle max
  dt[A_C_OF/10 > 20, flemax := 0.20]
  dt[grepl('^Sa$|^SaL$',B_TEXTURE_USDA), flemax := 1.0]
  dt[grepl('^CL$|^L$|^SiL$|^SaCL$',B_TEXTURE_USDA),flexmax := 0.75]
  dt[grepl('^C$|^SaC$',B_TEXTURE_USDA),flexmax := 0.5]
  
  # calculate flu
  dt[crop_cat1 %in% c('grassland','forest'), flu := 0.85]
  dt[crop_cat1 %in% c('arable'), flu := 1.0]
  
  # calculate PS
  dt[crop_s == 'summer',B_PS := B_PREC_SUM - abs(B_PET_SUM)]
  dt[crop_s == 'winter',B_PS := B_PREC_WIN - abs(B_PET_WIN)]
  
  # calculate fp
  dt[flemax == 0.2 & B_PS <= 50,fp := 0.25]
  dt[flemax == 0.2 & B_PS > 50 & B_PS <= 100, fp := 1 + (B_PS - 100)*0.015]
  dt[flemax == 0.2 & B_PS > 100 & B_PS <= 300, fp := 1]
  dt[flemax == 0.2 & B_PS > 300 & B_PS <= 400, fp := 1 - (B_PS - 300)*0.005]
  dt[flemax == 0.2 & B_PS > 400, fp := 0.5]
  
  dt[flemax == 0.5 & B_PS <= 50,fp := 0.25]
  dt[flemax == 0.5 & B_PS > 50 & B_PS <= 100, fp := 1 + (B_PS - 100)*0.015]
  dt[flemax == 0.5 & B_PS > 100 & B_PS <= 300, fp := 1]
  dt[flemax == 0.5 & B_PS > 300 & B_PS <= 400, fp := 1 - (B_PS - 300)*0.005]
  dt[flemax == 0.5 & B_PS > 400, fp := 0.5]
  
  dt[flemax == 1 & B_PS <= 50,fp := 0.25]
  dt[flemax == 1 & B_PS > 50 & B_PS <= 100, fp := 1 + (B_PS - 100)*0.003]
  dt[flemax == 1 & B_PS > 100 & B_PS <= 300, fp := 1]
  dt[flemax == 1 & B_PS > 300, fp := 1] 
  
  dt[flemax == 0.75 & B_PS <= 50,fp := 0.25]
  dt[flemax == 0.75 & B_PS > 50 & B_PS <= 300, fp := 1 + (B_PS - 300)*0.003]
  dt[flemax == 0.75 & B_PS > 300, fp := 1]   
  
  # calculate temperature
  dt[crop_s=='summer', B_TEMP := B_TEMP_SUM]
  dt[crop_s=='winter', B_TEMP := B_TEMP_WIN]
  
  # calculate ft
  dt[B_TEMP <=5, ft := 1]
  dt[B_TEMP > 5 & B_TEMP <= 15, ft := 0.75]
  dt[B_TEMP > 15, ft := 0.5]
  
  # calculate fc
  dt[A_C_OF * 0.1 <= 1, fc := 1.00]
  dt[A_C_OF * 0.1 > 1 & A_C_OF * 0.1 <= 2, fc := 0.90]
  dt[A_C_OF * 0.1 > 2 & A_C_OF * 0.1 <= 5, fc := 0.75]
  dt[A_C_OF * 0.1 > 5, fc := 0.5]
  
  # calculate N loss
  dt[,nloss := flemax * flu * NS * pmin(fp,ft,fc)]
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = nloss, b = -0.79825, x0 = 2.5, v=1)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}
#' Calculate the risk for nitrogen leaching in Finland
#' 
#' This function calculates the N leaching risks for the soil
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_gw_nleach_fi(B_LU = '772',A_N_RT = 1200, A_C_OF = 25, A_CLAY_MI = 3.5, A_SAND_MI = 15)
#' 
#' @return 
#' The risk for N leaching
#' 
#' @export
osi_gw_nleach_fi <- function(B_LU, A_N_RT, A_C_OF, A_CLAY_MI, A_SAND_MI) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='FI']
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  #dt.thresholds <- as.data.table(euosi::osi_thresholds)
  #dt.thresholds <- dt.thresholds[osi_country == 'FI' & osi_indicator =='i_e_n']
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   value = NA_real_)
  

  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = c('crop_cat1'),
  #             by.y = c('osi_threshold_cropcat'),
  #             all.x = TRUE)
  
  # calculate N loss
  dt[,nloss := NA_real_]
  
  # convert to the OSI score
  dt[,value := osi_evaluate_logistic(x = nloss, b = -0.79255, x0 = 2.5, v=1)]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
  return(value)
  
}
