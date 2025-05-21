#' Calculate the risk for nitrogen leaching in Europe
#' 
#' This function calculates the N leaching risks for the soils in Europe
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
#' @param B_COUNTRY (character) The country code
#' 
#' @import data.table
#'
#' @return 
#' The risk for N leaching from agricultural soils in Europe
#' 
#' @export
osi_gw_nleach <- function(B_LU = NA_character_, 
                          A_CLAY_MI = NA_real_, A_SAND_MI = NA_real_,A_CACO3_IF = NA_real_,
                          A_N_RT = NA_real_, A_C_OF = NA_real_, 
                          B_PREC_SUM = NA_real_,B_PREC_WIN = NA_real_, 
                          B_PET_SUM = NA_real_,B_PET_WIN = NA_real_,
                          B_TEMP_SUM = NA_real_,B_TEMP_WIN = NA_real_,
                          B_COUNTRY) {
  
  # add visual bindings

  # load internal database with climatic data
  dt.clim <- as.data.table(euosi::osi_clim)
  
  # note that checks on the inputs are done by the country specific functions
  
  # Check length of desired input
  arg.length <- max(length(B_LU), length(A_CLAY_MI), length(A_SAND_MI), length(A_CACO3_IF),
                    length(A_N_RT), length(A_C_OF), 
                    length(B_PREC_SUM),length(B_PREC_WIN), length(B_PET_SUM),length(B_PET_WIN),
                    length(B_TEMP_SUM),length(B_TEMP_WIN),
                    length(B_COUNTRY))
  
  # Collect data in a table
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
                   A_SILT_MI = pmax(0,100-A_CLAY_MI - A_SAND_MI),
                   A_C_OF = A_C_OF,
                   A_SOM_LOI = NA_real_,
                   A_N_RT = A_N_RT,
                   A_CACO3_IF = A_CACO3_IF,
                   B_COUNTRY = B_COUNTRY,
                   value = NA_real_)
  
  # merge with climatic data, and adapt when missing
  dt <- merge(dt,dt.clim,
              by.x = 'B_COUNTRY',
              by.y = 'osi_country', 
              all.x = TRUE)
  dt[is.na(B_PREC_SUM),B_PREC_SUM := b_prec_sum]
  dt[is.na(B_PREC_WIN),B_PREC_WIN := b_prec_win]
  dt[is.na(B_PET_SUM),B_PET_SUM := b_pet_sum]
  dt[is.na(B_PET_WIN),B_PET_WIN := b_pet_win]
  dt[is.na(B_TEMP_SUM),B_TEMP_SUM := b_temp_sum]
  dt[is.na(B_TEMP_WIN),B_TEMP_WIN := b_temp_win]
  
  # estimate texture information
  dt[,B_TEXTURE_USDA := osi_get_TEXTURE_USDA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_HYPRES := osi_get_TEXTURE_HYPRES(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_BE := osi_get_TEXTURE_BE(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  dt[,B_TEXTURE_GEPPA := osi_get_TEXTURE_GEPPA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  
  # estimate missing soil properties
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 2]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 * 0.5]
  
  # calculate the OSI score for N leaching
  
    # for unknown countries, start with annual weather data
    dt[,B_PREC_Y := B_PREC_SUM + B_PREC_WIN ]
    dt[,B_PET_Y := B_PET_SUM  + B_PET_WIN]
    dt[,B_TEMP_Y := (B_TEMP_SUM + B_TEMP_WIN)/2]
  
  # Austria (AT), Belgium (BE), Switzerland (CH), Czech Republic (CZ), Germany (DE)
  dt[B_COUNTRY == 'AT', value := NA_real_]
  dt[B_COUNTRY == 'BE', value := osi_gw_nleach_be(B_LU, A_N_RT, A_C_OF, A_CLAY_MI, A_SAND_MI, A_CACO3_IF,    
                                                  B_PREC_SUM, B_PREC_WIN, B_PET_SUM, B_PET_WIN, B_TEMP_SUM,        
                                                  B_TEMP_WIN)]
  dt[B_COUNTRY == 'CH', value := NA_real_]
  dt[B_COUNTRY == 'CZ', value := NA_real_]
  dt[B_COUNTRY == 'DE', value := NA_real_]
  
  # Denmark (DK), Estonia (EE), Spain (ES),France (FR), Finland (FI) 
  dt[B_COUNTRY == 'DK', value := NA_real_]
  dt[B_COUNTRY == 'EE', value := NA_real_]
  dt[B_COUNTRY == 'ES', value := NA_real_]
  dt[B_COUNTRY == 'FR', value := osi_gw_nleach_fr(B_LU, A_N_RT, A_C_OF, A_CLAY_MI, A_SAND_MI, A_CACO3_IF,    
                                                  B_PREC_SUM, B_PREC_WIN, B_PET_SUM, B_PET_WIN, B_TEMP_SUM,        
                                                  B_TEMP_WIN)]
  dt[B_COUNTRY == 'FI', value := osi_gw_nleach_fi(B_LU, A_N_RT, A_C_OF, A_CLAY_MI, A_SAND_MI)]
  
  # Hungary (HU), Ireland (IE), Italy (IT), Latvia (LV), Lithuania (LT)
  dt[B_COUNTRY == 'HU', value := NA_real_]
  dt[B_COUNTRY == 'IE', value := NA_real_]
  dt[B_COUNTRY == 'IT', value := NA_real_]
  dt[B_COUNTRY == 'LV', value := NA_real_]
  dt[B_COUNTRY == 'LT', value := NA_real_]
  
  # the Netherlands (NL), Norway (NO),  Sweden (SE), Slovak Republic (SK), Slovenia (SL)
  dt[B_COUNTRY == 'NL', value := NA_real_]
  dt[B_COUNTRY == 'NO', value := NA_real_]
  dt[B_COUNTRY == 'SE', value := NA_real_]
  dt[B_COUNTRY == 'SK', value := NA_real_]
  dt[B_COUNTRY == 'SL', value := NA_real_]
  
  # Poland (PL), United Kingdom (UK)
  dt[B_COUNTRY == 'PL', value := NA_real_]
  dt[B_COUNTRY == 'UK', value := NA_real_]
  
  # when country specific data is missing,use then the EU template
  dt[is.na(value), value := osi_gw_nleach_eu(B_LU = B_LU, A_N_RT = A_N_RT, A_C_OF = A_C_OF, 
                                             A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,
                                             B_PREC_Y = B_PREC_Y, B_PET_Y = B_PET_Y, 
                                             B_TEMP_Y = B_TEMP_Y)]
  
  # setorderid
  setorder(dt,id)
  
  # select the output variable
  value <- dt[,value]
  
  # return the OSI score
  return(value)
  
}

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
  
  # get maximum length of input data
  arg.length <- max(length(B_LU), length(A_N_RT), length(A_C_OF), length(A_CLAY_MI), 
                    length(A_SAND_MI),length(A_CACO3_IF),
                    length(B_PREC_SUM),length(B_PREC_WIN), length(B_PET_SUM),length(B_PET_WIN),
                    length(B_TEMP_SUM),length(B_TEMP_WIN))
  
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
                   A_SILT_MI = pmax(0,100-A_CLAY_MI - A_SAND_MI),
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   A_CACO3_IF = A_CACO3_IF,
                   value = NA_real_)
  
  # estimate bulk density
  dt[, D_BDS := 0.80806 + 0.823844*exp(0.0578*0.1*A_C_OF) + 0.0014065 * A_SAND_MI - (0.0010299 * A_CLAY_MI)]
  
  # estimate helper variables
  dt[,D_NHA := A_N_RT * 0.2 * D_BDS * 10000 * 1000 * 10^-6]
  dt[,D_NSC := (22/((12+A_CLAY_MI)*(545+A_CACO3_IF))) * D_NHA * 21.35 * 0.33]
  
  # add texture class
  dt[,B_TEXTURE_USDA := osi_get_TEXTURE_USDA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_s)],
              by.x = 'B_LU', 
              by.y = 'crop_code',
              all.x=TRUE)
  
  # merge thresholds
  # dt <- merge(dt,
  #             dt.thresholds,
  #             by.x = c('crop_cat1'),
  #             by.y = c('osi_threshold_cropcat'),
  #             all.x = TRUE)
  
  # calculate N surplus that potentially can leach
  #dt[crop_cat1 == 'grassland', NS := pmax(0,D_NSC - 140)]
  #dt[!crop_cat1 == 'grassland', NS := pmax(0,D_NSC - 100)]
  dt[crop_cat1 == 'grassland', NS := D_NSC * 4/12 ]
  dt[!crop_cat1 == 'grassland', NS := D_NSC * 6/12]
  
  # calculate fle max
  dt[A_C_OF/10 > 20, flemax := 0.20]
  dt[grepl('^Sa$|^SaL$|^SaLo$',B_TEXTURE_USDA), flemax := 1.0]
  dt[grepl('^CL$|^ClLo$|^L$|^SiL$|^SaCL$|^SiCL$|^SiLo$|^Lo$|^SiClLo$|^Si$',B_TEXTURE_USDA),flemax := 0.75]
  dt[grepl('^C$|^SaC$|^SaCl$|^Cl$',B_TEXTURE_USDA),flemax := 0.5]
  
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
  
  # get maximum length of input data
  arg.length <- max(length(B_LU), length(A_N_RT), length(A_C_OF), length(A_CLAY_MI), 
                    length(A_SAND_MI),length(A_CACO3_IF),
                    length(B_PREC_SUM),length(B_PREC_WIN), length(B_PET_SUM),length(B_PET_WIN),
                    length(B_TEMP_SUM),length(B_TEMP_WIN))
  
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
                   A_SILT_MI = pmax(0,100-A_CLAY_MI - A_SAND_MI),
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   A_CACO3_IF = A_CACO3_IF,
                   value = NA_real_)
  
  # estimate bulk density
  dt[, D_BDS := 0.80806 + 0.823844*exp(0.0578*0.1*A_C_OF) + 0.0014065 * A_SAND_MI - (0.0010299 * A_CLAY_MI)]
  
  # estimate helper variables
  dt[,D_NHA := A_N_RT * 0.2 * D_BDS * 10000 * 1000 * 10^-6]
  dt[,D_NSC := (22/((12+A_CLAY_MI)*(545+A_CACO3_IF))) * D_NHA * 21.35 * 0.33]
  
  # add texture class
  dt[,B_TEXTURE_USDA := osi_get_TEXTURE_USDA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  
  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_s)],
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
  # dt[crop_cat1 == 'grassland', NS := pmax(0,D_NSC - 140)]
  # dt[!crop_cat1 == 'grassland', NS := pmax(0,D_NSC - 100)]
  dt[crop_cat1 == 'grassland', NS := D_NSC * 4/12 ]
  dt[!crop_cat1 == 'grassland', NS := D_NSC * 6/12]
  
  # calculate fle max
  dt[A_C_OF/10 > 20, flemax := 0.20]
  #dt[grepl('^Sa$|^SaL$',B_TEXTURE_USDA), flemax := 1.0]
  #dt[grepl('^CL$|^L$|^SiL$|^SaCL$',B_TEXTURE_USDA),flemax := 0.75]
  #dt[grepl('^C$|^SaC$',B_TEXTURE_USDA),flemax := 0.5]
  dt[grepl('^Sa$|^SaL$|^SaLo$',B_TEXTURE_USDA), flemax := 1.0]
  dt[grepl('^CL$|^ClLo$|^L$|^SiL$|^SaCL$|^SiCL$|^SiLo$|^Lo$|^SiClLo$|^Si$',B_TEXTURE_USDA),flemax := 0.75]
  dt[grepl('^C$|^SaC$|^SaCl$|^Cl$',B_TEXTURE_USDA),flemax := 0.5]
  
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
  
  # get maximum length of input data
  arg.length <- max(length(B_LU), length(A_N_RT), length(A_C_OF), length(A_CLAY_MI), 
                    length(A_SAND_MI))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = pmax(0,100-A_CLAY_MI - A_SAND_MI),
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   value = NA_real_)
  

  # merge crop properties
  dt <- merge(dt,
              dt.crops[,.(crop_code,crop_cat1,crop_s)],
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

#' Calculate the risk for nitrogen leaching template for EU
#' 
#' This function calculates the N leaching risks for the soil
#' 
#' @param B_LU (character) The crop code
#' @param B_PREC_Y (numeric) Total potential precipitation in year (mm)
#' @param B_PET_Y (numeric) Total potential evapotranspiration in year (mm)
#' @param B_TEMP_Y (numeric) Mean year temperature (degrees Celcius)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil (mg N / kg)
#' 
#' @import data.table
#' 
#' @examples 
#' 
#' @return 
#' The risk for N leaching
#' 
#' @export
osi_gw_nleach_eu <- function(B_LU, A_N_RT, A_C_OF, A_CLAY_MI, A_SAND_MI,
                             B_PREC_Y, B_PET_Y, B_TEMP_Y) {
  
  # set visual bindings
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # parameters
  dt.parms <- as.data.table(euosi::osi_parms)
  
  # thresholds
  # dt.thresholds <- as.data.table(euosi::osi_thresholds)

  # get maximum length of input data
  arg.length <- max(length(B_LU), length(A_N_RT), length(A_C_OF), length(A_CLAY_MI), 
                    length(A_SAND_MI),
                    length(B_PREC_Y), length(B_PET_Y),length(B_TEMP_Y))
 
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_PREC_Y = B_PREC_Y,
                   B_PET_Y = B_PET_Y,
                   B_TEMP_Y = B_TEMP_Y,
                   A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = pmax(0,100-A_CLAY_MI - A_SAND_MI),
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   value = NA_real_)

  # estimate soil texture class
  dt[,B_TEXTURE_USDA := osi_get_TEXTURE_USDA(A_CLAY_MI,A_SILT_MI,A_SAND_MI, type = 'code')]
  
  # estimate bulk density
  dt[, D_BS := 0.80806 + 0.823844*exp(0.0578*0.1*A_C_OF) + 0.0014065 * A_SAND_MI - (0.0010299 * A_CLAY_MI)]
  
  # set annual decomposition rate of 2%
  dt[, arate := 0.02]
  
  # estimate N supply for top 30 cm soil layer, set at max at 400 kg / yr
  dt[, NSC := pmin(400,(100 * 100 * 0.3 * D_BS) * A_N_RT * arate * 0.001 * 0.001)]
  
  # assume all sites are cropland, most vulnerably for leaching
  dt[, crop_cat1 := 'cropland']
  
  # set N surplus equal to N supply during winter
  dt[,NS := NSC * 6 /12]
  
  # calculate fle max
  dt[A_C_OF/10 > 20, flemax := 0.20]
  #dt[grepl('^Sa$|^SaL$',B_TEXTURE_USDA), flemax := 1.0]
  #dt[grepl('^CL$|^L$|^SiL$|^SaCL$',B_TEXTURE_USDA),flemax := 0.75]
  #dt[grepl('^C$|^SaC$',B_TEXTURE_USDA),flemax := 0.5]
  dt[grepl('^Sa$|^SaL$|^SaLo$',B_TEXTURE_USDA), flemax := 1.0]
  dt[grepl('^CL$|^ClLo$|^L$|^SiL$|^SaCL$|^SiCL$|^SiLo$|^Lo$|^SiClLo$|^Si$',B_TEXTURE_USDA),flemax := 0.75]
  dt[grepl('^C$|^SaC$|^SaCl$|^Cl$',B_TEXTURE_USDA),flemax := 0.5]
  
  # calculate flu
  dt[crop_cat1 %in% c('grassland','forest','other'), flu := 0.85]
  dt[crop_cat1 %in% c('arable','cropland','maize'), flu := 1.0]
  dt[crop_cat1 %in% c('Permanent','other','nature')|is.na(crop_cat1), flu := 0.5]
  
  # calculate PS
  dt[,B_PS := max(0,B_PREC_Y - abs(B_PET_Y))]

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
  
  # calculate ft
  dt[B_TEMP_Y <=5, ft := 1]
  dt[B_TEMP_Y > 5 & B_TEMP_Y <= 15, ft := 0.75]
  dt[B_TEMP_Y > 15, ft := 0.5]
  
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
