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
#' @param pwarning (boolean) Option to print a warning rather than error (stop) message for input checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen(B_LU = '256', B_SOILTYPE_AGR = 'dekzand',A_N_RT = 2500, 
#' A_CLAY_MI = 11, A_SAND_MI = 3,A_C_OF = NA,A_CACO3_IF = NA,
#' A_SOM_LOI = 4.5, B_COUNTRY = 'NL')
#'
#' @return 
#' the capacity of soils to supply  nitorgen, converted to an OSI score.
#' 
#' @export
osi_c_nitrogen <- function(B_LU, B_SOILTYPE_AGR = NA_character_,A_CLAY_MI = NA_real_,
                           A_SAND_MI = NA_real_,A_SOM_LOI = NA_real_,A_C_OF = NA_real_,
                           A_N_RT, A_CACO3_IF = NA_real_, B_COUNTRY,pwarning = FALSE) {
  
  # add visual bindings
  A_CN_FR = value = id = osi_parm_name = osi_parm_min = osi_parm_max =  NULL
  
  # note that qualitative checks on the inputs are done by the country specific functions
  
  # Check length of desired input
  arg.length <- max(length(A_N_RT), length(A_SOM_LOI),length(B_LU), length(B_SOILTYPE_AGR),length(B_COUNTRY))
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_CACO3_IF = A_CACO3_IF,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT, 
                   B_COUNTRY = B_COUNTRY,
                   value = NA_real_  
                   )
  # check inputs
  osi_checkvar(parm = list(B_LU = dt$B_LU,
                           B_COUNTRY = dt$B_COUNTRY,
                           A_CLAY_MI = dt$A_CLAY_MI,
                           A_SAND_MI = dt$A_SAND_MI,
                           A_CACO3_IF = dt$A_CACO3_IF,
                           A_SOM_LOI = dt$A_SOM_LOI,
                           A_N_RT = dt$A_N_RT),
               fname = 'osi_c_nitrogen',
               na_allowed = TRUE,
               unitcheck = TRUE,
               pwarning = pwarning)
  
  # load internal table for all euosi parameters
  dtp <- as.data.table(euosi::osi_parms)
  
  # estimate missing properties (if applicable)
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 2]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 * 0.5]
  dt[, A_CN_FR := A_C_OF * 1000/ A_N_RT]
  
  # avoid values beyond acceptable range
  dt[, A_CN_FR := pmax(dtp[osi_parm_name=='A_CN_FR',osi_parm_min], A_CN_FR)]
  dt[, A_CN_FR := pmin(dtp[osi_parm_name=='A_CN_FR',osi_parm_max], A_CN_FR)]
  dt[, A_SOM_LOI := pmax(dtp[osi_parm_name=='A_SOM_LOI',osi_parm_min], A_SOM_LOI)]
  dt[, A_SOM_LOI := pmin(dtp[osi_parm_name=='A_SOM_LOI',osi_parm_max], A_SOM_LOI)]
  dt[, A_C_OF := pmax(dtp[osi_parm_name=='A_C_OF',osi_parm_min], A_C_OF)]
  dt[, A_C_OF := pmin(dtp[osi_parm_name=='A_C_OF',osi_parm_max], A_C_OF)]
  
  # check calculated properties
  osi_checkvar(parm = list(A_CN_FR = dt$A_CN_FR,
                           A_C_OF = dt$A_C_OF,
                           A_SOM_LOI = dt$A_SOM_LOI),
               fname = 'osi_c_nitrogen',
               unitcheck = TRUE,
               pwarning = pwarning)
  
  # Austria (AT), Belgium (BE), Switzerland (CH), Czech Republic (CZ), Germany (DE)
  dt[B_COUNTRY == 'AT', value := NA_real_]
  dt[B_COUNTRY == 'BE', value := osi_c_nitrogen_be(B_LU = B_LU, A_N_RT = A_N_RT, A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, 
                                                   A_SAND_MI = A_SAND_MI, A_CACO3_IF = A_CACO3_IF, 
                                                   unitcheck = FALSE)]
  dt[B_COUNTRY == 'CH', value := NA_real_]
  dt[B_COUNTRY == 'CZ', value := NA_real_]
  dt[B_COUNTRY == 'DE', value := osi_c_nitrogen_de(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,A_C_OF = A_C_OF, 
                                                   A_SOM_LOI = A_SOM_LOI,A_N_RT = A_N_RT, 
                                                   unitcheck = FALSE)]
  
  # Denmark (DK), Estonia (EE), Greece (EL), Spain (ES),France (FR), Finland (FI) 
  dt[B_COUNTRY == 'DK', value := NA_real_]
  dt[B_COUNTRY == 'EE', value := NA_real_]
  dt[B_COUNTRY == 'EL', value := NA_real_]
  dt[B_COUNTRY == 'ES', value := NA_real_]
  dt[B_COUNTRY == 'FR', value := osi_c_nitrogen_fr(B_LU = B_LU,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,
                                                   A_C_OF = A_C_OF,A_N_RT = A_N_RT, A_CACO3_IF = A_CACO3_IF, 
                                                   unitcheck = FALSE)]
  dt[B_COUNTRY == 'FI', value := NA_real_]
  
  # Hungary (HU), Ireland (IE), Italy (IT), Latvia (LV), Lithuania (LT)
  dt[B_COUNTRY == 'HU', value := NA_real_]
  dt[B_COUNTRY == 'IE', value := NA_real_]
  dt[B_COUNTRY == 'IT', value := NA_real_]
  dt[B_COUNTRY == 'LV', value := NA_real_]
  dt[B_COUNTRY == 'LT', value := NA_real_]
  
  # the Netherlands (NL), Norway (NO),  Sweden (SE), Slovak Republic (SK), Slovenia (SL)
  dt[B_COUNTRY == 'NL', value := osi_c_nitrogen_nl(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                                                   A_SOM_LOI = A_SOM_LOI,A_N_RT = A_N_RT, 
                                                   unitcheck = FALSE)]
  dt[B_COUNTRY == 'NO', value := NA_real_]
  dt[B_COUNTRY == 'SE', value := NA_real_]
  dt[B_COUNTRY == 'SK', value := NA_real_]
  dt[B_COUNTRY == 'SL', value := NA_real_]
  
  # Poland (PL), Portugal (PT) and United Kingdom (UK)
  dt[B_COUNTRY == 'PL', value := NA_real_]
  dt[B_COUNTRY == 'PT', value := NA_real_]
  dt[B_COUNTRY == 'UK', value := NA_real_]
  
  # estimate N supply capacity for all soils without specific evaluation yet
  dt[is.na(value), value := osi_c_nitrogen_eu(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,
                                              A_SOM_LOI = A_SOM_LOI,A_C_OF = A_C_OF, A_N_RT = A_N_RT, 
                                              B_COUNTRY = B_COUNTRY, unitcheck = FALSE)]
  
  # sort the internal table on id
  setorder(dt,id)
  
  # select the output variable
  out <- dt[,value]
  
  # return the OSI score
  return(out)
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_be(B_LU = '9823',A_N_RT = 1200, A_C_OF = 25, 
#' A_CLAY_MI = 3.5, A_SAND_MI = 15,A_CACO3_IF = 0.8)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_be <- function(B_LU, A_N_RT, A_C_OF, A_CLAY_MI, A_SAND_MI,A_CACO3_IF, unitcheck = TRUE) {
  
  # set visual bindings
  osi_country = osi_indicator = id = crop_cat1 = NULL
  crop_code = osi_st_c1 = osi_st_c2 = osi_st_c3 = . = NULL
  BD = D_NHA = D_NSC = A_SILT_MI = stype = NULL
  
  # crop data
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country=='BE']
  
  # thresholds
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country == 'FR' & osi_indicator =='i_c_n']
  
  # max length of input parameters
  arg.length <- max(length(B_LU), length(A_N_RT), length(A_C_OF), 
                    length(A_CLAY_MI), length(A_SAND_MI),length(A_CACO3_IF))
  
  # check inputs
  osi_checkvar(parm = list(B_LU = B_LU,
                           B_COUNTRY = rep('BE',arg.length),
                           A_CLAY_MI = A_CLAY_MI,
                           A_SAND_MI = A_SAND_MI,
                           A_CACO3_IF = A_CACO3_IF,
                           A_C_OF = A_C_OF,
                           A_N_RT = A_N_RT),
               fname = 'osi_c_nitrogen_be',
               unitcheck = unitcheck)
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = as.character(B_LU),
                   A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   A_CACO3_IF = A_CACO3_IF,
                   value = NA_real_)
  
  # estimate bulk density
  dt[, BD := 0.80806 + 0.823844*exp(0.0578*0.1*A_C_OF) + 0.0014065 * A_SAND_MI - (0.0010299 * A_CLAY_MI)]
  
  # estimate helper variables
  dt[,D_NHA := A_N_RT * 0.2 * BD * 10000 * 1000 * 10^-6]
  dt[,D_NSC := (22/((12+A_CLAY_MI)*(545+A_CACO3_IF))) * D_NHA * 21.35 * 0.33]
  
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
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # set the order to the original inputs
  setorder(dt, id)
  
  # return value
  value <- dt[, value]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_de(B_LU= '3301010100', A_CLAY_MI = 25, 
#' A_SAND_MI = 7.5, A_SOM_LOI = 4.5, A_N_RT = 2500)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_de <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_, unitcheck = TRUE) {
  
  # add visual bindings
  A_SILT_MI = stype = cf1 = BD = NSC = NSCPS = arate = NULL
  osi_country = crop_code = crop_cat1 = . = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'DE']
  
  # max length of input parameters
  arg.length <- max(length(B_LU), 
                    length(A_CLAY_MI), length(A_SAND_MI),
                    length(A_SOM_LOI), length(A_C_OF),length(A_N_RT)) 
  
  # check inputs
  osi_checkvar(parm = list(B_LU = B_LU,
                           B_COUNTRY = rep('DE',arg.length),
                           A_CLAY_MI = A_CLAY_MI,
                           A_SAND_MI = A_SAND_MI,
                           A_SOM_LOI = A_SOM_LOI,
                           A_C_OF = A_C_OF,
                           A_N_RT = A_N_RT),
               fname = 'osi_c_nitrogen_de',
               unitcheck = unitcheck)
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_C_OF = A_C_OF,
                   A_SILT_MI = 100 - A_CLAY_MI - A_SAND_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   value = NA_real_)
  
  # merge with crop_category (arable or grassland)
  dt <- merge(dt, 
              dt.crops[, list(crop_code, crop_cat1)], 
              by.x = "B_LU", 
              by.y = "crop_code", 
              all.x = TRUE)
  
  # add missing ones
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 2]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 * 0.5]
  
  # add soil type
  dt[A_SAND_MI >= 85 & A_SILT_MI <= 25 & A_CLAY_MI <= 5 & A_C_OF < 150, stype := "BG1"]
  dt[A_SAND_MI >= 42 & A_SAND_MI <= 95 & A_SILT_MI <= 40 & A_CLAY_MI <= 17 & A_C_OF < 150,  stype :="BG2"]
  dt[A_SAND_MI >= 33 & A_SAND_MI <= 83 & A_SILT_MI <= 50 & A_CLAY_MI >= 8 & A_CLAY_MI <= 25 & A_C_OF < 150,  stype :="BG3"]
  dt[A_SAND_MI <= 75 & A_SILT_MI <= 100 & A_CLAY_MI <= 35 & A_C_OF < 150,  stype :="BG4"]
  dt[A_SAND_MI <= 65 & A_SILT_MI <= 75 & A_CLAY_MI >= 25 & A_CLAY_MI <= 100 & A_C_OF < 150, stype := "BG5"]
  dt[A_C_OF >= 150,  stype := "BG6"]
  
  # source: "Düngung in Thüringen 2007 nach ”Guter fachlicher Praxis"
  
  # add soil type based deduction factor
  dt[stype %in% c('BG1','BG2'), cf1 := 0.25]
  dt[stype %in% c('BG3','BG4'), cf1 := 0.50]
  dt[stype %in% c('BG5'), cf1 := 0.25]
  dt[stype == 'BG6', cf1 := 0]
  
  # estimate bulk density via pedotransfer function
  dt[, BD := (1/(0.02525 * A_SOM_LOI + 0.6541)) * 1000]
  
  # set annual decomposition rate of 2%
  dt[, arate := 0.02]
  
  # estimate N supply for top 30 cm soil layer
  dt[, NSC := (100 * 100 * 0.3 * BD) * A_N_RT * arate * 0.001 * 0.001]
  
  # assume that 25% is mineralized in winter, and available in pre-season
  dt[, NSCPS := 0.25 * NSC]
  
  # correction per soil type (being related to losses)
  dt[,NSCPS := (1- cf1) * NSCPS]
  
  # run default NSC calculation for Nmin in springg (optimum around 100 kg N/ ha)
  dt[, value := osi_evaluate_logistic(x = NSCPS, b =   0.13683559, x0 = -14.84508232, v =  0.03487421)]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
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
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
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
osi_c_nitrogen_fr <- function(B_LU,A_CLAY_MI,A_SAND_MI,A_C_OF,A_N_RT, A_CACO3_IF, unitcheck = TRUE) {
  
  # add visual bindings
  osi_country = osi_indicator = NULL
  BD = id = value = crop_code = crop_cat1 = D_NHA = D_NSC = osi_st_c1 = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'FR']
  
  # load and subset thresholds for situation in France
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='FR' & osi_indicator=='i_c_n']
  checkmate::assert_data_table(dt.thresholds,max.rows = 2,min.rows = 2)
  
  # check length and of arguments
  arg.length <- max(length(A_N_RT), length(A_C_OF),length(B_LU), 
                    length(A_CACO3_IF),length(A_SAND_MI),length(A_CLAY_MI))
  
  # check inputs
  osi_checkvar(parm = list(B_LU = B_LU,
                           B_COUNTRY = rep('FR',arg.length),
                           A_CLAY_MI = A_CLAY_MI,
                           A_SAND_MI = A_SAND_MI,
                           A_C_OF = A_C_OF,
                           A_N_RT = A_N_RT),
               fname = 'osi_c_nitrogen_fr',
               unitcheck = unitcheck)
  
  # Collect data in an internal table
  dt <- data.table(id = 1 : arg.length,
                   B_LU = B_LU,
                   A_CLAY_MI = A_CLAY_MI,
                   A_CACO3_IF = A_CACO3_IF,
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_)
  
  # merge with crop_category (arable or grassland)
  dt <- merge(dt, 
              dt.crops[, list(crop_code, crop_cat1)], 
              by.x = "B_LU", 
              by.y = "crop_code", 
              all.x = TRUE)
  
  # calculate derivative supporting soil properties: bulk density and N pool (kg N / ha)
  dt[, BD := 0.80806 + (0.823844*exp(0.0578*0.1*A_C_OF)) + (0.0014065 * A_SAND_MI) - (0.0010299 * A_CLAY_MI)] 
  dt[, D_NHA := A_N_RT * 0.2 * BD * 10000 * 1000 * 10^-6]  
  
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
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # setorder dt
  setorder(dt,id)
  
  # select output variable
  out <- dt[,value]
  
  # return value
  return(out)
}


#' Calculate the soil nitrogen supplying capacity in the Netherlands
#' 
#' This function calculates the NLV (nitrogen producing capacity) for the soil
#' 
#' @param B_LU (numeric) The crop code from the BRP 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#' 
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_nl(B_LU = '256', B_SOILTYPE_AGR = 'dekzand',
#' A_SOM_LOI = 4.5,A_N_RT = 2500)
#' osi_c_nitrogen_nl(1019,'dekzand',5.5,2315)
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_nl <- function(B_LU, B_SOILTYPE_AGR,A_SOM_LOI,A_N_RT, unitcheck = TRUE) {
  
  # add visual bindings
  osi_country = osi_indicator = crop_code = crop_cat1 = osi_threshold_cropcat = NULL
  A_CN_FR = BD = D_RD = D_OC = D_GA = id = value = osi_st_c1 = NULL
  soiltype.n = a = c.diss = c.ass = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[osi_country == 'NL']
  
  # load and subset thresholds to Dutch situation for PMN
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  dt.thresholds <- dt.thresholds[osi_country=='NL' & osi_indicator=='i_c_n']
  checkmate::assert_data_table(dt.thresholds,max.rows = 2,min.rows = 2)
  
  # convert B_LU to a character if the input is different
  B_LU = as.character(B_LU)
  
  # check length and of arguments
  arg.length <- max(length(A_N_RT), length(A_SOM_LOI),length(B_LU), length(B_SOILTYPE_AGR))
  
  # check inputs
  osi_checkvar(parm = list(B_LU = B_LU,
                           B_COUNTRY = rep('NL',arg.length),
                           B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                           A_SOM_LOI = A_SOM_LOI,
                           A_N_RT = A_N_RT),
               fname = 'osi_c_nitrogen_nl',
               unitcheck = unitcheck)
  
  # Collect data in an internal table
  dt <- data.table(id = 1:arg.length,
                   B_LU = as.character(B_LU),
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_SOM_LOI * 10 * 0.5 * 1000/ A_N_RT, 
                   value = NA_real_)
  
  # merge with crop_category  
  dt <- merge(dt, 
              dt.crops[, list(crop_code = as.character(crop_code), crop_cat1)], 
              by.x = "B_LU", 
              by.y = "crop_code", 
              all.x = TRUE)
  
  # calculate derivative supporting soil properties
  dt[, BD := OBIC::calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI)]
  dt[crop_cat1 == 'grassland', D_RD := 0.10]
  dt[grepl('maize|perman|arable', crop_cat1), D_RD := 0.25]
  dt[grepl('forest|nature|other', crop_cat1), D_RD := 0.25]
  dt[is.na(D_RD), D_RD := 0.25]
  dt[, D_OC := 0.58 * (A_SOM_LOI / 100) * 100 * 100 *  D_RD * BD]
  
  # calculate grassland age (to be updated later)
  dt[, D_GA := 1]
  
  # calculate the N supplying capacity for Dutch soils conform OBIC
  
    # Settings
    param.a <- 20 # Age of organic matter
    param.b <- 2^((14.1 - 9)/ 9) # Temperature correction
    param.cn.micro <- 10 # CN ratio of micro organisms
    param.t <- 5 / 12 # 5 months a year
    param.diss.micro <- 2 # Dissimilation : assimilation ratio of micro organisms
  
    # set soiltype.n
    dt[grepl('duinzand|dekzand|dalgrond|loess',B_SOILTYPE_AGR), soiltype.n := 'zand']
    dt[grepl('klei',B_SOILTYPE_AGR), soiltype.n := 'klei']
    dt[grepl('moerig|veen',B_SOILTYPE_AGR), soiltype.n := 'veen']
    
    # Calculate NLV for grass
    dt.grass <- dt[grepl('grass',crop_cat1)]
    dt.grass[soiltype.n == "zand" & D_GA < 4, a := 30.79]
    dt.grass[soiltype.n == "zand" & D_GA >= 4 & D_GA < 7, a := 28.36]
    dt.grass[soiltype.n == "zand" & D_GA >= 7 & D_GA < 10, a := 27.78]
    dt.grass[soiltype.n == "zand" & D_GA >= 10, a := 26.57]
    dt.grass[soiltype.n == "klei" & D_GA < 4, a := 34.25]
    dt.grass[soiltype.n == "klei" & D_GA >= 4 & D_GA < 7, a := 31.54]
    dt.grass[soiltype.n == "klei" & D_GA >= 7 & D_GA < 10, a := 30.90]
    dt.grass[soiltype.n == "klei" & D_GA >= 10, a := 29.56]
    
    dt.grass[soiltype.n == "zand", value := 78 + a * (A_N_RT/1000) ^ 1.0046]
    dt.grass[soiltype.n == "klei", value := 31.7 + a * (A_N_RT/1000) ^ 1.0046]
    dt.grass[soiltype.n == "zand" & value > 200, value := 200]
    dt.grass[soiltype.n == "klei" & value > 250, value := 250]
    dt.grass[soiltype.n == "veen", value := 250]
    
    # Calculate the NLV for arable land
    dt.arable <- dt[grepl('arable|maize|permanent',crop_cat1)]
    dt.arable[, c.diss := D_OC * (1 - exp(4.7 * ((param.a + param.b * param.t)^-0.6 - param.a^-0.6)))]
    dt.arable[, c.ass := c.diss / param.diss.micro]
    dt.arable[, value := ((c.diss + c.ass) / A_CN_FR) - (c.ass / param.cn.micro)]
    dt.arable[value > 250, value := 250]
    dt.arable[value < -30, value := -30]
    
    # Combine both tables and extract values
    dt <- rbindlist(list(dt.grass, dt.arable), fill = TRUE)
    
    # setorder back
    setorder(dt, id)
  
  # convert to OSI score
  
  # subset and evaluate for arable soils
  dths <- dt.thresholds[osi_threshold_cropcat == 'arable']
  dt[grepl('arable|maize|perman',crop_cat1), value := osi_evaluate_parabolic(value, x.top = dths[,osi_st_c1])]
  
  # subset and evaluate for grassland soils
  dths <- dt.thresholds[osi_threshold_cropcat == 'grassland']
  dt[grepl('grassland',crop_cat1), value := osi_evaluate_parabolic(value, x.top = dths[,osi_st_c1])]
  
  # set value for nature to NA
  dt[crop_cat1 %in% c('nature','forest','other'), value := NA_real_]
  
  # select output variable
  out <- dt[,value]
  
  # return value
  return(out)
}

#' Calculate the soil nitrogen supplying capacity template in Europe
#' 
#' This function calculates the nitrogen producing capacity of the soil, and applies correction factors used affecting N availability.
#' 
#' @param B_LU (character) The crop code
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param A_C_OF (numeric) The organic carbon content in the soil (g C / kg)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param B_COUNTRY (character) The country code
#' @param unitcheck (character) Option to switch off unit checks (TRUE or FALSE)
#'  
#' @import data.table
#' 
#' @examples 
#' osi_c_nitrogen_eu(B_LU = '256',A_N_RT = 650, A_CLAY_MI = 25, A_SAND_MI = 3,
#' A_C_OF = NA_real_,A_SOM_LOI = 4.5,B_COUNTRY = 'NL')
#' 
#' @return 
#' The capacity of the soil to supply nitrogen (kg N / ha / yr). A numeric value, converted to a OSI score.
#' 
#' @export
osi_c_nitrogen_eu <- function(B_LU = NA_character_, 
                              A_CLAY_MI= NA_real_, A_SAND_MI= NA_real_,
                              A_SOM_LOI= NA_real_,A_C_OF = NA_real_,A_N_RT = NA_real_, 
                              B_COUNTRY = NA_real_, unitcheck = TRUE) {
  
  # add visual bindings
  A_CN_FR = NSCPS = BD = value = arate = NSC = NULL
  
  # Load in the datasets
  dt.crops <- as.data.table(euosi::osi_crops)
  
  # load and subset thresholds 
  dt.thresholds <- as.data.table(euosi::osi_thresholds)
  
  # get length of function arguments
  arg.length <- max(length(B_LU),
                    length(A_CLAY_MI),length(A_SAND_MI), length(B_COUNTRY),
                    length(A_SOM_LOI),length(A_C_OF),length(A_N_RT))
  
  # check inputs
  osi_checkvar(parm = list(B_LU = B_LU,
                           B_COUNTRY = B_COUNTRY,
                           A_CLAY_MI = A_CLAY_MI,
                           A_SAND_MI = A_SAND_MI,
                           A_SOM_LOI = A_SOM_LOI,
                           A_N_RT = A_N_RT),
               fname = 'osi_c_nitrogen_eu',
               na_allowed = TRUE,
               unitcheck = unitcheck)
  
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
  dt[,A_CN_FR := A_C_OF * 1000 / A_N_RT ]
  
  # estimate bulk density via pedotransfer function
  dt[, BD := (1/(0.02525 * A_SOM_LOI + 0.6541)) * 1000]
  
  # set annual decomposition rate of 2%
  dt[, arate := 0.02]
  
  # estimate N supply for top 30 cm soil layer, set at max at 600 kg / yr
  dt[, NSC := pmin(600,(100 * 100 * 0.3 * BD) * A_N_RT * arate * 0.001 * 0.001)]
  
  # assume that 25% is mineralized in winter, and available in pre-season
  dt[, NSCPS := 0.25 * NSC]
  
  # correction for losses (a generic 50%)
  dt[, NSCPS := (1- 0.5) * NSCPS]
  
  # convert to OSI score, optimum is 40 kg N / ha / yr in spring
  dt[, value := osi_evaluate_logistic(x = NSC, b =   0.13683559, x0 = -14.84508232, v =  0.03487421)]
  
  # select output variable
  out <- dt[,value]
  
  # return value
  return(out)
}


