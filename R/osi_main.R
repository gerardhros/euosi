#' Calculate the Open Soil Index score for a single field using input from LUCAS
#' 
#' This functions wraps the functions of the OSI into one main function to calculate the score for Open Soil Index (OBI) for a location where data is available from LUCAS (and other open source datasets).
#' 
#' @param ID (character) A field id
#' @param B_LU (numeric) a series with crop codes given the crop rotation plan (source: the BRP) 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_BGZ (factor) an European region-id used for carbon analyses 
#' @param B_COUNTRY (character) The country code
#' @param B_PREC_SUM (numeric) Total potential precipitation in summer (mm)
#' @param B_PREC_WIN (numeric) Total potential precipitation in winter (mm)
#' @param B_PET_SUM (numeric) Total potential evapotranspiration in summer (mm)
#' @param B_PET_WIN (numeric) Total potential evapotranspiration in winter (mm)
#' @param B_TEMP_SUM (numeric) Mean winter temperature (degrees Celcius)
#' @param B_TEMP_WIN (numeric) Mean winter temperature (degrees Celcius)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_C_OF (numeric) The carbon content of the soil layer (g/ kg)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analyzed via Cobalt-hexamine extraction
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CACO3_IF (numeric) the percentage of CaCO3 (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param A_N_PMN (numeric) The potentially mineralizable N pool (mg N / kg soil) 
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen (mg P / kg)
#' @param A_K_AAA (numeric) The exchangeable K-content of the soil measured via ammonium acetate extraction 
#' @param A_MG_AAA (numeric) is the exchangeable Mg concentration (mg/kg)
#' @param A_B_HW (numeric) The plant available content of B in the soil (mg  B per kg) extracted by hot water
#' @param A_ZN_CC (numeric) The plant available content of Zn in the soil (mg Zn per kg) extracted by CaCl2 
#' @param A_ZN_EDTA (numeric) The plant available content of Zn in the soil (mg Zn per kg) extracted by EDTA 
#' @param output (character) An optional argument to select output: obic_score, scores, indicators, recommendations, or all. (default = all)
#' 
#' @details 
#' It is assumed that the crop series is a continuous series in decreasing order of years. So most recent year first, oldest year last.
#' 
#' @import data.table
#' 
#'  
#' @return 
#' The output of the Open Soil Index Calculator for a specific agricultural field. 
#' Depending on the output type, different output objects can be returned.
#' These include the estimated OSI scores (both total and aggregated subscores), the value of the underling indicators as well the possible recommendations to improve the soil quality.
#' The output is always a data.table.
#' 
#' @export
osi_field <- function(B_LU,B_SOILTYPE_AGR,B_COUNTRY, B_BGZ = NA_character_,
                      B_PREC_SUM = NA_real_,B_PREC_WIN = NA_real_, 
                      B_PET_SUM = NA_real_,B_PET_WIN = NA_real_,
                      B_TEMP_SUM = NA_real_,B_TEMP_WIN = NA_real_,
                      A_CLAY_MI = NA_real_,A_SAND_MI = NA_real_,
                      A_SOM_LOI = NA_real_, A_C_OF= NA_real_,A_CEC_CO = NA_real_,
                      A_PH_CC = NA_real_, A_CACO3_IF = NA_real_,
                      A_N_RT = NA_real_,A_N_PMN = NA_real_,
                      A_P_OL = NA_real_,A_K_AAA = NA_real_,A_MG_AAA = NA_real_, A_B_HW = NA_real_, 
                      A_ZN_CC = NA_real_, A_ZN_EDTA = NA_real_,
                      ID = 1, output = 'all') {
  
  # add visual bindings
  i_c_p = i_p_whc = i_p_dens = i_p_wef = i_p_paw = i_p_cr = i_p_ksat = i_b_pmn = NULL
  i_c_n = i_c_k = i_c_mg = i_c_b = i_c_zn = i_c_ph = i_c_pr =i_b_biodiv = i_e_watererosie = NULL
  i_e_carbon = i_e_nleach = i_e_pexcess = i_e_kexcess = . = NULL
  crop_code = crop_cat1 = osi_country = value = indicator = cat1 = cat2 = weight = cf = NULL
  A_CN_FR = value.w = ncat = cf_yr = NULL
  
  # define variables used within the function
  
  # combine input into one data.table
  # field properties start with B, soil analysis with A, Soil Visual Assessment ends with BCS and management starts with M
  dt <- data.table(ID = ID,
                   B_LU = B_LU,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_COUNTRY = B_COUNTRY,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI, 
                   A_SILT_MI = pmax(0,100-A_CLAY_MI - A_SAND_MI), 
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_CEC_CO = A_CEC_CO,
                   A_PH_CC = A_PH_CC,
                   A_CACO3_IF = A_CACO3_IF,
                   A_N_RT = A_N_RT,
                   A_N_PMN = A_N_PMN,
                   A_CN_FR = NA_real_,
                   A_P_OL = A_P_OL,
                   A_K_AAA = A_K_AAA,
                   A_MG_AAA = A_MG_AAA,
                   A_B_HW = A_B_HW,
                   A_ZN_CC = A_ZN_CC,
                   A_ZN_EDTA = A_ZN_EDTA)
  
  # Load in the crops data set
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.soils <- as.data.table(euosi::osi_soiltype)
  
  # add year, assuming that first year is the most recent ones
  dt[,year := 1:.N,by=ID]
  
  # calculate relevant properties from package tables
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 2]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 * 0.5]
  dt[is.na(A_CN_FR) & !is.na(A_C_OF) & !is.na(A_N_RT), A_CN_FR := A_C_OF * 1000 / A_N_RT]
  
  # --- soil chemical functions ----
  
    # calculate OSI indicator for N supply
    dt[,i_c_n := osi_c_nitrogen(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR,A_CLAY_MI = A_CLAY_MI, 
                                A_SAND_MI = A_SAND_MI, A_C_OF = A_C_OF, A_SOM_LOI = A_SOM_LOI,
                                A_CACO3_IF = A_CACO3_IF,A_N_RT = A_N_RT,B_COUNTRY = B_COUNTRY)]
    
    # calculate OSI indicator for P supply
    dt[,i_c_p := osi_c_posphor(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR, A_CLAY_MI = A_CLAY_MI, 
                               A_SAND_MI = A_SAND_MI, A_C_OF = A_C_OF, A_SOM_LOI = A_SOM_LOI,    
                               A_PH_WA = NA_real_, A_PH_CC = A_PH_CC, A_CACO3_IF = A_CACO3_IF,    
                               A_P_OL = A_P_OL, A_P_M3 = NA_real_, A_P_CAL = NA_real_,         
                               A_P_AAA = NA_real_, A_P_DL = NA_real_, A_P_AL = NA_real_,         
                               A_P_CC = NA_real_, A_P_WA = NA_real_, B_COUNTRY)]
    
    # calculate OSI indicator for K supply
    dt[,i_c_k := osi_c_potassium(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                 A_SOM_LOI = A_SOM_LOI, A_C_OF = A_C_OF, 
                                 A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,
                                 A_PH_CC = A_PH_CC, A_PH_WA = NA_real_, A_CACO3_IF = A_CACO3_IF,
                                 A_CEC_CO = A_CEC_CO, 
                                 A_K_AAA = A_K_AAA,A_K_AL = NA_real_,A_K_AN = NA_real_,A_K_CAL = NA_real_,A_K_CC = NA_real_,
                                 A_K_CO_PO = NA_real_,A_K_DL = NA_real_,A_K_M3 = NA_real_,A_K_NaAAA = NA_real_,
                                 A_K_WA = NA_real_,
                                 B_COUNTRY)]
    
    # calculate OSI indicator for Mg supply
    dt[, i_c_mg := osi_c_magnesium(B_LU = B_LU,
                                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                   A_SOM_LOI = A_SOM_LOI, A_C_OF = A_C_OF, 
                                   A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,
                                   A_PH_CC = A_PH_CC, A_CACO3_IF = A_CACO3_IF,
                                   A_CEC_CO = A_CEC_CO, 
                                   A_MG_AAA = A_MG_AAA,A_MG_AL = NA_real_, A_MG_AN = NA_real_,A_MG_CC = NA_real_,
                                   A_MG_CO_PO = NA_real_, A_MG_DL = NA_real_,A_MG_KCL = NA_real_,A_MG_M3 = NA_real_,
                                   A_MG_NaAAA = NA_real_,
                                   A_K_CO_PO = NA_real_,A_K_CC = NA_real_,
                                   B_COUNTRY = B_COUNTRY)]
     
    # calculate OSI indicator for B supply
    dt[,i_c_b := osi_c_boron(B_LU = B_LU,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI, 
                             A_SOM_LOI = A_SOM_LOI, A_PH_CC = A_PH_CC,A_B_HW = A_B_HW, 
                             B_COUNTRY = B_COUNTRY)]
    
    # calculate OSI indicator for Zn supply
    dt[,i_c_zn := osi_c_zinc(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,
                             A_SOM_LOI = A_SOM_LOI, A_C_OF = A_C_OF,
                             A_PH_WA = NA_real_, A_PH_CC = A_PH_CC,
                             A_ZN_EDTA = NA_real_,A_ZN_CC = A_ZN_CC, 
                             B_COUNTRY = B_COUNTRY)]
    
    # calculate OSI indicator for soil pH
    dt[,i_c_ph := osi_c_ph(B_LU= B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                           A_CLAY_MI= A_CLAY_MI, A_SAND_MI = A_SAND_MI,
                           A_SOM_LOI = A_SOM_LOI, A_C_OF = A_C_OF,
                           A_CA_CO_PO = NA_real_, A_MG_CO_PO = NA_real_,A_K_CO_PO = NA_real_, 
                           A_NA_CO_PO = NA_real_,
                           A_PH_WA = NA_real_, A_PH_CC= A_PH_CC, A_PH_KCL= NA_real_,
                           B_COUNTRY = B_COUNTRY)]
  
  # --- soil physical functions ----  
    
    # water holding capacity (equal for all countries)
    dt[,i_p_whc := osi_p_whc(A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SOM_LOI = A_SOM_LOI,type = 'whc')]
  
    # plant available water (equal for all countries)
    dt[,i_p_paw := osi_p_whc(A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SOM_LOI = A_SOM_LOI,type = 'paw')]
  
    # permeabilty of the soil (equal for all countries)
    dt[,i_p_ksat := osi_p_whc(A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SOM_LOI = A_SOM_LOI,type = 'ksat')]
  
    # bulk density (equal for all countries)
    dt[,i_p_dens := osi_p_density(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  
    # soil structure: crumbleability
    dt[, i_p_cr := osi_p_crumbleability(B_LU = B_LU,A_CLAY_MI = A_CLAY_MI,A_SOM_LOI = A_SOM_LOI,
                                        A_PH_CC = A_PH_CC, B_COUNTRY = B_COUNTRY)]
    
    # risk for wind erodibility (equal for all countries)
    dt[,i_p_wef := osi_p_wef(B_LU = B_LU, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, B_COUNTRY = B_COUNTRY)]
    
 # --- soil biological functions ----  
    
    # potentially mineralable N / microbial activity
    dt[,i_b_pmn := osi_b_pmn(B_LU = B_LU,B_SOILTYPE_AGR = B_SOILTYPE_AGR,A_CLAY_MI = A_CLAY_MI, 
                             A_N_PMN = NA_real_,A_N_RT = A_N_RT,B_COUNTRY = B_COUNTRY)]
    
    # conditions for biodiversity (equal for all countries)
    dt[,i_b_biodiv := osi_biodiversity(A_SOM_LOI = A_SOM_LOI, A_PH_CC = A_PH_CC)]
    
    
 # --- soil environmental function ---
    
    # risk for water erosion
    dt[,i_e_watererosie := osi_erosion(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI,
                                       A_CLAY_MI = A_CLAY_MI, A_SAND_MI=A_SAND_MI, 
                                       B_COUNTRY = B_COUNTRY)]
    
    # carbon sequestration
    dt[,i_e_carbon := osi_carbon(B_LU = B_LU,A_C_OF = A_C_OF, 
                                 B_BGZ = '4', A_CLAY_MI = A_CLAY_MI, 
                                 A_SAND_MI=A_SAND_MI, B_COUNTRY = B_COUNTRY)]
    
    # nitrate loss risks from soil organic nitrogen
    dt[, i_e_nleach := osi_gw_nleach(B_LU = B_LU, 
                                     A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,
                                     A_CACO3_IF = A_CACO3_IF,
                                     A_N_RT = A_N_RT, A_C_OF = A_C_OF, 
                                     B_PREC_SUM = B_PREC_SUM,B_PREC_WIN = B_PREC_WIN, 
                                     B_PET_SUM = B_PET_SUM,B_PET_WIN = B_PET_WIN,
                                     B_TEMP_SUM = B_TEMP_SUM,B_TEMP_WIN = B_TEMP_WIN,
                                     B_COUNTRY = B_COUNTRY)]
    
    # phosphorus excess risk indicator
    dt[,i_e_pexcess := osi_nut_p(B_LU, 
                                 B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                                 A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI,
                                 A_C_OF = A_C_OF, A_SOM_LOI = A_SOM_LOI, 
                                 A_PH_WA = NA_real_,A_PH_CC = A_PH_CC, A_CACO3_IF = A_CACO3_IF,
                                 A_P_OL = A_P_OL, A_P_M3 = NA_real_, A_P_CAL = NA_real_,
                                 A_P_AAA = NA_real_,A_P_DL = NA_real_,
                                 A_P_AL = NA_real_, A_P_CC = NA_real_, A_P_WA = NA_real_, B_COUNTRY = B_COUNTRY)]
    
    # potassium excess risk indicator
    dt[,i_e_kexcess := osi_nut_k(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                 A_SOM_LOI = A_SOM_LOI, A_C_OF = A_C_OF, 
                                 A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,
                                 A_PH_CC = A_PH_CC, A_PH_WA = NA_real_,
                                 A_CEC_CO = A_CEC_CO, 
                                 A_K_AAA = A_K_AAA,A_K_AL = NA_real_,A_K_AN = NA_real_,A_K_CAL = NA_real_,A_K_CC = NA_real_,
                                 A_K_CO_PO = NA_real_,A_K_DL = NA_real_,A_K_M3 = NA_real_,A_K_NaAAA = NA_real_,
                                 B_COUNTRY)]
    
 # --- aggregate indicators before scoring ----
  
    # add crop category to allow exclusion of certain crops (e.g. nature) in aggregation
    dt <- merge(dt,
                euosi::osi_crops[,.(crop_code,crop_cat1,osi_country)],
                by.x= c('B_LU','B_COUNTRY'),
                by.y=c('crop_code','osi_country'),
                all.x=TRUE)
    
    # set default to cropland (temporary solution since crop database is not complete yet)
    dt[is.na(crop_cat1), crop_cat1 := 'arable']
    
    # make a data.table for indicators that are not relevant for aggregation for a given crop
    # in this example: wind erosion has no impact on soil loss when grassland is there
    # a negative weight value excludes this indicator from the aggregation process
    w <- data.table(crop_cat1 = c('grasland'),
                    indicator = c('i_p_wef'),
                    weight = c(-1))
    
    # select all indicators used for scoring, only for I_C, I_P, I_B and I_B
    cols <- colnames(dt)[grepl('^ID$|^i_c|^i_p|^i_b|^i_e|year|crop_cat1',colnames(dt))]
    
    # melt dt and assign main categories for OBI
    dt.melt <- melt(dt[,mget(cols)],
                    id.vars = c('year', 'ID','crop_cat1'),
                    variable.name = 'indicator')
    
    # remove the indicators that have a NA value
    dt.melt <- dt.melt[!is.na(value)]
    
    # add main categories relevant for aggregating
    dt.melt[grepl('^i_c|^i_p|^i_b',indicator), cat1 := 'ess_prod']
    dt.melt[grepl('excess',indicator), cat1 := 'ess_env']
    dt.melt[grepl('nleac|water',indicator), cat1 := 'ess_env']
    dt.melt[grepl('carbon',indicator), cat1 := 'ess_env']
    
    # add sub categories relevant for aggregating
    dt.melt[grepl('^i_c',indicator), cat2 := 'chemistry']
    dt.melt[grepl('^i_p',indicator), cat2 := 'physics']
    dt.melt[grepl('^i_b',indicator), cat2 := 'biology']
    dt.melt[grepl('excess',indicator), cat2 := 'nutcycle']
    dt.melt[grepl('nleac|water',indicator), cat2 := 'water']
    dt.melt[grepl('carbon',indicator), cat2 := 'climate']
    
    # Determine amount of indicators per (sub)category
    dt.melt.ncat <- dt.melt[year==1][,list(ncat = .N),by = .(ID, cat1,cat2)]
    
    # add weighing factor to indicator values
    dt.melt <- merge(dt.melt,
                     w[,list(crop_cat1,indicator,weight)],
                     by = c('crop_cat1','indicator'), all.x = TRUE)
    
    # calculate correction factor for indicator values (low values have more impact than high values, a factor 5)
    dt.melt[,cf := euosi::cf_ind_importance(value)]
    
    # calculate weighted value for crop category
    dt.melt[,value.w := value]
    dt.melt[weight < 0 | is.na(value),value.w := -999]
    
    # subset dt.melt for relevant columns only
    out.score <-  dt.melt[,list(ID, cat1,cat2, year, cf, value = value.w)]
    
    # calculate weighted average per indicator category per year
    out.score <- out.score[,list(value = sum(cf * pmax(0,value) / sum(cf[value >= 0]))),by = list(ID, cat2,year)]
    
    # for case that a cat has one indicator or one year and has NA
    out.score[is.na(value), value := -999]
    
    # calculate correction factor per year; recent years are more important
    out.score[,cf := log(12 - pmin(10,year))]
    
    # calculate weighted average per indicator category per year
    out.score <- out.score[,list(value = sum(cf * pmax(0,value)/ sum(cf[value >= 0]))), by = list(ID, cat2)]
    
    # merge out with number per category
    out.score <- merge(out.score,dt.melt.ncat, by=c("ID","cat2"),all.x=TRUE)
    
    # subscores for OSI subgroups
    out.score.cat2 <- dcast(out.score,ID~cat2,value.var = 'value')
    
    # overwrite names
    setnames(out.score.cat2,
             old = c('biology', 'chemistry', 'climate', 'nutcycle', 'physics', 'water'),
             new = c('s_euosi_prod_b','s_euosi_prod_c','s_euosi_clim','s_euosi_nutcycle','s_euosi_prod_p','s_eusi_water'),
             skip_absent = TRUE)
    
    # calculate weighing factor depending on number of indicators
    out.score[,cf := log(ncat + 1)]
    
    # estimate mean BLN score per ESD
    out.score <- out.score[,list(value = sum(value * cf / sum(cf[value >= 0]))),by= c('ID','cat1')]
    
    # count number of indicators per cat1
    dt.melt.ncat <- dt.melt[year==1][,list(ncat = .N),by = .(ID, cat1)]
    
    # merge out with number per category
    out.score <- merge(out.score,dt.melt.ncat, by=c("ID","cat1"),all.x=TRUE)
    
    # OSI scores
    out.score.cat1 <-  dcast(out.score,ID~cat1,value.var = 'value')
    
    # overwrite names
    setnames(out.score.cat1,
             old = c('ess_prod','ess_env','ess_climate', 'ess_nutcycle', 'ess_water'),
             new = c('s_euosi_ess_prod','s_euosi_ess_env','s_euosi_ess_clim','s_euosi_ess_nut','s_euosi_ess_water'),
             skip_absent = TRUE)
    
    # calculate weighing factor depending on number of indicators
    out.score[,cf := log(ncat + 1)]
    
    # calculate total score over all categories
    out.score.total <- out.score[,list(s_bln_total = round(sum(value * cf / sum(cf[value >= 0])),3)),by= c('ID')]
    
    # combine OSI subscores, BLN scores and BLN total score
    out.score.osi <- merge(out.score.cat1,
                           out.score.cat2,by='ID',all.x=TRUE)
    out.score.osi <- merge(out.score.osi,out.score.total,by='ID',all.x = TRUE)
    
    # round all scores to two numbers
    cols <- colnames(out.score.osi)[grepl('^s_euosi',colnames(out.score.osi))]
    out.score.osi[, c(cols) := lapply(.SD,function(x) round(x,2)),.SDcols = cols]
    
    # remove temporary files
    rm(out.score, out.score.cat1,out.score.cat2,out.score.total)
    
    # subset dt.melt for relevant columns only
    out.ind <-  dt.melt[,list(ID, indicator,year,cf,value = value.w)]
    
    # add cf factor for the year; recent years are more important
    out.ind[,cf_yr := log(12 - pmin(10,year))]
    
    # calculate weighted average per indicator category per year
    out.ind <- out.ind[,list(value = sum(cf * cf_yr * pmax(0,value) /sum(cf[value >= 0] * cf_yr[value >= 0]))),by = list(ID, indicator)]
    
    # round at two numbers
    out.ind[, value := round(value,3)]
    
    # reformat to one line per field
    out.ind[value== -999, value := NA_real_]
    out.ind[!is.finite(value), value := NA_real_]
    out.ind <- dcast(out.ind,ID~indicator,value.var='value')
    
    # prepare output, with default
    if(output == 'all') {out <- merge(out.ind,out.score.osi,by='ID',all.x=TRUE)}
    if(output == 'scores'){out <- copy(out.score.osi)}
    if(output == 'indicators'){out <- copy(out.ind)}
  
  # return output
  return(out)
}