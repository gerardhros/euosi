# test database
  
  # clear environment and load all scripts
  rm(list = ls())
  devtools::load_all()
  
  # select a few crops
  cr_nl <- euosi::osi_crops[osi_country =='NL'][1:20,]
  cr_be <- euosi::osi_crops[osi_country =='BE'][1:20,]  
  cr_fi <- euosi::osi_crops[osi_country == 'FI'][1:20,]
  
  # assume for NL a crop rotation of 4 years
  cr_nl[, year:= rep(1:4,5)]
  cr_be[, year:= 1]
  cr_fi[, year:= 1]
  
  # combine in one data.table
  dt <- rbind(cr_nl,cr_be,cr_fi)

  # add unique row number
  dt[, id := 1:.N]
  
  # add field-ID
  dt[osi_country == 'NL', fieldid := paste0('F',year)]
  dt[osi_country != 'NL', fieldid := paste0('F',id)]

  # the Dutch case requires B_SOILTYPE_AGR as input (to be adjusted like soil type internally for others)
  dt[,B_SOILTYPE_AGR := 'dekzand']

  # set inputs
  dt[,B_COUNTRY := osi_country]
  dt[,B_LU := crop_code]

  # assume that weather is unknown, then it select from package table data
  dt[,B_PREC_SUM := NA_real_]
  dt[,B_PREC_WIN := NA_real_]
  dt[,B_PET_SUM := NA_real_]
  dt[,B_PET_WIN := NA_real_]
  dt[,B_TEMP_SUM := NA_real_]
  dt[,B_TEMP_WIN := NA_real_]
  
  # add soil property info that is always needed
  dt[, A_SOM_LOI := rnorm(.N,mean = 4.5,sd = 0.3)]
  dt[, A_CLAY_MI := runif(.N,min=0,max = 100)]
  dt[, A_SAND_MI := (100 - A_CLAY_MI) * runif(.N,min=0,max= 1)]
  dt[, A_PH_CC := rnorm(.N,mean = 5.5,sd=0.8)]
  dt[, A_CACO3_IF := fifelse(A_PH_CC < 6, 0.02, 3.5)]
  dt[, A_CEC_CO := NA_real_]
  dt[, A_C_OF := A_SOM_LOI * 10 * 0.5]  
  dt[, A_N_RT := A_C_OF *1000/ rnorm(.N,mean=12,sd=1)]  
  dt[, A_N_PMN := NA_real_]
  dt[, A_P_OL := rnorm(.N,mean=25,sd=8)]
  dt[, A_K_AAA := rnorm(.N, mean =75, sd = 12)]
  dt[, A_B_HW := rnorm(.N,mean = 0.4, sd = 0.05)]
  dt[, A_ZN_CC := rnorm(.N, mean = 14.5, sd = 1.5)]
  dt[,A_MG_AAA := rnorm(.N,mean = 30,sd=7.5)]
  dt[,ID := fieldid]  

  
  
  # select input osi_c_nitrogen
  B_LU = dt$B_LU
  B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR
  A_CLAY_MI = dt$A_CLAY_MI
  A_SAND_MI = dt$A_SAND_MI
  A_SOM_LOI = dt$A_SOM_LOI
  A_C_OF= NA_real_
  A_N_RT = dt$A_N_RT
  A_CACO3_IF = dt$A_CACO3_IF
  B_COUNTRY = dt$B_COUNTRY
  A_MG_AAA = dt$A_MG_AAA
  A_CEC_CO = NA_real_
  A_PH_WA = NA_real_ 
  A_PH_CC = dt$A_PH_CC 
  A_P_OL = dt$A_P_OL
  A_B_HW = dt$A_B_HW
  
  
  # test P
  A_P_AAA = NA_real_
  A_P_AL = NA_real_; A_P_CAL = NA_real_
  A_P_CC = NA_real_; A_P_DL = NA_real_; A_P_M3 = NA_real_
  A_P_WA = NA_real_
  
  A_MG_AL = NA_real_; A_MG_AN = NA_real_;A_MG_CC = NA_real_;
  A_MG_CO_PO = NA_real_; A_MG_DL = NA_real_;A_MG_KCL = NA_real_;A_MG_M3 = NA_real_;
  A_MG_NaAAA = NA_real_;
  A_K_CO_PO = NA_real_;A_K_CC = NA_real_
  