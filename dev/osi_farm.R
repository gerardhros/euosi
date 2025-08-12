# test database for a virtual farm
  
  # clear environment and load all scripts
  rm(list = ls())
  devtools::load_all()
  
  # select a few crops
  cr_nl <- euosi::osi_crops[osi_country =='NL'][1:20,.(osi_country,crop_code)]
  cr_be <- euosi::osi_crops[osi_country =='BE'][1:20,.(osi_country,crop_code)]  
  cr_fi <- euosi::osi_crops[osi_country == 'FI'][1:20,.(osi_country,crop_code)]
  
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
  
  # set fixed seed
  set.seed(123)
  
  # add soil property info that is always needed
  dt[, A_SOM_LOI := rnorm(.N,mean = 4.5,sd = 0.3)]
  dt[, A_CLAY_MI := runif(.N,min=0,max = 100)]
  dt[, A_SAND_MI := (100 - A_CLAY_MI) * runif(.N,min=0,max= 1)]
  dt[, A_PH_CC := 1.5 + rnorm(.N,mean = 5.5,sd=0.8)]
  dt[, A_CACO3_IF := fifelse(A_PH_CC < 6, 0.02, 3.5)]
  dt[, A_CEC_CO := NA_real_]
  dt[, A_C_OF := A_SOM_LOI * 10 * 0.5]  
  dt[, A_N_RT := A_C_OF *1000/ rnorm(.N,mean=12,sd=1)]  
  dt[, A_N_PMN := exp(-3.440931 + 1.1012449 * log(A_N_RT) - 0.055858 * log(A_CLAY_MI))]
  dt[, A_P_OL := 4 + rnorm(.N,mean=25,sd=8)]
  dt[, A_K_AAA := 25 + rnorm(.N, mean =75, sd = 12)]
  dt[, A_B_HW := 0.1 + rnorm(.N,mean = 0.4, sd = 0.05)]
  dt[, A_ZN_CC := rnorm(.N, mean = 14.5, sd = 1.5)]
  dt[,A_MG_AAA := 15 + rnorm(.N,mean = 30,sd=7.5)]
  dt[,ID := fieldid]  

  # remove temporary variables
  dt[,c('osi_country','crop_code','year','id','fieldid') := NULL ]
  
  # copy to euosi package table
  osi_farm <- copy(dt)
  
  # setcolorder
  setcolorder(osi_farm,c('ID','B_COUNTRY','B_LU'))
  
  # save measures as bbwp table
  usethis::use_data(osi_farm, overwrite = TRUE)
  
  
  
  