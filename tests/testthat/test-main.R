test_that("osi_field works", {
  
  # combine in one data.table
  dt <- copy(osi_crops)
  dt <- dt[,.SD[1:4],by=osi_country]
  
  # add unique row number
  dt[, id := 1:.N]
  
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
  set.seed(123)
  dt[, A_SOM_LOI := rnorm(.N,mean = 4.5,sd = 0.3)]
  dt[, A_CLAY_MI := runif(.N,min=0,max = 100)]
  dt[, A_SAND_MI := (100 - A_CLAY_MI) * runif(.N,min=0,max= 1)]
  dt[, A_PH_CC := pmin(8.5,1.5 + rnorm(.N,mean = 5.5,sd=0.8))]
  dt[, A_CACO3_IF := fifelse(A_PH_CC < 6, 0.02, 3.5)]
  dt[, A_CEC_CO := NA_real_]
  dt[, A_C_OF := A_SOM_LOI * 10 * 0.5]  
  dt[, A_N_RT := A_C_OF *1000/ rnorm(.N,mean=12,sd=1)]  
  dt[, A_N_PMN := exp(-3.440931 + 1.1012449 * log(A_N_RT) - 0.055858 * log(A_CLAY_MI))]
  dt[, A_P_OL := 4 + rnorm(.N,mean=25,sd=8)]
  dt[, A_K_AAA := 25 + rnorm(.N, mean =75, sd = 12)]
  dt[, A_B_HW := 0.1 + rnorm(.N,mean = 0.4, sd = 0.05)]
  dt[, A_ZN_CC := rnorm(.N, mean = 14.5, sd = 1.5)]
  dt[A_PH_CC > 7, A_ZN_RT := rnorm(.N, mean = 57,sd = 3)]
  dt[A_PH_CC <= 7, A_ZN_RT := rnorm(.N, mean = 20,sd = 3)]
  dt[,A_MG_AAA := 15 + rnorm(.N,mean = 30,sd=7.5)]
  dt[,ID := id]  
  
  dt.test <- osi_field(B_LU = dt$B_LU,
                       B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
                       B_COUNTRY = dt$B_COUNTRY, 
                       B_BGZ = NA_character_,
                       B_PREC_SUM = dt$B_PREC_SUM,
                       B_PREC_WIN = dt$B_PREC_WIN, 
                       B_PET_SUM = dt$B_PET_SUM,
                       B_PET_WIN = dt$B_PET_WIN,
                       B_TEMP_SUM = dt$B_TEMP_SUM,
                       B_TEMP_WIN = dt$B_TEMP_WIN,
                       A_CLAY_MI = dt$A_CLAY_MI,
                       A_SAND_MI = dt$A_SAND_MI,
                       A_SOM_LOI = dt$A_SOM_LOI, 
                       A_C_OF= dt$A_C_OF,
                       A_CEC_CO = dt$A_CEC_CO,
                       A_PH_CC = dt$A_PH_CC, 
                       A_CACO3_IF = dt$A_CACO3_IF,
                       A_N_RT = dt$A_N_RT,
                       A_N_PMN = dt$A_N_PMN,
                       A_P_OL = dt$A_P_OL,
                       A_K_AAA = dt$A_K_AAA,
                       A_MG_AAA = dt$A_MG_AAA, 
                       A_B_HW = dt$A_B_HW, 
                       A_ZN_CC = dt$A_ZN_CC, 
                       A_ZN_EDTA = NA_real_,
                       A_ZN_RT = dt$A_ZN_RT,
                       ID = dt$ID, 
                       output = 'all')
  
  expect_equal(
    dim(dt.test),
    expected = c(56,30),
    tolerance = 0.01
  )
  expect_equal(
    colnames(dt.test)[c(1,6,11,16,21,26)],
    expected = c("ID","i_c_mg","i_e_carbon","i_p_cr","i_p_whc","s_euosi_clim"),
    tolerance = 0.01
  )
  
  expect_equal(
    as.numeric(dt.test[ID==34,c('i_c_p','i_c_mg','i_p_wef','i_p_cr','s_euosi_ess_prod','s_euosi_total')]),
    expected = c(0.816,0.644,0.606,0.945,0.77,0.84),
    tolerance = 0.01
  )
  
  
  
})

