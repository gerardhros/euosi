test_that("osi_conv_som works", {
  expect_equal(
    osi_conv_som(element = 'A_C_OF',
                 A_SOM_LOI = 3.5,
                 A_C_OF = NA_real_, 
                 A_N_RT = NA_real_,
                 A_CN_FR = NA_real_),
    expected = c(17.5),
    tolerance = 0.01
  )
  expect_equal(
    osi_conv_som(element = c('A_N_RT'),
                 A_SOM_LOI = c(3.5,3.5),
                 A_C_OF = c(NA_real_, NA_real_), 
                 A_N_RT = c(NA_real_, NA_real_),
                 A_CN_FR = c(15, 25)),
    expected = c(1166.677,700),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_conv_som(element = c('A_SOM_LOI'),
                 A_SOM_LOI = c(NA_real_, NA_real_), 
                 A_C_OF = c(NA_real_, NA_real_), 
                 A_N_RT = c(4500, 8900),
                 A_CN_FR = c(15, 12)),
    expected = c(6.75,10.68),
    tolerance = 0.01
  )

})

test_that("osi_conv_hwb works", {
  expect_equal(
    osi_conv_hwb(B_SOILTYPE_AGR = 'dekzand', 
                 A_SOM_LOI = 4.5, 
                 A_B_CC= 25, 
                 A_PH_CC= 5.4),
    expected = c(0.064),
    tolerance = 0.01
  )
  expect_equal(
    osi_conv_hwb(B_SOILTYPE_AGR = c('dekzand', 'dalgrond','loess','rivierklei','zeeklei','duinzand'),
                 A_SOM_LOI = c(4.5,5,6,7,8,6), 
                 A_B_CC= c(25,50,75,100,150,175), 
                 A_PH_CC= c(5.4,5.6,5.8,6.0,6.2,6.4)),
    expected = c(0.064,0.2455,0.5605,0.678,1.092,0.464),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_conv_hwb(B_SOILTYPE_AGR = rep('dekzand', 4),
                 A_SOM_LOI = rep(8.5,4), 
                 A_B_CC= c(25,150,250,500), 
                 A_PH_CC= rep(5.4,4)),
    expected = c(0.105,0.319,0.4896,0.9172),
    tolerance = 0.01
  )
  
})

test_that("osi_conv_ph works", {
  expect_equal(
    osi_conv_ph(element = 'A_PH_WA',
                A_PH_KCL = NA_real_,
                A_PH_CC = 6.5, 
                A_PH_WA = NA_real_),
    expected = c(7.23),
    tolerance = 0.01
  )
  expect_equal(
    osi_conv_ph(element = c('A_PH_KCL'),
                A_PH_KCL = NA_real_,
                A_PH_CC = seq(4,9,0.5), 
                A_PH_WA = NA_real_),
    expected = c(3.74,4.278,4.817,5.355,5.893,6.432,6.97,7.508,8.047,8.585,9.123),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_conv_ph(element = 'A_PH_CC',
                A_PH_KCL = 4.5,
                A_PH_CC = NA_real_, 
                A_PH_WA = 5.5),
    expected = c(4.706),
    tolerance = 0.01
  )
  
})


test_that("osi_conv_npmn works", {
  expect_equal(
    osi_conv_npmn(A_N_RT = 4500,
                  A_CLAY_MI = 45),
    expected = c(178.57),
    tolerance = 0.01
  )
  expect_equal(
    osi_conv_npmn(A_N_RT = rep(4500,6),
                  A_CLAY_MI = seq(1,75,length.out = 6)),
    expected = c(220.87,189.33,182.46,178.49,175.69,173.55),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_conv_npmn(A_N_RT = seq(1000,9500,length.out = 6),
                  A_CLAY_MI = rep(7.5,6)),
    expected = c(37.66,112.45,192.55,275.91,361.7,449.42),
    tolerance = 0.01
  )
  
})


test_that("osi_conv_phosphor works", {
  expect_equal(
    osi_conv_phosphor(element ='A_P_CAL', 
                      A_P_AL = NA_real_,
                      A_P_CC = NA_real_, 
                      A_P_WA = NA_real_,
                      A_P_OL = 38,
                      A_P_CAL = NA_real_,
                      A_P_DL = NA_real_,
                      A_P_AAA = NA_real_,
                      A_P_AAA_EDTA = NA_real_,
                      A_P_M3 = NA_real_,
                      A_PH_CC = 7.5),
    expected = c(60.8),
    tolerance = 0.01
  )
  expect_equal(
    osi_conv_phosphor(element ='A_P_DL', 
                      A_P_AL = NA_real_,
                      A_P_CC = NA_real_, 
                      A_P_WA = NA_real_,
                      A_P_OL = 38,
                      A_P_CAL = NA_real_,
                      A_P_DL = 45,
                      A_P_AAA = NA_real_,
                      A_P_AAA_EDTA = NA_real_,
                      A_P_M3 = NA_real_,
                      A_PH_CC = 7.5),
    expected = c(45),
    tolerance = 0.01
  )
  expect_equal(
    osi_conv_phosphor(element ='A_P_AL', 
                      A_P_AL = NA_real_,
                      A_P_CC = NA_real_, 
                      A_P_WA = NA_real_,
                      A_P_OL = c(15,30,45,85,125,130),
                      A_P_CAL = NA_real_,
                      A_P_DL = NA_real_,
                      A_P_AAA = NA_real_,
                      A_P_AAA_EDTA = NA_real_,
                      A_P_M3 = NA_real_,
                      A_PH_CC = rep(7.5,6)),
    expected = c(61.9,116.5,171,316.45,461.91,480.09),
    tolerance = 0.01
  )
  
})

test_that("osi_conv_potassium works", {
  expect_equal(
    osi_conv_potassium(element ='A_K_CAL', 
                       A_K_AAA = 145,
                       A_K_AL = NA_real_,
                       A_K_AN = NA_real_,
                       A_K_CAL = NA_real_,
                       A_K_CC = NA_real_,
                       A_K_CO_PO = NA_real_, 
                       A_K_DL = NA_real_, 
                       A_K_M3 = NA_real_,
                       A_K_NaAAA = NA_real_,
                       A_K_WA = NA_real_,
                       A_CEC_CO = 125,
                       A_PH_CC = 7.5),
    expected = c(131.567),
    tolerance = 0.01
  )
  expect_equal(
    osi_conv_potassium(element ='A_K_DL', 
                       A_K_AAA = 145,
                       A_K_AL = NA_real_,
                       A_K_AN = NA_real_,
                       A_K_CAL = NA_real_,
                       A_K_CC = NA_real_,
                       A_K_CO_PO = NA_real_, 
                       A_K_DL = 146, 
                       A_K_M3 = NA_real_,
                       A_K_NaAAA = NA_real_,
                       A_K_WA = NA_real_,
                       A_CEC_CO = 125,
                       A_PH_CC = 7.5),
    expected = c(146),
    tolerance = 0.01
  )
  expect_equal(
    osi_conv_potassium(element ='A_K_CAL', 
                       A_K_AAA = c(50,145,185,250,300),
                       A_K_AL = NA_real_,
                       A_K_AN = NA_real_,
                       A_K_CAL = NA_real_,
                       A_K_CC = NA_real_,
                       A_K_CO_PO = NA_real_, 
                       A_K_DL = 146, 
                       A_K_M3 = NA_real_,
                       A_K_NaAAA = NA_real_,
                       A_K_WA = NA_real_,
                       A_CEC_CO = 125,
                       A_PH_CC = 7.5),
    expected = c(15.45,131.57,180.45,259.90,321.01),
    tolerance = 0.01
  )
  
})

test_that("osi_conv_magnesium works", {
  expect_equal(
    osi_conv_magnesium(element ='A_MG_AL', 
                       A_MG_AAA = 145,
                       A_MG_AL = NA_real_, 
                       A_MG_AN = NA_real_,
                       A_MG_CC = NA_real_,
                       A_MG_CO_PO = NA_real_, 
                       A_MG_DL = NA_real_,
                       A_MG_KCL = NA_real_,
                       A_MG_M3 = NA_real_, 
                       A_MG_NaAAA = NA_real_,
                       A_CEC_CO = 145,
                       A_PH_CC = 4.5),
    expected = c(252.375),
    tolerance = 0.01
  )
  expect_equal(
    osi_conv_magnesium(element ='A_MG_DL', 
                       A_MG_AAA = 145,
                       A_MG_AL = NA_real_, 
                       A_MG_AN = NA_real_,
                       A_MG_CC = NA_real_,
                       A_MG_CO_PO = NA_real_, 
                       A_MG_DL = 345,
                       A_MG_KCL = NA_real_,
                       A_MG_M3 = NA_real_, 
                       A_MG_NaAAA = NA_real_,
                       A_CEC_CO = 145,
                       A_PH_CC = 4.5),
    expected = c(345),
    tolerance = 0.01
  )
  expect_equal(
    osi_conv_magnesium(element ='A_MG_M3', 
                       A_MG_AAA = c(50,145,185,250,300),
                       A_MG_AL = NA_real_, 
                       A_MG_AN = NA_real_,
                       A_MG_CC = NA_real_,
                       A_MG_CO_PO = NA_real_, 
                       A_MG_DL = NA_real_,
                       A_MG_KCL = NA_real_,
                       A_MG_M3 = NA_real_, 
                       A_MG_NaAAA = NA_real_,
                       A_CEC_CO = 145,
                       A_PH_CC = 4.5),
    expected = c(60.5863,167.69,212.785,286.065,342.435),
    tolerance = 0.01
  )
})
