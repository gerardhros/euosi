test_that("osi_c_copper works", {
  expect_equal(
    osi_c_copper(B_LU = '265',
                 A_CLAY_MI = 25,
                 B_CF = 3,
                 A_SOM_LOI = 3.5,
                 A_PH_WA = 6.5,
                 A_CACO3_IF = 0.5,
                 A_CU_RT = 350,
                 A_CU_EDTA = 85,
                 A_CU_CC = 65, 
                 B_COUNTRY = 'NL'),
    expected = c(NA_real_),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_copper(B_LU = 'ORP',
                 A_CLAY_MI = 25,
                 B_CF = 3,
                 A_SOM_LOI = 3.5,
                 A_PH_WA = 6.5,
                 A_CACO3_IF = 0.5,
                 A_CU_RT = 430,
                 A_CU_EDTA = 85,
                 A_CU_CC = 65, 
                 B_COUNTRY = 'FR'),
    expected = c(1.00),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_copper(B_LU = rep('ORP',5),
                 A_CLAY_MI = rep(25,5),
                 B_CF = rep(3,5),
                 A_SOM_LOI = rep(3.5,5),
                 A_PH_WA = rep(6.5,5),
                 A_CACO3_IF = rep(0.5,5),
                 A_CU_RT = rep(430,5),
                 A_CU_EDTA = c(0.1,0.5,0.8,1.0,2.0),
                 A_CU_CC = rep(65,5), 
                 B_COUNTRY = rep('FR',5)),
    expected = c(0.168,0.5,0.768,0.88,0.997),
    tolerance = 0.01
  )

})

test_that("osi_c_copper_fr works", {
  expect_equal(
    osi_c_copper_fr(B_LU = 'ORP',
                 A_CLAY_MI = 25,
                 B_CF = 3,
                 A_SOM_LOI = 3.5,
                 A_CU_EDTA = 0.650),
    expected = c(0.645),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_copper_fr(B_LU = 'ORP',
                 A_CLAY_MI = 25,
                 B_CF = 3,
                 A_SOM_LOI = 3.5,
                 A_CU_EDTA = 0.85),
    expected = c(0.802),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_copper_fr(B_LU = c('ORP','BTA','CIT','3301010102','3306990000'),
                 A_CLAY_MI = rep(25,5),
                 B_CF = rep(3,5),
                 A_SOM_LOI = rep(3.5,5),
                 A_CU_EDTA = c(0.1,0.5,0.8,1.0,2.0)),
    expected = c(0.168,0.5,0.768,0.88,0.997),
    tolerance = 0.01
  )
  
})

