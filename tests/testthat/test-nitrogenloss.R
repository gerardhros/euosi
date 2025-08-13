test_that("osi_gw_nleach works", {
  expect_equal(
    osi_gw_nleach(B_LU = '265', 
                  A_CLAY_MI = 25, 
                  A_SAND_MI = 45,
                  A_CACO3_IF = 1.5,
                  A_N_RT = 3300, 
                  A_C_OF = 40, 
                  B_PREC_SUM = NA_real_,
                  B_PREC_WIN = NA_real_, 
                  B_PET_SUM = NA_real_,
                  B_PET_WIN = NA_real_,
                  B_TEMP_SUM = NA_real_,
                  B_TEMP_WIN = NA_real_,
                  B_COUNTRY ='NL'),
    expected = c(0.8701),
    tolerance = 0.01
  )
  expect_equal(
    osi_gw_nleach(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                  A_CLAY_MI = rep(25,4), 
                  A_SAND_MI = rep(45,4),
                  A_CACO3_IF = rep(1.5,4),
                  A_N_RT = c(1000,2000,3000,6000), 
                  A_C_OF = c(1000,2000,3000,6000) * 12/1000, 
                  B_PREC_SUM = NA_real_,
                  B_PREC_WIN = NA_real_, 
                  B_PET_SUM = NA_real_,
                  B_PET_WIN = NA_real_,
                  B_TEMP_SUM = NA_real_,
                  B_TEMP_WIN = NA_real_,
                  B_COUNTRY = rep('NL',4)),
    expected = c(0.876,0.874,0.871,0.866),
    tolerance = 0.01
  )
  expect_equal(
    osi_gw_nleach(B_LU = c('265','AVH',rep('3301000000',3)), 
                  A_CLAY_MI = rep(25,5), 
                  A_SAND_MI = rep(45,5),
                  A_CACO3_IF = rep(1.5,5),
                  A_N_RT = c(1000,2000,3000,6000,8000), 
                  A_C_OF = c(1000,2000,3000,6000,8000) * 12/1000, 
                  B_PREC_SUM = NA_real_,
                  B_PREC_WIN = NA_real_, 
                  B_PET_SUM = NA_real_,
                  B_PET_WIN = NA_real_,
                  B_TEMP_SUM = NA_real_,
                  B_TEMP_WIN = NA_real_,
                  B_COUNTRY = c('NL','FR','SE','ES','SK')),
    expected = c(0.876,3.229461e-05,0.871,0.866,0.86),
    tolerance = 0.01
  )
})

test_that("osi_gw_nleach_be works", {
  expect_equal(
    osi_gw_nleach_be(B_LU = '9823', 
                     A_CLAY_MI = 25, 
                     A_SAND_MI = 45,
                     A_CACO3_IF = 1.5,
                     A_N_RT = 3300, 
                     A_C_OF = 40),
    expected = c(0.11596),
    tolerance = 0.01
  )
  expect_equal(
    osi_gw_nleach_be(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                    A_CLAY_MI = rep(25,4), 
                    A_SAND_MI = rep(45,4),
                    A_CACO3_IF = rep(1.5,4),
                    A_N_RT = c(1000,2000,3000,6000), 
                    A_C_OF = c(1000,2000,3000,6000) * 12/1000),
    expected = c(0.503,0,0.0124,0),
    tolerance = 0.01
  )
  expect_equal(
    osi_gw_nleach_be(B_LU = c('9823','201',rep('3301000000',3)), 
                  A_CLAY_MI = rep(25,5), 
                  A_SAND_MI = rep(45,5),
                  A_CACO3_IF = rep(1.5,5),
                  A_N_RT = c(1000,2000,3000,6000,8000), 
                  A_C_OF = c(1000,2000,3000,6000,8000) * 12/1000,
                  B_PREC_WIN = c(450,750,1000,1500,2000),
                  B_PREC_SUM = rep(400,5),
                  B_TEMP_SUM = rep(2,5)
                  ),
    expected = c(0.7038,0.109,0.0124,0,0),
    tolerance = 0.01
  )
})

test_that("osi_gw_nleach_fr works", {
  expect_equal(
    osi_gw_nleach_fr(B_LU = 'BDH', 
                     A_CLAY_MI = 25, 
                     A_SAND_MI = 45,
                     A_CACO3_IF = 1.5,
                     A_N_RT = 3300, 
                     A_C_OF = 40),
    expected = c(0.00),
    tolerance = 0.01
  )
  expect_equal(
    osi_gw_nleach_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                     A_CLAY_MI = rep(25,4), 
                     A_SAND_MI = rep(45,4),
                     A_CACO3_IF = rep(1.5,4),
                     A_N_RT = c(1000,2000,3000,6000), 
                     A_C_OF = c(1000,2000,3000,6000) * 12/1000),
    expected = c(0.503,0,0.0124,0),
    tolerance = 0.01
  )
  expect_equal(
    osi_gw_nleach_fr(B_LU = c('CPL','CRF',rep('3301000000',3)), 
                     A_CLAY_MI = rep(25,5), 
                     A_SAND_MI = rep(45,5),
                     A_CACO3_IF = rep(1.5,5),
                     A_N_RT = c(1000,2000,3000,6000,8000), 
                     A_C_OF = c(1000,2000,3000,6000,8000) * 12/1000,
                     B_PREC_WIN = c(450,750,1000,1500,2000),
                     B_PREC_SUM = rep(400,5),
                     B_TEMP_SUM = rep(2,5)
    ),
    expected = c(0.503,0.1075,0.0121,0,0),
    tolerance = 0.01
  )
})

test_that("osi_gw_nleach_eu works", {
  expect_equal(
    osi_gw_nleach_eu(B_LU = '5129', 
                     A_CLAY_MI = 25, 
                     A_SAND_MI = 45,
                     A_N_RT = 3300, 
                     A_C_OF = 40,
                     B_PREC_Y = NA_real_, 
                     B_PET_Y = NA_real_, 
                     B_TEMP_Y = NA_real_,
                     B_COUNTRY = 'FI'),
    expected = c(0.8697),
    tolerance = 0.01
  )
  expect_equal(
    osi_gw_nleach_eu(B_LU = c('5198','5143','6710','5113'), 
                     A_CLAY_MI = rep(25,4), 
                     A_SAND_MI = rep(45,4),
                     A_N_RT = c(1000,2000,3000,6000), 
                     A_C_OF = c(1000,2000,3000,6000) * 12/1000,
                     B_PREC_Y = NA_real_, 
                     B_PET_Y = NA_real_, 
                     B_TEMP_Y = NA_real_,
                     B_COUNTRY = rep('FI',4)),
    expected = c(0.8763,0.8736,0.87065,0.86638),
    tolerance = 0.01
  )
  expect_equal(
    osi_gw_nleach_eu(B_LU = c('5119','6562',rep('1220',3)), 
                     A_CLAY_MI = rep(25,5), 
                     A_SAND_MI = rep(45,5),
                     A_N_RT = c(1000,2000,3000,6000,8000), 
                     A_C_OF = c(1000,2000,3000,6000,8000) * 12/1000,
                     B_PREC_Y = c(450,750,1000,1500,2000),
                     B_PET_Y = rep(400,5),
                     B_TEMP_Y = rep(2,5),
                     B_COUNTRY = rep('FI',5)),
    expected = c(0.876,0.874,0.871,0.866,0.860),
    tolerance = 0.01
  )
})



