test_that("osi_c_nitrogen works", {
  expect_equal(
    osi_c_nitrogen(B_LU = '256', 
                   B_SOILTYPE_AGR = 'dekzand',
                   A_N_RT = 900, 
                   A_CLAY_MI = 11, 
                   A_SAND_MI = 3,
                   A_C_OF = NA_real_,
                   A_CACO3_IF = NA_real_,
                   A_SOM_LOI = 4.5, 
                   B_COUNTRY = 'NL'),
    expected = c(0.223),
    tolerance = 0.01
  )
  
  
  expect_equal(
    osi_c_nitrogen(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301000000'), 
                   B_SOILTYPE_AGR = rep('dekzand',5),
                   A_CLAY_MI = rep(25,5), 
                   A_SAND_MI = rep(45,5),
                   A_CACO3_IF = rep(1.5,5),
                   A_N_RT = c(1000,2000,3000,6000,6000), 
                   A_SOM_LOI = c(1000,2000,3000,6000,6000) * 12 * 0.1 * 2/1000,
                   B_COUNTRY = rep('NL',5)),
    expected = c(0.76,0.9965,1,1,1),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_nitrogen(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301000000'), 
                   B_SOILTYPE_AGR = c('dekzand',rep(NA_character_,4)),
                   A_CLAY_MI = rep(25,5), 
                   A_SAND_MI = rep(45,5),
                   A_CACO3_IF = rep(1.5,5),
                   A_N_RT = c(1000,2000,3000,6000,6000), 
                   A_SOM_LOI = c(1000,2000,3000,6000,6000) * 12 * 0.1 * 2/1000,
                   B_COUNTRY = c('NL', 'DE','FI','SE','LT')),
    expected = c(0.76,0.9316,1,1,1),
    tolerance = 0.01
  )
  
  
})

test_that("osi_c_nitrogen_be works", {
  expect_equal(
    osi_c_nitrogen_be(B_LU = '9823', 
                   A_N_RT = 900, 
                   A_CLAY_MI = 11, 
                   A_SAND_MI = 3,
                   A_CACO3_IF = 1.5,
                   A_C_OF = 4.5*0.5*10),
    expected = c(0.4755),
    tolerance = 0.01
  )
  
  
  expect_equal(
    osi_c_nitrogen_be(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301000000'), 
                   A_CLAY_MI = rep(25,5), 
                   A_SAND_MI = rep(45,5),
                   A_CACO3_IF = rep(1.5,5),
                   A_N_RT = c(1000,2000,3000,6000,6000), 
                   A_C_OF = c(1000,2000,3000,6000,6000) * 12*0.001),
    expected = c(0.459,0.797,0.979,1,1),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_nitrogen_be(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301000000'), 
                   A_CLAY_MI = rep(25,5), 
                   A_SAND_MI = rep(45,5),
                   A_CACO3_IF = c(0.5,1.5,2.5,4.5,6.5),
                   A_N_RT = c(1000,2000,3000,6000,6000), 
                   A_C_OF = c(1000,2000,3000,6000,6000) * 12 /1000),
    expected = c(0.4605,0.797,0.9786,1,1),
    tolerance = 0.01
  )
  
  
})

test_that("osi_c_nitrogen_fr works", {
  expect_equal(
    osi_c_nitrogen_fr(B_LU = 'BDH', 
                      A_N_RT = 900, 
                      A_CLAY_MI = 11, 
                      A_SAND_MI = 3,
                      A_CACO3_IF = 1.5,
                      A_C_OF = 4.5*0.5*10),
    expected = c(0.623),
    tolerance = 0.01
  )
  
  
  expect_equal(
    osi_c_nitrogen_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301000000'), 
                      A_CLAY_MI = rep(25,5), 
                      A_SAND_MI = rep(45,5),
                      A_CACO3_IF = rep(1.5,5),
                      A_N_RT = c(1000,2000,3000,6000,6000), 
                      A_C_OF = c(1000,2000,3000,6000,6000) * 12*0.001),
    expected = c(0.459,0.797,0.979,1,1),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_nitrogen_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301000000'), 
                      A_CLAY_MI = rep(25,5), 
                      A_SAND_MI = rep(45,5),
                      A_CACO3_IF = c(0.5,1.5,2.5,4.5,6.5),
                      A_N_RT = c(1000,2000,3000,6000,6000), 
                      A_C_OF = c(1000,2000,3000,6000,6000) * 12 /1000),
    expected = c(0.4605,0.797,0.9786,1,1),
    tolerance = 0.01
  )
  
  
})

test_that("osi_c_nitrogen_de works", {
  expect_equal(
    osi_c_nitrogen_de(B_LU = '3301010100', 
                      A_N_RT = 900, 
                      A_CLAY_MI = 11, 
                      A_SAND_MI = 3,
                      A_C_OF = 4.5*0.5*10),
    expected = c(0.330),
    tolerance = 0.01
  )
  
  
  expect_equal(
    osi_c_nitrogen_de(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301000000'), 
                      A_CLAY_MI = rep(25,5), 
                      A_SAND_MI = rep(45,5),
                      A_N_RT = c(1000,2000,3000,6000,6000), 
                      A_SOM_LOI = rep(NA_real_,5),
                      A_C_OF = c(1000,2000,3000,6000,6000) * 12*0.001),
    expected = c(0.648,0.932,0.985,0.9995,0.9995),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_nitrogen_de(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301000000'), 
                      A_CLAY_MI = rep(25,5), 
                      A_SAND_MI = rep(45,5),
                      A_N_RT = c(1000,2000,3000,6000,6000), 
                      A_SOM_LOI = rep(NA_real_,5),
                      A_C_OF = c(1000,2000,3000,6000,6000) * 12 /1000),
    expected = c(0.648,0.932,0.985,0.9995,0.9995),
    tolerance = 0.01
  )
  
  
})

test_that("osi_c_nitrogen_nl works", {
  expect_equal(
    osi_c_nitrogen_nl(B_LU = '256', 
                   B_SOILTYPE_AGR = 'dekzand',
                   A_N_RT = 900, 
                   A_SOM_LOI = 4.5),
    expected = c(0.223),
    tolerance = 0.01
  )
  
  
  expect_equal(
    osi_c_nitrogen_nl(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301000000'), 
                   B_SOILTYPE_AGR = rep('dekzand',5),
                   A_N_RT = c(1000,2000,3000,6000,6000), 
                   A_SOM_LOI = c(1000,2000,3000,6000,6000) * 12 * 0.1 * 2/1000),
    expected = c(0.76,0.9965,1,1,1),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_nitrogen_nl(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301000000'), 
                   B_SOILTYPE_AGR = c('dekzand','zeeklei','loess','veen','dalgrond'),
                   A_N_RT = c(1000,2000,3000,6000,6000), 
                   A_SOM_LOI = c(1000,2000,3000,6000,6000) * 12 * 0.1 * 2/1000),
    expected = c(0.76,0.9648,1,1,1),
    tolerance = 0.01
  )
  
  
})

test_that("osi_c_nitrogen_eu works", {
  expect_equal(
    osi_c_nitrogen_eu(B_LU = '256', 
                     A_N_RT = 650, 
                     A_CLAY_MI = 25, 
                     A_SAND_MI = 3,
                     A_C_OF = NA_real_,
                     A_SOM_LOI = 4.5, 
                     B_COUNTRY = 'NL'),
    expected = c(0.9964),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_nitrogen_eu(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301000000'), 
                      A_CLAY_MI = rep(25,5), 
                      A_SAND_MI = rep(45,5),
                      A_N_RT = c(1000,2000,3000,6000,6000), 
                      A_SOM_LOI = c(1000,2000,3000,6000,6000) * 12 * 0.1 * 2/1000,
                      B_COUNTRY = rep('NL',5)),
    expected = c(0.999,1,1,1,1),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_nitrogen_eu(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301000000'), 
                      A_CLAY_MI = rep(25,5), 
                      A_SAND_MI = rep(45,5),
                      A_N_RT = c(1000,2000,3000,6000,6000), 
                      A_SOM_LOI = c(1000,2000,3000,6000,6000) * 12 * 0.1 * 2/1000,
                      B_COUNTRY = c('NL', 'DE','FI','SE','LT')),
    expected = c(0.999,1,1,1,1),
    tolerance = 0.01
  )
  
  
})
