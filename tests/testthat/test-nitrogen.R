test_that("osi_c_nitrogen_nl works", {
  expect_equal(
    osi_c_nitrogen_nl(B_LU = 256, B_SOILTYPE_AGR = 'dekzand',A_N_RT = 2500, A_SOM_LOI = 4.5),
    expected = c(1),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_nitrogen_nl(B_LU = 1016, B_SOILTYPE_AGR = 'dekzand',A_N_RT = 1500, A_SOM_LOI = 4.5),
    expected = c(0.83),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_nitrogen_nl(B_LU = c(265,1019),B_SOILTYPE_AGR = c('dekzand','veen'),
                      A_N_RT = c(1500,750),A_SOM_LOI = c(3,3)),
    expected = c(0.99,0.33),
    tolerance = 0.01
  )
  
  
  
})

test_that("osi_c_nitrogen works", {
  expect_equal(
    osi_c_nitrogen(B_LU = 256, B_SOILTYPE_AGR = 'dekzand',A_N_RT = 1500, 
                   A_SOM_LOI = 4.5,B_COUNTRY = 'NL'),
    expected = c(0.835),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_nitrogen(B_LU = 1016, B_SOILTYPE_AGR = 'dekzand',A_N_RT = 2500, 
                   A_SOM_LOI = 9.5,B_COUNTRY = 'NL'),
    expected = c(0.86),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_nitrogen(B_LU = c(265,1019),B_SOILTYPE_AGR = c('dekzand','veen'),A_N_RT = c(1500,750),
                   A_SOM_LOI = c(3,3),B_COUNTRY = c('NL','NL')),
    expected = c(0.99,0.33),
    tolerance = 0.01
  )
  
  
  
})
