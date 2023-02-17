test_that("osi_c_potassium_nl works", {
  expect_equal(
    osi_c_potassium_nl(B_LU = 265, B_SOILTYPE_AGR = 'dekzand',A_SOM_LOI = 4, 
                       A_CLAY_MI = 11,A_PH_CC = 5.4, A_CEC_CO = 125, 
                       A_K_CO_PO = 8.5, A_K_CC = 145),
    expected = c(0.99),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_nl(B_LU = 265, B_SOILTYPE_AGR = 'dekzand',A_SOM_LOI = 4, 
                       A_CLAY_MI = 11,A_PH_CC = 5.4, A_CEC_CO = 125, 
                       A_K_CO_PO = 1.5, A_K_CC = 14.5),
    expected = c(0.098),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_potassium_nl(c(265,1019), rep('dekzand',2),c(4,6), c(11,14),
                       c(5.4,5.6),  c(1.25,1.45),c(8.5,3.5), c(145,180)),
    expected = c(0.999,0.819),
    tolerance = 0.01
  )
  
  
  
})

test_that("osi_c_potassium works", {
  expect_equal(
    osi_c_potassium(265, 'dekzand',4, 11,5.4,  125,8.5, 145,'NL'),
    expected = c(0.999),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium(1019, 'dekzand',4, 11,5.4,  1.25,8.5, 145,'NL'),
    expected = c(0.778),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_potassium(c(265,1019), rep('dekzand',2),c(4,6), c(11,14),
                  c(5.4,5.6),  c(1.25,1.45),c(8.5,3.5), c(145,180),
                  B_COUNTRY = c('NL','NL')),
    expected = c(0.999,0.819),
    tolerance = 0.01
  )
  
  
  
})
