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


test_that("osi_c_potassium_fr works", {
  expect_equal(
    osi_c_potassium_fr(B_LU = 'SOJ', A_K_AA = 45,B_SOILTYPE_AGR = 'limons battants', B_AER_FR = 'nord picardie'),
    expected = c(0.02),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_fr(B_LU = rep('SOJ',2), A_K_AA = c(45,120),B_SOILTYPE_AGR = rep('limons battants',2), B_AER_FR = rep('nord picardie',2)),
    expected = c(0.02,0.998),
    tolerance = 0.01
  )
  
})

test_that("osi_c_potassium works", {
  expect_equal(
    osi_c_potassium(B_LU = 265, B_SOILTYPE_AGR = 'dekzand',A_SOM_LOI = 4, 
                    A_CLAY_MI = 11,A_PH_CC = 5.4, A_CEC_CO = 125, 
                    A_K_CO_PO = 8.5, A_K_CC = 145,B_COUNTRY = 'NL'),
    expected = c(0.999),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium(B_LU = 1019, B_SOILTYPE_AGR = 'dekzand',A_SOM_LOI = 4, 
                    A_CLAY_MI = 11,A_PH_CC = 5.4, A_CEC_CO = 125, 
                    A_K_CO_PO = 4.5, A_K_CC = 45,B_COUNTRY = 'NL'),
    expected = c(0.9756),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_potassium(B_LU = c(265,1019), 
                    B_SOILTYPE_AGR = rep('dekzand',2),
                    A_SOM_LOI=c(4,6), A_CLAY_MI = c(11,14),
                    A_PH_CC = c(5.4,5.6), A_CEC_CO = c(1.25,1.45),A_K_CO_PO = c(8.5,3.5), A_K_CC = c(145,180),
                  B_COUNTRY = c('NL','NL')),
    expected = c(0.999,0.819),
    tolerance = 0.01
  )
  
  
  
})
