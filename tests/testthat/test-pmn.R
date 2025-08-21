test_that("osi_b_pmn_nl works", {
  expect_equal(
    osi_b_pmn_nl(B_LU = '256', B_SOILTYPE_AGR = 'dekzand', A_N_PMN = 125),
    expected = c(0.99),
    tolerance = 0.01
  )
  expect_equal(
    osi_b_pmn_nl(B_LU = '1019', B_SOILTYPE_AGR = 'dekzand', A_N_PMN = 25),
    expected = c(0.587),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_b_pmn_nl(B_LU = c('256' ,'1027'), B_SOILTYPE_AGR = c('dekzand','rivierklei'), A_N_PMN = c(125,45)),
    expected = c(0.999,0.718),
    tolerance = 0.01
  )
  
  
  
})

test_that("osi_b_pmn works", {
  
  expect_equal(
    osi_b_pmn(B_LU = '256', B_SOILTYPE_AGR = 'dekzand', A_N_PMN = 125, B_COUNTRY = 'NL'),
    expected = c(0.99),
    tolerance = 0.01
  )
  expect_equal(
    osi_b_pmn(B_LU = '1019', B_SOILTYPE_AGR = 'dekzand', A_N_PMN = 25, B_COUNTRY = 'NL'),
    expected = c(0.587),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_b_pmn(B_LU = c('256','1027'), B_SOILTYPE_AGR = c('dekzand','rivierklei'), A_N_PMN = c(125,45),B_COUNTRY = c('NL','NL')),
    expected = c(0.999,0.718),
    tolerance = 0.01
  )
  
  
  
})
