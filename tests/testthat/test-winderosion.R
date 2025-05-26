test_that("osi_p_wef works", {
  expect_equal(
    osi_p_wef(B_LU = 265, A_CLAY_MI = 4, A_SAND_MI = 15,B_COUNTRY='NL'),
    expected = c(1.0),
    tolerance = 0.01
  )
  expect_equal(
    osi_p_wef(B_LU = 1019, A_CLAY_MI = 4, A_SAND_MI = 15,B_COUNTRY='NL'),
    expected = c(0.944),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_p_wef(B_LU = c(1019,1019), A_CLAY_MI = c(4,1), A_SAND_MI = c(65,15),B_COUNTRY=c('NL','NL')),
    expected = c(0.69,0.944),
    tolerance = 0.01
  )
  
  
  
})

