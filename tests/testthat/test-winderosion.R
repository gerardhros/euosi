test_that("osi_p_wef works", {
  expect_equal(
    osi_p_wef(B_LU = 265, A_CLAY_MI = 4, A_SILT_MI = 15),
    expected = c(1.0),
    tolerance = 0.01
  )
  expect_equal(
    osi_p_wef(B_LU = 1019, A_CLAY_MI = 4, A_SILT_MI = 65),
    expected = c(00.88),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_p_wef(B_LU = c(1019,1019), A_CLAY_MI = c(4,1), A_SILT_MI = c(65,15)),
    expected = c(0.885,0.467),
    tolerance = 0.01
  )
  
  
  
})

