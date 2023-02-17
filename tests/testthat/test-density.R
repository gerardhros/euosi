test_that("osi_p_density works", {
  expect_equal(
    osi_p_density(A_SOM_LOI = 6.5, A_CLAY_MI = 28),
    expected = c(0.947),
    tolerance = 0.01
  )
  expect_equal(
    osi_p_density(A_SOM_LOI = 6.5, A_CLAY_MI = 1.5),
    expected = c(0.93),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_p_density(A_SOM_LOI = c(3.5,8.5),A_CLAY_MI = c(2,28)),
    expected = c(0.84,0.962),
    tolerance = 0.01
  )
  
  
  
})

