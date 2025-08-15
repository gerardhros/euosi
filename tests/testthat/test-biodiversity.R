test_that("osi_p_density works", {
  expect_equal(
    osi_biodiversity(A_SOM_LOI = 6.5, A_PH_CC = 5.5),
    expected = c(0.9975),
    tolerance = 0.01
  )
  expect_equal(
    osi_biodiversity(A_SOM_LOI = c(1,5,10,20), A_PH_CC = c(5,5,5,5)),
    expected = c(0.668,0.986,0.995,0.995),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_biodiversity(A_SOM_LOI = rep(4,6),A_PH_CC = c(4,5,6,7,8,9)),
    expected = c(0.897,0.964,0.969,0.969,0.969,0.969),
    tolerance = 0.01
  )
  
  
  
})

