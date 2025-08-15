# test water retention functions
test_that("osi_p_paw works", {
  expect_equal(
    osi_p_paw(
      A_CLAY_MI = 25,
      A_SAND_MI = 60,
      A_SILT_MI = 15,
      A_C_OF = 25),
    expected = 0.372,
    tolerance = 0.01
  )
  expect_equal(
    osi_p_paw(
      A_CLAY_MI = seq(5,90,length.out = 7),
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_SILT_MI = rep(10,7),
      A_C_OF = rep(25,7)),
    expected = c(0.304,0.348,0.396,0.446,0.498,0.5497,0.601),
    tolerance = 0.01
  )
  expect_equal(
    osi_p_paw(
      A_CLAY_MI = rep(10,7), 
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_SILT_MI = rep(10,7),
      A_C_OF = c(10,20,30,40,50,60,70)),
    expected = c(0.2499,0.303,0.3315,0.3495,0.3618,0.371,0.378),
    tolerance = 0.01
  )
  
})

