# test water retention functions
test_that("osi_p_whc works", {
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = 25,
      A_SAND_MI = 60,
      A_SOM_LOI = 6,
      type = 'plant available water',
      ptf = 'Wosten1999'
    ),
    expected = 0.809,
    tolerance = 0.01
  )
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = seq(5,90,length.out = 7),
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_SOM_LOI = rep(6,7),
      type = 'plant available water',
      ptf = 'Wosten1999'
    ),
    expected = c(0.83,0.82,0.79,0.78,0.82,0.89,0.93),
    tolerance = 0.01
  )
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(10,7), 
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_SOM_LOI = rep(6,7),
      type = 'plant available water',
      ptf = 'Wosten1999'
    ),
    expected = c(0.81,0.84,0.86,0.88,0.90,0.92,0.94),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(25,7), 
      A_SAND_MI = rep(70,7),
      A_SOM_LOI = seq(1,70,length.out = 7),
      type = 'plant available water',
      ptf = 'Wosten1999'
    ),
    expected = c(0.765,0.794,0.806,0.977,1,0.065,0.065),
    tolerance = 0.01
  )
  
  # test for ksat
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(10,7), 
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_SOM_LOI = rep(6,7),
      type = 'Ksat',
      ptf = 'Wosten1999'
    ),
    expected = c(0.176,0.134,0.113,0.094,0.075,0.057,0.041),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(25,7), 
      A_SAND_MI = rep(70,7),
      A_SOM_LOI = seq(1,75,length.out = 7),
      type = 'Ksat',
      ptf = 'Wosten1999'
    ),
    expected = c(0.447,0.388,0.3057,0.264,0.2238,0.0212,0.0699),
    tolerance = 0.01
  )
  
  # water holding capacity
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(10,7), 
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_SOM_LOI = rep(6,7),
      type = 'water holding capacity',
      ptf = 'Wosten1999'
    ),
    expected = c(00.80,0.83,0.84,0.84,0.84,0.83,0.83),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(25,7), 
      A_SAND_MI = rep(70,7),
      A_SOM_LOI = seq(1,40,length.out = 7),
      type = 'water holding capacity',
      ptf = 'Wosten1999'
    ),
    expected = c(0.777,0.872,0.90,0.92,0.93,0.95,0.97),
    tolerance = 0.01
  )
  
})

