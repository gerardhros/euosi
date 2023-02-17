# test water retention functions
test_that("osi_p_whc works", {
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = 25,
      A_SILT_MI = 15,
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
      A_SILT_MI = rep(10,7),
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_SOM_LOI = rep(6,7),
      type = 'plant available water',
      ptf = 'Wosten1999'
    ),
    expected = c(0.83,0.82,0.79,0.78,0.82,0.89,0.93),
    tolerance = 1
  )
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(10,7), 
      A_SILT_MI = seq(5,90,length.out = 7),
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_SOM_LOI = rep(6,7),
      type = 'plant available water',
      ptf = 'Wosten1999'
    ),
    expected = c(0.81,0.84,0.86,0.88,0.90,0.92,0.94),
    tolerance = 1
  )
  
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(25,7), 
      A_SILT_MI = rep(5,7),
      A_SAND_MI = rep(70,7),
      A_SOM_LOI = seq(1,80,length.out = 7),
      type = 'plant available water',
      ptf = 'Wosten1999'
    ),
    expected = c(0.76,0.78,0.86,0.9968,0.9999,0.065,0.065),
    tolerance = 1
  )
  
  # test for ksat
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(10,7), 
      A_SILT_MI = seq(5,90,length.out = 7),
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_SOM_LOI = rep(6,7),
      type = 'Ksat',
      ptf = 'Wosten1999'
    ),
    expected = c(0.176,0.134,0.113,0.094,0.075,0.057,0.041),
    tolerance = 0.1
  )
  
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(25,7), 
      A_SILT_MI = rep(5,7),
      A_SAND_MI = rep(70,7),
      A_SOM_LOI = seq(1,80,length.out = 7),
      type = 'Ksat',
      ptf = 'Wosten1999'
    ),
    expected = c(0.447,0.383,0.297,0.261,0.184,0.0699,0.0699),
    tolerance = .1
  )
  # water holding capacity
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(10,7), 
      A_SILT_MI = seq(5,90,length.out = 7),
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_SOM_LOI = rep(6,7),
      type = 'water holding capacity',
      ptf = 'Wosten1999'
    ),
    expected = c(00.80,0.83,0.84,0.84,0.84,0.83,0.83),
    tolerance = 1
  )
  
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(25,7), 
      A_SILT_MI = rep(5,7),
      A_SAND_MI = rep(70,7),
      A_SOM_LOI = seq(1,40,length.out = 7),
      type = 'water holding capacity',
      ptf = 'Wosten1999'
    ),
    expected = c(0.777,0.872,0.90,0.92,0.93,0.95,0.97),
    tolerance = 1
  )
  
  # wilting point
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(10,7), 
      A_SILT_MI = seq(5,90,length.out = 7),
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_SOM_LOI = rep(6,7),
      type = 'wilting point',
      ptf = 'Wosten1999'
    ),
    expected = c(0.014,0.0876,0.137,0.1596,0.165,0.1599,0.149),
    tolerance = 1
  )
  
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(25,7), 
      A_SILT_MI = rep(5,7),
      A_SAND_MI = rep(70,7),
      A_SOM_LOI = seq(1,40,length.out = 7),
      type = 'wilting point',
      ptf = 'Wosten1999'
    ),
    expected = c(0.014,0.255,0.552,0.675,0.6694,0.515,0.166),
    tolerance = 1
  )
  
  # field capacity
  expect_equal(
    osi_p_whc(
      A_CLAY_MI = rep(25,7), 
      A_SILT_MI = rep(5,7),
      A_SAND_MI = rep(70,7),
      A_SOM_LOI = seq(1,40,length.out = 7),
      type = 'field capacity',
      ptf = 'Wosten1999'
    ),
    expected = c(0.755,0.943,0.969,0.979,0.9856,0.991,0.995),
    tolerance = 1
  )
  
})
