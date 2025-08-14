test_that("osi_erosion works", {
  expect_equal(
    osi_erosion(B_LU = '265',
                A_SOM_LOI = 3.5,
                A_CLAY_MI = 5,
                A_SAND_MI = 15,
                B_COUNTRY='NL'),
    expected = c(0.973),
    tolerance = 0.01
  )
  expect_equal(
    osi_erosion(B_LU = c('5198','5143','6710','5113'), 
                     A_CLAY_MI = rep(25,4), 
                     A_SAND_MI = rep(45,4),
                     A_SOM_LOI = c(2,4,6,8),
                     B_COUNTRY = rep('FI',4)),
    expected = rep(0.9729,4),
    tolerance = 0.01
  )
  expect_equal(
      osi_erosion(B_LU = c('265','AVH','60','1110',rep('3301000000',10)), 
                  A_CLAY_MI = rep(25,14), 
                  A_SAND_MI = rep(45,14),
                  A_SOM_LOI = rep(7.5,14),
                  B_COUNTRY = c("NL","FR", "BE", "FI", "AT", "BG", "CZ",
                                "DK" ,"DE", "IT", "PT", "SK", "SE", "ES")),
    expected = rep(0.9731,14),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_erosion(B_LU = c('265','AVH','60','1110',rep('3301000000',10)), 
                A_CLAY_MI = seq(0,75,length.out = 14), 
                A_SAND_MI = 100 - 25 - seq(0,75,length.out = 14),
                A_SOM_LOI = rep(7.5,14),
                B_COUNTRY = c("NL","FR", "BE", "FI", "AT", "BG", "CZ",
                              "DK" ,"DE", "IT", "PT", "SK", "SE", "ES")),
    expected = rep(0.9731,14),
    tolerance = 0.01
  )
})



