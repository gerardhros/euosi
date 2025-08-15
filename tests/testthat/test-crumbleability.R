test_that("osi_p_crumbleability works", {
  expect_equal(
    osi_p_crumbleability(B_LU = '256',
                         A_CLAY_MI = 5, 
                         A_SOM_LOI = 3.5, 
                         A_PH_CC = 5.5, 
                         B_COUNTRY = 'NL'),
    expected = c(1.0),
    tolerance = 0.01
  )
  expect_equal(
    osi_p_crumbleability(B_LU = c('256',rep('3301010102',4)),
                         A_CLAY_MI = rep(35,5), 
                         A_SOM_LOI = rep(3.5,5), 
                         A_PH_CC = rep(5.5,5), 
                         B_COUNTRY = c('NL','DE','FI','SE','ES')),
    expected = c(0.50,0.751,0.751,0.751,0.751),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_p_crumbleability(B_LU = rep('256',6),
                         A_CLAY_MI = c(1,35,10,15,20,35), 
                         A_SOM_LOI = c(1.5,3.5,5,4,2,1), 
                         A_PH_CC = rep(3.5,6), 
                         B_COUNTRY = rep('NL',6)),
    expected = c(1.0,0.29,1,1,0.851,0.197),
    tolerance = 0.01
  )
  
  
  
})

