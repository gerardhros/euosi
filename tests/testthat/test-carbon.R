test_that("osi_carbon works", {
  expect_equal(
    osi_carbon(B_LU = '172',
               A_C_OF = 25, 
               B_BGZ = '4',
               A_CLAY_MI=5,
               A_SAND_MI=25,
               B_COUNTRY='NL'),
    expected = c(0.9568),
    tolerance = 0.01
  )
  expect_equal(
    osi_carbon(B_LU = c('172','60','3301000000','1110',
                        '3301000000','3301000000','3301000000'),
               A_C_OF = rep(25,7), 
               B_BGZ = rep('4',7),
               A_CLAY_MI = rep(5,7),
               A_SAND_MI = rep(25,7),
               B_COUNTRY= c('NL','BE','DE','FI','SE','LV','SK')),
    expected = c(0.957,0.267,0.957,0.957,0.957,NA,0.957),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_carbon(B_LU = rep('3301000000',7),
               A_C_OF = seq(2.5,100,length.out = 7), 
               B_BGZ = rep('4',7),
               A_CLAY_MI = rep(5,7),
               A_SAND_MI = rep(25,7),
               B_COUNTRY= c('NL','BE','DE','FI','SE','LV','SK')),
    expected = c(0.047,0.802,0.997,NA,1,NA,1),
    tolerance = 0.01
  )

})
