test_that("osi_c_zinc works", {
  expect_equal(
    osi_c_zinc(B_LU = 'SOJ', 
               A_CLAY_MI = 45, 
               A_SAND_MI = 15,
               A_PH_WA = 6.5,
               A_PH_CC = NA, 
               A_ZN_CC = NA, 
               A_ZN_EDTA = 4.5, 
               A_ZN_RT = 25,
               B_COUNTRY='FR'),
    expected = c(1),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_zinc(B_LU = c('265','AVH',rep('3301000000',3)),
               A_CLAY_MI = rep(45,5), 
               A_SAND_MI = rep(15,5),
               A_SOM_LOI = rep(4.5,5),
               A_PH_WA = rep(6.5,5),
               A_PH_CC = rep(NA_real_,5), 
               A_ZN_CC = rep(NA_real_,5), 
               A_ZN_EDTA = rep(4.5,5), 
               A_ZN_RT = rep(25,5),
               B_COUNTRY = c('NL','FR','SE','ES','SK')),
    expected = c(0.1886,1,NA,NA,NA),
    tolerance = 0.01
  )
  
})

test_that("osi_c_zinc_de works", {
  expect_equal(
    osi_c_zinc_de(B_LU = '3301061299',
                  A_C_OF=25, 
                  A_CLAY_MI=5,
                  A_SAND_MI=15,
                  A_ZN_EDTA = 1.50),
    expected = c(0.69937),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_zinc_de(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                  A_C_OF = rep(25,4), 
                  A_CLAY_MI = rep(5,4),
                  A_SAND_MI = rep(15,4),
                  A_ZN_EDTA = rep(1.50,4)),
    expected = c(0.699,0.699,0.699,0.699),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_zinc_de(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                  A_C_OF = rep(25,4), 
                  A_CLAY_MI = rep(5,4),
                  A_SAND_MI = rep(15,4),
                  A_ZN_EDTA = c(0.5,0.8,1.6,3.5)),
    expected = c(0.064,0.1453,0.7868,0.9999),
    tolerance = 0.01
  )
})

test_that("osi_c_zinc_fr works", {
  expect_equal(
    osi_c_zinc_fr(B_LU = 'DLN',
                  A_PH_WA = 6.5, 
                  A_ZN_EDTA = 1.50),
    expected = c(0.73066),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_zinc_fr(B_LU = c('DLN','MID','MIE','3301010699'),
                  A_PH_WA = rep(5.6,4), 
                  A_ZN_EDTA = rep(1.1,4)),
    expected = c(0.916,0.916,0.916,0.9164),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_zinc_fr(B_LU = c('MIE','3301010699','3301061299','3304990000'),
                  A_PH_WA = rep(5.6,4),
                  A_ZN_EDTA = c(0.5,0.8,1.6,3.5)),
    expected = c(0.5,0.768,1,1),
    tolerance = 0.01
  )
})

test_that("osi_c_zinc_ie works", {
  expect_equal(
    osi_c_zinc_ie(B_LU = 'testcrop',
                  A_SOM_LOI = 4,
                  A_PH_WA = 7.5, 
                  A_ZN_EDTA = 0.75),
    expected = c(0.797),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_zinc_ie(B_LU = c('testcrop','testcrop','testcrop','testcrop'),
                  A_SOM_LOI = rep(4,4),
                  A_PH_WA = c(5,6,7,8), 
                  A_ZN_EDTA = rep(0.4,4)),
    expected = c(0.805,0.805,0.805,0.64),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_zinc_ie(B_LU = c('testcrop','3301010699','3301061299','3304990000'),
                  A_SOM_LOI = rep(4,4),
                  A_PH_WA = rep(5.6,4),
                  A_ZN_EDTA = c(0.1,0.2,0.3,0.4)),
    expected = c(0.00097,0.1124,0.5022,0.8049),
    tolerance = 0.01
  )
})

test_that("osi_c_zinc_nl works", {
  expect_equal(
    osi_c_zinc_nl(B_LU = '265',
                  A_PH_CC = 7.5, 
                  A_ZN_CC = 75),
    expected = c(0.991),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_zinc_nl(B_LU = c('265','3731','2785','1015'),
                  A_PH_CC = c(5,6,7,8), 
                  A_ZN_CC = rep(75,4)),
    expected = c(0.099,0.2028,0.268,0.352),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_zinc_nl(B_LU = c('265','3301010699','3301061299','3304990000'),
                  A_PH_CC = rep(5.6,4),
                  A_ZN_CC = c(75,125,175,225)),
    expected = c(0.193,0.237,0.2822,0.3208),
    tolerance = 0.01
  )
})

test_that("osi_c_zinc_ro works", {
  expect_equal(
    osi_c_zinc_ro(B_LU = 'testcrop',
                  A_PH_WA = 6.5, 
                  A_P_AL = 45,
                  A_ZN_EDTA = 0.150),
    expected = c(0.2042037),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_zinc_ro(B_LU = c('testcrop1','testcrop2','testcrop3','3301010699'),
                  A_PH_WA = c(4,5,6,7), 
                  A_P_AL = rep(45,4),
                  A_ZN_EDTA = rep(0.5,4)),
    expected = c(0.9081472, 0.9989704, 0.9997297, 0.9981355),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_zinc_ro(B_LU = c('testcrop1','3301010699','3301061299','3304990000'),
                  A_PH_WA = rep(5.6,4),
                  A_P_AL = rep(25,4),
                  A_ZN_EDTA = c(0.5,0.8,1.6,3.5)/10),
    expected = c(0.1562884, 0.2012715, 0.8510877, 0.9999937),
    tolerance = 0.01
  )
})

test_that("osi_c_zinc_uk works", {
  expect_equal(
    osi_c_zinc_uk(B_LU = 'testcrop',
                  A_PH_WA = 6.5, 
                  A_ZN_EDTA = 1.50),
    expected = c(0.79996),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_zinc_uk(B_LU = c('testcrop1','testcrop2','testcrop3','3301010699'),
                  A_PH_WA = rep(5.6,4), 
                  A_ZN_EDTA = rep(1.1,4)),
    expected = rep(0.998,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_zinc_uk(B_LU = c('testcrop1','3301010699','3301061299','3304990000'),
                  A_PH_WA = rep(5.6,4),
                  A_ZN_EDTA = c(0.5,0.8,1.6,3.5)),
    expected = c(0.7548,0.9796,0.9999,1.00),
    tolerance = 0.01
  )
})
