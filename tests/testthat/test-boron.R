test_that("osi_c_boron works", {
  expect_equal(
    osi_c_boron(B_LU = 'SOJ', 
                A_CLAY_MI = 7.5,
                A_SAND_MI = 45,
                A_SOM_LOI = 4.5,
                A_PH_CC = 5.5, 
                A_B_HW = 2.5,
                B_COUNTRY='FR'),
    expected = c(1),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron(B_LU = c('265','AVH',rep('3301000000',3)),
               A_CLAY_MI = rep(45,5), 
               A_SAND_MI = rep(15,5),
               A_SOM_LOI = rep(4.5,5),
               A_PH_CC = rep(6.5,5),
               A_B_HW = rep(1.5,5),
               B_COUNTRY = c('NL','FR','SE','ES','SK')),
    expected = c(1,1,0.997,NA,NA),
    tolerance = 0.01
  )
  
})

test_that("osi_c_boron_ch works", {
  expect_equal(
    osi_c_boron_ch(B_LU = 'testcrop1',
                   A_B_HW = 0.50),
    expected = c(0.1085),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_ch(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                   A_B_HW = rep(0.50,4)),
    expected = rep(0.1085,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_ch(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                   A_B_HW = c(0.1,0.4,0.8,1.2)),
    expected = c(0.00021,0.03977,0.6183,1),
    tolerance = 0.01
  )
})


test_that("osi_c_boron_de works", {
  expect_equal(
    osi_c_boron_de(B_LU = '3301061299',
                   A_C_OF=25, 
                   A_CLAY_MI=5,
                   A_SAND_MI=15,
                   A_PH_CC = 4,
                   A_B_HW = 1.50),
    expected = c(0.9989),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_de(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                   A_C_OF= rep(25,4), 
                   A_CLAY_MI= rep(5,4),
                   A_SAND_MI= rep(15,4),
                   A_PH_CC = rep(4,4),
                   A_B_HW = rep(0.50,4)),
    expected = rep(0.7946785,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_de(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                   A_C_OF= rep(25,4), 
                   A_CLAY_MI= rep(5,4),
                   A_SAND_MI= rep(15,4),
                   A_PH_CC = rep(4,4),
                   A_B_HW = c(0.1,0.4,0.8,1.2)),
    expected = c(0.1369134, 0.6742514, 0.9554665, 0.9947486),
    tolerance = 0.01
  )
})

test_that("osi_c_boron_ie works", {
  expect_equal(
    osi_c_boron_ie(B_LU = 'cabbage',
                   A_B_HW = 0.50),
    expected = c(0.318),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_ie(B_LU = c('cabbage','cabbage','cabbage','cabbage'),
                   A_B_HW = rep(0.50,4)),
    expected = rep(0.3186252 ,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_ie(B_LU = c('cabbage','cabbage','cabbage','cabbage'),
                   A_B_HW = c(0.1,0.4,0.8,1.2)),
    expected = c(0.04359523, 0.21841281 ,0.64542789, 0.89945411),
    tolerance = 0.01
  )
})

test_that("osi_c_boron_fr works", {
  expect_equal(
    osi_c_boron_fr(B_LU = 'VED',
                   A_CLAY_MI = 5,
                   A_B_HW = 0.50),
    expected = c(1),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_fr(B_LU = c('TRN','3301010901','3301061299','3301020100'),
                   A_CLAY_MI =rep(5,4),
                   A_B_HW = rep(0.50,4)),
    expected = c(0.7887971 ,1.0000000, 1.0000000, 0.7887971),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_fr(B_LU = c('TRN','TRN','TRN','TRN'),
                   A_CLAY_MI =rep(5,4),
                   A_B_HW = c(0.1,0.4,0.8,1.2)),
    expected = c(0.4276894, 0.7071068, 0.9382613, 0.9908873),
    tolerance = 0.01
  )
})

test_that("osi_c_boron_pt works", {
  expect_equal(
    osi_c_boron_pt(B_LU = '3301010901',
                   A_B_HW = 0.50),
    expected = c(0.4811162),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_pt(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                   A_B_HW = rep(0.50,4)),
    expected = rep(0.4811162,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_pt(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                   A_B_HW = c(0.1,0.4,0.8,1.2)),
    expected = c(0.3135989, 0.4399255, 0.5962419, 0.7224623),
    tolerance = 0.01
  )
})

test_that("osi_c_boron_nl works", {
  expect_equal(
    osi_c_boron_nl(B_LU = '256',
                   A_CLAY_MI = 5,
                   A_SOM_LOI = 5,
                   A_B_HW = 0.50),
    expected = c(0.988543),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_nl(B_LU = c('265','3301010901','3301061299','3301020100'),
                   A_CLAY_MI =rep(5,4),
                   A_SOM_LOI =rep(5,4),
                   A_B_HW = rep(0.50,4)),
    expected = c(0.988543, 0.988543, 0.988543, 0.988543),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_nl(B_LU = c('265','265','265','265'),
                   A_CLAY_MI =rep(5,4),
                   A_SOM_LOI =rep(5,4),
                   A_B_HW = c(0.1,0.4,0.8,1.2)),
    expected = c(0.1220805, 0.9456863, 0.9999046, 0.9999998),
    tolerance = 0.01
  )
})


test_that("osi_c_boron_se works", {
  expect_equal(
    osi_c_boron_se(B_LU = '3301010901',
                   A_PH_WA = 5.0),
    expected = c(0.0005),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                   A_PH_WA = rep(5.5,4)),
    expected = rep(0.207478 ,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                   A_PH_WA = c(5.5,6,7,8)),
    expected = c(0.2074780, 0.7784430, 0.9936760, 0.9998393),
    tolerance = 0.01
  )
})


test_that("osi_c_boron_uk works", {
  expect_equal(
    osi_c_boron_uk(B_LU = '3301010901',
                   B_TEXTURE_HYPRES = 'C',
                   A_SOM_LOI = 2.4,
                   A_PH_CC = 5.5,
                   A_B_HW = 1.2),
    expected = c(1),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_uk(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                   B_TEXTURE_HYPRES = c('C','F','M','M'),
                   A_SOM_LOI = c(3,3,4,4),
                   A_PH_CC = c(5.5,5.5,6.5,6.5),
                   A_B_HW = c(3.5,5.5,2,2)),
    expected = rep(1 ,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_boron_uk(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                   B_TEXTURE_HYPRES = c('C','F','M','M'),
                   A_SOM_LOI = c(3,3,4,4),
                   A_PH_CC = c(5.5,5.5,6.5,6.5),
                   A_B_HW = c(3.5,5.5,2,2)/10),
    expected = c(0.30470237, 0.97392256, 0.08929927, 0.08929927),
    tolerance = 0.01
  )
})
