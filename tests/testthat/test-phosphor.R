test_that("osi_c_phosphor works", {
  expect_equal(
    osi_c_phosphor(B_LU = '265', 
                   B_SOILTYPE_AGR = 'dekzand', 
                   A_CLAY_MI = 15, 
                   A_SAND_MI = 45,
                   A_SOM_LOI = 4.5,
                   A_PH_CC = 5.4,
                   A_P_OL = 23,
                   A_C_OF = NA_real_,
                   A_PH_WA = NA_real_,
                   A_CACO3_IF = NA_real_,
                   A_P_AAA = NA_real_,A_P_AL = NA_real_, A_P_CAL = NA_real_,
                   A_P_CC = NA_real_, A_P_DL = NA_real_, A_P_M3 = NA_real_,
                   A_P_WA = NA_real_, 
                  B_COUNTRY = 'NL'),
    expected = c(0.9187),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor(B_LU = c('265','AVH',rep('3301000000',3)), 
                    B_SOILTYPE_AGR = c('dekzand',rep(NA,4)),
                    A_CLAY_MI = rep(15,5),
                    A_SAND_MI = rep(45,5),
                    A_SOM_LOI = rep(4.5,5),
                    A_PH_CC = rep(4.5,5),
                    A_P_OL = rep(15,5),
                    A_C_OF = NA_real_,
                    A_PH_WA = NA_real_,
                    A_CACO3_IF = NA_real_,
                    A_P_AAA = NA_real_,A_P_AL = NA_real_, A_P_CAL = NA_real_,
                    A_P_CC = NA_real_, A_P_DL = NA_real_, A_P_M3 = NA_real_,
                    A_P_WA = NA_real_, 
                    B_COUNTRY = c('NL','FR','SE','ES','SK')),
    expected = c(0.7488,0.9999,0.665,0.7927,0.00557),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_phosphor(B_LU = c('265','AVH',rep('3301000000',3)), 
                    B_SOILTYPE_AGR = c('dekzand',rep(NA,4)),
                    A_CLAY_MI = rep(15,5),
                    A_SAND_MI = rep(45,5),
                    A_SOM_LOI = rep(4.5,5),
                    A_PH_CC = rep(4.5,5),
                    A_P_OL = c(5,15,30,60,90),
                    A_C_OF = NA_real_,
                    A_PH_WA = NA_real_,
                    A_CACO3_IF = NA_real_,
                    A_P_AAA = NA_real_,A_P_AL = NA_real_, A_P_CAL = NA_real_,
                    A_P_CC = NA_real_, A_P_DL = NA_real_, A_P_M3 = NA_real_,
                    A_P_WA = NA_real_, 
                    B_COUNTRY = c('NL','FR','SE','ES','SK')),
    expected = c(0.0116,0.9999,1.0,0.9999,0.999),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_at works", {
  expect_equal(
    osi_c_phosphor_at(A_P_CAL = 47,
                       B_LU = '3301000000'),
    expected = c(0.894),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_at(A_P_CAL = rep(47,4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = rep(0.894,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_at(A_P_CAL = c(15,30,60,120),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0,0.3119,0.982,0.999),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_be works", {
  expect_equal(
    osi_c_phosphor_be(A_P_AL = 47,
                      B_LU = '3301000000'),
    expected = c(0.01145),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_be(A_P_AL = rep(47,4),
                      B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = rep(0.01145,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_be(A_P_AL = c(15,30,60,120),
                      B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0,0.0015,0.035,0.4207),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_ch works", {
  expect_equal(
    osi_c_phosphor_ch(B_LU = 'testcrop', A_P_AAA = 50),
    expected = c(0.9788),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_ch(A_P_AAA = rep(47,4),
                       B_LU = rep('testcrop',4)),
    expected = rep(0.9788,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_ch(A_P_AAA = c(1,5,25,65,165),
                       B_LU = rep('testcrop',5)),
    expected = c(0.005,0.0328,0.6955,0.996,0.9999),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_cz works", {
  expect_equal(
    osi_c_phosphor_cz(A_P_M3 = 85,
                      B_LU = '3301000000'),
    expected = c(0.9085),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_cz(A_P_M3 = rep(85,4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = rep(0.9085,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_cz(A_P_M3 = c(85,115,135,175)/1.5,
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.304,0.8175,0.940,0.9943),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_de works", {
  expect_equal(
    osi_c_phosphor_de(B_LU = '3301000000', 
                      A_CLAY_MI=15,
                      A_SOM_LOI = 45,
                      A_P_CAL = 25,
                      A_P_DL = NA_real_),
    expected = c(0.238),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_de( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_CLAY_MI = c(5,15,25,45),
                       A_SOM_LOI = rep(2.5,4),
                       A_P_CAL = rep(50,4),
                       A_P_DL = rep(NA_real_,4)),
    expected = c(0.595,0.7588,0.7588,0.7588),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_de( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_CLAY_MI = rep(25,4),
                       A_SOM_LOI = rep(45,4),
                       A_P_CAL = c(25,50,100,150)),
    expected = c(0.238,0.8586,0.998,0.9999),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_dk works", {
  expect_equal(
    osi_c_phosphor_dk(B_LU = '3301000000', 
                      A_P_OL = 50),
    expected = c(0.97),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_dk( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_P_OL = rep(50,4)),
    expected = rep(0.97,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_dk(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(15,35,50,100)),
    expected = c(0.426,0.88,0.97,0.9997),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_ee works", {
  expect_equal(
    osi_c_phosphor_ee(B_LU = '3301000000', 
                      A_SOM_LOI = 5,
                      A_P_M3 = 14.5),
    expected = c(0.8548),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_ee(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_SOM_LOI = c(2,4,6,8),
                      A_P_M3 = rep(15,4)),
    expected = c(0.1355,0.866,0.866,0.866),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_ee(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_SOM_LOI = c(2,4,6,8),
                      A_P_M3 = c(20,40,60,80)),
    expected = c(0.309,0.998,0.9999,0.9999),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_es works", {
  expect_equal(
    osi_c_phosphor_es(B_LU = '3301000000', 
                      A_CLAY_MI = 7.5,
                      A_SAND_MI = 45,
                      A_P_OL = 25),
    expected = c(0.9844),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_es(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_CLAY_MI = rep(7.5,4),
                      A_SAND_MI = rep(45,4),
                      A_P_OL = c(5,10,20,40)),
    expected = c(0.0581,0.4233,0.9412,0.9997),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_es(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                      A_CLAY_MI = c(5,10,20,40),
                      A_SAND_MI = c(5,10,20,40),
                      A_P_OL = c(5,10,20,40)),
    expected = c(0.0581,0.4233,0.7923,0.9958),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_fi works", {
  expect_equal(
    osi_c_phosphor_fi(B_LU = '1110',
                       B_TEXTURE_USDA = 'Si',
                       A_P_AAA = 15,
                       A_C_OF = 35),
    expected = c(0.828),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_fi(B_LU = c('5302','2180','9404','6305'), 
                       B_TEXTURE_USDA = rep('Si',4),
                      A_P_AAA = rep(15,4),
                       A_C_OF = c(10,20,100,300)),
    expected = rep(0.828,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_fi(B_LU = c('5302','2180','9404','6305'), 
                       B_TEXTURE_USDA = rep('Si',4),
                      A_P_AAA = c(8,15,45,90),
                       A_C_OF = c(10,20,100,300)),
    expected = c(0.418,0.828,0.999,1),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_fr works", {
  expect_equal(
    osi_c_phosphor_fr(B_LU = '3301090400',
                      A_PH_WA = 6.5, 
                      A_P_OL = 12),
    expected = c(0.3753),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                      A_PH_WA = rep(6.5,4),
                      A_P_OL = c(6,12,24,48)),
    expected = c(0.0017,0.999,0.9999,1.000),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_PH_WA = rep(3.5,4),
                       A_P_OL = c(6,8,10,14)),
    expected = c(0.0017,0.991,0.0789,0.8081),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_hu works", {
  expect_equal(
    osi_c_phosphor_hu(B_LU = 'testcrop1',
                      A_P_AL = 45,
                      A_CACO3_IF = 5,
                      A_CLAY_MI = 5,
                      A_SOM_LOI = 5),
    expected = c(0.0453),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_hu(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_P_AL = rep(45,4),
                       A_CACO3_IF = rep(5,4),
                       A_CLAY_MI = rep(5,4),
                       A_SOM_LOI = rep(5,4)),
    expected = rep(0.04529228,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_hu(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(20,40,80,100),
                      A_CACO3_IF = c(0,1,2,4),
                      A_CLAY_MI = c(2,4,6,8),
                      A_SOM_LOI = c(2,4,6,8)),
    expected = c(0.0055,0.4129,0.7689,0.9385),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_ie works", {
  expect_equal(
    osi_c_phosphor_ie(B_LU = 'testcrop1',
                      A_P_OL = 5),
    expected = c(0.4539),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(1,2,4,8)),
    expected = c(0.0559,0.1088,0.313,0.811),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(5,6,10,15)),
    expected = c(0.4539,0.5947,0.9235,0.9934),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_it works", {
  expect_equal(
    osi_c_phosphor_it(B_LU = '3301000000', 
                      A_P_OL = 15),
    expected = c(0.9908),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_it(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(5,10,15,20)),
    expected = c(0.4747,0.9204,0.9908,0.9989),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_it(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(1,2,4,8)),
    expected = c(0.0144,0.0636,0.3154,0.8189),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_lv works", {
  expect_equal(
    osi_c_phosphor_lv(B_LU = 'testcrop', 
                      B_TEXTURE_USDA = 'Cl',
                      A_P_DL = 15),
    expected = c(0.0504),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_lv(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      B_TEXTURE_USDA = c('Cl','ClLo','Lo','Sa'),
                      A_P_DL = rep(25,4)),
    expected = c(0.515,0.515,0.688,0.945),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_lv(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = rep('Cl',4),
                      A_P_DL = c(40,60,80,120)/2),
    expected = c(0.244,0.7322,0.9336,0.9966),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_lt works", {
  expect_equal(
    osi_c_phosphor_lt(B_LU = 'testcrop', 
                       A_SOM_LOI = 6.5,
                       A_P_AL = 15),
    expected = c(0.0140),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_lt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_SOM_LOI = c(4,4,8,8),
                      A_P_AL = c(25,40,60,80)),
    expected = c(0.255,0.783,0.9758,0.9975),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_lt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_SOM_LOI = c(6,6,60,60),
                      A_P_AL = c(25,75,45,90)),
    expected = c(0.255,0.9956,0.5945,0.9945),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_nl works", {
  expect_equal(
    osi_c_phosphor_nl(B_LU = '265', 
                      A_P_AL = 45, 
                      A_P_CC = 2.5),
    expected = c(0.9404),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_nl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_CC = c(1,4,8,12),
                      A_P_AL = rep(45,4),
                      B_SOILTYPE_AGR = rep('dekzand',4)),
    expected = c(0.839,0.998,0.999,0.999),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_nl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_SOILTYPE_AGR = c('rivierklei', 'zeeklei','veen','loess'),
                       A_P_CC = c(1,2,3,4),
                       A_P_AL = rep(35,4)),
    expected = c(0.839,0.9709,0.989,0.997),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_no works", {
  expect_equal(
    osi_c_phosphor_no(B_LU = '3301000000', 
                      A_P_AL = 15),
    expected = c(0.2418),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_no(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(5,10,15,20)),
    expected = c(0.02496,0.1006,0.2417,0.4168),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_no(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(1,2,4,8)),
    expected = c(0.00469,0.00754,0.01735,0.06216),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_pl works", {
  expect_equal(
    osi_c_phosphor_pl(B_LU = '3301000000', 
                      A_P_DL = 45),
    expected = c(0.8716),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_pl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_DL = c(20,40,60,80)),
    expected = c(0.065,0.7785,0.9775,0.9979),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_pl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_DL = c(15,35,45,55)),
    expected = c(0.00716,0.63396,0.8716,0.9594),
    tolerance = 0.01
  )
})



test_that("osi_c_phosphor_se works", {
  expect_equal(
    osi_c_phosphor_se(B_LU = '3301000000', 
                      A_P_AL = 15),
    expected = c(0.2339),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(6,12,14,20)),
    expected = c(0.0503,0.857,0.2037,0.4104),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(6,7,8,9)),
    expected = c(0.05,0.047,0.0747,0.090),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_sk works", {
  expect_equal(
    osi_c_phosphor_sk(A_P_M3 = 85,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.7308),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_sk(A_P_M3 = rep(65,4),
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.504,0.2295,0.7919,0.7919),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_sk(A_P_M3 = c(30,60,90,120),
                       B_TEXTURE_HYPRES = rep('F',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.00589,0.6945,0.9753,0.9983),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_sl works", {
  expect_equal(
    osi_c_phosphor_sl(B_LU = '3301000000', 
                      A_P_AL = 35),
    expected = c(0.427),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_sl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(20,30,40,50)),
    expected = c(0.01434,0.233,0.6087,0.8447),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_sl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(30,30,60,60)),
    expected = c(0.233,0.233,0.944,0.944),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_uk works", {
  expect_equal(
    osi_c_phosphor_uk(A_P_OL = 14,
                       A_SOM_LOI = 4.5,
                       B_LU = '3301000000'),
    expected = c(0.865),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_uk(A_P_OL = rep(14,4),
                       A_SOM_LOI = c(4,8,12,20),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.8737,0.7996,0.7192,0.565),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_uk(A_P_OL = c(2.5,5.0,7.5,10.0),
                       A_SOM_LOI = c(8,8,12,12),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.067,0.1668,0.275,0.452),
    tolerance = 0.01
  )
})
