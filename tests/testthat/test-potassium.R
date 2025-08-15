test_that("osi_c_potassium works", {
  expect_equal(
    osi_c_potassium(B_LU = '265', B_SOILTYPE_AGR = 'dekzand',
                    A_CLAY_MI = 15,A_SAND_MI = 45,
                    A_SOM_LOI = 4.5, A_CEC_CO = 95,
                    A_PH_CC = 6.5, A_CACO3_IF = 0.4,
                    A_C_OF = NA_real_, 
                    A_PH_WA = NA_real_,
                    A_K_AAA = 85,
                    A_K_AL = NA_real_,
                    A_K_AN = NA_real_,
                    A_K_CAL = NA_real_,
                    A_K_CC = NA_real_,
                    A_K_CO_PO = NA_real_,
                    A_K_DL = NA_real_,
                    A_K_M3 = NA_real_,
                    A_K_NaAAA = NA_real_, 
                    A_K_WA = NA_real_,
                    B_COUNTRY = 'NL'),
    expected = c(0.73),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium(B_LU = c('265','AVH',rep('3301000000',3)), 
                    B_SOILTYPE_AGR = c('dekzand',rep(NA,4)),
                    A_CLAY_MI = rep(15,5),A_SAND_MI = rep(45,5),
                    A_SOM_LOI = rep(4.5,5),
                    A_CEC_CO = rep(95,5),
                    A_PH_CC = rep(6.5,5), 
                    A_CACO3_IF = rep(0.4,5),
                    A_C_OF = NA_real_, 
                    A_PH_WA = NA_real_,
                    A_K_AAA = rep(125,5),
                    A_K_AL = NA_real_,
                    A_K_AN = NA_real_,
                    A_K_CAL = NA_real_,
                    A_K_CC = NA_real_,
                    A_K_CO_PO = NA_real_,
                    A_K_DL = NA_real_,
                    A_K_M3 = NA_real_,
                    A_K_NaAAA = NA_real_, 
                    A_K_WA = NA_real_,
                    B_COUNTRY = c('NL','FR','SE','ES','SK')),
    expected = c(0.9938,0.9996,0.9837,0.6394,0.0636),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_potassium(B_LU = c('265','AVH',rep('3301000000',3)), 
                    B_SOILTYPE_AGR = c('dekzand',rep(NA,4)),
                    A_CLAY_MI = rep(15,5),A_SAND_MI = rep(45,5),
                    A_SOM_LOI = rep(4.5,5),
                    A_CEC_CO = rep(95,5),
                    A_PH_CC = c(4,4,6,6,7), 
                    A_CACO3_IF = rep(0.4,5),
                    A_C_OF = NA_real_, 
                    A_PH_WA = NA_real_,
                    A_K_AAA = c(50,50,150,80,200),
                    A_K_AL = NA_real_,
                    A_K_AN = NA_real_,
                    A_K_CAL = NA_real_,
                    A_K_CC = NA_real_,
                    A_K_CO_PO = NA_real_,
                    A_K_DL = NA_real_,
                    A_K_M3 = NA_real_,
                    A_K_NaAAA = NA_real_, 
                    A_K_WA = NA_real_,
                    B_COUNTRY = c('NL','FR','SE','ES','SK')),
    expected = c(0.335,0.313,0.998,0.335,0.8656),
    tolerance = 0.01
  )
})

test_that("osi_c_potassium_at works", {
  expect_equal(
    osi_c_potassium_at(A_K_CAL = 47,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.0696),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_at(A_K_CAL = rep(47,4),
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = rep(0.0696,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_at(A_K_CAL = c(40,60,80,100),
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.0106,0.375,0.812,0.957),
    tolerance = 0.01
  )
})

test_that("osi_c_potassium_be works", {
  expect_equal(
    osi_c_potassium_be(B_LU = '638', A_K_AAA = 50, B_TEXTURE_BE='S'),
    expected = c(0.1197),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_be(A_K_AAA = rep(47,4),
                       B_TEXTURE_BE= c('S','P','L','A'),
                       B_LU = rep('8410',4)),
    expected = c(0.1028,0.0659,0.0659,0.0659),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_be(A_K_AAA = c(1,5,25,65,165),
                       B_TEXTURE_BE= c('S','P','L','A','U'),
                       B_LU = rep('3301061299',5)),
    expected = c(0.008,0.008,0.0229,0.148,0.9046),
    tolerance = 0.01
  )
})

test_that("osi_c_potassium_ch works", {
  expect_equal(
    osi_c_potassium_ch(B_LU = 'testcrop', A_K_AAA = 50, A_CLAY_MI=15),
    expected = c(0.3838),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_ch(A_K_AAA = rep(47,4),
                       A_CLAY_MI = c(1,5,25,65),
                       B_LU = rep('testcrop',4)),
    expected = c(0.1703,0.1703,0.524,0.935),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_ch(A_K_AAA = c(1,5,25,65,165),
                       A_CLAY_MI = rep(47,5),
                       B_LU = rep('testcrop',5)),
    expected = c(0.0487,0.111,0.653,0.985,0.999),
    tolerance = 0.01
  )
})

test_that("osi_c_potassium_cz works", {
  expect_equal(
    osi_c_potassium_cz(A_K_M3 = 85,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.011),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_cz(A_K_M3 = rep(185,4),
                       B_TEXTURE_HYPRES = c('C','M','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.960,0.936,0.252,0.252),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_cz(A_K_M3 = c(85,115,135,175),
                       B_TEXTURE_HYPRES = rep('M',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.0046,0.2367,0.5507,0.903),
    tolerance = 0.01
  )
})

test_that("osi_c_potassium_de works", {
  expect_equal(
    osi_c_potassium_de(B_LU = '3301000000', 
                       A_CLAY_MI=15,
                       A_SAND_MI = 45,
                       A_C_OF = 25,
                       A_K_CAL = 50),
    expected = c(0.1191),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_de( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                        A_CLAY_MI = c(5,15,25,45),
                        A_SAND_MI = rep(45,4),
                        A_C_OF = rep(25,4),
                        A_K_CAL = rep(50,4)),
    expected = c(0.1191,0.1191,0.0143,0.0143),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_de( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                        A_CLAY_MI = rep(25,4),
                        A_SAND_MI = rep(45,4),
                        A_C_OF = rep(25,4),
                        A_K_CAL = c(50,100,150,250)),
    expected = c(00.0143,0.462,0.8725,0.9957),
    tolerance = 0.01
  )
})

test_that("osi_c_potassium_dk works", {
  expect_equal(
    osi_c_potassium_dk(B_LU = '3301000000', 
                       A_K_AL = 50),
    expected = c(0.205),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_dk( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                        A_K_AL = rep(50,4)),
    expected = rep(0.2053,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_dk(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_K_AL = c(15,35,50,100)),
    expected = c(0.0052,0.067,0.2053,0.7868),
    tolerance = 0.01
  )
})

test_that("osi_c_potassium_ee works", {
  expect_equal(
    osi_c_potassium_ee(B_LU = '3301000000', 
                       B_TEXTURE_USDA = 'ClLo',
                       A_K_M3 = 145),
    expected = c(0.3198),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_ee(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = c('Sa','ClLo','Lo','Si'),
                       A_K_M3 = rep(125,4)),
    expected = c(0.9998,0.0682,0.8327,0.068),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_ee(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = rep('ClLo',4),
                       A_K_M3 = c(125,150,175,200)),
    expected = c(0.0682,0.3985,0.7299,0.8978),
    tolerance = 0.01
  )
})

test_that("osi_c_potassium_es works", {
  expect_equal(
    osi_c_potassium_es(B_LU = '3301000000', 
                       B_TEXTURE_HYPRES='C',
                       A_K_AAA = 35),
    expected = c(0.1258),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_es(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_HYPRES= c('C', 'M','F','VF'),
                       A_K_AAA = c(20,30,40,50)),
    expected = c(0.056,0.067,0.075,0.1047),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_es(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_HYPRES= c('C', 'M','F','VF'),
                       A_K_AAA = c(20,50,80,160)),
    expected = c(0.056,0.148,0.2298,0.6534),
    tolerance = 0.01
  )
})

test_that("osi_c_potassium_fi works", {
  expect_equal(
    osi_c_potassium_fi(B_LU = '1110',
                       B_TEXTURE_USDA = 'Si',
                       A_K_AAA = 45,
                       A_C_OF = 35),
    expected = c(0.1835),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_fi(B_LU = c('5302','2180','9404','6305'), 
                       B_TEXTURE_USDA = rep('Si',4),
                       A_K_AAA = rep(45,4),
                       A_C_OF = c(10,20,100,300)),
    expected = rep(0.1835,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_fi(B_LU = c('5302','2180','9404','6305'), 
                       B_TEXTURE_USDA = rep('Si',4),
                       A_K_AAA = c(15,45,90,180),
                       A_C_OF = c(10,20,100,300)),
    expected = c(0.077,0.1835,0.498,0.9508),
    tolerance = 0.01
  )
})

test_that("osi_c_potassium_fr works", {
  expect_equal(
    osi_c_potassium_fr(B_LU = '3301000000', 
                       B_TEXTURE_GEPPA = 'SS',
                       A_PH_WA = 6.5, 
                       A_K_AAA = 65),
    expected = c(0.997),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_GEPPA = c('SS','A','L','AA'),
                       A_PH_WA = rep(5.5,4),
                       A_K_AAA = c(150,300,350,400)*.4),
    expected = c(0.921,0.011,1,0.267),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_GEPPA = c('SS','SS','SS','SS'), 
                       A_PH_WA = rep(6.5,4),
                       A_K_AAA = c(50,60,80,100)),
    expected = c(0.01373,0.92,0.999,1.0),
    tolerance = 0.01
  )
})


test_that("osi_c_potassium_hu works", {
  expect_equal(
    osi_c_potassium_hu(B_LU = 'testcrop', 
                       A_CLAY_MI = 5,
                       A_SOM_LOI = 5,
                       A_CACO3_IF = 1.5,
                       A_K_AL = 150),
    expected = c(0.808),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_hu(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_CLAY_MI = rep(5,4),
                       A_SOM_LOI = rep(5,4),
                       A_CACO3_IF = rep(1.5,4),
                       A_K_AL = rep(85,4)),
    expected = c(rep(0.0132474,4)),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_hu(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301010901'), 
                       A_CLAY_MI = rep(5,5),
                       A_SOM_LOI = rep(5,5),
                       A_CACO3_IF = rep(1.5,5),
                       A_K_AL = c(85,100,125,150,200)),
    expected = c(0.013,0.1147,0.5066,0.808,0.979),
    tolerance = 0.01
  )
})


test_that("osi_c_potassium_it works", {
  expect_equal(
    osi_c_potassium_it(B_LU = '3301000000', 
                       B_TEXTURE_HYPRES ='C',
                       A_K_AAA = 105),
    expected = c(0.9319),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_it(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_HYPRES = rep('C',4),
                       A_K_AAA = rep(85,4)),
    expected = rep(0.7111124,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_it(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       A_K_AAA = c(70,85,100,150)),
    expected = c(0.007,0.711,0.337,0.876),
    tolerance = 0.01
  )
})

test_that("osi_c_potassium_ie works", {
  expect_equal(
    osi_c_potassium_ie(B_LU = '3301000000', 
                       A_SOM_LOI = 6.5,
                       A_K_NaAAA = 45),
    expected = c(0.3635),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_SOM_LOI = c(1,2,4,8),
                       A_K_NaAAA = rep(45,4)),
    expected = c(0.490,0.463,0.4147,0.337),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_SOM_LOI = c(1,2,4,8),
                       A_K_NaAAA = c(40,10,40,10)),
    expected = c(0.406,0.066,0.341,0.058),
    tolerance = 0.01
  )
})


test_that("osi_c_potassium_lv works", {
  expect_equal(
    osi_c_potassium_lv(B_LU = 'testcrop', 
                       B_TEXTURE_USDA = 'Sa',
                       A_K_DL = 150),
    expected = c(0.999),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_lv(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = c('Si','ClLo','Lo','Sa'),
                       A_K_DL = rep(85,4)),
    expected = c(0.893,0.893,0.940,0.997),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_lv(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = rep('Sa',4),
                       A_K_DL = c(40,60,80,120)),
    expected = c(0.655,0.955,0.995,0.999),
    tolerance = 0.01
  )
})


test_that("osi_c_potassium_lt works", {
  expect_equal(
    osi_c_potassium_lt(B_LU = 'testcrop', 
                       A_SOM_LOI = 6.5,
                       A_K_AL = 150),
    expected = c(0.998),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_lt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_SOM_LOI = c(4,4,8,8),
                       A_K_AL = c(75,150,75,150)),
    expected = c(0.7795,0.998,0.779,0.998),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_lt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_SOM_LOI = c(6,6,6,6),
                       A_K_AL = c(25,75,150,300)),
    expected = c(0.0016,0.779,0.998,0.999),
    tolerance = 0.01
  )
})


test_that("osi_c_potassium_nl works", {
  expect_equal(
    osi_c_potassium_nl(B_LU = '265', 
                       B_SOILTYPE_AGR = 'dekzand',
                       A_SOM_LOI = 3.5,
                       A_CLAY_MI = 8.5,
                       A_PH_CC = 5.4, 
                       A_CEC_CO = 185,
                       A_K_CO_PO = 4.5,
                       A_K_CC = 65),
    expected = c(0.6155),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_nl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_SOILTYPE_AGR = rep('dekzand',4),
                       A_SOM_LOI = rep(3.5,4),
                       A_CLAY_MI = rep(8.5,4),
                       A_PH_CC = rep(5.4,4), 
                       A_CEC_CO = rep(185,4),
                       A_K_CO_PO = c(1,4,8,12),
                       A_K_CC = c(20,40,60,80)),
    expected = c(0.2890905, 0.9953643,0.9999988, 1.0000000),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_nl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_SOILTYPE_AGR = c('rivierklei', 'zeeklei','dalgrond','loess'),
                       A_SOM_LOI = rep(3.5,4),
                       A_CLAY_MI = rep(8.5,4),
                       A_PH_CC = rep(5.4,4), 
                       A_CEC_CO = rep(185,4),
                       A_K_CO_PO = c(1,4,8,12),
                       A_K_CC = c(20,40,60,80)),
    expected = c(0.953,0.999,0.999,1.0),
    tolerance = 0.01
  )
})

test_that("osi_c_potassium_no works", {
  expect_equal(
    osi_c_potassium_no(B_LU = 'testcrop', 
                       A_CLAY_MI = 6.5,
                       A_K_AL = 150),
    expected = c(0.985),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_no(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_CLAY_MI = c(4,4,8,8),
                       A_K_AL = c(75,150,75,150)),
    expected = c(0.3881,0.985,0.3881,0.985),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_no(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_CLAY_MI = c(4,6,6,6),
                       A_K_AL = c(50,75,150,300)),
    expected = c(0.0247,0.388,0.985,0.999),
    tolerance = 0.01
  )
})

test_that("osi_c_potassium_pl works", {
  expect_equal(
    osi_c_potassium_pl(A_K_DL = 15,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.03190841),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_pl(A_K_DL = rep(75,4),
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.3955,0.961,0.0177,0.0177),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_pl(A_K_DL = c(5,10,20,40),
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.0016,0.00868,0.0856,0.564),
    tolerance = 0.01
  )
})


test_that("osi_c_potassium_se works", {
  expect_equal(
    osi_c_potassium_se(B_LU = '3301000000', 
                       A_K_AL = 35),
    expected = c(0.041),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_K_AL = c(75,150,75,150)),
    expected = c(0.84,0.999,0.84,0.999),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_K_AL = c(30,30,50,50)),
    expected = c(0.0106,0.0106,0.337,0.337),
    tolerance = 0.01
  )
})


test_that("osi_c_potassium_sk works", {
  expect_equal(
    osi_c_potassium_sk(A_K_M3 = 150,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.8688),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_sk(A_K_M3 = rep(125,4),
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.07,0.644,0.0002,0.0002),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_sk(A_K_M3 = c(65,85,125,150),
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.0012,0.0665,0.644,0.868),
    tolerance = 0.01
  )
})


test_that("osi_c_potassium_sl works", {
  expect_equal(
    osi_c_potassium_sl(A_K_AL = 54,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.0024),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_sl(A_K_AL = rep(154,4),
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.851,0.851,0.71,0.71),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_sl(A_K_AL = c(50,75,100,150),
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.001,0.0563,0.311,0.829),
    tolerance = 0.01
  )
})


test_that("osi_c_potassium_uk works", {
  expect_equal(
    osi_c_potassium_uk(A_K_AN = 54,
                       A_SOM_LOI = 4.5,
                       B_LU = '3301000000'),
    expected = c(0.4045),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_uk(A_K_AN = rep(54,4),
                       A_SOM_LOI = c(4,8,12,20),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.4155,0.3379,0.2808,0.206),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_potassium_uk(A_K_AN = c(25,50,75,100),
                       A_SOM_LOI = c(8,8,12,12),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.109,0.297,0.480,0.707),
    tolerance = 0.01
  )
})
