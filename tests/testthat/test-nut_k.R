test_that("osi_nut_k works", {
  expect_equal(
    osi_nut_k(B_LU = '265', B_SOILTYPE_AGR = 'dekzand',
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
    expected = c(0.948),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k(B_LU = c('265','AVH',rep('3301000000',3)), 
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
    expected = c(0.9111,0.4531196,0.847,0.943,0.950),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_nut_k(B_LU = c('265','AVH',rep('3301000000',3)), 
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
    expected = c(0.9738,0.8487036 ,0.79,0.956,0.914),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_at works", {
  expect_equal(
    osi_nut_k_at(A_K_CAL = 47,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.927),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_at(A_K_CAL = rep(47,4),
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = rep(0.9276,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_at(A_K_CAL = c(40,60,180,200),
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.933,0.916,0.707,0.65),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_be works", {
  expect_equal(
    osi_nut_k_be(B_LU = '638', A_K_AAA = 150, B_TEXTURE_BE='S'),
    expected = c(0.904),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_be(A_K_AAA = rep(147,4),
                       B_TEXTURE_BE= c('S','P','L','A'),
                       B_LU = rep('8410',4)),
    expected = c(0.907,0.907,0.907,0.907),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_be(A_K_AAA = c(1,5,25,65,165)*5,
                       B_TEXTURE_BE= c('S','P','L','A','U'),
                       B_LU = rep('3301061299',5)),
    expected = c(0.9778,0.9727,0.925,0.626,0.0405),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_ch works", {
  expect_equal(
    osi_nut_k_ch(B_LU = 'testcrop', A_K_AAA = 150, A_CLAY_MI=15),
    expected = c(0.8992),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_ch(A_K_AAA = rep(147,4),
                       A_CLAY_MI = c(1,5,25,65),
                       B_LU = rep('testcrop',4)),
    expected = c(0.9156452, 0.9156452, 0.8682482, 0.4574739),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_ch(A_K_AAA = c(1,5,25,65,165)*2,
                       A_CLAY_MI = rep(47,5),
                       B_LU = rep('testcrop',5)),
    expected = c(0.969,0.962,0.900,0.554,0.023),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_cz works", {
  expect_equal(
    osi_nut_k_cz(A_K_M3 = 85,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.9556),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_cz(A_K_M3 = rep(185,4),
                       B_TEXTURE_HYPRES = c('C','M','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.892,0.901,0.999,0.999),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_cz(A_K_M3 = c(85,115,135,175)*2,
                       B_TEXTURE_HYPRES = rep('M',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.9128,0.86,0.81,0.69),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_de works", {
  expect_equal(
    osi_nut_k_de(B_LU = '3301000000', 
                       A_CLAY_MI=15,
                       A_SAND_MI = 45,
                       A_C_OF = 25,
                       A_K_CAL = 150),
    expected = c(0.945),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_de( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                        A_CLAY_MI = c(5,15,25,45),
                        A_SAND_MI = rep(45,4),
                        A_C_OF = rep(25,4),
                        A_K_CAL = rep(150,4)),
    expected = c(0.9453455, 0.9453455, 0.9592519, 0.9592519),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_de( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                        A_CLAY_MI = rep(25,4),
                        A_SAND_MI = rep(45,4),
                        A_C_OF = rep(25,4),
                        A_K_CAL = c(50,100,150,250)*2),
    expected = c( 0.9681687, 0.9480250, 0.9166577, 0.8045720),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_dk works", {
  expect_equal(
    osi_nut_k_dk(B_LU = '3301000000', 
                       A_K_AL = 150),
    expected = c(0.887),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_dk( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                        A_K_AL = rep(150,4)),
    expected = rep(0.8879914,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_dk(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_K_AL = c(15,35,50,100)*2),
    expected = c(0.9655047, 0.9482426, 0.9303471, 0.8260389),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_ee works", {
  expect_equal(
    osi_nut_k_ee(B_LU = '3301000000', 
                       B_TEXTURE_USDA = 'ClLo',
                       A_K_M3 = 145),
    expected = c(0.9397),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_ee(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = c('Sa','ClLo','Lo','Si'),
                       A_K_M3 = rep(125,4)),
    expected = c(0.7404823, 0.9483544, 0.9203539, 0.9483544),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_ee(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = rep('ClLo',4),
                       A_K_M3 = c(125,150,175,200)*2),
    expected = c(0.8702154, 0.8193603, 0.7560906, 0.6820271),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_es works", {
  expect_equal(
    osi_nut_k_es(B_LU = '3301000000', 
                       B_TEXTURE_HYPRES='C',
                       A_K_AAA = 135),
    expected = c(0.92),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_es(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_HYPRES= c('C', 'M','F','VF'),
                       A_K_AAA = c(20,30,40,50)*2),
    expected = c(0.9631145, 0.9610893, 0.960263, 0.9563922),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_es(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_HYPRES= c('C', 'M','F','VF'),
                       A_K_AAA = c(20,50,80,160)*3),
    expected = c(0.9566609, 0.934396, 0.9177971, 0.7830586),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_fi works", {
  expect_equal(
    osi_nut_k_fi(B_LU = '1110',
                       B_TEXTURE_USDA = 'Si',
                       A_K_AAA = 145,
                       A_C_OF = 35),
    expected = c(0.9168),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_fi(B_LU = c('5302','2180','9404','6305'), 
                       B_TEXTURE_USDA = rep('Si',4),
                       A_K_AAA = rep(145,4),
                       A_C_OF = c(10,20,100,300)),
    expected = rep(0.9168192,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_fi(B_LU = c('5302','2180','9404','6305'), 
                       B_TEXTURE_USDA = rep('Si',4),
                       A_K_AAA = c(15,45,90,180)*2,
                       A_C_OF = c(10,20,100,300)),
    expected = c( 0.9861550, 0.9626355, 0.8725474, 0.5736836),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_fr works", {
  expect_equal(
    osi_nut_k_fr(B_LU = '3301000000', 
                       B_TEXTURE_GEPPA = 'SS',
                       A_PH_WA = 6.5, 
                       A_K_AAA = 165),
    expected = c(0.6089),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_GEPPA = c('SS','A','L','AA'),
                       A_PH_WA = rep(5.5,4),
                       A_K_AAA = c(150,300,350,400)),
    expected = c(0.67410643, 0.15159303, 0.08290219, 0.04495985),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_GEPPA = c('SS','SS','SS','SS'), 
                       A_PH_WA = rep(6.5,4),
                       A_K_AAA = c(50,60,80,100)),
    expected = c(0.9426281, 0.9297406, 0.8960380, 0.8497202),
    tolerance = 0.01
  )
})


test_that("osi_nut_k_hu works", {
  expect_equal(
    osi_nut_k_hu(B_LU = 'testcrop', 
                       A_CLAY_MI = 5,
                       A_SOM_LOI = 5,
                       A_CACO3_IF = 1.5,
                       A_K_AL = 150),
    expected = c(0.9209),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_hu(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_CLAY_MI = rep(5,4),
                       A_SOM_LOI = rep(5,4),
                       A_CACO3_IF = rep(1.5,4),
                       A_K_AL = rep(85,4)),
    expected = c(rep(0.9558275,4)),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_hu(B_LU = c('3301000000','3301010901','3301061299','3304990000','3301010901'), 
                       A_CLAY_MI = rep(5,5),
                       A_SOM_LOI = rep(5,5),
                       A_CACO3_IF = rep(1.5,5),
                       A_K_AL = c(85,100,125,150,200)),
    expected = c(0.9558275, 0.9493443, 0.9365566, 0.9209121, 0.8792955),
    tolerance = 0.01
  )
})


test_that("osi_nut_k_it works", {
  expect_equal(
    osi_nut_k_it(B_LU = '3301000000', 
                       B_TEXTURE_HYPRES ='C',
                       A_K_AAA = 105),
    expected = c(0.902),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_it(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_HYPRES = rep('C',4),
                       A_K_AAA = rep(185,4)),
    expected = rep(0.734252,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_it(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       A_K_AAA = c(70,85,100,150)*2),
    expected = c(0.8840186, 0.7747788, 0.8706735, 0.7314768),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_ie works", {
  expect_equal(
    osi_nut_k_ie(B_LU = '3301000000', 
                       A_SOM_LOI = 6.5,
                       A_K_NaAAA = 145),
    expected = c(0.8507),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_SOM_LOI = c(1,2,4,8),
                       A_K_NaAAA = rep(145,4)),
    expected = c(0.7961732, 0.8087152, 0.8299037, 0.8609207),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_SOM_LOI = c(1,2,4,8),
                       A_K_NaAAA = c(40,10,40,10)*3),
    expected = c(0.8514901, 0.9604193, 0.8736481, 0.9634326),
    tolerance = 0.01
  )
})


test_that("osi_nut_k_lv works", {
  expect_equal(
    osi_nut_k_lv(B_LU = 'testcrop', 
                       B_TEXTURE_USDA = 'Sa',
                       A_K_DL = 150),
    expected = c(0.486),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_lv(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = c('Si','ClLo','Lo','Sa'),
                       A_K_DL = rep(185,4)),
    expected = c(0.6898692, 0.6898692, 0.6286503, 0.3118947),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_lv(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = rep('Sa',4),
                       A_K_DL = c(40,60,80,120)*3),
    expected = c(0.65339965, 0.33398159, 0.13811324 ,0.02062277),
    tolerance = 0.01
  )
})


test_that("osi_nut_k_lt works", {
  expect_equal(
    osi_nut_k_lt(B_LU = 'testcrop', 
                       A_SOM_LOI = 6.5,
                       A_K_AL = 350),
    expected = c(0.227),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_lt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_SOM_LOI = c(4,4,8,8),
                       A_K_AL = c(75,150,75,150)*2),
    expected = c(0.8021532, 0.3450715, 0.8021532, 0.3450715),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_lt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_SOM_LOI = c(6,6,6,6),
                       A_K_AL = c(25,75,150,300)),
    expected = c(0.9659751, 0.9278736, 0.8021532, 0.3450715),
    tolerance = 0.01
  )
})


test_that("osi_nut_k_nl works", {
  expect_equal(
    osi_nut_k_nl(B_LU = '265', 
                       B_SOILTYPE_AGR = 'dekzand',
                       A_SOM_LOI = 3.5,
                       A_CLAY_MI = 8.5,
                       A_PH_CC = 5.4, 
                       A_CEC_CO = 185,
                       A_K_CO_PO = 4.5,
                       A_K_CC = 65),
    expected = c(0.9558),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_nl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_SOILTYPE_AGR = rep('dekzand',4),
                       A_SOM_LOI = rep(3.5,4),
                       A_CLAY_MI = rep(8.5,4),
                       A_PH_CC = rep(5.4,4), 
                       A_CEC_CO = rep(185,4),
                       A_K_CO_PO = c(1,4,8,12),
                       A_K_CC = c(20,40,60,80)),
    expected = c(0.9610846, 0.8234049, 0.3968223, 0.1147088),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_nl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_SOILTYPE_AGR = c('rivierklei', 'zeeklei','dalgrond','loess'),
                       A_SOM_LOI = rep(3.5,4),
                       A_CLAY_MI = rep(8.5,4),
                       A_PH_CC = rep(5.4,4), 
                       A_CEC_CO = rep(185,4),
                       A_K_CO_PO = c(1,4,8,12),
                       A_K_CC = c(20,40,60,80)),
    expected = c(0.8983678, 0.7261814, 0.3968223, 0.0182759),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_no works", {
  expect_equal(
    osi_nut_k_no(B_LU = 'testcrop', 
                       A_CLAY_MI = 6.5,
                       A_K_AL = 150),
    expected = c(0.868),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_no(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_CLAY_MI = c(4,4,8,8),
                       A_K_AL = c(75,150,75,150)),
    expected = c(0.9442129, 0.8689894, 0.9442129, 0.8689894),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_no(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_CLAY_MI = c(4,6,6,6),
                       A_K_AL = c(50,75,150,300)),
    expected = c(0.9587464, 0.9442129, 0.8689894, 0.5394343),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_pl works", {
  expect_equal(
    osi_nut_k_pl(A_K_DL = 15,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.9662013),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_pl(A_K_DL = rep(75,4),
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.9404183, 0.8920393, 0.9516634, 0.9516634),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_pl(A_K_DL = c(5,10,20,40),
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.9724055, 0.9694540, 0.9626197, 0.9443917),
    tolerance = 0.01
  )
})


test_that("osi_nut_k_pt works", {
  expect_equal(
    osi_nut_k_pt(B_LU = '3301000000', 
                       A_K_AAA = 35),
    expected = c(0.9321845),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_pt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_K_AAA = c(75,150,75,150)),
    expected = c(0.7962954, 0.3306004, 0.7962954, 0.3306004),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_pt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_K_AAA = c(30,30,50,50)),
    expected = c(0.9416766, 0.9416766, 0.8949124, 0.8949124),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_ro works", {
  expect_equal(
    osi_nut_k_ro(A_K_AL = 150,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.8514),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_ro(A_K_AL = rep(425,4),
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c( 0.5524721, 0.2245681, 0.6131377, 0.6131377),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_ro(A_K_AL = c(65,85,125,150)*3,
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.7688326, 0.6235956, 0.3184966, 0.1869643),
    tolerance = 0.01
  )
})

test_that("osi_nut_k_se works", {
  expect_equal(
    osi_nut_k_se(B_LU = '3301000000', 
                       A_K_AL = 35),
    expected = c(0.9323676),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_K_AL = c(75,150,75,150)),
    expected = c(0.8931732, 0.7625710, 0.8931732, 0.7625710),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_K_AL = c(30,30,50,50)*3),
    expected = c(0.8737608, 0.8737608, 0.7625710, 0.7625710),
    tolerance = 0.01
  )
})


test_that("osi_nut_k_sk works", {
  expect_equal(
    osi_nut_k_sk(A_K_M3 = 150,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.9151174),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_sk(A_K_M3 = rep(125,4),
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.9497716, 0.9321923, 0.9593946, 0.9593946),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_sk(A_K_M3 = c(65,85,125,150)*2,
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.9290453, 0.8988612, 0.8066011, 0.7257211),
    tolerance = 0.01
  )
})


test_that("osi_nut_k_sl works", {
  expect_equal(
    osi_nut_k_sl(A_K_AL = 35,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(1),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_sl(A_K_AL = rep(154,4),
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c( 0.9513654, 0.9513654, 0.9302630, 0.9302630),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_sl(A_K_AL = c(50,75,100,150)*3,
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.9611483, 0.7868050, 0.6434678, 0.4303742),
    tolerance = 0.01
  )
})


test_that("osi_nut_k_uk works", {
  expect_equal(
    osi_nut_k_uk(A_K_AN = 54,
                       A_SOM_LOI = 4.5,
                       B_LU = '3301000000'),
    expected = c(0.952),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_uk(A_K_AN = rep(54,4),
                       A_SOM_LOI = c(4,8,12,20),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.9525090, 0.9558587, 0.9583434, 0.9617735),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_k_uk(A_K_AN = c(25,50,75,100)*3,
                       A_SOM_LOI = c(8,8,12,12),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.9454526, 0.8875212, 0.8233296, 0.7050668),
    tolerance = 0.01
  )
})
