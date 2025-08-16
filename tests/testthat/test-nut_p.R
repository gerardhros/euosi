test_that("osi_nut_p works", {
  expect_equal(
    osi_nut_p(B_LU = '265', 
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
    expected = c(0.9141),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p(B_LU = c('265','AVH',rep('3301000000',3)), 
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
    expected = c(0.9354,0.622,0.94,0.929,0.960),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_nut_p(B_LU = c('265','AVH',rep('3301000000',3)), 
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
    expected = c(0.972,0.6222,0.731,0.409,0.529),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_at works", {
  expect_equal(
    osi_nut_p_at(A_P_CAL = 47,
                       B_LU = '3301000000'),
    expected = c(0.8786),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_at(A_P_CAL = rep(47,4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = rep(0.8786,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_at(A_P_CAL = c(30,60,120,240),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.911,0.846,0.594,0.036),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_be works", {
  expect_equal(
    osi_nut_p_be(A_P_AL = 47,
                      B_LU = '3301000000'),
    expected = c(0.9846),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_be(A_P_AL = rep(47,4),
                      B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = rep(0.9846,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_be(A_P_AL = c(30,60,120,240),
                      B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.987,0.982,0.966,0.884),
    tolerance = 0.01
  )
})


test_that("osi_nut_p_ch works", {
  expect_equal(
    osi_nut_p_ch(B_LU = 'testcrop', A_P_AAA = 50),
    expected = c(0.8736),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_ch(A_P_AAA = rep(47,4),
                       B_LU = rep('testcrop',4)),
    expected = rep(0.8839,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_ch(A_P_AAA = c(125,65,165,265,400),
                       B_LU = rep('testcrop',5)),
    expected = c(0.4214,0.81,0.206,0.026,0.0015),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_cz works", {
  expect_equal(
    osi_nut_p_cz(A_P_M3 = 85,
                      B_LU = '3301000000'),
    expected = c(0.912),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_cz(A_P_M3 = rep(85,4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = rep(0.9124,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_cz(A_P_M3 = c(85,115,135,275),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.912,0.8507,0.795,0.316),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_de works", {
  expect_equal(
    osi_nut_p_de(B_LU = '3301000000', 
                      A_SOM_LOI = 45,
                      A_P_CAL = 25,
                      A_P_DL = NA_real_),
    expected = c(0.97335),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_de( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_SOM_LOI = rep(2.5,4),
                       A_P_CAL = rep(50,4),
                       A_P_DL = rep(NA_real_,4)),
    expected = rep(0.9239,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_de( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_SOM_LOI = rep(45,4),
                       A_P_CAL = c(100,150,300,400)),
    expected = c(0.963,0.953,0.912,0.869),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_dk works", {
  expect_equal(
    osi_nut_p_dk(B_LU = '3301000000', 
                      A_P_OL = 50),
    expected = c(0.8289),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_dk( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_P_OL = rep(50,4)),
    expected = rep(0.8289039,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_dk(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(35,50,100,200)),
    expected = c(0.8769,0.8289,0.5452,0.00729),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_ee works", {
  expect_equal(
    osi_nut_p_ee(B_LU = '3301000000', 
                      A_SOM_LOI = 5,
                      A_P_M3 = 14.5),
    expected = c(0.9307),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_ee(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_SOM_LOI = c(2,4,6,8),
                      A_P_M3 = rep(150,4)),
    expected = rep(0.412,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_ee(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_SOM_LOI = c(2,4,6,8),
                      A_P_M3 = c(40,60,80,160)),
    expected = c(0.889,0.8415,0.777,0.351),
    tolerance = 0.01
  )
})


test_that("osi_nut_p_el works", {
  expect_equal(
    osi_nut_p_el(B_LU = '3301000000', 
                      A_P_OL =75),
    expected = c(0.04853163),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_el( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_P_OL = rep(16,4)),
    expected = rep(0.8664154,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_el(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(15,20,40,60)),
    expected = c(0.8726970, 0.8383430, 0.6090856, 0.2481140),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_es works", {
  expect_equal(
    osi_nut_p_es(B_LU = '3301000000', 
                      A_CLAY_MI = 7.5,
                      A_SAND_MI = 45,
                      A_P_OL = 25),
    expected = c(0.86711),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_es(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_CLAY_MI = rep(7.5,4),
                      A_SAND_MI = rep(45,4),
                      A_P_OL = c(5,10,20,40)),
    expected = c(0.9639,0.949,0.902,0.703),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_es(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                      A_CLAY_MI = c(5,10,20,40),
                      A_SAND_MI = c(5,10,20,40),
                      A_P_OL = c(20,40,60,80)),
    expected = c(0.902,0.703,0.6326,0.4099),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_fi works", {
  expect_equal(
    osi_nut_p_fi(B_LU = '1110',
                       B_TEXTURE_USDA = 'Si',
                       A_P_AAA = 15,
                       A_C_OF = 35),
    expected = c(0.935),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_fi(B_LU = c('5302','2180','9404','6305'), 
                       B_TEXTURE_USDA = rep('Si',4),
                      A_P_AAA = rep(15,4),
                       A_C_OF = c(10,20,100,300)),
    expected = rep(0.9354294 ,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_fi(B_LU = c('5302','2180','9404','6305'), 
                       B_TEXTURE_USDA = rep('Si',4),
                      A_P_AAA = c(8,15,45,90),
                       A_C_OF = c(10,20,100,300)),
    expected = c(0.938,0.935,0.9199,0.8907),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_fr works", {
  expect_equal(
    osi_nut_p_fr(B_LU = '3301090400',
                      A_PH_WA = 6.5, 
                      A_P_OL = 12),
    expected = c(0.9960),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                      A_PH_WA = rep(6.5,4),
                      A_P_OL = c(6,12,24,48)),
    expected = c(0.9985,0.8316,0.9720,0.393),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_PH_WA = rep(3.5,4),
                       A_P_OL = c(8,10,14,85)),
    expected = c(0.997,0.9112,0.9945,0.00139),
    tolerance = 0.01
  )
})


test_that("osi_nut_p_hu works", {
  expect_equal(
    osi_nut_p_hu(B_LU = 'testcrop1',
                      A_P_AL = 45,
                      A_CACO3_IF = 5,
                      A_CLAY_MI = 5,
                      A_SOM_LOI = 5),
    expected = c(0.956),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_hu(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_P_AL = rep(45,4),
                       A_CACO3_IF = rep(5,4),
                       A_CLAY_MI = rep(5,4),
                       A_SOM_LOI = rep(5,4)),
    expected = rep(0.9562671 ,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_hu(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(20,40,200,400),
                      A_CACO3_IF = c(0,1,2,4),
                      A_CLAY_MI = c(2,4,6,8),
                      A_SOM_LOI = c(2,6,10,24)),
    expected = c(0.964,0.956,0.668,0.149),
    tolerance = 0.01
  )
})


test_that("osi_nut_p_ie works", {
  expect_equal(
    osi_nut_p_ie(B_LU = 'testcrop1',
                      A_P_OL = 5),
    expected = c(0.952),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(1,20,40,80)),
    expected = c(0.971,0.755,0.263,0.0116),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(5,40,80,100)),
    expected = c(0.952,0.263,0.0116,0.00235),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_it works", {
  expect_equal(
    osi_nut_p_it(B_LU = '3301000000', 
                      A_P_OL = 15),
    expected = c(0.850),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_it(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(5,10,15,20)),
    expected = c(0.952,0.913,0.850,0.7555),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_it(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(18,16,32,62)),
    expected = c(0.797,0.8339,0.443,0.0487),
    tolerance = 0.01
  )
})


test_that("osi_nut_p_lv works", {
  expect_equal(
    osi_nut_p_lv(B_LU = 'testcrop', 
                      B_TEXTURE_USDA = 'Cl',
                      A_P_DL = 15),
    expected = c(0.921),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_lv(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      B_TEXTURE_USDA = c('Cl','ClLo','Lo','Sa'),
                      A_P_DL = rep(25,4)),
    expected = c(0.9027048 ,0.9027048 ,0.8604878,0.8604878),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_lv(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = rep('Cl',4),
                      A_P_DL = c(40,60,80,120)),
    expected = c(0.8665,0.7995,0.706,0.438),
    tolerance = 0.01
  )
})


test_that("osi_nut_p_lt works", {
  expect_equal(
    osi_nut_p_lt(B_LU = 'testcrop', 
                       A_SOM_LOI = 6.5,
                       A_P_AL = 15),
    expected = c(0.976),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_lt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_SOM_LOI = c(4,4,8,8),
                      A_P_AL = c(25,40,60,80)),
    expected = c(0.962,0.928,0.851,0.7429),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_lt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_SOM_LOI = c(6,6,60,60),
                      A_P_AL = c(25,75,45,90)),
    expected = c(0.962,0.7718,0.937,0.839),
    tolerance = 0.01
  )
})


test_that("osi_nut_p_nl works", {
  expect_equal(
    osi_nut_p_nl(B_LU = '265', 
                      A_P_AL = 45, 
                      A_P_CC = 2.5),
    expected = c(0.9077),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_nl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_CC = c(1,4,8,12),
                      A_P_AL = rep(45,4),
                      B_SOILTYPE_AGR = rep('dekzand',4)),
    expected = c(0.927,0.8099,0.615,0.428),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_nl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_SOILTYPE_AGR = c('rivierklei', 'zeeklei','veen','loess'),
                       A_P_CC = c(1,2,3,4),
                       A_P_AL = rep(35,4)),
    expected = c(0.927,0.891,0.8655,0.818),
    tolerance = 0.01
  )
})


test_that("osi_nut_p_no works", {
  expect_equal(
    osi_nut_p_no(B_LU = '3301000000', 
                      A_P_AL = 15),
    expected = c(0.928),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_no(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(5,10,15,20)),
    expected = c(0.9406,0.9349,0.9287,0.9219),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_no(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(5,10,50,100)),
    expected = c(0.94,0.934,0.8669,0.6993),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_pl works", {
  expect_equal(
    osi_nut_p_pl(B_LU = '3301000000', 
                      A_P_DL = 45),
    expected = c(0.8988),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_pl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_DL = c(20,40,60,80)),
    expected = c(0.9405,0.9089,0.862,0.7977),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_pl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_DL = c(45,55,100,150)),
    expected = c(0.898,0.875,0.711,0.424),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_pt works", {
  expect_equal(
    osi_nut_p_pt(B_LU = '3301000000', 
                      A_P_OL = 45),
    expected = c(0.455),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_pt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(10,20,40,60)),
    expected = c(0.9456603, 0.8730602, 0.5517760, 0.2255477),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_pt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(5,15,35,45)),
    expected = c(0.9653914, 0.9160007, 0.6487710, 0.4553924),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_ro works", {
  expect_equal(
    osi_nut_p_ro(B_LU = '3301000000', 
                 A_P_AL = 145),
    expected = c(0.1056),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_ro(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                 A_P_AL = 50+c(10,20,40,60)),
    expected = c(0.9202589, 0.7134385, 0.4287582, 0.2576727),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_ro(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                 A_P_AL = 50+c(5,15,35,45)),
    expected = c(0.9993681, 0.8102938, 0.4869658, 0.3775082),
    tolerance = 0.01
  )
})
test_that("osi_nut_p_se works", {
  expect_equal(
    osi_nut_p_se(B_LU = '3301000000', 
                      A_P_AL = 15),
    expected = c(0.959),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(6,12,14,20)),
    expected = c(0.969,0.878,0.96,0.952),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(5,10,50,100)),
    expected = c(0.970,0.893,0.8808,0.6097),
    tolerance = 0.01
  )
})

test_that("osi_nut_p_sk works", {
  expect_equal(
    osi_nut_p_sk(A_P_M3 = 85,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.9268),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_sk(A_P_M3 = rep(65,4),
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.938,0.945,0.885,0.885),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_sk(A_P_M3 = c(30,60,90,120),
                       B_TEXTURE_HYPRES = rep('F',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.923,0.8914,0.847,0.789),
    tolerance = 0.01
  )
})


test_that("osi_nut_p_sl works", {
  expect_equal(
    osi_nut_p_sl(B_LU = '3301000000', 
                      A_P_AL = 35),
    expected = c(0.945),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_sl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(20,30,40,50)),
    expected = c(0.96203,0.951,0.938,0.921),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_sl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(60,80,140,250)),
    expected = c(0.9009,0.846,0.572,0.1375),
    tolerance = 0.01
  )
})


test_that("osi_nut_p_uk works", {
  expect_equal(
    osi_nut_p_uk(A_P_OL = 14,
                       A_SOM_LOI = 4.5,
                       B_LU = '3301000000'),
    expected = c(0.8806),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_uk(A_P_OL = rep(14,4),
                       A_SOM_LOI = c(4,8,12,20),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.879,0.888,0.8949,0.904),
    tolerance = 0.01
  )
  expect_equal(
    osi_nut_p_uk(A_P_OL = c(2.5,5.0,7.5,10.0),
                       A_SOM_LOI = c(12,20,30,60),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.931,0.927,0.9247,0.9266),
    tolerance = 0.01
  )
})
