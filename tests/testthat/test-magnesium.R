test_that("osi_c_magnesium works", {
  expect_equal(
    osi_c_magnesium(B_LU = '265', B_SOILTYPE_AGR = 'dekzand',
                    A_CLAY_MI = 15,A_SAND_MI = 45,
                    A_SOM_LOI = 4.5, A_C_OF = NA_real_, A_CEC_CO = 95,
                    A_PH_CC = 6.5, A_CACO3_IF = 0.4,
                    A_MG_AAA = 68,A_MG_AL = NA_real_, 
                    A_MG_AN = NA_real_,A_MG_CC = NA_real_,
                    A_MG_CO_PO = NA_real_, A_MG_DL = NA_real_,
                    A_MG_KCL = NA_real_,A_MG_M3 = NA_real_,
                    A_MG_NaAAA = NA_real_,A_K_AAA = NA_real_,
                    A_K_CO_PO = NA_real_,A_K_CC = NA_real_,
                    B_COUNTRY = 'NL'),
    expected = c(0.65),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium(B_LU = c('265','AVH',rep('3301000000',3)), 
                    B_SOILTYPE_AGR = c('dekzand',rep(NA,4)),
                    A_CLAY_MI = rep(15,5),
                    A_SAND_MI = rep(45,5),
                    A_SOM_LOI = rep(4.5,5),
                    A_C_OF = NA_real_, 
                    A_CEC_CO = rep(95,5),
                    A_PH_CC = rep(6.5,5), 
                    A_CACO3_IF = rep(0.4,5),
                    A_MG_AAA = rep(68,5),
                    A_MG_AL = NA_real_, 
                    A_MG_AN = NA_real_,A_MG_CC = NA_real_,
                    A_MG_CO_PO = NA_real_, A_MG_DL = NA_real_,
                    A_MG_KCL = NA_real_,A_MG_M3 = NA_real_,
                    A_MG_NaAAA = NA_real_,A_K_AAA = NA_real_,
                    A_K_CO_PO = NA_real_,A_K_CC = NA_real_,
                    B_COUNTRY = c('NL','FR','SE','ES','SK')),
    expected = c(0.6597,0.97096,1,0.4486,0.0064),
    tolerance = 0.01
  )

  expect_equal(
    osi_c_magnesium(B_LU = c('265','AVH',rep('3301000000',3)), 
                    B_SOILTYPE_AGR = c('dekzand',rep(NA,4)),
                    A_CLAY_MI = rep(15,5),
                    A_SAND_MI = rep(45,5),
                    A_SOM_LOI = rep(4.5,5),
                    A_C_OF = NA_real_, 
                    A_CEC_CO = rep(95,5),
                    A_PH_CC = rep(6.5,5), 
                    A_CACO3_IF = rep(0.4,5),
                    A_MG_AAA = rep(68,5),
                    A_MG_AL = rep(85,5), 
                    A_MG_AN = rep(85,5),
                    A_MG_CC = rep(85,5),
                    A_MG_CO_PO = rep(16,5), A_MG_DL = rep(85,5),
                    A_MG_KCL = rep(85,5),A_MG_M3 = rep(585,5),
                    A_MG_NaAAA = NA_real_,A_K_AAA = NA_real_,
                    A_K_CO_PO = NA_real_,A_K_CC = NA_real_,
                    B_COUNTRY = c('NL','FR','SE','ES','SK')),
    expected = c(0.6597,0.97096,1,0.4486,0.0064),
    tolerance = 0.01
  )
})

test_that("osi_c_magnesium_at works", {
  expect_equal(
    osi_c_magnesium_at(A_MG_CC = 47,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.964),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_at(A_MG_CC = rep(47,4),
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = rep(0.964,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_at(A_MG_CC = c(5,10,20,40),
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.0386,0.1117,0.432,0.9188),
    tolerance = 0.01
  )
})

test_that("osi_c_magnesium_ch works", {
  expect_equal(
    osi_c_magnesium_ch(B_LU = 'testcrop', A_MG_AAA = 50, A_CLAY_MI=15),
    expected = c(0.5839),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_ch(A_MG_AAA = rep(47,4),
                       A_CLAY_MI = c(1,5,25,65),
                       B_LU = rep('testcrop',4)),
    expected = c(0.693,0.693,0.397,0.113),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_ch(A_MG_AAA = c(1,5,25,65,165),
                       A_CLAY_MI = rep(47,5),
                       B_LU = rep('testcrop',5)),
    expected = c(0.012,0.0155,0.0452,0.1996,0.7726),
    tolerance = 0.01
  )
})

test_that("oci_c_magnesium_cz works", {
  expect_equal(
    oci_c_magnesium_cz(A_MG_M3 = 85,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.1855),
    tolerance = 0.01
  )
  expect_equal(
    oci_c_magnesium_cz(A_MG_M3 = rep(85,4),
                       B_TEXTURE_HYPRES = c('C','M','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.1855,0.0021,0.00714,0.00714),
    tolerance = 0.01
  )
  expect_equal(
    oci_c_magnesium_cz(A_MG_M3 = c(85,115,135,175),
                       B_TEXTURE_HYPRES = rep('M',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.0021,0.265,0.621,0.941),
    tolerance = 0.01
  )
})

test_that("osi_c_magnesium_de works", {
  expect_equal(
    osi_c_magnesium_de(B_LU = '3301000000', 
                       A_CLAY_MI=15,
                       A_SAND_MI = 45,
                       A_C_OF = 25,
                       A_MG_CC = 50),
    expected = c(0.3657),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_de( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_CLAY_MI = c(5,15,25,45),
                       A_SAND_MI = rep(45,4),
                       A_C_OF = rep(25,4),
                       A_MG_CC = rep(50,4)),
    expected = c(0.366,0.366,0.116,0.116),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_de( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                        A_CLAY_MI = rep(25,4),
                        A_SAND_MI = rep(45,4),
                        A_C_OF = rep(25,4),
                        A_MG_CC = c(50,100,150,250)),
    expected = c(0.116,0.899,0.9949,0.9999),
    tolerance = 0.01
  )
})

test_that("osi_c_magnesium_dk works", {
  expect_equal(
    osi_c_magnesium_dk(B_LU = '3301000000', 
                       A_MG_AL = 50),
    expected = c(0.787),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_dk( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                        A_MG_AL = rep(50,4)),
    expected = rep(0.787,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_dk(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_MG_AL = c(15,35,50,100)),
    expected = c(0.0405,0.4707,0.787,0.9951),
    tolerance = 0.01
  )
})

test_that("oci_c_magnesium_ee works", {
  expect_equal(
    oci_c_magnesium_ee(B_LU = '3301000000', 
                       B_TEXTURE_USDA = 'clay',
                       A_MG_M3 = 145),
    expected = c(0.93),
    tolerance = 0.01
  )
  expect_equal(
    oci_c_magnesium_ee(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = c('clay','ClLo','Lo','Si'),
                       A_MG_M3 = rep(125,4)),
    expected = c(0.836,0.836,0.969,0.836),
    tolerance = 0.01
  )
  expect_equal(
    oci_c_magnesium_ee(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = rep('clay',4),
                       A_MG_M3 = c(75,100,125,150)),
    expected = c(0.17898,0.573,0.8362,0.944),
    tolerance = 0.01
  )
})

test_that("osi_c_magnesium_es works", {
  expect_equal(
    osi_c_magnesium_es(B_LU = '3301000000', 
                       A_MG_CO_PO = 3.5),
    expected = c(0.23),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_es(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_MG_CO_PO = c(2,3,4,5)),
    expected = c(0.1179,0.1893,0.274,0.366),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_es(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_MG_CO_PO = c(2,5,8,16)),
    expected = c(0.1179,0.3661,0.6263,0.9418),
    tolerance = 0.01
  )
})

test_that("osi_c_magnesium_fi works", {
  expect_equal(
    osi_c_magnesium_fi(B_LU = '1110',
                       B_TEXTURE_USDA = 'Si',
                       A_MG_AAA = 45,
                       A_C_OF = 35),
    expected = c(0.38),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_fi(B_LU = c('5302','2180','9404','6305'), 
                       B_TEXTURE_USDA = rep('Si',4),
                       A_MG_AAA = rep(45,4),
                       A_C_OF = c(10,20,100,300)),
    expected = rep(0.3811,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_fi(B_LU = c('5302','2180','9404','6305'), 
                       B_TEXTURE_USDA = rep('Si',4),
                       A_MG_AAA = c(15,45,90,180),
                       A_C_OF = c(10,20,100,300)),
    expected = c(0.052,0.381,0.8589,0.99662),
    tolerance = 0.01
  )
})

test_that("osi_c_magnesium_fr works", {
  expect_equal(
    osi_c_magnesium_fr(B_LU = '3301000000', 
                       A_CLAY_MI = 25, 
                       A_CEC_CO = 165, 
                       A_CACO3_IF = 6.5, 
                       A_MG_AAA = 65),
    expected = c(0.963),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_CLAY_MI = rep(25,4), 
                       A_CEC_CO = c(85,120,160,200), 
                       A_CACO3_IF = rep(6.5,4),
                       A_MG_AAA = c(10,30,60,100)),
    expected = c(0.244,0.615,0.946,0.9977),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_CLAY_MI = rep(25,4), 
                       A_CEC_CO = c(85,120,160,200), 
                       A_CACO3_IF = c(0.5,1,2,18),
                       A_MG_AAA = c(10,30,60,100)),
    expected = c(0.244,0.6153,0.9463,0.9977),
    tolerance = 0.01
  )
})


test_that("osi_c_magnesium_hu works", {
  expect_equal(
    osi_c_magnesium_hu(B_LU = 'testcrop', 
                       B_TEXTURE_USDA = 'clay',
                       A_MG_KCL = 15),
    expected = c(0.84),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_hu(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = c('clay','ClLo','Lo','Sa'),
                       A_MG_KCL = rep(8.5,4)),
    expected = c(rep(0.7958,3),0.066),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_hu(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = rep('clay',4),
                       A_MG_KCL = c(0.2,4,8,12)),
    expected = c(0.73,0.762,0.792,0.820),
    tolerance = 0.01
  )
})


test_that("osi_c_magnesium_it works", {
  expect_equal(
    osi_c_magnesium_it(B_LU = '3301000000', 
                       A_MG_CO_PO = 6.5,
                       A_K_CO_PO = 4.5),
    expected = c(0.557),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_it(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_MG_CO_PO = c(1,2,4,8),
                       A_K_CO_PO = rep(4.5,4)),
    expected = c(0.05,0.107,0.2911,0.6879),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_it(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_MG_CO_PO = c(1,2,4,8),
                       A_K_CO_PO = c(4,1,4,1)),
    expected = c(0.0557,0.4064,0.345,0.9996),
    tolerance = 0.01
  )
})

test_that("osi_c_magnesium_ie works", {
  expect_equal(
    osi_c_magnesium_ie(B_LU = '3301000000', 
                       A_SOM_LOI = 6.5,
                       A_MG_NaAAA = 45),
    expected = c(0.8081),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_SOM_LOI = c(1,2,4,8),
                       A_MG_NaAAA = rep(45,4)),
    expected = c(0.891,0.877,0.847,0.784),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_SOM_LOI = c(1,2,4,8),
                       A_MG_NaAAA = c(40,10,40,10)),
    expected = c(0.842,0.145,0.788,0.111),
    tolerance = 0.01
  )
})


test_that("osi_c_magnesium_lv works", {
  expect_equal(
    osi_c_magnesium_lv(B_LU = 'testcrop', 
                       B_TEXTURE_USDA = 'clay',
                       A_MG_DL = 150),
    expected = c(0.968),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_lv(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = c('clay','ClLo','Lo','Sa'),
                       A_MG_DL = rep(85,4)),
    expected = c(rep(0.6537,2),0.7734,0.956),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_lv(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = rep('clay',4),
                       A_MG_DL = c(40,60,80,120)),
    expected = c(0.0958,0.327,0.5964,0.8987),
    tolerance = 0.01
  )
})


test_that("osi_c_magnesium_lt works", {
  expect_equal(
    osi_c_magnesium_lt(B_LU = 'testcrop', 
                       A_PH_KCL = 6.5,
                       A_MG_AL = 150),
    expected = c(0.576),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_lt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_PH_KCL = c(4,4,8,8),
                       A_MG_AL = c(75,150,75,150)),
    expected = c(0.5718,0.9909,0.0014,0.02389),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_lt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_PH_KCL = c(6,6,6,6),
                       A_MG_AL = c(25,75,150,300)),
    expected = c(0.0004,0.5718,0.9909,0.9999),
    tolerance = 0.01
  )
})


test_that("osi_c_magnesium_nl works", {
  expect_equal(
    osi_c_magnesium_nl(B_LU = '265', 
                       B_SOILTYPE_AGR = 'dekzand',
                       A_SOM_LOI = 3.5,
                       A_CLAY_MI = 8.5,
                       A_PH_CC = 5.4, 
                       A_CEC_CO = 185,
                       A_K_CO_PO = 4.5,
                       A_MG_CC = 125,
                       A_K_CC = 65),
    expected = c(0.9833),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_nl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_SOILTYPE_AGR = rep('dekzand',4),
                       A_SOM_LOI = rep(3.5,4),
                       A_CLAY_MI = rep(8.5,4),
                       A_PH_CC = rep(5.4,4), 
                       A_CEC_CO = rep(185,4),
                       A_K_CO_PO = c(1,4,8,12),
                       A_MG_CC = rep(45,4),
                       A_K_CC = c(20,40,60,80)),
    expected = rep(0.7482,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_nl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_SOILTYPE_AGR = c('rivierklei', 'zeeklei','veen','loess'),
                       A_SOM_LOI = rep(3.5,4),
                       A_CLAY_MI = rep(8.5,4),
                       A_PH_CC = rep(5.4,4), 
                       A_CEC_CO = rep(185,4),
                       A_K_CO_PO = c(1,4,8,12),
                       A_MG_CC = c(15,25,30,50),
                       A_K_CC = c(20,40,60,80)),
    expected = c(0.075,0.1788,0.2694,0.8801),
    tolerance = 0.01
  )
})

test_that("osi_c_magnesium_pl works", {
  expect_equal(
    osi_c_magnesium_pl(A_MG_CC = 15,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.556),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_pl(A_MG_CC = rep(55,4),
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.922,0.9999,0.7476,0.7476),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_pl(A_MG_CC = c(5,10,20,40),
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.0003,0.100,0.8655,0.9994),
    tolerance = 0.01
  )
})


test_that("osi_c_magnesium_pt works", {
  expect_equal(
    osi_c_magnesium_pt(B_LU = '3301000000', 
                       A_MG_AAA = 35),
    expected = c(0.25),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_pt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_MG_AAA = c(40,70,80,100)),
    expected = c(0.4107403, 0.9397339, 0.9747513, 0.9956818),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_pt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_MG_AAA = c(15,40,70,100)),
    expected = c(0.0003538678, 0.4107402791, 0.9397338991, 0.9956818386),
    tolerance = 0.01
  )
})


test_that("osi_c_magnesium_ro works", {
  expect_equal(
    osi_c_magnesium_ro(B_LU = 'testcrop', 
                       B_TEXTURE_HYPRES ='M', 
                       A_CEC_CO = 140,
                       A_MG_CC = 60, 
                       A_MG_CO_PO = 8,
                       A_K_CO_PO = 12, 
                       A_PH_WA = 5),
    expected = c(0.6392354),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_ro(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_HYPRES = rep('M',4), 
                       A_CEC_CO = rep(140,4),
                       A_MG_CC = c(20,40,60,80), 
                       A_MG_CO_PO = rep(8,4),
                       A_K_CO_PO = c(2,3,4,5) ,
                       A_PH_WA = rep(5,4)),
    expected = c(0.4684300, 0.8334871, 0.9533450, 0.9420577),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_ro(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_HYPRES = rep('M',4), 
                       A_CEC_CO = rep(140,4),
                       A_MG_CC = rep(40,4), 
                       A_MG_CO_PO = c(2,3,4,5),
                       A_K_CO_PO = c(2,3,4,5) ,
                       A_PH_WA = rep(5,4)),
    expected = c(0.6956271, 0.6956271, 0.6956271, 0.6956271),
    tolerance = 0.01
  )
})

test_that("osi_c_magnesium_se works", {
  expect_equal(
    osi_c_magnesium_se(B_LU = '3301000000', 
                       A_MG_AL = 3.5),
    expected = c(0.5814),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_MG_AL = c(0.75,1.50,3.5,7.5)),
    expected = c(0.00115,0.03288,0.5814,0.9866),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_MG_AL = c(3,3,5,5)),
    expected = c(0.42,0.423,0.873,0.873),
    tolerance = 0.01
  )
})


test_that("osi_c_magnesium_sk works", {
  expect_equal(
    osi_c_magnesium_sk(A_MG_M3 = 150,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.934),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_sk(A_MG_M3 = rep(125,4),
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.351,0.791,0.036,0.036),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_sk(A_MG_M3 = c(65,85,125,150),
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.0115,0.1855,0.791,0.9341),
    tolerance = 0.01
  )
})


test_that("osi_c_magnesium_sl works", {
  expect_equal(
    osi_c_magnesium_sl(A_MG_AL = 54,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.780),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_sl(A_MG_AL = rep(54,4),
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.780,0.78,0.182,0.182),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_sl(A_MG_AL = c(25,50,75,100),
                       B_TEXTURE_HYPRES = rep('C',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.0352,0.6992,0.9645,0.9964),
    tolerance = 0.01
  )
})


test_that("osi_c_magnesium_uk works", {
  expect_equal(
    osi_c_magnesium_uk(A_MG_AN = 54,
                       A_SOM_LOI = 4.5,
                       B_LU = '3301000000'),
    expected = c(0.913),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_uk(A_MG_AN = rep(54,4),
                       A_SOM_LOI = c(4,8,12,20),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.918,0.872,0.821,0.713),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_magnesium_uk(A_MG_AN = c(25,50,75,100),
                       A_SOM_LOI = c(8,8,12,12),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.4186,0.838,0.943,0.9863),
    tolerance = 0.01
  )
})
