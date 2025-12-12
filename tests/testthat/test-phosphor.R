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
    expected = c(0.74885847, 0.99222072, 0.36435406, 0.90087403, 0.01260354),
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
    expected = c(0.818),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_at(A_P_CAL = rep(47,4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = rep(0.818,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_at(A_P_CAL = c(15,30,60,120),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.0004846092, 0.2447048855, 0.9558695929 ,0.9999542597),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_be works", {
  expect_equal(
    osi_c_phosphor_be(A_P_AL = 47,
                      B_LU = '3301000000'),
    expected = c(0.000125 ),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_be(A_P_AL = rep(47,4),
                      B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = rep(0.000125 ,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_be(A_P_AL = c(60,160,120,200),
                      B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.03667614, 0.99858614 ,0.96894434 ,0.99993655),
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
    expected = c(.84),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_cz(A_P_M3 = rep(85,4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = rep(.84,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_cz(A_P_M3 = c(85,115,135,175)/1.5,
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c( 0.2648876, 0.7279080, 0.8852491, 0.9822447),
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
    expected = c(0.209 ),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_de( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_CLAY_MI = c(5,15,25,45),
                       A_SOM_LOI = rep(2.5,4),
                       A_P_CAL = rep(50,4),
                       A_P_DL = rep(NA_real_,4)),
    expected = c(0.6534812, 0.7639578, 0.7639578, 0.7639578),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_de( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_CLAY_MI = rep(25,4),
                       A_SOM_LOI = rep(45,4),
                       A_P_CAL = c(25,50,100,150)),
    expected = c(0.2087802, 0.7956281, 0.9958094, 0.9999239),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_dk works", {
  expect_equal(
    osi_c_phosphor_dk(B_LU = '3301000000', 
                      A_P_OL = 50),
    expected = c(1),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_dk( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_P_OL = rep(25,4)),
    expected = rep(0.611 ,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_dk(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(25,35,50,100)),
    expected = c(0.6112225, 0.9778582, 0.9997830, 1.0000000),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_ee works", {
  expect_equal(
    osi_c_phosphor_ee(B_LU = '3301000000', 
                      A_SOM_LOI = 5,
                      A_P_M3 = 14.5),
    expected = c( 0.344),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_ee(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_SOM_LOI = c(2,4,6,8),
                      A_P_M3 = rep(15,4)),
    expected = c(0.1399633, 0.3655542, 0.3655542, 0.3655542),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_ee(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_SOM_LOI = c(2,4,6,8),
                      A_P_M3 = c(20,40,60,80)),
    expected = c( 0.2645796, 0.9482871, 0.9949953, 0.9995262),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_el works", {
  expect_equal(
    osi_c_phosphor_el(B_LU = '3301000000', 
                      A_P_OL = 25),
    expected = c(0.983),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_el( B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_P_OL = rep(16,4)),
    expected = rep(0.7733823 ,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_el(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(5,10,15,20)),
    expected = c(0.001051181, 0.209728988, 0.706512742, 0.926024788),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_es works", {
  expect_equal(
    osi_c_phosphor_es(B_LU = '3301000000', 
                      A_CLAY_MI = 7.5,
                      A_SAND_MI = 45,
                      A_P_OL = 25),
    expected = c(0.997),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_es(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_CLAY_MI = rep(7.5,4),
                      A_SAND_MI = rep(45,4),
                      A_P_OL = c(5,10,20,40)),
    expected = c(0.04139369, 0.55495779, 0.98180692, 0.99998254),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_es(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                      A_CLAY_MI = c(5,10,20,40),
                      A_SAND_MI = c(5,10,20,40),
                      A_P_OL = c(5,10,20,40)),
    expected = c(0.04139369, 0.55495779, 0.91082393, 0.99961090),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_fi works", {
  expect_equal(
    osi_c_phosphor_fi(B_LU = '1110',
                       B_TEXTURE_USDA = 'Si',
                       A_P_AAA = 15,
                       A_C_OF = 35),
    expected = c(.761),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_fi(B_LU = c('5302','2180','9404','6305'), 
                       B_TEXTURE_USDA = rep('Si',4),
                      A_P_AAA = rep(15,4),
                       A_C_OF = c(10,20,100,300)),
    expected = c(0.8092366, 0.8407289, 0.279 ,NA),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_fi(B_LU = c('5302','2180','9404','6305'), 
                       B_TEXTURE_USDA = rep('Si',4),
                      A_P_AAA = c(8,15,45,90),
                       A_C_OF = c(10,20,100,300)),
    expected = c(0.4634595, 0.8407289, 0.8293157 ,NA),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_fr works", {
  expect_equal(
    osi_c_phosphor_fr(B_LU = '3301090400',
                      A_PH_WA = 6.5, 
                      A_P_OL = 12),
    expected = c(0.0373 ),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000'),
                      A_PH_WA = rep(6.5,4),
                      A_P_OL = c(6,12,24,48)),
    expected = c(0.0003364196, 0.9486657839, 0.6216014368, 0.9912144303),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_fr(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_PH_WA = rep(3.5,4),
                       A_P_OL = c(6,8,10,14)),
    expected = c(0.0003364196, 0.5122732270, 0.0114967018, 0.0901759553),
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
    expected = c(0.0658),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_hu(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       A_P_AL = rep(45,4),
                       A_CACO3_IF = rep(5,4),
                       A_CLAY_MI = rep(5,4),
                       A_SOM_LOI = rep(5,4)),
    expected = rep(0.06579038 ,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_hu(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(20,40,80,100),
                      A_CACO3_IF = c(0,1,2,4),
                      A_CLAY_MI = c(2,4,6,8),
                      A_SOM_LOI = c(2,4,6,8)),
    expected = c( 0.01632166, 0.34098191, 0.64693893, 0.86047659),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_ie works", {
  expect_equal(
    osi_c_phosphor_ie(B_LU = '3301000000',
                      A_P_OL = 5),
    expected = c(0.0001154996),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(1,2,4,8)*10),
    expected = c(0.0002589238, 0.0031302533, 0.1677345440, 0.9471579032),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(5,6,10,15)*5),
    expected = c(0.01089926, 0.03275658 ,0.43153785, 0.91445163),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_it works", {
  expect_equal(
    osi_c_phosphor_it(B_LU = '3301000000', 
                      B_TEXTURE_HYPRES ='C',
                      A_P_OL = 15),
    expected = c(0.96745),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_it(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      B_TEXTURE_HYPRES = rep('C',4),
                      A_P_OL = c(5,10,15,20)),
    expected = c( 0.4449934, 0.8462635, 0.9674500, 0.9935142),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_it(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      B_TEXTURE_HYPRES = rep('C',4),
                      A_P_OL = c(1,2,4,8)),
    expected = c(0.07513605, 0.14019271, 0.33396838, 0.72865308),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_lv works", {
  expect_equal(
    osi_c_phosphor_lv(B_LU = 'testcrop', 
                      B_TEXTURE_USDA = 'Cl',
                      A_P_DL = 15),
    expected = c(0.0747),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_lv(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      B_TEXTURE_USDA = c('Cl','ClLo','Lo','Sa'),
                      A_P_DL = rep(25,4)),
    expected = c( 0.4161053, 0.4161053, 0.5636625, 0.8550899),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_lv(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                       B_TEXTURE_USDA = rep('Cl',4),
                      A_P_DL = c(40,60,80,120)/2),
    expected = c( 0.2209768, 0.6013049, 0.8429243, 0.9809434),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_lt works", {
  expect_equal(
    osi_c_phosphor_lt(B_LU = 'testcrop', 
                       A_SOM_LOI = 6.5,
                       A_P_AL = 15),
    expected = c(0.0182467),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_lt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_SOM_LOI = c(4,4,8,8),
                      A_P_AL = c(25,40,60,80)),
    expected = c(0.2482182, 0.7542800, 0.9672558, 0.9960816),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_lt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_SOM_LOI = c(6,6,60,60),
                      A_P_AL = c(25,75,45,90)),
    expected = c( 0.2482182, 0.9933225, 0.5006325 ,0.9843323),
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
    expected = c(0.802),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_pl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_DL = c(20,40,60,80)),
    expected = c(0.0637611, 0.6938137, 0.9532240, 0.9937527),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_pl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_DL = c(15,35,45,55)),
    expected = c(0.01083167, 0.54502064, 0.80249086, 0.92344427),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_pt works", {
  expect_equal(
    osi_c_phosphor_pt(B_LU = '3301000000', 
                      A_P_OL = 45),
    expected = c(0.999),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_pt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(10,20,40,60)),
    expected = c( 0.3303376, 0.9485484, 0.9998811, 0.9999997),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_pt(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_OL = c(5,15,35,45)),
    expected = c(0.006837443, 0.784817359, 0.999454338, 0.999974111),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_ro works", {
  expect_equal(
    osi_c_phosphor_ro(B_LU = 'testcrop1',
                      A_P_AL = 5),
    expected = c(0.2051),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_ro(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(1,2,4,8)),
    expected = c(0.06591730, 0.09278344, 0.16292502, 0.34926133),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_ro(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(5,6,10,15)),
    expected = c(0.2051349, 0.2509624, 0.4494902, 0.6691964),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_se works", {
  expect_equal(
    osi_c_phosphor_se(B_LU = '3301000000', 
                      A_P_AL = 15),
    expected = c(0.000297 ),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(6,12,14,20)*5),
    expected = c(0.5435624, 1.0000000, 0.9994477 ,0.9999971),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(6,7,8,9)+25),
    expected = c(0.5993673, 0.9999991, 0.6970982, 0.7386580),
    tolerance = 0.01
  )
})

test_that("osi_c_phosphor_sk works", {
  expect_equal(
    osi_c_phosphor_sk(A_P_M3 = 85,
                       B_TEXTURE_HYPRES = 'C',
                       B_LU = '3301000000'),
    expected = c(0.615),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_sk(A_P_M3 = rep(65,4),
                       B_TEXTURE_HYPRES = c('M','C','F','VF'),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =c(0.4194907, 0.2054070 ,0.8784174, 0.8784174),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_sk(A_P_M3 = c(30,60,90,120),
                       B_TEXTURE_HYPRES = rep('F',4),
                       B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.03621508, 0.81307913, 0.98760778, 0.99925045),
    tolerance = 0.01
  )
})


test_that("osi_c_phosphor_sl works", {
  expect_equal(
    osi_c_phosphor_sl(B_LU = '3301000000', 
                      A_P_AL = 35),
    expected = c(0.35),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_sl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(20,30,40,50)),
    expected = c( 0.03751894, 0.21503478, 0.48857581 ,0.71671669),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_phosphor_sl(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                      A_P_AL = c(30,30,60,60)),
    expected = c(0.2150348, 0.2150348, 0.8566469, 0.8566469),
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

