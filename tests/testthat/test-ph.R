test_that("osi_c_ph works", {
  expect_equal(
    osi_c_ph(B_LU = '265', 
             B_SOILTYPE_AGR = 'dekzand', 
             A_CLAY_MI= 25, 
             A_SAND_MI = 15,
             A_SOM_LOI = 4.5, 
             A_C_OF = NA_real_,
             A_CA_CO_PO = NA_real_, 
             A_MG_CO_PO = NA_real_,
             A_K_CO_PO = NA_real_, 
             A_NA_CO_PO = NA_real_,
             A_PH_WA = NA_real_, 
             A_PH_CC= 4.9, 
             A_PH_KCL= NA_real_,
             B_COUNTRY = 'NL'),
    expected = c(0.546),
    tolerance = 0.01
  )
  
  expect_equal(
     osi_c_ph(B_LU = c('265','AVH',rep('3301000000',3)), 
              B_SOILTYPE_AGR = c('dekzand',rep(NA,4)),
              A_CLAY_MI = rep(15,5),
              A_SAND_MI = rep(45,5),
              A_SOM_LOI = rep(4.5,5),
              A_C_OF = NA_real_, 
              A_CA_CO_PO = NA_real_, 
              A_MG_CO_PO = NA_real_,
              A_K_CO_PO = NA_real_, 
              A_NA_CO_PO = NA_real_,
              A_PH_CC = rep(6.5,5), 
              A_PH_WA = NA_real_, 
              A_PH_KCL= NA_real_,
              B_COUNTRY = c('NL','FR','SE','ES','SK')),
    expected = c(0.999,0.9807,0.9999,NA,NA),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_c_ph(B_LU = c('265','AVH',rep('3301000000',3)), 
            B_SOILTYPE_AGR = c('dekzand',rep(NA,4)),
            A_CLAY_MI = rep(15,5),
            A_SAND_MI = rep(45,5),
            A_SOM_LOI = rep(4.5,5),
            A_C_OF = NA_real_, 
            A_CA_CO_PO = c(60,65,70,75,80), 
            A_MG_CO_PO = c(10,5,3,2,2),
            A_K_CO_PO = c(10,5,3,2,2), 
            A_NA_CO_PO = c(5,5,3,2,2),
            A_PH_CC = c(4.5,4.5,5.5,6.5,7), 
            A_PH_WA = NA_real_, 
            A_PH_KCL= NA_real_,
            B_COUNTRY = c('NL','FR','SE','ES','SK')),
    expected = c(0.0249,0.3727,0.984,NA,NA),
    tolerance = 0.01
  )
  
  
  
})

test_that("osi_c_ph_at works", {
  expect_equal(
    osi_c_ph_at(A_PH_CC = 4.7,
                B_TEXTURE_HYPRES = 'C',
                B_LU = '3301000000'),
    expected = c(0.514),
    tolerance = 0.01
  )
  expect_equal(
     osi_c_ph_at(A_PH_CC = rep(4.7,4),
                 B_TEXTURE_HYPRES = rep('C',4),
                 B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = rep(0.514,4),
    tolerance = 0.01
  )
  expect_equal(
     osi_c_ph_at(A_PH_CC = c(4.5,5.5,6.5,7.5),
                 B_TEXTURE_HYPRES = rep('C',4),
                 B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.0474,0.9998,1,1),
    tolerance = 0.01
  )
})


test_that("osi_c_ph_be works", {
  expect_equal(
    osi_c_ph_be(A_PH_KCL = 4.7,
                B_TEXTURE_BE = 'U',
                B_LU = '3301000000'),
    expected = c(0.0309),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_be(A_PH_KCL = rep(4.7,4),
                B_TEXTURE_BE = c('S','P','E','A'),
                B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.582,0.1786,0.0309,0.0629),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_be(A_PH_KCL = c(4.5,5.5,6.5,7.5),
                B_TEXTURE_BE = rep('P',4),
                B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.1196,0.587,0.937,0.9936),
    tolerance = 0.01
  )
})

test_that("osi_c_ph_ch works", {
  expect_equal(
    osi_c_ph_ch(A_PH_WA = 4.7,
                A_CLAY_MI = 7.5,
                A_CA_CO_PO = NA_real_, 
                A_MG_CO_PO = NA_real_,
                A_K_CO_PO = NA_real_, 
                A_NA_CO_PO = NA_real_,
                B_LU = '3301000000'),
    expected = c(0.01597),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_ch(A_PH_WA = rep(4.7,5),
                A_CLAY_MI = rep(7.5,5),
                A_CA_CO_PO = c(60,65,70,75,80), 
                A_MG_CO_PO = c(10,5,3,2,2),
                A_K_CO_PO = c(10,5,3,2,2), 
                A_NA_CO_PO = c(5,5,3,2,2),
                B_LU = c('testcrop1','3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.985,0.9733,0.9699,0.9764,0.987),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_ch(A_PH_WA = c(3,3.5,4,4.5,5),
                A_CLAY_MI = rep(7.5,5),
                A_CA_CO_PO = c(60,65,70,75,80)/2, 
                A_MG_CO_PO = c(10,5,3,2,2),
                A_K_CO_PO = c(10,5,3,2,2), 
                A_NA_CO_PO = c(5,5,3,2,2),
                B_LU = c('testcrop1','3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.564,0.240,0.1125,0.098,0.1804),
    tolerance = 0.01
  )
})

test_that("osi_c_ph_pt works", {
  expect_equal(
    osi_c_ph_pt(A_PH_WA = 4.7,
                A_CEC_CO = 7.5,
                A_CA_CO_PO = 80, 
                A_MG_CO_PO = 8,
                A_K_CO_PO = 5, 
                A_NA_CO_PO = 2,
                B_LU = '3301000000'),
    expected = c(0.07667),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_pt(A_PH_WA = rep(4.7,5),
                A_CEC_CO = rep(7.5,5),
                A_CA_CO_PO = c(60,65,70,75,80), 
                A_MG_CO_PO = c(10,5,3,2,2),
                A_K_CO_PO = c(10,5,3,2,2), 
                A_NA_CO_PO = c(5,5,3,2,2),
                B_LU = c('testcrop1','3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.08727666, 0.07655451, 0.07199826, 0.06981598, 0.06986265),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_pt(A_PH_WA = c(3,3.5,4,4.5,5),
                A_CEC_CO = rep(7.5,5),
                A_CA_CO_PO = c(60,65,70,75,80)/2, 
                A_MG_CO_PO = c(10,5,3,2,2),
                A_K_CO_PO = c(10,5,3,2,2), 
                A_NA_CO_PO = c(5,5,3,2,2),
                B_LU = c('testcrop1','3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.05761967, 0.04871134, 0.04743586, 0.06118419, 0.07792056),
    tolerance = 0.01
  )
})
test_that("osi_c_ph_de works", {
  expect_equal(
    osi_c_ph_de(B_LU = '3301000000', 
                A_CLAY_MI=15,
                A_SAND_MI = 45,
                A_C_OF = 25,
                A_SOM_LOI = 25 * 2 /10,
                A_PH_CC = 5.5),
    expected = c(0.496),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_de(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                A_CLAY_MI = c(5,15,25,45),
                A_SAND_MI = rep(45,4),
                A_C_OF = rep(25,4),
                A_SOM_LOI = rep(25,4) * 2 /10,
                A_PH_CC = rep(5.5,4)),
    expected = c(0.496,0.496,0.707,0.707),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_de(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                A_CLAY_MI = rep(25,4),
                A_SAND_MI = rep(45,4),
                A_C_OF = rep(25,4),
                A_SOM_LOI = rep(25,4) * 2 /10,
                A_PH_CC = c(4.5,5,5.5,6)),
    expected = c(0.0007,0.20599,0.707,0.9268),
    tolerance = 0.01
  )
})

test_that("osi_c_ph_fr works", {
  expect_equal(
    osi_c_ph_fr(A_PH_WA = 4.7,
                B_TEXTURE_GEPPA = 'SS',
                B_LU = '3301000000'),
    expected = c(0.232),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_fr(A_PH_WA = rep(4.7,4),
                B_TEXTURE_GEPPA = c('Sl','La','SS','L'),
                B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.232,0.0576,0.23196,0.05765),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_fr(A_PH_WA = c(4.5,5.5,6.5,7.5),
                B_TEXTURE_GEPPA = rep('SS',4),
                B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.1197,0.8803,0.9975,0.99995),
    tolerance = 0.01
  )
})


test_that("osi_c_ph_fi works", {
  expect_equal(
    osi_c_ph_fi(B_LU = '1110',
               B_TEXTURE_USDA = 'Si',
               A_PH_WA = 4.5,
               A_C_OF = 35),
    expected = c(0.0762),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_fi(B_LU = c('5302','2180','9404','6305'), 
                 B_TEXTURE_USDA = rep('Si',4),
                A_PH_WA = rep(4.5,4),
                 A_C_OF = c(10,20,100,300)),
    expected = rep(0.07621,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_fi(B_LU = c('5302','2180','9404','6305'), 
               B_TEXTURE_USDA = rep('Si',4),
               A_PH_WA = c(4.5,5,5.5,6),
               A_C_OF = c(10,20,100,300)),
    expected = c(0.0762,0.2231,0.5,0.777),
    tolerance = 0.01
  )
})


test_that("osi_c_ph_ie works", {
  expect_equal(
    osi_c_ph_ie(B_LU = '3301000000', 
                 A_SOM_LOI = 3.5,
                A_PH_WA = 5.5),
    expected = c(0.336),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
               A_SOM_LOI = c(1,2,4,8),
               A_PH_WA = rep(5.5,4)),
    expected = rep(0.3364289 ,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_ie(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                A_SOM_LOI = c(1,2,4,8),
                A_PH_WA = c(5,5.5,5.5,6)),
    expected = c(0.0251,0.336,0.336,0.7287),
    tolerance = 0.01
  )
})


test_that("osi_c_ph_nl works", {
  expect_equal(
    osi_c_ph_nl(ID = 1,
                B_LU = '265', 
                B_SOILTYPE_AGR = 'dekzand',
                A_SOM_LOI = 3.5,
                A_CLAY_MI = 8.5,
                A_PH_CC = 4.8),
    expected = c(0.296),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_nl(ID = rep(1,4),
                B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                B_SOILTYPE_AGR = rep('dekzand',4),
                A_SOM_LOI = rep(3.5,4),
                A_CLAY_MI = rep(8.5,4),
                A_PH_CC = rep(5.4,4)),
    expected = rep(0.8232,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_nl(ID = 1:4,
                B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                B_SOILTYPE_AGR = c('rivierklei', 'zeeklei','veen','loess'),
                A_SOM_LOI = rep(3.5,4),
                A_CLAY_MI = rep(8.5,4),
                A_PH_CC = c(4.8,5,5.2,5.5)),
    expected = c(0.0112,0.0652,0.3178,0.9551),
    tolerance = 0.01
  )
})

test_that("osi_c_ph_se works", {
  expect_equal(
    osi_c_ph_se(B_LU = '3301000000', 
                A_SOM_LOI = 3.5,
                A_CLAY_MI = 15,
                A_PH_WA = 6.1),
    expected = c(0.87997),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                A_SOM_LOI = c(1,2,4,8),
                A_CLAY_MI = c(5,10,15,20),
                A_PH_WA = rep(5.8,4)),
    expected = c(0.8348,0.3473,0.3474,0.99889),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_se(B_LU = c('3301000000','3301010901','3301061299','3304990000'), 
                A_SOM_LOI = c(1,2,4,8),
                A_CLAY_MI = c(5,5,5,5),
                A_PH_WA = c(5.5,5.8,5.9,6)),
    expected = c(0.172,0.83,0.926,0.9996),
    tolerance = 0.01
  )
})


test_that("osi_c_ph_uk works", {
  expect_equal(
    osi_c_ph_uk(A_PH_WA = 5.4,
               A_SOM_LOI = 4.5,
               B_LU = 'testcrop1'),
    expected = c(6.597736e-07),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_uk(A_PH_WA = rep(6,4),
               A_SOM_LOI = c(4,8,12,20),
               B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected =rep(0.2585293 ,4),
    tolerance = 0.01
  )
  expect_equal(
    osi_c_ph_uk(A_PH_WA = c(5.8,6.5,7.5,6),
               A_SOM_LOI = c(1,8,12,12),
               B_LU = c('3301000000','3301010901','3301061299','3304990000')),
    expected = c(0.0165,0.963,0.9999,0.2585),
    tolerance = 0.01
  )
})
