test_that("osi_get_TEXTURE_USDA works", {
  expect_equal(
    osi_get_TEXTURE_USDA(A_CLAY_MI = 5, 
                         A_SAND_MI = 55, 
                         A_SILT_MI = 40),
    expected = c('SaLo'),
    tolerance = 0.01
  )
  expect_equal(
    osi_get_TEXTURE_USDA(A_CLAY_MI = c(5,15,25,50,0,0,85), 
                         A_SAND_MI = c(25,25,25,25,85,15,15), 
                         A_SILT_MI = c(70,60,50,25,15,85,0)),
    expected = c('SiLo','SiLo','Lo','Cl','SaLo','Si','Cl'),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_get_TEXTURE_USDA(A_CLAY_MI = c(5,15,25,50,0,0,85), 
                         A_SAND_MI = c(25,25,25,25,85,15,15), 
                         A_SILT_MI = c(70,60,50,25,15,85,0),
                         type='name'),
    expected = c('silty loam','silty loam','loam','clay','sandy loam','silt','clay'),
    tolerance = 0.01
  )
  
})

test_that("osi_get_TEXTURE_HYPRES works", {
  expect_equal(
    osi_get_TEXTURE_HYPRES(A_CLAY_MI = 5, 
                         A_SAND_MI = 55, 
                         A_SILT_MI = 40),
    expected = c('M'),
    tolerance = 0.01
  )
  expect_equal(
    osi_get_TEXTURE_HYPRES(A_CLAY_MI = c(5,15,25,50,0,0,85), 
                         A_SAND_MI = c(25,25,25,25,85,15,15), 
                         A_SILT_MI = c(70,60,50,25,15,85,0)),
    expected = c('M','M','M','F','C','MF','VF'),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_get_TEXTURE_HYPRES(A_CLAY_MI = c(5,15,25,50,0,0,85), 
                         A_SAND_MI = c(25,25,25,25,85,15,15), 
                         A_SILT_MI = c(70,60,50,25,15,85,0),
                         type='name'),
    expected = c('medium','medium','medium','fine','course','medium fine','very fine'),
    tolerance = 0.01
  )
  
})

test_that("osi_get_TEXTURE_GEPPA works", {
  expect_equal(
    osi_get_TEXTURE_GEPPA(A_CLAY_MI = 5, 
                         A_SAND_MI = 55, 
                         A_SILT_MI = 40),
    expected = c('Sl'),
    tolerance = 0.01
  )
  expect_equal(
    osi_get_TEXTURE_GEPPA(A_CLAY_MI = c(5,15,25,50,0,0,85), 
                         A_SAND_MI = c(25,25,25,25,85,15,15), 
                         A_SILT_MI = c(70,60,50,25,15,85,0)),
    expected = c('Ls','Lsa','LAS','A','SS','LL','AA'),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_get_TEXTURE_GEPPA(A_CLAY_MI = c(5,15,25,50,0,0,85), 
                         A_SAND_MI = c(25,25,25,25,85,15,15), 
                         A_SILT_MI = c(70,60,50,25,15,85,0),
                         type='name'),
    expected = c("limon sableux", "limon sablo-argileux", "limon argilo-sableux", "argileux",            
                 "sable", "limon pur", "argile lourde"),
    tolerance = 0.01
  )
  
})

test_that("osi_get_TEXTURE_BE works", {
  expect_equal(
    osi_get_TEXTURE_BE(A_CLAY_MI = 5, 
                          A_SAND_MI = 55, 
                          A_SILT_MI = 40),
    expected = c('P'),
    tolerance = 0.01
  )
  expect_equal(
    osi_get_TEXTURE_BE(A_CLAY_MI = c(5,15,25,50,0,0,85), 
                          A_SAND_MI = c(25,25,25,25,85,15,15), 
                          A_SILT_MI = c(70,60,50,25,15,85,0)),
    expected = c('L','L','E','E','Z','A','U'),
    tolerance = 0.01
  )
  
  expect_equal(
    osi_get_TEXTURE_BE(A_CLAY_MI = c(5,15,25,50,0,0,85), 
                          A_SAND_MI = c(25,25,25,25,85,15,15), 
                          A_SILT_MI = c(70,60,50,25,15,85,0),
                          type='name'),
    expected = c("zandleem","zandleem","klei","klei",
                 "zand","leem","zware klei"),
    tolerance = 0.01
  )
  
})