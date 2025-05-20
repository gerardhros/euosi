# OSI helper functions

#' Select the correct boundary values (min and max) for a given parameter
#' 
#' This function selects the correct boundary values (min and max) or possible options for a given parameter
#' 
#' @param parm (numeric) The osi parameter
#' 
#' @import data.table
#' 
#' @return 
#' The min and max for the selected numeric parameter and options for categorial parameter
#' 
#' @export
checkvar <- function(parm) {
  
  # load internal table
  
  return(NULL)
}


#' Select the correct boundary values (min and max) for a given parameter
#' 
#' This function gives a value a given weight depending on distance to target
#' 
#' @param value (numeric) The a OSI index value varying between 0 and 1.
#' 
#' @import data.table
#' 
#' @return 
#' a weighing factor that can be used in calculated a weighted mean
#' 
#' @export
cf_ind_importance <- function(x) {
  
  y <- 1/(x + 0.2)
  return(y)
}

#' Water retention curve
#' 
#' This function compute water content at given pressure head, using Van Genuchten water retention curve
#' 
#' @param head (numeric)  suction pressure ([L] or cm of water)
#' @param thetaR (numeric) residual water content (cm3/cm3)
#' @param thetaS (numeric) saturated water content (cm3/cm3)
#' @param alfa (numeric)  related to the inverse of the air entry suction, alfa > 0 (1/cm)
#' @param n (numeric)  a measure of the pore-size distribution, n>1, dimensionless
#' 
#' @import OBIC
#' 
#' @return theta (numeric) water content (cm3/cm3)
#' 
#' @examples 
#' pF_curve(head = 2.2, thetaR = 0.01, thetaS = 0.35, alfa = 0.3,n = 1.6)
#' pF_curve(head = 4.2, thetaR = 0.01, thetaS = 0.35, alfa = 0.3,n = 1.6)
#' 
#' @return 
#' The moisture content of a soil given a certain pressure head. A numeric value.
#' 
#' @export 
pF_curve <- function(head, thetaR, thetaS, alfa, n){
  
  theta <- thetaR+(thetaS-thetaR)/(1+abs(alfa*head)^n)^(1-1/n)
  
  return(theta)
}


#' Estimate water retention curve parameters based on Wosten 1999
#'
#' This function estimates water retention curve parameters using Pedo transfer function of Wosten (1999) based on HYPRES
#' 
#' 
#' @param Pklei (numeric) The clay content of the soil (\%) within soil mineral part. Pklei > 0
#' @param Psilt (numeric) The silt content of the soil (\%) within soil mineral part. Psilt > 0 
#' @param Psom (numeric) The organic matter content of the soil (\%). Psom > 0
#' @param Bovengrond (boolean) whether topsoil (1) or not (0)
#' 
#' @examples 
#' pFpara_ptf_Wosten1999(Pklei = 25, Psilt = 15, Psom = 4.5, Bovengrond = 1)
#' pFpara_ptf_Wosten1999(Pklei = 45, Psilt = 3, Psom = 4.5, Bovengrond = 1)
#' 
#' @return a table with the following columns:
#' 
#' Dichtheid (numeric) soil bulk density (g/cm3)
#' ThetaR (numeric) residual water content (cm3/cm3)
#' ThetaS (numeric) saturated water content (cm3/cm3)
#' alfa (numeric)  related to the inverse of the air entry suction, alfa > 0 (1/cm) 
#' n (numeric)  a measure of the pore-size distribution, n>1, dimensionless
#' ksat (numeric) saturated hydraulic conductivity (cm/d)
#' 
#' @references Wösten, J.H.M , Lilly, A., Nemes, A., Le Bas, C. (1999) Development and use of a database of hydraulic properties of European soils. Geoderma 90 (3-4): 169-185.
#' 
#' @export
pFpara_ptf_Wosten1999 <- function(Pklei, Psilt, Psom, Bovengrond){
  
  Dichtheid = ThetaR = ThetaS = alfa = n = ksat = id = Dichtheid_zand = Dichtheid_klei = Pklei_fr = NULL
  
  # Check input
  arg.length <- max(length(Pklei), length(Psilt), length(Psom), length(Bovengrond))
  checkmate::assert_numeric(Pklei, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(Psilt, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(Psom, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(Bovengrond, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_subset(Bovengrond, choices = c(0, 1), empty.ok = FALSE)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    Pklei = Pklei,
    Psilt = Psilt,
    Psom = Psom,
    Bovengrond = Bovengrond,
    Dichtheid = NA_real_,
    ThetaR = NA_real_,
    ThetaS = NA_real_, 
    alfa = NA_real_, 
    n = NA_real_
  )
  
  # Estimate of bulk density (source: Handboek Bodem en Bemesting )
  # 2 discrete PTF is combined based on clay content, so that it becomes 1 continuous function.
  #dt[Pklei<=25, Dichtheid :=  1/(0.02525*Psom+0.6541)]
  #dt[Pklei>25, Dichtheid :=  (0.00000067)*Psom^4-(0.00007792)*Psom^3+0.00314712*Psom^2-0.06039523*Psom+1.33932206]
  dt[, Dichtheid_zand :=  1/(0.02525*Psom+0.6541)]
  dt[, Dichtheid_klei :=  (0.00000067)*Psom^4-(0.00007792)*Psom^3+0.00314712*Psom^2-0.06039523*Psom+1.33932206]
  dt[, Pklei_fr := pmin(1, Pklei/25)]
  dt[, Dichtheid := Pklei_fr * Dichtheid_klei + (1-Pklei_fr) * Dichtheid_zand]
  
  # Continue pedotransferfunctie Wosten 1999(in PFT manual), see Wosten 2001 (based on HYPRES dataset)
  dt[, ThetaR    := 0.01]
  dt[, ThetaS    := 0.7919+0.001691*Pklei-0.29619*Dichtheid-0.000001491*Psilt^2+0.0000821*Psom^2+
       0.02427/Pklei+0.01113/Psilt+0.01472*log(Psilt)-0.0000733*Psom*Pklei-0.000619*Dichtheid*Pklei-
       0.001183*Dichtheid*Psom-0.0001664*Bovengrond*Psilt]
  dt[,  alfa      := exp(-14.96+0.03135*Pklei+0.0351*Psilt+0.646*Psom+15.29*Dichtheid-0.192*Bovengrond-
                           4.671*Dichtheid^2-0.000781*Pklei^2-0.00687*Psom^2+0.0449/Psom+0.0663*log(Psilt)+
                           0.1482*log(Psom)-0.04546*Dichtheid*Psilt-0.4852*Dichtheid*Psom+0.00673*Bovengrond*Pklei)]
  dt[, n         := 1 + exp(-25.23-0.02195*Pklei+0.0074*Psilt-0.1940*Psom+45.5*Dichtheid-7.24*Dichtheid^2+
                              0.0003658*Pklei^2+0.002885*Psom^2-12.81/Dichtheid-0.1524/Psilt-0.01958/Psom-
                              0.2876*log(Psilt)-0.0709*log(Psom)-44.6*log(Dichtheid)-0.02264*Dichtheid*Pklei+
                              0.0896*Dichtheid*Psom+0.00718*Bovengrond*Pklei)]
  dt[, ksat      := 7.755 + 0.0352 * Psilt + 0.93 * Bovengrond - 0.967 * Dichtheid^2 - 0.000484 * Pklei^2 -
       0.000322 * Psilt^2 + 0.001 / Psilt - 0.0748 / Psom - 0.643 * log(Psilt) -
       0.01398 * Dichtheid * Pklei - 0.1673 * Dichtheid * Psom + 0.2986 * Bovengrond * Pklei -
       0.03305 * Bovengrond * Psilt]
  dt[Psom>25 & ksat < 0, ksat := 5] # Copied from the old version of waterretention.R. THe source information need to be verified.
  
  # order dt
  setorder(dt, id)
  
  return(dt[, list(Dichtheid, ThetaR, ThetaS, alfa, n, ksat)])
}


#' Estimate water retention curve parameters based on Wosten 2001
#'
#' This function estimates water retention curve parameters using Pedo transfer function of Wosten (2001)
#' 
#' @param Pklei (numeric) The clay (<2um) content of the soil (\%) 
#' @param Pleem (numeric) The loam (<50um) content of the soil (\%) Pleem > 0 
#' @param Psom (numeric) The organic matter content of the soil (\%) Psom > 0
#' @param M50 (numeric)size of sand fraction (um)
#' @param Bovengrond (boolean) whether topsoil (1) or not (0)
#' 
#' @references Wösten, J. H. M., Veerman, G. ., de Groot, W. J., & Stolte, J. (2001). Waterretentie en doorlatendheidskarakteristieken van boven- en ondergronden in Nederland: de Staringreeks. Alterra Rapport, 153, 86. https://doi.org/153
#'
#' @examples 
#' pFpara_ptf_Wosten2001(Pklei = 25, Pleem = 15, Psom = 4.5,M50 = 150, Bovengrond = 1)
#' pFpara_ptf_Wosten2001(Pklei = 45, Pleem = 3, Psom = 4.5,M50 = 150,Bovengrond = 1)
#' 
#' @return a table with the following columns:
#' Dichtheid (numeric) soil bulk density (g/cm3)
#' ThetaR (numeric) residual water content (cm3/cm3)
#' ThetaS (numeric) saturated water content (cm3/cm3)
#' alfa (numeric)  related to the inverse of the air entry suction, alfa > 0 (1/cm) 
#' n (numeric)  a measure of the pore-size distribution, n>1, dimensionless
#' ksat (numeric) saturated hydraulic conductivity (cm/d)
#' l (numeric) dimension parameter
#' 
#' @export 
pFpara_ptf_Wosten2001 <- function(Pklei, Pleem, Psom, M50, Bovengrond){
  
  Dichtheid = ThetaR = ThetaS = Ksat = alfa = l = n =  id = NULL
  
  # Check input
  arg.length <- max(length(Pklei), length(Pleem), length(Psom), length(M50), length(Bovengrond))
  checkmate::assert_numeric(Pklei, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(Pleem, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(Psom, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(M50, lower = 0, upper = 2000, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(Bovengrond, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_subset(Bovengrond, choices = c(0, 1), empty.ok = FALSE)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    Pklei = Pklei,
    Pleem = Pleem,
    Psom = Psom,
    M50 = M50,
    Bovengrond = Bovengrond,
    Dichtheid = NA_real_,
    ThetaR = NA_real_,
    ThetaS = NA_real_, 
    Ksat = NA_real_, 
    alfa = NA_real_, 
    l = NA_real_, 
    n = NA_real_
  )
  
  # sandgronden
  dt[Pklei<8, Dichtheid := 1/(-7.58+0.01791*Psom+0.0326*Bovengrond-0.00338*M50+0.00003937*Pleem^2+
                                157.7*(1/M50)+1.522*log(M50))]
  dt[Pklei<8, ThetaR    := 0.01]
  dt[Pklei<8, ThetaS    := -35.7-0.1843*Dichtheid - 0.03576*M50+0.0000261*M50^2-0.0564*(1/Pleem)+
       0.008*(1/Psom)+496*(1/M50)+0.02244*log(Psom)+7.56*log(M50)]
  dt[Pklei<8, Ksat      := exp(45.8-14.34*Dichtheid+0.001481*Pleem^2-27.5*(1/Dichtheid)-
                                 0.891*log(Pleem)-0.34*log(Psom))]
  dt[Pklei<8, alfa      := exp(13.66-5.91*Dichtheid-0.172*Bovengrond+0.003248*M50-
                                 11.89*(1/Dichtheid)-2.121*(1/Pleem)-0.3742*log(Pleem))]
  dt[Pklei<8, l         := (2*exp(-76.4-0.097*Pleem+59.6*Dichtheid+0.0332*M50-13.45*Dichtheid^2+
                                    0.001127*Pleem^2+0.00431*Psom^2-0.0000399*M50^2+40.8*(1/Dichtheid)+
                                    2.364*(1/Pleem)+1.014*log(Pleem))-2)/(1+
                                                                            exp(-76.4-0.097*Pleem+59.6*Dichtheid+0.0332*M50-13.45*Dichtheid^2+
                                                                                  0.001127*Pleem^2+0.00431*Psom^2-0.0000399*M50^2+40.8*(1/Dichtheid)+
                                                                                  2.364*(1/Pleem)+1.014*log(Pleem)))]
  dt[Pklei<8, n         := exp(-1.057+0.1003*Psom+1.119*Dichtheid+0.000764*Pleem^2 -
                                 0.1397*(1/Psom)-57.2*(1/M50)-0.557*log(Psom)-0.02997*Dichtheid*Pleem)+1]
  
  # klei en zavelgronden
  dt[Pklei>=8, Dichtheid := 1/(0.6117+0.003601*Pklei+0.002172*Psom^2+0.01715*log(Psom))]
  dt[Pklei>=8, ThetaR    := 0.01]
  dt[Pklei>=8, ThetaS    := 0.6311+0.003383*Pklei-0.09699*Dichtheid^2-0.00204*Dichtheid*Pklei]
  dt[Pklei>=8, Ksat      := exp(-42.6+8.71*Psom+61.9*Dichtheid-20.79*Dichtheid^2-
                                  0.2107*Psom^2-0.01622*Pklei*Psom-5.382*Dichtheid*Psom)]
  dt[Pklei>=8,  alfa     := exp(-19.13+0.812*Psom+23.4*Dichtheid-8.16*Dichtheid^2+
                                  0.423*(1/Psom)+2.388*log(Psom)-1.338*Dichtheid*Psom)]
  dt[Pklei>=8,  l        := (exp(0.102+0.0222*Pklei-0.043*Dichtheid*Pklei)-1)*10/(1+
                                                                                    exp(0.102+0.0222*Pklei-0.043*Dichtheid*Pklei))]
  dt[Pklei>=8, n         := exp(-0.235+0.972*(1/Dichtheid)-0.7743*log(Pklei)-0.3154*log(Psom)+
                                  0.0678*Dichtheid*Psom)+1 ]
  
  # order dt
  setorder(dt, id)
  
  return(dt[, list(Dichtheid, ThetaR,  ThetaS, alfa, n, ksat = Ksat, l)])
}

#' Parameter estimation based on class of Staringreeks (Tabel 3, Wosten 2001)
#' 
#' @param Pklei (numeric) The clay (<2um) content of the soil (\%) 
#' @param Pleem (numeric) The loam (<50um) content of the soil (\%) Pleem > 0 
#' @param Psom (numeric) The organic matter content of the soil (\%) Psom > 0
#' @param M50 (numeric)size of  sand fraction (um)
#'
#' @import OBIC
#' 
#' @examples 
#' pFpara_class(Pklei = 25, Pleem = 15, Psom = 4.5,M50 = 150)
#' pFpara_class(Pklei = 45, Pleem = 3, Psom = 4.5,M50 = 150)
#' 
#' @return a table with the following columns:
#' ThetaR (numeric) residual water content (cm3/cm3)
#' ThetaS (numeric) saturated water content (cm3/cm3)
#' alfa (numeric)  related to the inverse of the air entry suction, alfa > 0 (1/cm) 
#' n (numeric)  a measure of the pore-size distribution, n>1, dimensionless
#' ksat (numeric) saturated hydraulic conductivity (cm/d)
#' 
#' @export
pFpara_class <- function(Pklei, Pleem, Psom, M50){
  
  CF1 = CF2 = SEL1 = id = thres = thsat = alpha = n = Ks = NULL
  
  bouwsteen_tb <- as.data.table(OBIC::bouwsteen_tb)
  
  # Check input
  arg.length <- max(length(Pklei), length(Pleem), length(Psom), length(M50))
  checkmate::assert_numeric(Pklei, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(Pleem, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(Psom, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(M50, lower = 0, upper = 2000, any.missing = FALSE, min.len = 1)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    Pklei = Pklei,
    Pleem = Pleem,
    Psom = Psom,
    M50 = M50
  )
  
  dt[Pklei <= 8, CF1 := 0]
  dt[Pklei > 8, CF1 := 1]
  
  dt[Psom > 15, CF2 := 1]
  dt[Psom <= 15, CF2 := 0]
  
  # comment YF: B6 is missing: from the source table the definition is not clear
  dt[, SEL1 := "B20"]
  dt[CF1==0&CF2==0&Pleem>=00&Pleem<10 &M50<210, SEL1 := "B1"]
  dt[CF1==0&CF2==0&Pleem>=10&Pleem<18 &M50<210, SEL1 := "B2"]
  dt[CF1==0&CF2==0&Pleem>=18&Pleem<33 &M50<210, SEL1 := "B3"]
  dt[CF1==0&CF2==0&Pleem>=33&Pleem<50 &M50<210, SEL1 := "B4"]
  dt[CF1==0&CF2==0&Pleem<=50&M50>210&M50<=2000, SEL1 := "B5"]
  dt[CF1==1&CF2==0&Pklei>=8  &Pklei<12, SEL1 := "B7"]
  dt[CF1==1&CF2==0&Pklei>=12 &Pklei<18, SEL1 := "B8"]
  dt[CF1==1&CF2==0&Pklei>=18 &Pklei<25, SEL1 := "B9"]
  dt[CF1==1&CF2==0&Pklei>=25 &Pklei<35, SEL1 := "B10"]
  dt[CF1==1&CF2==0&Pklei>=35 &Pklei<50, SEL1 := "B11"]
  dt[CF1==1&CF2==0&Pklei>=50 &Pklei<=100, SEL1 := "B12"]
  dt[CF1==0&CF2==0&Pleem>=50 &Pleem<85, SEL1 := "B13"]
  dt[CF1==0&CF2==0&Pleem>=85 &Pleem<=100, SEL1 := "B14"]
  dt[CF1==0&CF2==1&Psom>=15  &Psom<25, SEL1 := "B15"]
  dt[CF1==0&CF2==1&Psom>=25  &Psom<=100, SEL1 := "B16"]
  dt[CF1==1&CF2==1&Psom>=16  &Psom<35, SEL1 := "B17"]
  dt[CF1==1&CF2==1&Psom>=35  &Psom<=70, SEL1 := "B18"]
  
  # merge table
  dt <- merge(dt, bouwsteen_tb, by.x = "SEL1", by.y = "bouwsteen", all.x = T,all.y = F)
  
  # order dt
  setorder(dt, id)
  
  return(dt[, list(ThetaR = thres, ThetaS = thsat, alfa = alpha, n, ksat = Ks)])
}

#' Estimate soil texture according to USDA classification
#' 
#' @param A_CLAY_MI (numeric) Clay content (\%)
#' @param A_SILT_MI (numeric) Silt content (\%)
#' @param A_SAND_MI (numeric) Silt content (\%)
#' @param type (character) return HYPRES classification names or codes (options: 'name' or 'code')
#' 
#' @return 
#' Texture classifcation according to the USDA classification system
#' 
#' @export 
osi_get_TEXTURE_USDA <- function(A_CLAY_MI, A_SILT_MI, A_SAND_MI, type = 'code'){
  
  # check inputs
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, any.missing = FALSE)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, any.missing = FALSE)
  
  # set internal copies
  dt <- data.table(cl = A_CLAY_MI,
                   sa = A_SAND_MI,
                   si = A_SILT_MI,
                   tname = NA_character_,
                   tcode = NA_character_)
  
  # derive soil texture USDA classfication
  cols <- c('tcode','tname')
  
   dt[cl>40 & sa <=45 & si<=40, c(cols) := list('Cl','clay')]
   dt[is.na(tcode) & cl>40 & sa <=20 & si<=60 & si>40 , c(cols) := list('SiCL','silty clay') ]
   dt[is.na(tcode) & cl>35 & cl <=55 & sa<=65 & sa>45 & si<20, c(cols) := list('SaCl','sandy clay') ]
   dt[is.na(tcode) & cl>20 & cl <=35 & sa>45 & sa<80 & si <=27.5, c(cols) := list('SaCL','sandy clay loam') ]
   dt[is.na(tcode) & cl>27.5 & cl<=40 & sa>20 & sa<=45 & si>15 & si<=52.5, c(cols) := list('ClLo','clay loam') ]
   dt[is.na(tcode) & cl>27.5 & cl<=40 & sa<=20 & si>40 & si<=72.5, c(cols) := list('SiClLo','silty clay loam') ]
   dt[is.na(tcode) & cl<=27.5& sa<=50 & si>50 & si<=80, c(cols) := list('SiLo','silty loam') ]
   dt[is.na(tcode) & cl<=20 & cl>12.5 & sa<=7.5 & si>80 & si<=87.5, c(cols) := list('SiLo','silty loam') ]
   dt[is.na(tcode) & cl<=12.5& sa <=20 & si>80, c(cols) := list('Si','silt') ]
   dt[is.na(tcode) & cl<=27.5& cl>7.5 & sa <=52.5 & sa>22.5& si<=50 & si>27.5, c(cols) := list('Lo','loam') ]
   dt[is.na(tcode) & cl<=7.5 & sa<=52.5 & sa>42.5 & si>40 & si<=50, c(cols) := list('SaLo','sandy loam') ]
   dt[is.na(tcode) & cl<=20 & sa>52.5 & sa<=70 & si>10 & si<=47.5, c(cols) := list('SaLo','sandy loam') ]
   dt[is.na(tcode) & cl>10 & cl<=20 & sa<=80 & sa>70 & si<=20, c(cols) := list('SaLo','sandy loam') ]
   dt[is.na(tcode) & cl<=10 & si<=15 & sa>85 & cl<=10-si*10/15, c(cols) := list('Sa','sand') ]
   dt[is.na(tcode) & cl<=15 & si<=30 & sa>70 & cl> 10-si*10/15 & cl<=15-si*15/30, c(cols) := list('LoSa','loamy sand') ]
   dt[is.na(tcode) & cl<=20 & si<=50 & sa>70 & cl>15-si*15/30, c(cols) := list('SaLo','sandy loam') ]
   dt[is.na(tcode), c(cols) := list('SaLo','sandy loam') ]
  
 # select soil texture HYPRES classification code or name
 if(type=='code'){value <- dt[,tcode]}
 if(type=='name'){value <- dt[,tname]}
 
 # return value
 return(value)
  
}

#' Estimate soil texture according to HYPRES classification
#' 
#' @param A_CLAY_MI (numeric) Clay content (\%)
#' @param A_SILT_MI (numeric) Silt content (\%)
#' @param A_SAND_MI (numeric) Silt content (\%)
#' @param type (character) return HYPRES classification names or codes (options: 'name' or 'code')
#' 
#' @return Texture class according to the HYPRES classification system
#' 
#'
#' @export 
osi_get_TEXTURE_HYPRES <- function(A_CLAY_MI, A_SILT_MI, A_SAND_MI, type='code'){
  
  # check inputs
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, any.missing = FALSE)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, any.missing = FALSE)
  
  # set internal copies
  dt <- data.table(cl = A_CLAY_MI,
                   sa = A_SAND_MI,
                   si = A_SILT_MI,
                   tname = NA_character_,
                   tcode = NA_character_)
  
  # derive soil texture HYPRES classfication
  cols <- c('tcode','tname')
  dt[cl <= 18 & sa > 65, c(cols) := list('C','course')]
  dt[cl <= 35 & sa <= 15, c(cols) := list('MF','medium fine')]
  dt[cl <= 35 & sa > 15 & is.na(tcode),c(cols) := list('M','medium')]
  dt[cl <= 60 & cl > 35, c(cols) := list('F','fine')]
  dt[cl > 60, c(cols) := list('VF','very fine')]
  
  # select soil texture HYPRES classification code or name
  if(type=='code'){value <- dt[,tcode]}
  if(type=='name'){value <- dt[,tname]}
  
  # return value
  return(value)
  
}

#' Estimate soil texture according to GEPPA classification
#' 
#' @param A_CLAY_MI (numeric) Clay content (\%)
#' @param A_SILT_MI (numeric) Silt content (\%)
#' @param A_SAND_MI (numeric) Silt content (\%)
#' @param type (character) return HYPRES classification names or codes (options: 'name' or 'code')
#' 
#' @return Texture class according to the French GEPPA classification system
#' 
#'
#' @export 
osi_get_TEXTURE_GEPPA <- function(A_CLAY_MI, A_SILT_MI, A_SAND_MI, type='code'){
  
  # check inputs
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, any.missing = FALSE)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, any.missing = FALSE)
  
  # set internal copies
  dt <- data.table(cl = round(A_CLAY_MI),
                   sa = round(A_SAND_MI),
                   si = round(A_SILT_MI),
                   tname = NA_character_,
                   tcode = NA_character_)
  
  # derive soil texture GEPPA classification
  cols <- c('tcode','tname')
  dt[,cr1 := 60 - si * (60-55)/45]
  dt[,cr2 := 45 - si * (45-40)/40]
  dt[cl > cr1 & sa <= 40, c(cols) := list('AA','argile lourde')]
  dt[cl <= cr1 & cl > cr2 & si <= 65 & sa <= 55, c(cols) := list('A','argileux')]
  
  dt[,cr1 := cr2]
  dt[,cr2 := 32 - si * (32 - 30)/40]
  dt[,cr3 := 20 + (cl - 42.5) * (25 - 20)/(31 - 42.5)]
  dt[,cr4 := 47 + (cl - 39.5) * (75 - 47)/(0-39.5)]
  dt[cl <= cr1  & cl > cr2 & si <= cr3 & sa > 35, c(cols) := list('As','argile sableuse')]
  dt[cl <= cr1  & cl > cr2 & si > cr3 & si <= cr4 & sa <= 45 & sa >= 14, c(cols) := list('Als','argile limono-sableuse')]
  dt[cl <= cr1  & cl > cr2 & si > cr4 & sa <= 18, c(cols) := list('Al','argile limoneuse')]
  
  dt[,cr1 := cr2]
  dt[,cr2 := 23 - si * (23 - 20)/40]
  dt[,cr3 := 25]
  dt[cl <= cr1  & cl > cr2 & si <= cr3 & sa > 44, c(cols) := list('AS','argilo-sableux')]
  dt[cl <= cr1  & cl > cr2 & si > cr3 & si <= cr4 & sa <= 55, c(cols) := list('LAS','limon argilo-sableux')]
  dt[cl <= cr1  & cl > cr2 & si > cr4 & sa <= 20, c(cols) := list('La','limon argileux')]
  
  dt[,cr1 := cr2]
  dt[,cr2 := 13 - si * (13 - 10)/40]
  dt[,cr3 := 25]
  dt[,cr5 := cr4]
  dt[,cr4 := 35.5 + (cl - 20.5) * (45 - 35.5)/(0-20.5)]
  
  dt[cl <= cr1  & cl > cr2 & si <= cr3 & sa > 53, c(cols) := list('Sa','sable argileux')]
  dt[cl <= cr1  & cl > cr2 & si > cr3 & si <= cr4 & sa > 44, c(cols) := list('SaI','sable argilo-limoneux')]
  dt[cl <= cr1  & cl > cr2 & si > cr4 & si <= cr5 & sa <= 50, c(cols) := list('Lsa','limon sablo-argileux')]
  dt[cl <= cr1  & cl > cr2 & si > cr5 & sa <= 25, c(cols) := list('L','limon')]
  
  dt[,cr1 := cr2]
  dt[,cr2 := 0]
  dt[,cr3 := 25]
  dt[,cr4 := cr4]
  dt[,cr5 := cr5]
  
  dt[cl <= cr1  & cl >= cr2 & si <= cr3 & sa > 63, c(cols) := list('S','sableux')]
  dt[cl <= cr1  & cl >= cr2 & si > cr3 & si <= cr4 & sa <= 75, c(cols) := list('Sl','sable limoneux')]
  dt[cl <= cr1  & cl >= cr2 & si > cr4 & si <= cr5 & sa <= 55, c(cols) := list('Ls','limon sableux')]
  dt[cl <= cr1  & cl >= cr2 & si > cr5 & sa <= 25, c(cols) := list('LL','limon pur')]
  
  dt[,cr1 := 8 - si *(8-0)/50]
  dt[cl <= cr1  & si <= cr3 & sa > 71, c(cols) := list('SS','sable')]
  
  # select soil texture HYPRES classification code or name
  if(type=='code'){value <- dt[,tcode]}
  if(type=='name'){value <- dt[,tname]}
  
  # return value
  return(value)
  
}

#' Estimate soil texture according to Belgium classification
#' 
#' @param A_CLAY_MI (numeric) Clay content (\%)
#' @param A_SILT_MI (numeric) Silt content (\%)
#' @param A_SAND_MI (numeric) Silt content (\%)
#' @param type (character) return Belgium classification names or codes (options: 'name' or 'code')
#' 
#' @return Texture class according to the Belgium classification system
#' 
#'
#' @export 
osi_get_TEXTURE_BE <- function(A_CLAY_MI, A_SILT_MI, A_SAND_MI, type='code'){
  
  # check inputs
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, any.missing = FALSE)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, any.missing = FALSE)
  
  # set internal copies
  dt <- data.table(cl = A_CLAY_MI,
                   sa = A_SAND_MI,
                   si = A_SILT_MI,
                   tname = NA_character_,
                   tcode = NA_character_)
  
  # derive soil texture Belgium classfication
  cols <- c('tcode','tname')
  dt[cl > 65 & si <= 55, c(cols) := list('U','zware klei')]
  dt[cl <= 65 & cl > pmax(17.5, (30 - sa * (30 - 20)/20)) & si <= 70 & is.na(tcode), c(cols) := list('E','klei')]
  dt[sa <= 15 & cl <= (30 - sa * (30 - 20)/20) & si > 60, c(cols) := list('A','leem')]
  dt[cl <= 9 & sa > 82.5, c(cols) := list ('Z', 'zand')]
  dt[cl <= 17.5 & sa > 67.5 & is.na(tcode), c(cols) := list ('S','lemig zand')]
  dt[cl <= 11 & sa > 50 & sa <= 67.5, c(cols) := list('P','licht zandleem')]
  dt[si > 15 & si <= 85 & cl <= 25 & sa >15 & sa <= 67.5 & is.na(tcode), c(cols) := list('L','zandleem')]
  
  # select soil texture Belgium classification code or name
  if(type=='code'){value <- dt[,tcode]}
  if(type=='name'){value <- dt[,tname]}
  
  # return value
  return(value)
  
}
