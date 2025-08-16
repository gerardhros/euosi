findoptvalue <- function(spool, spoolopt,p0 = NULL) {
  
  # set intial guess
  if(is.null(p0)){p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)}
  
  # optimiser function
  spopt <- function(par,x,y){
    
    pred <- OBIC::evaluate_logistic(x, b = par[1], x0 = par[2], v = par[3])
    lsd <- sum((y - pred)^2)
    return(lsd)
  }
  
  xopt <- optim(par = p0,fn = spopt,x = spool,y=spoolopt)
  
  x <- seq(min(spool),max(spool),0.1)
  y <- OBIC::evaluate_logistic(x, b = xopt$par[1], x0 = xopt$par[2], v = xopt$par[3])
  
  x2 = spool
  y2 = OBIC::evaluate_logistic(x2, b = xopt$par[1], x0 = xopt$par[2], v = xopt$par[3])
  
  plot(y~x,type='l',ylim=c(0,1))
  points(y2~x2,type='p',pch=16,col='blue',cex=2)
  xpar <- xopt$par
  return(xpar)
}


# text for romenai
# The level of magnesium supply in the soil, extracted in 0.025n CaCl2 solution, is
# determined according to the soil texture. 
# Thus, in sandy soils with a content of less than 2.5 mg/100 g soil, the Mg supply level is low, 
# between 2.5 and 5.0 mg/100 g soil it is medium, and above 5.0 mg/100 g soil it is high. 
# In loamy soils, the three levels are: < 3.5 mg/100 g soil, between 3.6 and 7.0 mg/100 g soil, and above 7.00 mg/100 g soil, 
# while in soils with clay texture, the ranges are < 6.0 mg/100 g soil, 6.1–12.0 mg/100 g soil, above 12.0
# mg/100 g soil.
# The probability of Mg deficiency can be determined using the Mg deficiency index
# (ICMg), developed by Borlan (in Lăcătuşu, 2000). This is calculated using the following formula:
# ICMG = MG x Fr / K = (A_MG_CO 24.305 * 0.5) * (1.1 ^ (-0.555 * (A_PH_WA - 4))) / (A_K_CO * 39.098)
# Values of this index lower than 0.15 indicate a very high probability of Mg deficiency,
# while values higher than 12 indicate a very low probability of
# the phenomenon occurring. Between these two limits, with increments of 0.15, 0.30, and 0.60, there are intervals that
# indicate a high, medium, and low probability of Mg deficiency.
# https://www.icpa.ro/documente/coduri/Evaluarea_continutului_de_nutrienti_din_sol.pdf

# Belgium Magnesium, optimum 45 (mg Mg/kg) 
# evaluation soil (A+, A, B, C, D, E)  
spool <- c(1,2.5,10,(10+20)/2,21,1.5*20)*45/21
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Norway Magnesium, optimum 45 (mg Mg/kg) 
# evaluation soil (A+, A, B, C, D, E)  
spool <- c(1,5,25,(25+45)/2,45,1.5*45)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Greece soil pH-water sand
# evaluation soil (A+, A, B, C, D, E)  
spool <- c(1,4,4.5,5.5,6.1,7)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)
    
# Greece soil pH-water others textures
# evaluation soil (A+, A, B, C, D, E)  
spool <- c(1,4,4.5,5.5,6.6,7.6)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Greece soilESP evaluation
# evaluation soil (A+, A, B, C, D, E)  
spool <- c(15,12,(6+15)/2,8,6.6,1)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Greece phosphours P-Olsen (mg P/kg) 
# evaluation soil (A+, A, B, C, D, E)  
spool <- c(1,2.5,10,(10+20)/2,21,1.5*20)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Greece phosphours Potassium K-AAA (mg K/kg) 
# evaluation soil (A+, A, B, C, D, E)  
spool <- c(1,50,200,(250+300)/2,301,1.5*300)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Romenia magnesium (mg M/kg) in CaCl2
# evaluation soil (A+, A, B, C, D, E) sand 
spool <- c(1,5,25,(25+50),55,1.5*50)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)
# evaluation soil (A+, A, B, C, D, E) loam 
spool <- c(1,5,35,(37+70),75,1.5*70)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)
# evaluation soil (A+, A, B, C, D, E) clay 
spool <- c(1,5,60,(61+120),125,1.5*120)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# evaluation soil (A+, A, B, C, D, E) clay , KMg index
spool <- c(0.01,0.08,(0.15+0.3)/2,(0.3+0.6),0.6,2*0.6)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# romenia
# Zn deficiency occurs in soils where the IRPM and ICZn values are lower than 0.384 and 1.7, respectively.
spool <- c(0.01,0.1,0.3,0.384,0.42,1.6)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)
spool <- c(0.01,0.4,0.6,1.7,1.8,2)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# romenia copper
spool <- c(0.01,0.15,0.6,.75,0.85,1.3)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Romenia pH
# evaluation soil (A+, A, B, C, D, E) sand 
spool <- c(1,1.5,5,5.8,5.9,6.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Romenia A_NA_CO_PO
# evaluation soil (A+, A, B, C, D, E) sand
spool <- c(14,7,5.2,5,4,1.5,1.1,0.9)
spoolopt <- c(0,0.01,0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Romenia K-AL (mg K/kg)
# evaluation soil (A+, A, B, C, D, E) sand
spool <- c(1,50,(50),100,150,200)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# evaluation soil (A+, A, B, C, D, E) loamy sand
spool <- c(1,50,(100),150,200,1.5*200)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# evaluation soil (A+, A, B, C, D, E) loamy sand
spool <- c(1,66,(66+132)/2,(132+200)/2,201,1.5*265)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)
# evaluation soil (A+, A, B, C, D, E) F and VF
spool <- c(1,80,(80+160)/2,(160+240)/2,241,1.5*320)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Romenia P-PAL (mg P/kg)
# evaluation soil (A+, A, B, C, D, E)
spool <- c(1,2,(8+18)/2,(18+36)/2,37,1.5*72)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Portugal P-Olsen (mg P2O5/kg)
# evaluation soil pool peat (A+, A, B, C, D, E)
spool <- c(1,18,(18+34)/2,(35+56)/2,57,1.5*115)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Portugal K-AAA (mg K2O/kg)
# evaluation soil pool peat (A+, A, B, C, D, E)
spool <- c(1,25,(26+50)/2,(51+100)/2,101,1.5*200)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Portugal Mg-AAA (mg Mg/kg)
# evaluation soil pool peat (A+, A, B, C, D, E)
spool <- c(1,30,(31+60)/2,(61+90)/2,91,1.5*125)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# PortugalB_HW (mg B/kg)
# evaluation soil pool peat (A+, A, B, C, D, E)
spool <- c(1,0.2,(0.2+0.4)/2,(0.41+1)/2,1.1,1.5*2.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Portugal Zn-AAA (mg Zn/kg)
# evaluation soil pool peat (A+, A, B, C, D, E)
spool <- c(1,0.6,(0.7+1.4)/2,(1.5+3.5)/2,3.6,1*10)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Portugal Base Saturation
# evaluation soil pool peat (A+, A, B, C, D, E)
spool <- c(1,20,(21+40)/2,(41+60)/2,61,1.5*80)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Portugal CEC
# evaluation soil pool peat (A+, A, B, C, D, E)
spool <- c(1,5,(5+10)/2,(10+20)/2,21,1.5*40)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Portugal Ca-cohex
# evaluation soil pool peat (A+, A, B, C, D, E)
spool <- c(1,2,(2+5)/2,(5+10)/2,10.1,1.5*20)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Portugal Mg-cohex
# evaluation soil pool peat (A+, A, B, C, D, E)
spool <- c(0.1,0.5,(0.6+1)/2,(1.1+2.5)/2,2.6,0.8*5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Portugal K-cohex
# evaluation soil pool peat (A+, A, B, C, D, E)
spool <- c(0.01,0.05,(0.1+0.25)/2,(0.26+0.5)/2,0.51,1.5*1)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Portugal pH
# evaluation soil pool peat (A+, A, B, C, D, E)
spool <- c(1,4.5,(4.6+6.5)/2,(6.6+7.5)/2,7.6,9.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Belgie PAL bouwland (mg P/kg)
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,5,9,18,30,75)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Finland (mg P/ L soil), P_AAA
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,2,6,(6+12)/2,13,45)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Belgie PAL grassland (mg P/ kg)
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,8,14,25,40,90)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Germany K-CAL updated via Thuringen. mg K/100g, cropland
# evaluation soil 1 (A+, A, B, C, D, E)
spool <- c(1,2,(3+6)/2,(7+10)/2,(11+15)/2,21*1.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.4488277, x0 = 9.718321, v = 1.254134)
findoptvalue(spool,spoolopt,p0)

# Germany K-CAL updated via Thuringen. mg K/100g, cropland
# evaluation soil 2 (A+, A, B, C, D, E)
spool <- c(1,3,(4+7)/2,(8+11)/2,(12+18)/2,19*1.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.4488277, x0 = 9.718321, v = 1.254134)
findoptvalue(spool,spoolopt,p0)

# Germany K-CAL updated via Thuringen. mg K/100g, cropland
# evaluation soil 3 (A+, A, B, C, D, E)
spool <- c(1,4,(5+9)/2,(10+14)/2,(15+22)/2,23*1.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.4488277, x0 = 9.718321, v = 1.254134)
findoptvalue(spool,spoolopt,p0)

# Germany K-CAL updated via Thuringen. mg K/100g, cropland
# evaluation soil 4 (A+, A, B, C, D, E)
spool <- c(1,5,(6+10)/2,(11+16)/2,(17+25)/2,26*1.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.4488277, x0 = 9.718321, v = 1.254134)
findoptvalue(spool,spoolopt,p0)

# Germany K-CAL updated via Thuringen. mg K/100g, cropland
# evaluation soil 5 (A+, A, B, C, D, E)
spool <- c(1,7,(8+14)/2,(15+23)/2,(24+36)/2,37*1.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.4488277, x0 = 9.718321, v = 1.254134)
findoptvalue(spool,spoolopt,p0)

# evaluation soil 6 (A+, A, B, C, D, E), cropland
spool <- c(1,4,(5+9)/2,(10+16)/2,(17+24)/2,31*1.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.4488277, x0 = 9.718321, v = 1.254134)
findoptvalue(spool,spoolopt,p0)

# Germany K-CAL updated via Thuringen. mg K/100g, grassland
# evaluation soil 1 (A+, A, B, C, D, E)
spool <- c(1,2,(3+6)/2,(7+11)/2,(12+18)/2,21*1.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.4488277, x0 = 9.718321, v = 1.254134)
findoptvalue(spool,spoolopt,p0)

# Germany K-CAL updated via Thuringen. mg K/100g, grassland
# evaluation soil 2 (A+, A, B, C, D, E)
spool <- c(1,3,(4+7)/2,(8+12)/2,(13+21)/2,22*1.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.4488277, x0 = 9.718321, v = 1.254134)
findoptvalue(spool,spoolopt,p0)

# Germany K-CAL updated via Thuringen. mg K/100g, grassland
# evaluation soil 3 (A+, A, B, C, D, E)
spool <- c(1,3,(4+8)/2,(9+14)/2,(15+24)/2,25*1.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.4488277, x0 = 9.718321, v = 1.254134)
findoptvalue(spool,spoolopt,p0)

# Germany K-CAL updated via Thuringen. mg K/100g, grassland
# evaluation soil 4 (A+, A, B, C, D, E)
spool <- c(1,4,(5+9)/2,(10+16)/2,(17+26)/2,27*1.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.4488277, x0 = 9.718321, v = 1.254134)
findoptvalue(spool,spoolopt,p0)

# Germany K-CAL updated via Thuringen. mg K/100g, grassland
# evaluation soil 5 (A+, A, B, C, D, E)
spool <- c(1,5,(+10)/2,(11+16)/2,(17+24)/2,25*1.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.4488277, x0 = 9.718321, v = 1.254134)
findoptvalue(spool,spoolopt,p0)

# evaluation soil 6 (A+, A, B, C, D, E), grassland
spool <- c(1,4,(+10)/2,(11+16)/2,(17+24)/2,25*1.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.4488277, x0 = 9.718321, v = 1.254134)
findoptvalue(spool,spoolopt,p0)

# Germany PCAL updated VDLUFA 2020, mg P/100g
# evaluation soil pool sandy soil (A+, A, B, C, D, E)
spool <- c(1,2,5,(6+10)/2,11,18)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Germany PCAL updated VDLUFA 2020, mg P/100g
# evaluation soil pool clay soil (A+, A, B, C, D, E)
spool <- c(1,2,4,(5+9)/2,10,16)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Germany PCAL updated VDLUFA 2020, mg P/100g
# evaluation soil pool mineral soil high SOM (A+, A, B, C, D, E)
spool <- c(1,2,6,(7+12)/2,13,20)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Germany PCAL updated VDLUFA 2020, mg P/100ml
# evaluation soil pool peat (A+, A, B, C, D, E)
spool <- c(1,1.1,2,(3+4)/2,5,13)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Germany nitrogen, N supply for Nmin in spring
# evaluation soil pool sandy soil (A+, A, B, C, D, E)
spool <- c(1,10,35,85,100,150)*0.4
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Germany PCAL
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,5,10,20,35,45)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Germany PCAL veenbodem
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,11,20,30,40,50)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Germany P-DL
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(0.1,2,4.5,9.1,15,20)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Italy P-Olsen
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(0.1,2.6,5.7,10.9,17.44,19)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# UK P-Olsen
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,10,15,20,45,60)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# evaluation soil pool (A+, A, B, C, D, E) vegetable
spool <- c(1,10,15,26,45,60)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Spain Arenoso
spool <- c(0.5,5,8,12,20,32)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)
# Spain Arcilloso
spool <- c(0.5,8,16,24,40,64)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)
# Spain Franco
spool <- c(0.5,6,12,18,30,48)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Ireland grassland
spool <- c(0.5,2,6,10,12,15)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Sweden, optimum class III
spool <- c(5,10,25,40,50,100)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Sweden, optimum class II
spool <- c(5,7.5,10,20,50,100)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Sweden, optimum class IVA
spool <- c(2.5,20,40,80,90,100)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Poland P-DL
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,22,(23+44)/2,(45+65)/2,66,89)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Chech Republic P-M3
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,50,(51+80)/2,(81+115)/2,116,186)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Austria P-CAL, 
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,25,(26+46)/2,(47+111),112,175)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Austria P-CAL, grassland, Bohner et al. (2013)
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,25,(26+46)/2,(47+68),69,175)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Austria P-CAL, ackerbou, Bohner et al. (2013)
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,25,(26+46)/2,(47+111),112,175)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Norway P-AL, only optimum (C) is known
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(0.5,10,22.5,45.5,75,100)*50/45.5
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Switzerland P-AA, only optimum (C) is known
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(0.5,10,22.5,45.5,75,100)*43/45.5
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# report Eatern Europe
# Estonia, mineral soils, P
spool <- c(1,14,55/2,136/2,100,206)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)
# Estonia, organic soils, P
spool <- c(1,9,(10+25)/2,(26+60)/2,61,126)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LV, sand for P
spool <- c(1,10,(11+21)/2,(22+43)/2,64,68)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LV, loamy sand for P
spool <- c(1,13,(14+26)/2,(27+52)/2,53,81)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LV, loamy sand for P
spool <- c(1,15,(16+30)/2,(31+57)/2,58,96)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LV, clay loam for P
spool <- c(1,17,(18+35)/2,(36+70)/2,71,118)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LT, mineral soils for P
spool <- c(1,21,(22+44)/2,(45+66)/2,67,88)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LT, organic soils for P
spool <- c(1,31,(32+57)/2,(58+87)/2,88,153)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# SK, light soils for P
spool <- c(1,60,(61+95)/2,(96+145)/2,146,200)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# SK, medium heavy soils for P
spool <- c(1,50,(51+85)/2,(86+125)/2,126,166)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# SK, heavy soils for P
spool <- c(1,40,(41+70)/2,(71+100)/2,101,136)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# SL, all soils for P
spool <- c(1,26,(27+52)/2,(53+109)/2,110,175)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# HU, chernozem, no kalk, for P
spool <- c(1,40,(41+80)/2,(81+130)/2,131,201)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# HU, chernozem, kalk, for P
spool <- c(1,50,(50+90)/2,(91+150)/2,151,251)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# HU, brown forest soil, no kalk, for P
spool <- c(1,30,(31+60)/2,(61+100)/2,101,161)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# HU, brown forest soil, kalk, for P
spool <- c(1,40,(41+70)/2,(71+120)/2,121,201)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# HU, sandy soil, no kalk, for P
spool <- c(1,30,(31+60)/2,(61+100)/2,101,200)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# HU,  sandy soil, kalk, for P
spool <- c(1,50,(51+80)/2,(81+130)/2,131,251)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# EE,  sandy for K
spool <- c(1,40,(41+65)/2,(66+115)/2,116,196)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# EE,  loamy sand for K
spool <- c(1,50,(51+90)/2,(91+140)/2,141,281)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# EE,  sandy loam for K
spool <- c(1,65,(66+105)/2,(106+170)/2,171,326)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# EE,  loam for K
spool <- c(1,75,(76+130)/2,(131+195)/2,196,361)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# EE,  clay for K
spool <- c(1,130,(130+195)/2,(196+285)/2,286,501)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LV, sand for K
spool <- c(1,25,(26+50)/2,(51+100)/2,101,171)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LV, loamy sand for K
spool <- c(1,33,(34+66)/2,(67+133)/2,134,225)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LV, loam for K
spool <- c(1,37,(38+75)/2,(76+149)/2,150,254)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LV, clay loam for K
spool <- c(1,41,(42+83)/2,(84+166)/2,167,283)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# LT, mineral soils for K
spool <- c(1,42,(43+83)/2,(84+125)/2,126,166)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LT, organic soils for K
spool <- c(1,101,(102+166)/2,(167+250)/2,251,416)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Austria K, light soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,49,(50+87)/2,(88+178),179,292)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Austria K, medium soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,65,(66+112)/2,(113+208),209,332)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Austria K, heavy soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,82,(83+137)/2,(138+245),246,375)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Poland K, very light soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,21,(22+61)/2,(63+104),105,146)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Poland K, light soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,41,(42+83)/2,(84+124),125,166)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Poland K, medium soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,62,(63+104)/2,(105+166),167,207)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Poland K, heavy soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,83,(84+125)/2,(126+207),207,250)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# SK, light soils for K
spool <- c(1,90,(91+150)/2,(151+230)/2,231,351)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# SK, medium heavy soils for K
spool <- c(1,130,(131+200)/2,(201+300)/2,301,401)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# SK, heavy soils for K
spool <- c(1,170,(171+260)/2,(261+370)/2,371,500)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 60, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# SL, light to medium heavy soils for K
spool <- c(1,83,(84+158)/2,(159+250)/2,251,330)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# SL, heavy soils for K
spool <- c(1,100,(101+180)/2,(181+270)/2,271,371)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# CZ, light soils for K
spool <- c(1,100,(101+160)/2,(161+275)/2,276,381)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# CZ, medium heavy soils for K
spool <- c(1,105,(106+170)/2,(171+310)/2,311,420)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# CZ, heavy soils for K
spool <- c(1,170,(171+260)/2,(261+350)/2,351,511)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.2, x0 = 80, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# HU, chernozem, no kalk, for K
spool <- c(1,80,(81+130)/2,(131+200)/2,201,301)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# HU, chernozem, kalk, for K
spool <- c(1,100,(100+160)/2,(161+240)/2,241,351)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# HU, brown forest soil, no kalk, for K
spool <- c(1,60,(61+100)/2,(101+160)/2,161,251)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# HU, brown forest soil, kalk, for K
spool <- c(1,90,(91+140)/2,(141+210)/2,211,301)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# HU, sandy soil, no kalk, for K
spool <- c(1,50,(51+80)/2,(81+120)/2,121,181)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# HU,  sandy soil, kalk, for K
spool <- c(1,90,(91+120)/2,(121+160)/2,161,221)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# DE,  BG1, for Mg
spool <- c(1,20,(21+35)/2,(36+50)/2,51,66)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# DE,  BG2, for Mg
spool <- c(1,25,(26+45)/2,(46+65)/2,66,86)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# DE,  BG3, for Mg
spool <- c(1,30,(31+55)/2,(56+80)/2,81,106)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# DE,  BG4, for Mg
spool <- c(1,40,(41+75)/2,(76+110)/2,111,146)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# DE,  BG5, for Mg
spool <- c(1,50,(51+95)/2,(96+140)/2,141,186)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# DE,  BG6, for Mg
spool <- c(1,20,(21+35)/2,(36+50)/2,51,66)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# EE,  sandy for Mg
spool <- c(1,30,(31+50)/2,(51+65)/2,66,86)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# EE,  loamy sand for Mg
spool <- c(1,35,(36+65)/2,(66+90)/2,91,116)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# EE,  sandy loam for Mg
spool <- c(1,46,(47+75)/2,(76+105)/2,106,141)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# EE,  loam for Mg
spool <- c(1,55,(56+100)/2,(101+150)/2,151,191)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# EE,  clay for Mg
spool <- c(1,70,(71+130)/2,(131+190)/2,191,246)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# LV, sand for Mg
spool <- c(1,25,(95)/2,(91+150)/2,151,200)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LV, loamy sand for Mg
spool <- c(1,30,(110)/2,(111+180)/2,181,225)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LV, loam for Mg
spool <- c(1,37,(140)/2,(141+240)/2,241,291)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LV, clay loam for Mg
spool <- c(1,41,(160)/2,(161+270)/2,271,320)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# LT, pH < 6.1 for Mg
spool <- c(1,50,(51+100)/2,(101+150)/2,151,201)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LT, pH 6.2 - 7.0 for Mg
spool <- c(1,100,(101+200)/2,(201+300)/2,301,401)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# LT,pH>7 for Mg
spool <- c(1,200,(201+400)/2,(401+600)/2,601,801)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 180, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Austria Mg, light soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,10,(49)/2,(50+75),76,151)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 12, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Austria Mg, medium soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(5,29,(30+55)/2,(56+105),106,190)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.8, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Austria Mg, heavy soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,39,(40+75)/2,(76+135),136,220)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Poland Mg, very light soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,10,(11+20)/2,(21+40),41,61)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Poland Mg, light soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,20,(21+30)/2,(31+50),51,71)
spoolopt <- c(0.05,0.15,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Poland Mg, medium soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,30,(31+50)/2,(51+70),71,91)
spoolopt <- c(0.05,0.15,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Poland Mg, heavy soils
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,40,(41+60)/2,(61+100),101,141)
spoolopt <- c(0.05,0.15,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# SK, light soils for Mg
spool <- c(1,80,(81+135)/2,(136+200)/2,201,301)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# SK, medium heavy soils for Mg
spool <- c(1,110,(111+175)/2,(176+255)/2,256,341)
spoolopt <- c(0.05,0.15,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# SK, heavy soils for Mg
spool <- c(1,145,(146+220)/2,(221+340)/2,341,470)
spoolopt <- c(0.05,0.15,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 60, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# SL, light to medium heavy soils for Mg
spool <- c(1,30,(31+60)/2,(61+100)/2,101,191)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# SL, heavy soils for Mg
spool <- c(1,50,(51+90)/2,(91+200)/2,201,391)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# CZ, light soils for Mg
spool <- c(1,80,(81+135)/2,(136+200)/2,201,286)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# CZ, medium heavy soils for Mg
spool <- c(1,105,(106+160)/2,(161+265)/2,266,331)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.87, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# CZ, heavy soils for Mg
spool <- c(1,120,(121+220)/2,(221+330)/2,331,461)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.2, x0 = 80, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# HU, sand for Mg
spool <- c(1,10,(40)/2,(41+60)/2,60,80)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.87, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# HU, loamy sand, sandy loam for Mg
spool <- c(1,15,(60)/2,(61+100)/2,101,140)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 10, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# HU, clay loam for Mg
spool <- c(1,20,(100)/2,(101+200)/2,201,250)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.209371117, x0 = 2, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

glogparm <- function(parm = c(4.5,5.3,5.8,6.2,6.3)){
  
  spool <- c(3.5,parm[1],mean(parm[1:2]),mean(parm[2:3]),parm[4],parm[5]+0.5)
  spoolopt <- c(0.05,0.10,0.6,0.9,1,1)
  p0 <- list(b = 0.109371117, x0 = 0.2, v = 0.07407514)
  round(findoptvalue(spool,spoolopt,p0),7)
}

# germany, arable, OS < 4 , per BG
rbind(glogparm(parm=c(4.5,5.3,5.8,6.2,6.3)),
glogparm(parm=c(4.8,5.7,6.3,6.7,6.8)),
glogparm(parm=c(5,6,6.7,7.1,7.2)),
glogparm(parm=c(5.2,6.2,7,7.4,7.5)),
glogparm(parm=c(5.3,6.3,7.2,7.7,7.8)),
glogparm(parm=c(2.5,4.2,4.3,4.4,4.5)))

# germany, arable, OS 4-8 , per BG
rbind(
glogparm(parm=c(4.2,4.9,5.4,5.8,5.9)),
glogparm(parm=c(4.5,5.3,5.9,6.3,6.4)),
glogparm(parm=c(4.7,5.5,6.2,6.7,6.8)),
glogparm(parm=c(4.9,5.7,6.5,7,7.1)),
glogparm(parm=c(4.9,5.8,6.7,7.2,7.3)),
glogparm(parm=c(2.5,4.2,4.3,4.4,4.5)))

# germany, arable, OS 8-15 , per BG
rbind(
  glogparm(parm=c(3.9,4.6,5.1,5.4,5.5)),
  glogparm(parm=c(4.1,4.9,5.5,5.9,6)),
  glogparm(parm=c(4.3,5.1,5.8,6.2,6.3)),
  glogparm(parm=c(4.5,5.3,6.1,6.5,6.6)),
  glogparm(parm=c(4.5,5.4,6.3,6.7,6.8)))
 

# germany, arable, OS 15-30 , per BG
rbind(
  glogparm(parm=c(3.6,4.2,4.7,5.1,5.2)),
  glogparm(parm=c(3.7,4.5,5.1,5.5,5.6)),
  glogparm(parm=c(3.8,4.7,5.4,5.8,5.9)),
  glogparm(parm=c(4,4.9,5.7,6.1,6.2)),
  glogparm(parm=c(4,5,5.9,6.3,6.4)))


# germany, grassland, OS <15 , per BG
rbind(
  glogparm(parm=c(4,4.6,5.2,5.6,5.7)),
  glogparm(parm=c(4.3,5.1,5.7,6.1,6.2)),
  glogparm(parm=c(4.5,5.3,6,6.5,6.6)),
  glogparm(parm=c(4.7,5.5,6.3,6.8,6.9)),
  glogparm(parm=c(4.7,5.6,6.5,7,7.1)))


# germany, grassland, OS >15 , per BG
rbind(
  glogparm(parm=c(3.6,4.2,4.7,5.1,5.2)),
  glogparm(parm=c(3.7,4.5,5.1,5.5,5.6)),
  glogparm(parm=c(3.9,4.7,5.4,5.8,5.9)),
  glogparm(parm=c(4.1,4.9,5.7,6.1,6.2)),
  glogparm(parm=c(4.1,5,5.9,6.4,6.5)))

# ireland, K arable non peat
spool <- c(1,25,(51+100)/2,(101+150)/2,151,200)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 10, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# ireland, K arable peat
spool <- c(1,50,(101+175)/2,(176+250)/2,251,350)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 10, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# ireland, Mg arable non peat
spool <- c(1,12.5,(26+50)/2,(51+100)/2,101,125)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 10, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# ireland, Mg arable peat
spool <- c(1,25,(51+100)/2,(101+150)/2,151,200)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 10, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# ireland, pH peat
spool <- c(4.5,5,5.2,5.4,5.5,5.8)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 10, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# ireland, pH potatoes
spool <- c(4.5,5,5.2,5.5,6,6.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 10, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# ireland, pH grassland
spool <- c(4.5,5,5.4,5.9,6.3,6.8)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# ireland, pH cereal
spool <- c(4.5,5,5.4,5.9,6.5,7)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# ireland, pH clover
spool <- c(4.5,5.2,5.8,6.5,7,7.5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# ireland, pH other optimum for NPK
spool <- c(4.5,5.2,5.8,6.5,7.2,7.8)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# spain, K arenoso
spool <- c(1,30,(60+120)/2,(120+180)/2,180,300)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# spain, K arcilloso
spool <- c(1,50,(100+200)/2,(200+300)/2,301,490)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# spain, K franco
spool <- c(1,40,(80+160)/2,(160+235)/2,236,390)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# italy, K sabbiosi, S-SF-FS
spool <- c(10,60,80,(80+120)/2,121,140)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# italy, K mdio impasto, F-FL-FA-FSA-L
spool <- c(10,80,100,(100-150)/2,151,175)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# italy, K argillosi e limos, A-AL-LA-AS
spool <- c(10,80,120,(120+180)/2,181,220)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# UK, boron
spool <- c(0.1,0.3,0.6,0.7,0.8,1.2)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# UK, K, all crops
spool <- c(1,30,(61+120)/2,(121+180)/2,181,250)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# UK, K, vegatables
spool <- c(1,30,(61+120)/2,(121+180)/2,241,300)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# UK, P, all crops
spool <- c(1,4.5,(10+15)/2,(16+25)/2,26,45)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# UK, P, vegetable
spool <- c(1,12.5,(16+25)/2,(26+45)/2,46,71)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# UK, Mg, all crops
spool <- c(1,12.5,(26+50)/2,(51+100)/2,101,175)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 10, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# UK, pH peat, arable
spool <- c(4.5,5,5.2,5.4,5.5,5.8)*5.8/5.5
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 10, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# UK, pH peat, grassland
spool <- c(4.5,5,5.2,5.4,5.5,5.8)*5.3/5.5
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# UK, pH arable, mineral
spool <- c(4.5,5,5.2,5.4,5.5,5.8)*6.5/5.5
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# UK, pH grass and clover, mineral
spool <- c(4.5,5,5.2,5.4,5.5,5.8)*6.0/5.5
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# UK, zinc, mineral
spool <- c(0.1,0.3,0.5,1,.5,2.5)
spoolopt <- c(0.05,0.15,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 3.5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# germany BG1 bohr
a = c(0.14,0.15,0.25,0.26)
spool <- c(0.02,a[1]*0.5,a[1],(a[2]+a[3])/2,a[3]+0.01,a[3]+.5,2)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1,1)
p0 <- list(b = 0.09371117, x0 = 0.1135, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# germany BG2 bohr
a = c(0.19,0.2,0.3,0.3)
spool <- c(0.02,a[1]*.5,a[1],(a[2]+a[3])/2,a[3]+0.01,a[3]+.5,2)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1,1)
p0 <- list(b = 0.09371117, x0 = 0.135, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# germany BG3 bohr
a = c(.24,0.25,0.4,0.41)
spool <- c(0.02,a[1]*.5,a[1],(a[2]+a[3])/2,a[3]+0.01,a[3]+.5,2)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1,1)
p0 <- list(b = 0.09371117, x0 = 1.2135, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# germany BG4 bohr
a = c(0.34,0.35,0.6,0.61)
spool <- c(0.02,a[1]*.5,a[1],(a[2]+a[3])/2,a[3]+0.01,a[3]+.5,2)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1,1)
p0 <- list(b = 0.09371117, x0 = 1.2135, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# germany BG5 bohr
a = c(0.34,0.35,0.6,0.61)
spool <- c(0.02,a[1]*.5,a[1],(a[2]+a[3])/2,a[3]+0.01,a[3]+.5,2)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1,1)
p0 <- list(b = 0.09371117, x0 = 1.2135, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# germany BG6 bohr
a = c(0.14,0.15,0.25,0.26)
spool <- c(0.02,a[1]*.5,a[1],(a[2]+a[3])/2,a[3]+0.01,a[3]+.5,2)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1,1)
p0 <- list(b = 0.09371117, x0 = 1.2135, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# germany BG1-BG2 zinc
a = c(0.9,1,2.5,2.6)
spool <- c(0.02,a[1]*0.5,a[1],(a[2]+a[3])/2,a[3]+0.01,a[3]+.5,2)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1,1)
p0 <- list(b = 0.09371117, x0 = 0.1135, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# germany BG3-4-5 zinc
a = c(1.4,1.5,3,3.1)
spool <- c(0.02,a[1]*0.5,a[1],(a[2]+a[3])/2,a[3]+0.01,a[3]+.5,2)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1,1)
p0 <- list(b = 0.09371117, x0 = 0.1135, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# germany BG6 zinc
a = c(0.5,0.6,1.5,1.6)
spool <- c(0.02,a[1]*0.5,a[1],(a[2]+a[3])/2,a[3]+0.01,a[3]+.5,2)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1,1)
p0 <- list(b = 0.09371117, x0 = 0.1135, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)



# Ireland, boron
spool <- c(0.1,0.25,(0.5+1)/2,(1.1+1.5)/2,1.5,2)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Switseralnd, klei <=10, , arable/grass, K
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,40,(40+140)/2,(140+220)/2,230,340)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 100, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Switseralnd, klei 10-20, , arable/grass, K
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,20,(20+120)/2,(120+200)/2,210,360)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Switseralnd, klei 20-30, , arable/grass, K
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,5,(5+100)/2,(100+180)/2,190,340)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)



# Switseralnd, klei 30-40, arable/grass, K
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,5,(5+40)/2,(40+160)/2,170,320)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Switseralnd, klei <=10, , vegetable, K
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,40,(40+140)/2,(140+220)/2,230,400)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 100, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Switseralnd, klei 10-20, vegetable, K
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,20,(20+120)/2,(120+200)/2,210,380)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Switseralnd, klei 20-30, vegetable, K
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,5,(5+100)/2,(100+180)/2,190,360)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)



# Switseralnd, klei 30-40,vegetable, K
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,5,(5+80)/2,(80+160)/2,170,320)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Switseralnd, klei >40,arable, K
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,5,(5+40)/2,(40+120)/2,130,280)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)



# Switseralnd, klei <=10, , arable/grass, Mg
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,1,75/2,(75+125)/2,135,250)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 100, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Switseralnd, klei 10-20, , arable/grass, Mg
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,1,(100)/2,(100+175)/2,185,300)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Switseralnd, klei 20-30, , arable/grass, Mg
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,5,(5+125)/2,(125+200)/2,215,350)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Switseralnd, klei 30-40, arable/grass, Mg
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,25,(25+150)/2,(150+300)/2,315,400)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Switseralnd, klei >40, arable/grass, Mg
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,50,(50+200)/2,(200+300)/2,315,425)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Switseralnd, CEC arable and grassland
# A > 1.4 (arm), B 1.2-1.4, C 0.9-1.1 (optimim), D 0.4-0.8 and E < 0.4
# H2O10 = water AAE10 = AA+EDTA
spool <- c(0.1,50,50,70,80,110)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# pH switzerland
glogparm(parm=c(5.3,5.8,6.2,6.3,6.8))
glogparm(parm=c(5.3,5.8,6.7,6.8,7.2))



# switzerland, boron
spool <- c(0.1,0.6,(0.6+1.5)/2,(1.6+2)/2,2.1,5)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = .5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# sweden, kalium
spool <- c(0.1,40,(40+80)/2,(81+160)/2,161,320)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 =0.50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# norway, kalium
# https://www.nibio.no/tema/jord/gjodslingshandbok/korreksjonstabeller/kalium--korn-oljevekster-potet-og-gronnsaker
spool <- c(0.1,60,(70+100)/2,(110+150)/2,160,300)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 =0.50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# spain, magnesium, CEC occupation
spool <- c(0.1,2,7.5,(10+20)/2,21,25)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 =0.50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# sweden, magnesium, A_MG_AL
spool <- c(0.1,2,3.5,(4+10)/2,11,15)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 =0.50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# italy, magnesium, CEC occupation
spool <- c(0.1,1,2.5,(3+5)/2,5,10)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 =0.50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# italy, Mg-K ratio
spool <- c(0.1,0.5,1.5,(2+6)/2,6.5,10)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 =0.50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Denmark, P-Olsen, https://link.springer.com/chapter/10.1007/978-94-017-7612-7_11/tables/2
# evaluation soil pool (A+, A, B, C, D, E)
spool <- c(1,5,20,(20+40),60,100)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 2.871137, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# denmark potassium, IFS
spool <- c(0.1,0.2*200,.4*200,(200+250)/2,251,350)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 =0.50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# denmark magnesiu, IFS
spool <- c(0.1,0.2*100,.4*100,(100+125)/2,126,150)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 =0.50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# finland, sand and peat magnesiu, IFS
spool <- c(0.1,0.2*120,.5*120,(120+200)/2,205,150)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 =0.50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# finland, loam magnesiu, IFS
spool <- c(0.1,0.2*200,.5*200,(200+400)/2,410,500)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 =0.50, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Austria, pH arable, mineral, light
spool <- c(4.5,5,5.2,5.4,5.5,5.8)*5/5.5
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 4.5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Austria, pH arable, mineral, mittel
spool <- c(4.5,5,5.2,5.4,5.5,5.8)*5.5/5.5
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 4.5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Austria, pH arable, mineral, schwer
spool <- c(4.5,5,5.2,5.4,5.5,5.8)*6/5.5
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 4.5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# Sweden, only target is known, OS <6%, clay <5
spool <- c(4.5,5,5.2,5.4,5.5,5.8)*6/5.5
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 4.5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Sweden, only target is known, OS <6%, clay 5-25
spool <- c(4.5,5,5.2,5.4,5.5,5.8)*6.25/5.5
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 4.5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Sweden, only target is known, OS <6%, clay >25
spool <- c(4.5,5,5.2,5.4,5.5,5.8)*6.5/5.5
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 4.5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# Sweden, trial for others
spool <- c(4.5,5,5.2,5.4,5.5,5.8)*7.5/5.5
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 4.5, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)


# generic, 150 kg NLV
spool <- c(5,15,45,85,150,250)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 45, v = 0.07407514)
findoptvalue(spool,spoolopt,p0)

# the P evaluation
get_p_nut<- function(pb,px0,pv,nb = -0.14,nx0 = 200,nv = 1){
  
  x = seq(0,300)
  y = osi_evaluate_logistic(x = x, b= pb,x0 = px0,v = pv)
  xmax <- (x[which(y>=0.99)][1])*3.5 # => 0.1
  xvmax <- (x[which(y>=0.99)][1])*5 # => 0.02
  xmid <- (x[which(y>=0.99)][1])*2 # => 0.5
  xlow <- (x[which(y>=0.9)][1]) # => 0.9
  xvlow <- x[1] # <- 1
  
  spool <- c(xvlow,xlow,xmid,xmax,xvmax)
  spoolopt <- c(1,0.9,0.5,0.1,0.02)
  spool <- c(spool,spool * 1.01,spool * 0.99)
  spoolopt <- c(spoolopt,spoolopt * 1.01,spoolopt * 0.99)
  p0 <- list(b = nb, x0 = nx0, v = nv)
  findoptvalue(spool,spoolopt,p0)
  
}
# Austria, Pnut
get_p_nut(pb= 0.138491,px0 = 2.81405015,pv = 0.01965865)
# Switzeraland
get_p_nut(pb= 0.113193,px0 = -21.4503795,pv = 0.01430497)
# CZ
get_p_nut(pb= 0.0890545,px0 = 4.429710417,pv = 0.007972486,nx0=150)
# germany, part 1
get_p_nut(pb = 0.2711, px0 = -5.9449, pv = 0.0239)
get_p_nut(pb = 0.1743, px0 = 2.92395, pv = 0.096079,nx0=150)
# denmark
get_p_nut(pb = 0.226612, px0 = 30.137321,pv = 1.247315)
# ee
get_p_nut(pb= 0.1078429,px0 = -17.16723,pv = 0.0153443)
# EL GReece
get_p_nut(pb = 0.3677886, px0 = 9.1132506,pv =  1.0632264 )
# spain
get_p_nut(pb = 0.47947, px0 = -1.94363, pv = 0.074075,nx0=40)
get_p_nut(pb = 0.27155, px0 = 2.81733, pv = 0.154671,nx0=40)
get_p_nut(pb = 0.20196, px0 = 2.87602, pv = 0.133171,nx0=40)
# hungary
rbind(
get_p_nut(pb= 0.068946,px0 = 3.064834,pv = 0.0349155,nx0=275),
get_p_nut(pb= 0.07114313,px0 = 3.6741042,pv = 0.016646,nx0=275),
get_p_nut(pb= 0.08938405,px0 = -17.8025024,pv = 0.006430696,nx0=75),
get_p_nut(pb= 0.0919443,px0 = 1.95992725,pv = 0.01361575,nx0=75),
get_p_nut(pb= 0.08938405,px0 = -17.80250244,pv = 0.006430696),
get_p_nut(pb= 0.09148938,px0 = 5.716193495,pv = 0.007882485))
# ireland
get_p_nut(pb = 0.6560111, px0 = 3.44709, pv = 0.588379,nx0=40)
get_p_nut(pb = 0.50194, px0 = 3.91821, pv = 0.5799892,nx0=40)
# italy
get_p_nut(pb = 0.43987, px0 = -5.7314, pv = 0.011909,nx0=40)
# latvia
get_p_nut(pb= 0.249934,px0 = 2.907279,pv = 0.070707)
get_p_nut(pb= 0.211789,px0 = 2.937473,pv = 0.050529)
get_p_nut(pb= 0.17789728,px0 = -7.742856,pv = 0.007900254)
get_p_nut(pb= 0.1513621,px0 = -9.9252899,pv = 0.00760908)

get_p_nut(pb= 0.115141,px0 = -13.66102,pv = 0.0084859,nx0 = 55)
get_p_nut(pb= 0.1014425,px0 = -10.704884,pv = 0.00674754)
#NL
get_p_nut(pb = 1.3,px0=1.3,pv=0.35,nx0=2.5)
#norway
get_p_nut(pb= 0.09801777,px0 = -20.28710038,pv = 0.0218218)
# portugal
get_p_nut(pb= 0.17869351 ,px0 = 3.01230206 ,pv = 0.03047017 )
# romenai
get_p_nut(pb= 0.1387092   ,px0 = -18.0674770 ,pv = 0.0252298 )
#sweden
rbind(
get_p_nut(pb = 0.126197, px0 = 14.6487, pv = 0.46202),
get_p_nut(pb = 0.60458, px0 = 2.8517965, pv = 0.0256494),
get_p_nut( pb = 0.126197, px0 = 14.6487, pv = 0.46202),
get_p_nut( pb = 0.0695783, px0 = -27.867195, pv = 0.0163328))

#SK
get_p_nut(pb= 0.07743388,px0 = -1.23994065,pv = 0.00401143,nx0=150)
get_p_nut(pb= 0.07683386,px0 = -7.05760285,pv = 0.00574734,nx0=150)
get_p_nut(pb= 0.089515,px0 = 3.0679272,pv = 0.01673862,nx0=350)

# slovena
get_p_nut(pb= 0.1080206,px0 = -11.5603261,pv = 0.00766528)
# poland
get_p_nut(pb= 0.1199736,px0 = -12.565624,pv = 0.0072809,nx0 = 150)
#UK
get_p_nut(pb = 0.2417380 , px0 = 7.8550060 , pv = 0.5393981 )
get_p_nut(pb = 0.17689431 , px0 = 0.45050382 , pv = 0.05213803)

# the K evaluation
get_k_nut<- function(pb,px0,pv,nb = -0.14,nx0 = 300,nv = 1){
  
  x = seq(0,500)
  y = osi_evaluate_logistic(x = x, b= pb,x0 = px0,v = pv)
  xmax <- (x[which(y>=0.99)][1])*3.5 # => 0.1
  xvmax <- (x[which(y>=0.99)][1])*5 # => 0.02
  xmid <- (x[which(y>=0.99)][1])*2 # => 0.5
  xlow <- (x[which(y>=0.9)][1]) # => 0.9
  xvlow <- x[1] # <- 1
  
  spool <- c(xvlow,xlow,xmid,xmax,xvmax)
  spoolopt <- c(1,0.9,0.5,0.1,0.02)
  spool <- c(spool,spool * 1.01,spool * 0.99)
  spoolopt <- c(spoolopt,spoolopt * 1.01,spoolopt * 0.99)
  p0 <- list(b = nb, x0 = nx0, v = nv)
  a = findoptvalue(spool,spoolopt,p0)
  paste0('b = ',round(a[1],5),', x0 = ',round(a[2],2),', v = ',round(a[3],4))
}
# Potassium_AT
get_k_nut(pb= 0.07778603,px0 = 3.34792775,pv = 0.01237349)
get_k_nut(pb= 0.063061856,px0 = 3.246009394,pv = 0.008754153)
get_k_nut(pb= 0.05395119,px0 = 1.739323688,pv = 0.005680624,nx0=400)
#Belgium
get_k_nut(pb = 0.05, px0 = 100, pv=1,nx0=150)
# Finland
get_k_nut(pb= 0.02920275,px0 = 5.86503285,pv = 0.14855417,nx0=150)
get_k_nut( pb= 0.03016788,px0 = 0.47412598,pv = 0.21149593,nx0=150)
get_k_nut(pb= 0.03615689,px0 = 0.62427132 ,pv = 0.26501704,nx0=150)
get_k_nut(pb= 0.03888415,px0 = -62.31854852 ,pv = 0.03100743,nx0=150)
get_k_nut(pb= 0.08417474 ,px0 = -22.01125870 ,pv = 0.04457961,nx0=150)
# CZ
get_k_nut(pb= 0.04721530,px0 = 0.86394309,pv = 0.00416517,nx0=150)
get_k_nut(pb= 0.044200350,px0 = 3.073835166,pv = 0.004912691,nx0=150)
get_k_nut(pb= 0.03095766,px0 = 91.95343771,pv = 0.03970912,nx0=150)

get_k_nut(pb = 0.4488277, px0 = 9.718321, pv = 1.254134,nx0=50)
get_k_nut(pb = 0.4025205, px0 = 11.358496, pv = 1.186427,nx0=50)
get_k_nut(pb = 0.3578666, px0 = 14.656902, pv = 1.373431,nx0=50)
get_k_nut(pb = 0.3457865, px0 = 17.303152, pv = 1.543785,nx0=50)
get_k_nut(pb = 0.2675499, px0 = 25.566018, pv = 1.827952,nx0=50)
get_k_nut(pb = 0.3735224, px0 = 17.424734, pv = 1.874988,nx0=100)

get_k_nut(pb = 9.030400, px0 = 6.895466, pv = 5.5870309,nx0=50)
get_k_nut(pb = 5.269679, px0 = 8.321927, pv = 4.4726794,nx0=50)
get_k_nut(pb = 4.616016, px0 = 9.324117, pv = 4.1282965,nx0=50)
get_k_nut( pb = 1.713005, px0 = 15.406360, pv = 0.4744978,nx0=50)
get_k_nut(pb = 1.595327, px0 = 15.460018, pv = 0.1341047,nx0=50)
get_k_nut(pb = 1.662136, px0 = 15.228511, pv = 0.4373691,nx0=50)

# estonia
get_k_nut(pb= 0.111228,px0 = 4.8454716,pv = 0.00894408,nx0=100)
get_k_nut(pb= 0.0681246,px0 = 2.8658315,pv = 0.01832195)
get_k_nut(pb= 0.06901738,px0 = 3.22236569,pv = 0.006392278)
get_k_nut(pb= 0.04955406,px0 = 2.80768003,pv = 0.01280431,nx0=150)
get_k_nut(pb= 0.0429129,px0 = 1.37161548,pv = 0.001844779,nx0=150)

#denmark
get_k_nut(pb = 0.03892854   , px0 = 0.55628171   , pv = 0.08602831,nx0=150)

#greece
get_k_nut(pb = 0.03390581, px0 = 232.24412487, pv = 2.67687384,nx0=300)

# spain
get_k_nut(pb= 0.02948845,px0 = 23.21207454,pv = 0.25785455,nx0=100)
get_k_nut(pb= 0.02108017,px0 = 5.43747198,pv = 0.17298447)
get_k_nut(pb= 0.01665276,px0 = 5.18031011,pv = 0.17195884)

# finland
get_k_nut(pb= 0.022,px0=135,pv=1,nx0=150)
get_k_nut(pb= 0.032,px0=90,pv=1,nx0=150)
get_k_nut(pb= 0.044,px0=68,pv=1,nx0=150)

# France
get_k_nut(pb = 0.02719,px0 = 206,pv = 206)
get_k_nut(pb = 0.4224, px0 = 96,pv = 96,nx0=100)
get_k_nut(pb = 0.1295, px0 = 57,pv=57)

# hungary
get_k_nut(pb= 0.054768759 ,px0 = 3.135654568 ,pv = 0.006806117,nx0=150)
get_k_nut(pb= 0.046479554 ,px0 = 2.791118699 ,pv = 0.005011318,nx0=150 )
get_k_nut(pb= 0.068578055,px0 = 2.128475547,pv = 0.008613139)
get_k_nut( pb= 0.054626825,px0 = 2.745261676,pv = 0.003915165,nx0=150)
get_k_nut(pb=0.089604257,px0 = 3.920048277,pv = 0.007403738)
get_k_nut(pb= 0.0890913887,px0 = 2.2516305413,pv = 0.0001867756)

#ireland
get_k_nut(pb = 0.04081745, px0 = 53.86819280, pv = 0.66253526,nx0=150)
get_k_nut(pb = 0.02000395 , px0 = 10.63516808  , pv = 0.15763251 )

# italy
get_k_nut(pb= 0.07914907 ,px0 = 33.82005169  ,pv = 0.05062077)
get_k_nut(pb= 0.075840953 ,px0 = 5.349772330 ,pv = 0.001498863)
get_k_nut(pb= 0.04242224 ,px0 = 5.62056414 ,pv = 0.01662994,nx0=75)

# latvia
get_k_nut(pb= 0.11109104,px0 = -11.43898675,pv = 0.007796498)
get_k_nut(pb= 0.085102248,px0 = -17.31157273,pv = 0.00616734,nx0=100)
get_k_nut(pb= 0.0754598,px0 = 3.001728,pv = 0.03331127)
get_k_nut(pb= 0.0685035,px0 = 2.97672246,pv = 0.03203499)

# lithuani
get_k_nut(pb= 0.06542787,px0 = -23.62084916,pv = 0.006326486,nx0=150)
get_k_nut(pb= 0.04211846,px0 = 3.19983935,pv = 0.007473128,nx0=150)

# norway
get_k_nut( pb = 0.05548162  , px0 = 0.66920970  , pv = 0.01696135,nx0=100)

# the netherlands
get_k_nut(pb=8,px0=2.5,pv=8,nx0=4.5)
get_k_nut(pb=0.3,px0=9,pv=1.1,nx0=4.5)
get_k_nut(pb=0.5,px0=11.5,pv=1.1,nx0=4.5)

# poland
get_k_nut(pb= 0.07752183,px0 = 3.04311052,pv = 0.09679474,nx0=75)
get_k_nut(pb= 0.07134238,px0 = 3.07588403,pv = 0.02805476)
get_k_nut(pb= 0.070369841,px0 = 4.237044272,pv = 0.007390134)
get_k_nut(pb= 0.070110090,px0 = 0.702553221,pv = 0.001352746)
#portugal
get_k_nut( pb = 0.07362818 , px0 = 0.51818429 , pv = 0.02380852)
# romenia
get_k_nut( pb= 0.04691703,px0 = 3.13861224,pv = 0.10035566)
get_k_nut( pb= 0.03211084,px0 = 3.42618554,pv = 0.08696479)
get_k_nut( pb= 0.04247343,px0 = 3.02608975,pv = 0.03096234)
get_k_nut( pb= 0.03502668,px0 = 3.35427054,pv = 0.03066438)

#sweden
get_k_nut( pb = 0.111091036  , px0 = -11.438986748 , pv = 0.007796498)

#SK
get_k_nut(pb= 0.04569915,px0 = 2.78099169 ,pv = 0.00851402,nx0=100)
get_k_nut( pb= 0.039305100,px0 = 2.045554487 ,pv = 0.003009242,nx0=100)
get_k_nut(pb= 0.03070035,px0 = 65.84134127,pv = 0.01856598,nx0=200)

# slowakij
get_k_nut(pb= 0.03689968,px0 = 2.80510928,pv = 0.02341685,nx0=100)
get_k_nut(pb= 0.03432105,px0 = 2.90565094,pv = 0.01630095,nx0=100)

#UK
get_k_nut( pb = 0.03399723, px0 = 64.381726, pv = 0.65964038,nx0=100)
get_k_nut(pb = 0.03023062, px0 = 46.87684513, pv = 0.45288470,nx0=100)


# NL, boron
spool <- c(0.01,0.1,0.25,0.4,0.42,0.6)
spoolopt <- c(0.05,0.1,0.6,0.9,1,1)
p0 <- list(b = 0.09371117, x0 = 1.5, v = 0.007407514)
findoptvalue(spool,spoolopt,p0)

dt[,value := 0]
dt[A_B_HWA < 0.2,value := 0.4]
dt[A_B_HWA >= 0.2 & A_B_HWA < 0.3, value := 0.3]
dt[A_B_HWA >= 0.3 & A_B_HWA < 0.35, value := 0.2]

