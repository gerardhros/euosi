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

