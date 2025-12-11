#' Estimate soil organic matter and nutrient values (-)
#' 
#' @param element (character) the method requested to be calculated
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_C_OF (numeric) The organic carbon content of the soil in g C / kg
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param A_CN_FR (numeric) The C-to-N ratio of the soil organic matter (-)
#' 
#' @export 
osi_conv_som <- function(element, A_SOM_LOI = NA_real_,A_C_OF = NA_real_, A_N_RT = NA_real_,A_CN_FR = NA_real_){
  
  # add visual bindings
  
  # check required inputs
  checkmate::assert_subset(element,
                           choices = c('A_SOM_LOI','A_C_OF','A_N_RT','A_CN_FR'),empty.ok = FALSE)
  
  # make internal table with inputs
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_CN_FR)
  
  # check required inputs
  osi_checkvar(parm = list(A_SOM_LOI = dt$A_SOM_LOI, A_C_OF = dt$A_C_OF,
                           A_N_RT = dt$A_N_RT, A_CN_FR = dt$A_CN_FR),
               fname ='osi_conv_som')
  
  # estimate SOM properties from other measurements
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 2]
  dt[is.na(A_SOM_LOI) & !is.na(A_N_RT) & !is.na(A_CN_FR), A_SOM_LOI := A_N_RT * A_CN_FR * 0.001 / 10]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 * 0.5]
  dt[is.na(A_C_OF) & !is.na(A_N_RT) & !is.na(A_CN_FR), A_C_OF := A_N_RT * A_CN_FR * 0.001]
  dt[is.na(A_N_RT) & !is.na(A_C_OF) & !is.na(A_CN_FR), A_N_RT := A_C_OF * 1000 / A_CN_FR]
  dt[is.na(A_CN_FR) & !is.na(A_C_OF) & !is.na(A_N_RT), A_CN_FR := A_C_OF * 1000 / A_N_RT]
  
  # check parameters after estimation
  osi_checkvar(parm = list(A_SOM_LOI = dt$A_SOM_LOI, A_C_OF = dt$A_C_OF,
                           A_N_RT = dt$A_N_RT, A_CN_FR = dt$A_CN_FR),
               fname ='osi_conv_som')
  
  # select the reqestred property
  value <- dt[,get(element)]
  
  # return value
  return(value)
  
}

#' Calculate A_B_HW
#' 
#' This function calculates the hot water extractable B content from A_B_CC (mg / kg).
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil. Options: duinzand, dekzand, zeeklei, rivierklei, maasklei, dalgrond, moerige_klei, veen en loess
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_B_CC (numeric) The extractable boron content of the soil (ug / kg), measured in a 0.01M CaCl2 extract
#' @param A_PH_CC (numeric) The pH of the soil, measured in a 0.01M CaCl2 extract (-)
#' 
#' @import data.table
#' 
#' @references Van Rotterdam & Bussink (2017) and de Haas et al. (2004)
#'
#' @export
osi_conv_hwb <- function(B_SOILTYPE_AGR, A_SOM_LOI = NA_real_, A_B_CC= NA_real_, A_PH_CC= NA_real_){
  
  # initialize global variables
  value1 = value2 = value3 = id = NULL
  
  # number of soils
  arg.length <- max(length(A_B_CC),length(A_SOM_LOI),length(B_SOILTYPE_AGR),length(A_PH_CC))
  
  # make local copy of inputs into data.table
  dt <- data.table(id = 1:arg.length,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_B_CC = A_B_CC * 0.001,
                   A_PH_CC = A_PH_CC,
                   value1 = NA_real_,
                   value2 = NA_real_,
                   value3 = NA_real_,
                   value = NA_real_)
  
  # check required inputs
  osi_checkvar(parm = list(B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR, A_SOM_LOI = dt$A_SOM_LOI, 
                           A_B_CC = dt$A_B_CC,A_PH_CC = dt$A_PH_CC),
               fname ='osi_conv_hwb')
  
  # estimate A_B_HW from A_SOM_LOI or A_B_CC using equations from Rotterdam & Bussink (2017), NMI project 1323
  dt[grepl('duin|dal|zand',B_SOILTYPE_AGR),value1 := 0.0305 * A_SOM_LOI + 0.2038]
  dt[grepl('klei|loss|loes',B_SOILTYPE_AGR) & A_PH_CC > 6,value1 := 0.2731 * A_SOM_LOI]
  dt[grepl('klei|loss|loes',B_SOILTYPE_AGR) & A_PH_CC <= 6,value1 := 0.1829 * A_SOM_LOI]
  dt[grepl('duin|dal|zand',B_SOILTYPE_AGR),value2 := 2.80599 * A_B_CC + 0.00745]
  dt[grepl('klei|loss|loes|veen',B_SOILTYPE_AGR),value2 := 4.38902 * A_B_CC - 0.10114]
  
  # estimate A_B_HW from A_B_CC using equations from De Haas et al. (2014)
  dt[grepl('duin',B_SOILTYPE_AGR), value3 := 0.0983 + 2.3256 * A_B_CC]
  dt[grepl('dekzand',B_SOILTYPE_AGR), value3 := 0.0983 + 2.3256 * A_B_CC - 0.383]
  dt[grepl('dalgrond',B_SOILTYPE_AGR), value3 := 0.0983 + 2.3256 * A_B_CC + 0.0179]
  dt[grepl('klei|loss|loes|veen',B_SOILTYPE_AGR), value3 := 0.1795 + 2.356 * A_B_CC]
  
  # estimate mean value from three regression equations
  dt[,value := mean(c(value1, value2, value3),na.rm = TRUE),by=id]
  
  # check parameters after calculation
  osi_checkvar(parm = list(A_B_HW = dt$value),
               fname ='osi_conv_hwb')
  
  # extract output variable
  value <- dt[,value]
  
  # return value
  return(value)
  
}

#' Estimate soil pH values (-)
#' 
#' @param element (character) the method requested to be calculated
#' @param A_PH_KCL (numeric) The soil pH determined in 1 M KCL extract
#' @param A_PH_CC (numeric) The soil pH determined in 0.01M CaCl2 extract
#' @param A_PH_WA (numeric) The soil pH determined in water
#' 
#' @export 
osi_conv_ph <- function(element, A_PH_KCL = NA_real_,A_PH_CC = NA_real_, A_PH_WA = NA_real_){
  
  # add visual bindings
  osi_parm_name = osi_parm_min = id = NULL
  
  # check options for element argument
  checkmate::assert_subset(element,choices = c('A_PH_CC','A_PH_KCL','A_PH_WA'),empty.ok = FALSE)
  
  # make internal table with inputs
  dt <- data.table(A_PH_KCL = A_PH_KCL,
                   A_PH_CC = A_PH_CC,
                   A_PH_WA = A_PH_WA)
  
  # check required inputs
  osi_checkvar(parm = list(A_PH_KCL = dt$A_PH_KCL, A_PH_CC = dt$A_PH_CC, 
                           A_PH_WA = dt$A_PH_WA),
               fname ='osi_conv_ph')
  
  # estimate pH from other measurements
  dt[is.na(A_PH_CC), A_PH_CC := A_PH_KCL * 0.9288 + 0.5262]
  dt[is.na(A_PH_CC), A_PH_CC := ((A_PH_WA - 2.23) / 0.777) * 0.9288 + 0.5262]
  dt[is.na(A_PH_WA), A_PH_WA := 2.23 + 0.777 * A_PH_KCL]
  dt[is.na(A_PH_WA), A_PH_WA := 2.23 + 0.777 * ((A_PH_CC - 0.5262) / 0.9288)]
  dt[is.na(A_PH_KCL), A_PH_KCL := (A_PH_WA - 2.23) / 0.777]
  dt[is.na(A_PH_KCL), A_PH_KCL := (A_PH_CC - 0.5262) / 0.9288]
  
  # load internal table for all euosi parameters
  dtp <- as.data.table(euosi::osi_parms)
  
  # do checks and replace minima
  dt[, A_PH_KCL := pmax(dtp[osi_parm_name=='A_PH_KCL',osi_parm_min], A_PH_KCL)]
  dt[, A_PH_CC := pmax(dtp[osi_parm_name=='A_PH_CC',osi_parm_min], A_PH_CC)]
  dt[, A_PH_WA := pmax(dtp[osi_parm_name=='A_PH_WA',osi_parm_min], A_PH_WA)]
  
  # check parameters after calculation
  osi_checkvar(parm = list(A_PH_KCL = dt$A_PH_KCL, 
                           A_PH_CC = dt$A_PH_CC, 
                           A_PH_WA = dt$A_PH_WA),
               fname ='osi_conv_ph')
  
  # select the requested pH
  value <- dt[,get(element)]
  
  # return value
  return(value)
  
}

#' Calculate potential mineralizable N for agricultural soils
#' default values are taken from global ISRIC database
#' 
#' @param A_N_RT (numeric)  Soil organic N content (mg N / kg) of top soil
#' @param A_CLAY_MI (numeric) Clay content (\%)
#' @param med_PMN (numeric) Regional median value of PMN (mg N / kg)
#' @param med_NRT (numeric) Regional median value of soil organic N content (mg N / kg)
#' @param med_CLAY (numeric) Regional median value of clay content (\%)
#' 
#' @import data.table
#' 
#' @export
osi_conv_npmn <- function(A_N_RT, A_CLAY_MI, med_PMN = 51.9, med_NRT = 1425, med_CLAY = 26){
  
  # initialize
  PMN = med_PMN_pred = cor_rg = NULL
  
  # length of inputs
  arg.length <- max(length(A_N_RT),length(A_CLAY_MI))
  
  # check required inputs
  checkmate::assert_numeric(med_PMN, lower = 0.1, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(med_NRT, lower = 0.1, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(med_CLAY, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  osi_checkvar(parm = list(A_N_RT = A_N_RT, A_CLAY_MI = A_CLAY_MI),
               fname ='osi_conv_npmn')
  
  # Collect data in a table
  # to avoid NaN from clay = 0, it it slightly increased
  dt <- data.table(id = 1:arg.length,
                   A_N_RT = A_N_RT,
                   A_CLAY_MI = A_CLAY_MI + 0.001,
                   med_PMN = med_PMN,
                   med_NRT = med_NRT,
                   med_CLAY = med_CLAY,
                   value = NA_real_)
  
  # Coefficient values of regression model to estimate PMN (mg N / kg), 
  # built on the large dataset of Dutch soils (R2 = 0.79, N=109.146 samples)
  b0 <- -3.440931;  b1 <- 1.1012449;  b2 <- 0.055858
  
  # predict PMN (based on Dutch emperical relationship)
  dt[, PMN := exp(b0 + b1 * log(A_N_RT) - b2 * log(A_CLAY_MI))]
  
  # Calculate correction factor for regional variation
  dt[, med_PMN_pred := exp(b0 + b1 * log(med_NRT) - b2 * log(med_CLAY))]
  dt[, cor_rg := med_PMN / med_PMN_pred]
  
  # adjust predicted PMN with regional correction
  # and do not allow values exceeding 500 mg/kg
  dt[, value := pmin(500,PMN * cor_rg)]
  
  # check parameters after calculation
  osi_checkvar(parm = list(A_N_PMN = dt$value),
               fname ='osi_conv_npmn')
  
  # extract value
  value <- dt[, value]
  
  # return value
  return(value)
}

#' Estimate soil extractable phosphorus (-)
#' 
#' @param element (character) the method requested to be calculated
#' @param A_P_AL (numeric) The P-content of the soil extracted with ammonium lactate
#' @param A_P_CC (numeric) The P-content of the soil extracted with CaCl2
#' @param A_P_WA (numeric) The P-content of the soil extracted with water
#' @param A_P_OL (numeric) The P-content of the soil extracted with Olsen
#' @param A_P_CAL (numeric) The P-content of the soil extracted with ammonium lactate(mg P2O5 / 100g)
#' @param A_P_DL (numeric) The P-content of the soil extracted with double lactate (mg P / kg)
#' @param A_P_AAA (numeric) The exchangeable P-content of the soil measured via acid ammonium acetate extraction
#' @param A_P_AAA_EDTA (numeric) The exchangeable P-content of the soil measured via acid ammonium acetate+EDTA extraction
#' @param A_P_M3 (numeric) The P-content of the soil extracted with Mehlig 3
#' @param A_P_MORGAN (numeric) The P-content of the soil extracted with sodium acetate (mg P / L)
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil.
#' @param A_PH_CC (numeric) The pH measured in cacl2. If missing, then assume its agronomic common value of 5. 
#' 
#' @references 
#' Steinfurth et al., (2021) Conversion equations between Olsen-P and other methods used to assess plant available soil phosphorus in Europe. A review
#' 
#' @export 
osi_conv_phosphor <- function(element, 
                              A_P_AL = NA_real_,A_P_CC = NA_real_, A_P_WA = NA_real_,
                              A_P_OL = NA_real_,A_P_CAL = NA_real_,A_P_DL = NA_real_,A_P_AAA = NA_real_,
                              A_P_AAA_EDTA = NA_real_,A_P_M3 = NA_real_,
                              A_P_MORGAN = NA_real_,
                              B_SOILTYPE_AGR = NA_real_, A_PH_CC = 5){
  
  # add visual bindings
  osi_parm_name = osi_parm_min = osi_parm_max = bd = NULL
  
  # check inputs
  checkmate::assert_subset(element,choices = c('A_P_AL','A_P_CAL','A_P_DL','A_P_AAA','A_P_AAA_EDTA',
                                               'A_P_WA','A_P_M3','A_P_CC','A_P_MORGAN'),empty.ok = FALSE)
   
  # make internal table with inputs
  dt <- data.table(A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA,
                   A_P_OL = A_P_OL,
                   A_P_CAL = A_P_CAL,
                   A_P_DL = A_P_DL,
                   A_P_M3 = A_P_M3,
                   A_P_MORGAN = A_P_MORGAN,
                   A_P_AAA = A_P_AAA,
                   A_P_AAA_EDTA = A_P_AAA_EDTA,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_PH_CC = A_PH_CC)
  
  # check required inputs, and allow that soiltype has NA values as category
  osi_checkvar(parm = list(A_P_OL = dt$A_P_OL, A_PH_CC = dt$A_PH_CC, 
                           B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR),
               fname ='osi_conv_phosphor',
               na_allowed = TRUE)
  
  # estimate A_P_CC from A_P_WA if available (NMI report 936.03)
  
    if(any(!is.na(dt$B_SOILTYPE_AGR))){
      
      # estimate A_P_WA using regression functions EA (NMI report 936.03), unit mg P2O5 / 100 ml soil
      dt[B_SOILTYPE_AGR %in% c('duinzand','veen') & A_P_CC <= 0.2, value := exp(3.0177 + 0.5891 * log(A_P_CC) - 0.1263 * log(A_P_CC)^2)]
      dt[B_SOILTYPE_AGR %in% c('dekzand','zeeklei','rivierklei') & A_P_CC <= 0.2, value := exp(3.0851 + 0.6646 * log(A_P_CC) - 0.1966 * log(A_P_CC)^2)]
      dt[B_SOILTYPE_AGR %in% c('maasklei','loess') & A_P_CC <= 0.2, value := exp(3.3862 + 0.5962 * log(A_P_CC))]
      dt[B_SOILTYPE_AGR %in% c('dalgrond','moerige_klei') & A_P_CC <= 0.2, value := exp(3.0851 + 0.6554 * log(A_P_CC) - 0.1966 * log(A_P_CC)^2)]
      dt[B_SOILTYPE_AGR %in% c('duinzand','veen') & A_P_CC > 0.2, value := exp(3.1818 + 0.2645 * log(A_P_CC) + 0.0891 * log(A_P_CC)^2)]
      dt[B_SOILTYPE_AGR %in% c('dekzand','zeeklei','rivierklei') & A_P_CC> 0.2, value := exp(3.533 + 0.4736 * log(A_P_CC) + 0.02595 * log(A_P_CC)^2)]
      dt[B_SOILTYPE_AGR %in% c('maasklei','loess') & A_P_CC > 0.2, value := exp(3.3862 + 0.5962 * log(A_P_CC))]
      dt[B_SOILTYPE_AGR %in% c('dalgrond','moerige_klei') & A_P_CC > 0.2, value := exp(3.2267 + 0.4619 * log(A_P_CC))]
      
      # convert back to mg P/ kg soil (assume 3.5% SOM for bulk density estimation)
      dt[B_SOILTYPE_AGR %in% c('dalgrond','moerige_klei'), bd := 1 / (0.02525 * 15 + 0.6541)]
      dt[B_SOILTYPE_AGR =='veen', bd := 1 / (0.02525 * 20 + 0.6541)]
      dt[is.na(bd),bd := 1 / (0.02525 * 3.5 + 0.6541)]
      dt[is.na(A_P_WA), A_P_WA := value * 0.43646 / bd]
      
    }
  
  # estimate P from A_P_OL (all in mg P per kg soil)
  # https://doi.org/10.1016/j.geoderma.2021.115339, tables 3 and 5
  dt[is.na(A_P_AL) & !is.na(A_P_OL), A_P_AL := pmax(1.5,(A_P_OL - 21.9 + 3.19 * A_PH_CC)/0.275)]
  dt[is.na(A_P_AAA) & !is.na(A_P_OL), A_P_AAA := 10^(log10((A_P_OL + 56.9)/54.9)/0.2824)]
  dt[is.na(A_P_AAA_EDTA) & !is.na(A_P_OL), A_P_AAA_EDTA := A_P_OL/ mean(0.4,0.5,0.79,0.4,0.25)]
  dt[is.na(A_P_CAL) & !is.na(A_P_OL), A_P_CAL := A_P_OL / 0.625]
  dt[is.na(A_P_DL) & !is.na(A_P_OL), A_P_DL := A_P_OL / 0.53]
  dt[is.na(A_P_WA) & !is.na(A_P_OL), A_P_WA := pmax(1.1,A_P_OL / mean(2.77,2.5,4,4,3,2.45))]
  dt[is.na(A_P_M3) & !is.na(A_P_OL), A_P_M3 := A_P_OL / 0.39]
  
  # estimate P Morgan (mg P/L) using two PDFs
  # DOI: 10.1002/jpln.202100194
  dt[is.na(A_P_MORGAN) & is.na(A_P_OL), A_P_MORGAN := 0.5 * ((1.368 - 0.173 * A_P_OL + 0.0649 * A_PH_CC) + exp(log(A_P_OL/5.96)/0.773))]
  
  # to do: add relationship
  dt[is.na(A_P_CC) & !is.na(A_P_OL), A_P_CC := A_P_OL/10]
  
  # ensure that values are higher than minimum
  
    # load internal table for all euosi parameters
    dtp <- as.data.table(euosi::osi_parms)
    
    # do checks and replace minima
    dt[, A_P_AL := pmax(dtp[osi_parm_name=='A_P_AL',osi_parm_min], A_P_AL)]
    dt[, A_P_AAA := pmax(dtp[osi_parm_name=='A_P_AAA',osi_parm_min], A_P_AAA)]
    dt[, A_P_AAA_EDTA := pmax(dtp[osi_parm_name=='A_P_AAA_EDTA',osi_parm_min], A_P_AAA_EDTA)]
    dt[, A_P_CAL := pmax(dtp[osi_parm_name=='A_P_CAL',osi_parm_min], A_P_CAL)]
    dt[, A_P_DL := pmax(dtp[osi_parm_name=='A_P_DL',osi_parm_min], A_P_DL)]
    dt[, A_P_WA := pmax(dtp[osi_parm_name=='A_P_WA',osi_parm_min], A_P_WA)]
    dt[, A_P_M3 := pmax(dtp[osi_parm_name=='A_P_M3',osi_parm_min], A_P_M3)]
    dt[, A_P_CC := pmax(dtp[osi_parm_name=='A_P_CC',osi_parm_min], A_P_CC)]
    dt[, A_P_MORGAN := pmax(A_P_MORGAN,0.5)]
    
    # do checks and replace maxima
    dt[, A_P_AL := pmin(dtp[osi_parm_name=='A_P_AL',osi_parm_max], A_P_AL)]
    dt[, A_P_AAA := pmin(dtp[osi_parm_name=='A_P_AAA',osi_parm_max], A_P_AAA)]
    dt[, A_P_AAA_EDTA := pmin(dtp[osi_parm_name=='A_P_AAA_EDTA',osi_parm_max], A_P_AAA_EDTA)]
    dt[, A_P_CAL := pmin(dtp[osi_parm_name=='A_P_CAL',osi_parm_max], A_P_CAL)]
    dt[, A_P_DL := pmin(dtp[osi_parm_name=='A_P_DL',osi_parm_max], A_P_DL)]
    dt[, A_P_WA := pmin(dtp[osi_parm_name=='A_P_WA',osi_parm_max], A_P_WA)]
    dt[, A_P_M3 := pmin(dtp[osi_parm_name=='A_P_M3',osi_parm_max], A_P_M3)]
    dt[, A_P_CC := pmin(dtp[osi_parm_name=='A_P_CC',osi_parm_max], A_P_CC)]
    dt[, A_P_MORGAN := pmin(A_P_MORGAN,40)]
    
  # check calculated inputs
  osi_checkvar(parm = list(A_P_AL = dt$A_P_AL, A_P_AAA = dt$A_P_AAA,A_P_AAA_EDTA=dt$A_P_AAA_EDTA,
                           A_P_CAL = dt$A_P_CAL,A_P_DL = dt$A_P_DL,A_P_WA = dt$A_P_WA,
                           A_P_M3 = dt$A_P_M3, A_P_CC = dt$A_P_CC),fname ='osi_conv_phosphor')
  
  # select the reqestred pH
  value <- dt[,get(element)]
  
  # return value
  return(value)
  
}

#' Estimate soil extractable potassium (-)
#' 
#' @param element (character) the method requested to be calculated
#' @param A_K_AAA (numeric) The exchangeable K-content of the soil measured via acid ammonium acetate extraction
#' @param A_K_AL (numeric) The K-content of the soil extracted with ammonium lactate
#' @param A_K_AN (numeric) The K-content of the soil extracted with ammonium nitrate (mg K /kg)
#' @param A_K_CAL (numeric) The K-content of the soil extracted with ammonium lactate(mg K / kg)
#' @param A_K_CC (numeric) The K-content of the soil extracted with CaCl2
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_K_DL (numeric) The K-content of the soil extracted with double lactate (mg K / kg)
#' @param A_K_M3 (numeric) The exchangeable K-content of the soil measured via Mehlich 3 extracton (mg K/ kg)
#' @param A_K_NaAAA (numeric) The K-content of the soil extracted with Morgan's solution, sodium acetate acetic acid (mg/ kg)
#' @param A_K_WA (numeric) The K-content of the soil extracted with water
#' @param A_PH_CC (numeric) The pH measured in cacl2 
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil, measured via Cohenx (mmol+ / kg)
#' 
#' @export 
osi_conv_potassium <- function(element, 
                              A_K_AAA = NA_real_,A_K_AL = NA_real_,A_K_AN = NA_real_,A_K_CAL = NA_real_,
                              A_K_CC = NA_real_,A_K_CO_PO = NA_real_, A_K_DL = NA_real_, 
                              A_K_M3 = NA_real_,A_K_NaAAA = NA_real_,A_K_WA = NA_real_,
                              A_CEC_CO = NA_real_,A_PH_CC = NA_real_){
  
  # add visual bindings
  osi_parm_name = osi_parm_min = osi_parm_max = A_PH_WA = NULL
  
  # check inputs
  checkmate::assert_subset(element,choices = c('A_K_AL','A_K_AN','A_K_CAL','A_K_CC',
                                               'A_K_CO_PO','A_K_DL','A_K_M3',
                                               'A_K_NaAAA','A_K_WA'),empty.ok = FALSE)
   
  # make internal table with inputs
  dt <- data.table(A_K_AAA = A_K_AAA,
                   A_K_AL = A_K_AL,
                   A_K_AN = A_K_AN,
                   A_K_CAL = A_K_CAL,
                   A_K_CC = A_K_CC,
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_DL = A_K_DL,
                   A_K_M3 = A_K_M3,
                   A_K_NaAAA = A_K_NaAAA,
                   A_K_WA = A_K_WA,
                   A_CEC_CO = A_CEC_CO,
                   A_PH_CC = A_PH_CC,
                   A_PH_WA = NA_real_
                   )
  
  # check required inputs
  osi_checkvar(parm = list(A_K_AAA = dt$A_K_AAA, A_PH_CC = dt$A_PH_CC,
                           A_CEC_CO = dt$A_CEC_CO),fname ='osi_conv_potassium')
  
  # estimate pH water from pH-CaCl2
  dt[,A_PH_WA := osi_conv_ph('A_PH_WA',A_PH_CC = A_PH_CC)]
  
  # estimate K from other measurements (note: these are not the best ones, but good ones are rare)
  
  # derive from Angelova et al. (2021), assume linearity
  dt[is.na(A_K_AL) & !is.na(A_K_AAA), A_K_AL := pmax(10,(A_K_AAA - 45 * 0.8301)) * 55 / 45] 
  dt[is.na(A_K_AN) & !is.na(A_K_AAA), A_K_AN := A_K_AAA]
  dt[is.na(A_K_CAL) & !is.na(A_K_AAA), A_K_CAL := A_K_AL]
  
  # derived from Zebect et al. (2017) assuming linearity (divided by 10 by GR since valus are far to high)
  dt[is.na(A_K_CC) & !is.na(A_K_AAA), A_K_CC := 0.1 * (A_K_AAA + 50 * 0.8301) * 55 / 13]
  
  # correction function developed by Gerard for OCP
  dt[is.na(A_K_CO_PO) & !is.na(A_K_AAA), A_K_CO_PO := (A_K_AAA / 39.0983 / fifelse(A_PH_WA <7, 1.184,1.175))*100/A_CEC_CO]
  
  # pedotransfer function from Breure et al. (2022)
  dt[is.na(A_K_M3) & !is.na(A_K_AAA), A_K_M3 := (A_K_AAA - 15.21 + 2.12 * A_PH_WA)/1.01]
  
  # pedotransfer function from Loide et al. (2005)
  dt[is.na(A_K_DL) & !is.na(A_K_AAA), A_K_DL := A_K_M3 / 1.319]
  
  # unknown, assume equality
  dt[is.na(A_K_NaAAA) & !is.na(A_K_AAA), A_K_NaAAA := A_K_AAA]
  dt[is.na(A_K_WA) & !is.na(A_K_AAA), A_K_WA := A_K_AAA]
  
  # ensure that values are higher than minimum
  
    # load internal table for all euosi parameters
    dtp <- as.data.table(euosi::osi_parms)
    
    # do checks and replace minima
    dt[, A_K_AL := pmax(dtp[osi_parm_name=='A_K_AL',osi_parm_min], A_K_AL)]
    dt[, A_K_AN := pmax(dtp[osi_parm_name=='A_K_AN',osi_parm_min], A_K_AN)]
    dt[, A_K_CAL := pmax(dtp[osi_parm_name=='A_K_CAL',osi_parm_min], A_K_CAL)]
    dt[, A_K_CC := pmax(dtp[osi_parm_name=='A_K_CC',osi_parm_min], A_K_CC)]
    dt[, A_K_CO_PO := pmax(dtp[osi_parm_name=='A_K_CO_PO',osi_parm_min], A_K_CO_PO)]
    dt[, A_K_DL := pmax(dtp[osi_parm_name=='A_K_DL',osi_parm_min], A_K_DL)]
    dt[, A_K_NaAAA := pmax(dtp[osi_parm_name=='A_K_NaAAA',osi_parm_min], A_K_NaAAA)]
    dt[, A_K_WA := pmax(dtp[osi_parm_name=='A_K_WA',osi_parm_min], A_K_WA)]
  
    # do checks and replace maxima
    dt[, A_K_AL := pmin(dtp[osi_parm_name=='A_K_AL',osi_parm_max], A_K_AL)]
    dt[, A_K_AN := pmin(dtp[osi_parm_name=='A_K_AN',osi_parm_max], A_K_AN)]
    dt[, A_K_CAL := pmin(dtp[osi_parm_name=='A_K_CAL',osi_parm_max], A_K_CAL)]
    dt[, A_K_CC := pmin(dtp[osi_parm_name=='A_K_CC',osi_parm_max], A_K_CC)]
    dt[, A_K_CO_PO := pmin(dtp[osi_parm_name=='A_K_CO_PO',osi_parm_max], A_K_CO_PO)]
    dt[, A_K_DL := pmin(dtp[osi_parm_name=='A_K_DL',osi_parm_max], A_K_DL)]
    dt[, A_K_NaAAA := pmin(dtp[osi_parm_name=='A_K_NaAAA',osi_parm_max], A_K_NaAAA)]
    dt[, A_K_WA := pmin(dtp[osi_parm_name=='A_K_WA',osi_parm_max], A_K_WA)]
    
  # check calculated inputs
  osi_checkvar(parm = list(A_PH_WA = dt$A_PH_WA, A_K_AL = dt$A_K_AL, A_K_AN = dt$A_K_AN,
                           A_K_CAL = dt$A_K_CAL, A_K_CC = dt$A_K_CC, A_K_CO_PO = dt$A_K_CO_PO,
                           A_K_M3 = dt$A_K_M3, A_K_DL = dt$A_K_DL, A_K_NaAAA = dt$A_K_NaAAA,
                           A_K_WA = dt$A_K_WA),fname ='osi_conv_potassium')
  
  # select the requested element
  value <- dt[,get(element)]
  
  # return value
  return(value)
  
}

#' Estimate soil extractable magnesium (-)
#' 
#' @param element (character) the method requested to be calculated
#' @param A_MG_AAA (numeric) The exchangeable K-content of the soil measured via acid ammonium acetate extraction
#' @param A_MG_AL (numeric) The exchangeable Mg-content of the soil measured via Ammonium Lactate extraction (mg Mg/ kg)
#' @param A_MG_AN (numeric) The Mg-content of the soil extracted with ammonium nitrate (mg Mg /kg)
#' @param A_MG_CC (numeric) The plant available content of Mg in the soil (mg  Mg per kg) extracted by 0.01M CaCl2
#' @param A_MG_CO_PO (numeric) The exchangeable Mg-content of the soil measured via Cohex extracton, percentage occupation at CEC (\%)
#' @param A_MG_DL (numeric) The exchangeable Mg-content of the soil measured via Double Lactate extraction (mg Mg/ kg)
#' @param A_MG_KCL (numeric) The plant available potassium, extracted with KCL (mg per kg)
#' @param A_MG_M3 (numeric) The exchangeable Mg-content of the soil measured via Mehlich 3 extracton (mg Mg/ kg)
#' @param A_MG_NaAAA (numeric) The Mg-content of the soil extracted with Morgan's solution, sodium acetate acetic acid (mg/ kg)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil, measured via Cohenx (mmol+ / kg)
#' @param A_PH_CC (numeric) The pH measured in cacl2 
#' 
#' @export 
osi_conv_magnesium <- function(element, 
                               A_MG_AAA = NA_real_,A_MG_AL = NA_real_, A_MG_AN = NA_real_,A_MG_CC = NA_real_,
                               A_MG_CO_PO = NA_real_, A_MG_DL = NA_real_,A_MG_KCL = NA_real_,A_MG_M3 = NA_real_, 
                               A_MG_NaAAA = NA_real_,
                               A_CEC_CO = NA_real_,A_PH_CC = NA_real_){
  
  # add visual bindings
  osi_parm_name = osi_parm_min = osi_parm_max = NULL
  
  # check inputs
  checkmate::assert_subset(element,choices = c('A_MG_AL','A_MG_AN','A_MG_CC','A_MG_CO_PO',
                                               'A_MG_DL','A_MG_KCL','A_MG_M3','A_MG_NaAAA'),empty.ok = FALSE)
  
  
  # make internal table with inputs
  dt <- data.table(A_MG_AAA = A_MG_AAA,
                   A_MG_AL = A_MG_AL,
                   A_MG_AN = A_MG_AN,
                   A_MG_CC = A_MG_CC,
                   A_MG_CO_PO = A_MG_CO_PO,
                   A_MG_DL = A_MG_DL,
                   A_MG_KCL = A_MG_KCL,
                   A_MG_M3 = A_MG_M3,
                   A_MG_NaAAA = A_MG_NaAAA,
                   A_CEC_CO = A_CEC_CO,
                   A_PH_CC = A_PH_CC)
  
  # check required inputs
  osi_checkvar(parm = list(A_MG_AAA = dt$A_MG_AAA, A_PH_CC = dt$A_PH_CC,
                           A_CEC_CO = dt$A_CEC_CO),fname ='osi_conv_magnesium')
  
  # estimate Mg from other measurements (note: these are not the best ones, but good ones are rare)
  
  # relationships from Staugaitis & Rutkuskiene (2010)
  dt[is.na(A_MG_AL) & !is.na(A_MG_AAA) & A_MG_AAA <= 400, A_MG_AL := 1.291 * A_MG_AAA + 65.18]
  dt[is.na(A_MG_AL) & !is.na(A_MG_AAA) & A_MG_AAA > 400, A_MG_AL := 4.49 * A_MG_AAA + 386.95]
  dt[is.na(A_MG_CC) & !is.na(A_MG_AAA), A_MG_CC := (A_MG_AAA + 30.49)/1.287] 
  dt[is.na(A_MG_KCL) & !is.na(A_MG_AAA), A_MG_KCL := (A_MG_AAA + 12.35)/1.03]
  dt[is.na(A_MG_M3) & !is.na(A_MG_AAA), A_MG_M3 := (A_MG_AAA + 3.74)/0.887]
  
  # unknown, estimate from comparable methodologies
  dt[is.na(A_MG_AN) & !is.na(A_MG_AAA), A_MG_AN := A_MG_AAA]
  dt[is.na(A_MG_CO_PO) & !is.na(A_MG_AAA), A_MG_CO_PO := A_MG_AAA * (2 / 24.305) * 100 / A_CEC_CO]
  dt[is.na(A_MG_DL) & !is.na(A_MG_AAA), A_MG_DL := A_MG_AL]
  dt[is.na(A_MG_NaAAA) & !is.na(A_MG_AAA), A_MG_NaAAA := A_MG_AAA]
  
  # ensure that values are higher than minimum
  
    # load internal table for all euosi parameters
    dtp <- as.data.table(euosi::osi_parms)
  
    # do checks and replace minima
    dt[, A_MG_AL := pmax(dtp[osi_parm_name=='A_MG_AL',osi_parm_min], A_MG_AL)]
    dt[, A_MG_CC := pmax(dtp[osi_parm_name=='A_MG_CC',osi_parm_min], A_MG_CC)]
    dt[, A_MG_KCL := pmax(dtp[osi_parm_name=='A_MG_KCL',osi_parm_min], A_MG_KCL)]
    dt[, A_MG_M3 := pmax(dtp[osi_parm_name=='A_MG_M3',osi_parm_min], A_MG_M3)]
    dt[, A_MG_AN := pmax(dtp[osi_parm_name=='A_MG_AN',osi_parm_min], A_MG_AN)]
    dt[, A_MG_CO_PO := pmax(dtp[osi_parm_name=='A_MG_CO_PO',osi_parm_min], A_MG_CO_PO)]
    dt[, A_MG_DL := pmax(dtp[osi_parm_name=='A_MG_DL',osi_parm_min], A_MG_DL)]
    dt[, A_MG_NaAAA := pmax(dtp[osi_parm_name=='A_MG_NaAAA',osi_parm_min], A_MG_NaAAA)]
    
    # do checks and replace maxima
    dt[, A_MG_AL := pmin(dtp[osi_parm_name=='A_MG_AL',osi_parm_max], A_MG_AL)]
    dt[, A_MG_CC := pmin(dtp[osi_parm_name=='A_MG_CC',osi_parm_max], A_MG_CC)]
    dt[, A_MG_KCL := pmin(dtp[osi_parm_name=='A_MG_KCL',osi_parm_max], A_MG_KCL)]
    dt[, A_MG_M3 := pmin(dtp[osi_parm_name=='A_MG_M3',osi_parm_max], A_MG_M3)]
    dt[, A_MG_AN := pmin(dtp[osi_parm_name=='A_MG_AN',osi_parm_max], A_MG_AN)]
    dt[, A_MG_CO_PO := pmin(dtp[osi_parm_name=='A_MG_CO_PO',osi_parm_max], A_MG_CO_PO)]
    dt[, A_MG_DL := pmin(dtp[osi_parm_name=='A_MG_DL',osi_parm_max], A_MG_DL)]
    dt[, A_MG_NaAAA := pmin(dtp[osi_parm_name=='A_MG_NaAAA',osi_parm_max], A_MG_NaAAA)]
    
  # check calculated inputs
  osi_checkvar(parm = list(A_MG_AL = dt$A_MG_AL, A_MG_CC = dt$A_MG_CC, A_MG_KCL = dt$A_MG_KCL,
                           A_MG_M3 = dt$A_MG_M3, A_MG_AN = dt$A_MG_AN, A_MG_CO_PO = dt$A_MG_CO_PO,
                           A_MG_DL = dt$A_MG_DL, A_MG_NaAAA = dt$A_MG_NaAAA),fname ='osi_conv_magnesium')
  
  # select the requested element
  value <- dt[,get(element)]
  
  # return value
  return(value)
  
}

#' Estimate soil extractable zinc (-)
#' 
#' @param element (character) the method requested to be calculated
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_ZN_RT (numeric) The total Zn-content of the soil via XRF or Dumas (mg Zn/kg)
#' @param A_ZN_AAA (numeric) The exchangeable Zn-content of the soil measured via acid amonium acetate (mg Zn/kg)
#' @param A_ZN_CC (numeric) The exchangeable Zn-content of the soil measured via 0.01M CaCl2 (ug Zn/kg)
#' @param A_ZN_CO (numeric) The exchangeable Zn-content of the soil measured via Cohex extraction (mg Zn/kg)
#' @param A_ZN_DTPA (numeric) The exchangeable Zn-content of the soil measured via DTPA (mg Zn/kg)
#' @param A_ZN_EDTA (numeric) The exchangeable Zn-content of the soil measured via EDTA (mg Zn/kg)
#' @param A_ZN_M3 (numeric) The exchangeable Zn-content of the soil measured via Mehlich-III (mg Zn/kg)
#' @param A_ZN_WA (numeric) The exchangeable Zn-content of the soil measured via water (mg Zn/kg)
#'  
#' @export 
osi_conv_zinc <- function(element, 
                          A_SOM_LOI,A_PH_CC, A_ZN_RT,A_ZN_AAA = NA_real_,
                          A_ZN_CC = NA_real_,A_ZN_CO = NA_real_,     
                          A_ZN_DTPA = NA_real_, A_ZN_EDTA = NA_real_,
                          A_ZN_M3 = NA_real_, A_ZN_WA = NA_real_){
  
  # add visual bindings
  A_PH_WA = A_C_OF = NULL
  
  # check inputs
  checkmate::assert_subset(element,choices = c('A_ZN_CC','A_ZN_CO', 'A_ZN_DTPA','A_ZN_AAA',
                                               'A_ZN_M3','A_ZN_WA','A_ZN_EDTA'),empty.ok = FALSE)
  
  # make internal table with inputs
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = NA_real_,
                   A_ZN_RT = A_ZN_RT,
                   A_PH_CC = A_PH_CC,
                   A_PH_WA = NA_real_,
                   A_ZN_AAA = A_ZN_AAA,
                   A_ZN_CC = A_ZN_CC,
                   A_ZN_CO = A_ZN_CO,
                   A_ZN_DTPA = A_ZN_DTPA,
                   A_ZN_EDTA = A_ZN_EDTA,
                   A_ZN_M3 = A_ZN_M3,
                   A_ZN_WA = A_ZN_WA)
  
  # check required inputs
  osi_checkvar(parm = list(A_ZN_CC = dt$A_ZN_CC,
                           A_ZN_CO = dt$A_ZN_CO,
                           A_ZN_DTPA = dt$A_ZN_DTPA,
                           A_ZN_M3 = dt$A_ZN_M3,
                           A_ZN_WA = dt$A_ZN_WA,
                           A_ZN_AAA = dt$A_ZN_AAA
                           ),
               fname ='osi_conv_zinc')
  
  # estimate Zn from other measurements (temporary, needs an update)
  
  # estimate pH water and A_C_OF
  dt[is.na(A_PH_WA) & !is.na(A_PH_CC), A_PH_WA := osi_conv_ph(element='A_PH_WA',A_PH_CC = A_PH_CC)]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 * 0.5]
  
  # own dataset Gerard (n = 6000, worldwide samples, R2 = 0.43 (for CO) amd 0.23 (for M3)
  dt[is.na(A_ZN_CO) & !is.na(A_ZN_RT), A_ZN_CO := 65.38*0.5*exp(5.36310 + 0.43778 * log(A_ZN_RT) - 6.83436 * log(A_PH_WA) + 0.29749 * log(A_C_OF))]
  dt[is.na(A_ZN_M3) & !is.na(A_ZN_RT), A_ZN_M3 := exp(-1.72904 + 0.46599 * log(A_ZN_RT) - 0.28679 * log(A_PH_WA) + 0.48858 * log(A_C_OF))]
  
  # own dataset Gerard (n = 6000, worldwide samples, R2 = 0.56-0.52), both in mg/kg
  dt[is.na(A_ZN_M3) & !is.na(A_ZN_WA), A_ZN_M3 := exp(-0.50807 + 1.14445 * log(A_ZN_WA) + 1.6462 * log(A_PH_WA) + 0.45322 * log(A_C_OF))]
  dt[!is.na(A_ZN_M3) & is.na(A_ZN_WA), A_ZN_WA := exp(-0.382704 + 0.410019 * log(A_ZN_M3) - 1.251069 * log(A_PH_WA) - 0.083771 * log(A_C_OF))]
  
  # own dataset Gerard (n = 6000, worldwide samples, R2 = 0.44-0.63), Cohex was in mmol+/kg
  dt[!is.na(A_ZN_CO) & is.na(A_ZN_WA), A_ZN_WA := exp(-2.983367 + 0.351938 * log(A_ZN_CO*2/65.38) + 1.091454 * log(A_PH_WA) + 0.026521 * log(A_C_OF))]
  dt[is.na(A_ZN_CO) & !is.na(A_ZN_WA), A_ZN_CO := 65.38*0.5*exp(6.54734 + 1.10855 * log(A_ZN_WA) - 4.98216 * log(A_PH_WA) + 0.25785 * log(A_C_OF))]
  
  # Wang et al. (2006), Louisiana soils, https://doi/10.1081/CSS-120027640
  dt[is.na(A_ZN_M3) & !is.na(A_ZN_DTPA), A_ZN_M3 := 1.686 * A_ZN_DTPA + 0.171]
  dt[!is.na(A_ZN_M3) & is.na(A_ZN_DTPA), A_ZN_DTPA := (A_ZN_M3 - 0.171)/1.686]
  
  # set A_ZN_WA equal to A_ZN_CC given similarity in extraction conditions (in units ug/kg)
  dt[is.na(A_ZN_CC),A_ZN_CC := A_ZN_WA * 1000]

  # set EDTA equal to 5 times DTPA (rough estimate, Han et al., 2020) 
  dt[is.na(A_ZN_EDTA) & !is.na(A_ZN_DTPA), A_ZN_EDTA := 5 * A_ZN_DTPA]
  
  # set ammonium acetate extraction equal to M3 (for the moment)
  dt[is.na(A_ZN_AAA) & !is.na(A_ZN_M3), A_ZN_AAA := A_ZN_M3]
  
  # select the requested element
  value <- dt[,get(element)]
  
  # return value
  return(value)
  
}
