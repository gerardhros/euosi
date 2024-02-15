#' Calculate the nitrogen surplus losses by surface runoff and leaching (wrapper function)
#' 
#' This function calculates the nitrogen losses by surface runoff and leaching (if available). 
#' 
#' @param A_CLAY_MI (numeric) The percentage of clay 
#' @param A_SAND_MI (numeric) The percentage of sand
#' @param B_LU (character) The crop code 
#' @param B_SLOPE_DEGREE (numeric) The slope degree
#' @param B_T (numeric) The mean annual temperature in degree Celcius
#' @param A_C_OF (numeric) The organic carbon content (g/kg)
#' @param B_TEXTURE_USDA (char) The textural class 
#' @param B_SOILTYPE_AGR
#' @param A_SOM_LOI
#' @param A_CN_FR
#' @param D_OC
#' @param A_N_RT
#' 
#' 
#' @import data.table
#' 
#' @examples 
#' 
osi_e_nitrogensurplus <- function(B_LU, 
                                  A_C_OF , A_CLAY_MI ,
                                  A_SAND_MI , B_T = NA_real_, B_COUNTRY) {
  
  # add visual bindings
  
  # note that qualitative checks on the inputs are done by the country specific functions
  
  # Check length of desired input
  arg.length <- max(length(B_LU),length(A_C_OF),length(A_CLAY_MI),
                    length(A_SAND_MI), length(B_SLOPE_DEGREE),length(B_T),length(B_COUNTRY))
  
  # Collect the data into a table
  dt <- data.table(id = 1:arg.length,
                   B_LU = B_LU,
                   A_C_OF = A_C_OF, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   B_T = B_T,
                   B_COUNTRY = B_COUNTRY,
                   n_supply = NA_real_,
                   n_surplus = NA_real_,
                   fle = NA_real_,
                   flu = NA_real_,
                   fp = NA_real_,
                   flemax = NA_real_,
                   ft = NA_real_,
                   fc = NA_real_,
                   PS = NA_real_,
                   value = NA_real_
  )
  
  # Load in the crops data set retain only relevant crop
  dt.crops <- as.data.table(euosi::osi_crops)
  dt.crops <- dt.crops[crop_code=='B_LU']
  
  dt <- merge(dt, 
              dt.crops[, list(crop_code, crop_cat1)], 
              by.x = "B_LU", 
              by.y = "crop_code", 
              all.x = TRUE)
  
  # Calculate the fraction of N soil surplus that is lost by leaching below the root zone
  
  
   # calculate the soil N supply for the NL using the Dutch OBIC
    #Calculate derivate soil properties
  dt[, D_BDS := OBIC::calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI)]
  dt[, D_RD := OBIC::calc_root_depth(B_LU_BRP = B_LU)]
  dt[, D_OC := OBIC::calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
  dt[, D_GA := OBIC::calc_grass_age(id, B_LU_BRP = B_LU)]
    #Calculate N supply
  dt[B_COUNTRY == 'NL', n_supply := OBIC::calc_nlv(B_LU_BRP = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                                                A_N_RT = A_N_RT, A_CN_FR = A_CN_FR, D_OC = D_OC, D_BDS = D_BDS, 
                                                D_GA = D_GA)]
    #Calculate N surplus 
  dt[B_COUNTRY == 'NL', n_surplus := ifelse(dt.crops$crop_cat1=='grassland',n_supply-140,ifelse(dt.crops$crop_cat1=='arable',n_supply-100,0))]
  
    # calculate N leaching
  dt[B_COUNTRY == 'NL', value := n_surplus*Fqsro*0.5]

  # calculate the N supply for FR
    #Calculate derivate soil properties
  dt[, D_BDS := 0.80806 + (0.823844*exp(0.0578*0.1*A_C_OF)) + (0.0014065 * A_SAND_MI) - (0.0010299 * A_CLAY_MI)] 
  dt[, D_NHA := A_N_RT * 0.2 * D_BDS * 10000 * 1000 * 10^-6]  
  
  # calculate the temperature and moisture content correction 
  dt[, fT := exp(0.115*(B_TEMP-15))]
  dt[, gW := 0.2 + (0.8 * ((B_SOILMOISTURE-Wwp)/(Wfc-Wwp)))]
  

   # calculate the N supplying capacity for France (kg N/ha/yr)
  dt[B_COUNTRY == 'FR', n_supply := ((22/((12+A_CLAY_MI)*(545+A_CACO3_IF))) * D_NHA)*21.35 * 0.33 * fT * gW]
  
    # calculate the N surplus
  dt[B_COUNTRY == 'FR', n_surplus  := ifelse(dt.crops$crop_cat1=='grassland',n_supply-140,ifelse(dt.crops$crop_cat1=='arable',n_supply-100,0))]
    # calculate N leaching
  dt[B_COUNTRY == 'FR', value := n_surplus*Fqsro*0.5]
  
  # calculate the leaching fraction 
    #fle max
  dt[B_TEXTURE_USDA == 'sand' | B_TEXTURE_USDA == 'sandy loam' | B_TEXTURE_USDA == 'sandy clay loam' , flemax := 1 ]
  dt[B_TEXTURE_USDA == 'loam' | B_TEXTURE_USDA == 'silt loam' | B_TEXTURE_USDA == 'silt'| B_TEXTURE_USDA == 'clay loam'| 
       B_TEXTURE_USDA == 'silty clay loam'| B_TEXTURE_USDA == 'sandy clay' , flemax := 0.75 ]
  dt[B_TEXTURE_USDA == 'clay' | B_TEXTURE_USDA == 'silty clay' , flemax := 0.50 ]
  dt[B_TEXTURE_USDA == 'clay' | B_TEXTURE_USDA == 'silty clay' , flemax := 0.50 ]
  dt[A_C_OF > 200 , flemax := 0.20 ]
    #flu
  dt$flu <- ifelse(dt$crop_cat1 == 'grassland', 0.85, 1)
    #fp
  dt[, PS := B_RAINFALL - B_TEMP]
  dt[PS < 50, fp := 0.25]
  dt[PS >= 50 & PS < 100, fp := 1+(PS-100)*0.015]
  dt[PS >= 100 & PS < 300, fp := 1]
  dt[PS >= 300 & PS < 400, fp := 1-(PS-300)*0.005]
  dt[PS >= 400, fp := 0.5]
    #ft
  dt[B_TEMP < 5, ft := 1]
  dt[B_TEMP >= 5 & B_TEMP <= 15, ft := 0.75]
  dt[B_TEMP > 15, ft := 0.50]
    #fc
  dt[A_C_OF/10 < 1, fc := 1]
  dt[A_C_OF/10 >= 1 & A_C_OF/10 < 2, fc := 0.90]
  dt[A_C_OF/10 >= 2 & A_C_OF/10 < 5, fc := 0.75]
  dt[A_C_OF/10 >= 5, fc := 0.50]
    #fle
  dt[, fle := flemax * flu * min(fp,ft,fc)]
  
    #Nle
  dt[, Nle := n_surplus * fle]
  
  dt[, value := evaluate_logistic(Nle, b=-0.39913, x0=5, v=1)]

  # sort data.table
  setorder(dt,id)
  
  # select the output variable
  out <- dt[,value]
  
  # return the OSI score
  return(out)
  
}
