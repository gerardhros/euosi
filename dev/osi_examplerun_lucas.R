# require packages
require(data.table)

# read in the data.file
d1 <- fread('D:/ESA/04 articles/2024/25 obi jrc/data/SAISdataset 20250820_GR.csv')

# preprocessing

  # remove cases with NA
  d1 <- d1[!(is.na(A_CLAY_MI)|is.na(A_C_OF)|is.na(A_N_RT)|is.na(A_EC_WA))]
  
  # replace NA with zero and set A_SOM_LOI to max 95%
  d1[is.na(A_CACO_IF), A_CACO_IF := 0]
  d1[A_SOM_LOI > 95, A_SOM_LOI := 95]
  
  # adjust notation B_LU and then class
  d1[,B_LU := format(B_LU, scientific = FALSE)]
  d1[,B_LU := as.character(B_LU)]

  # convert three crop codes that are not available in package to NA and remove spaces
  d1[!B_LU %in% osi_crops$crop_code, B_LU := NA_character_]
  
  # make subset
  d1 <- d1[!is.na(B_LU)]
  
  # add missing properties
  d1[,B_SOILTYPE_AGR := osi_get_SOILTYPE_AGR(A_CLAY_MI,A_SAND_MI,A_SOM_LOI,A_PH_CC)]
  d1[,A_CA_CO_PO := NA_real_]
  d1[,A_MG_CO_PO := NA_real_]
  d1[,A_K_CO_PO := NA_real_]
  d1[,A_NA_CO_PO := NA_real_]
  d1[,A_PH_KCL := NA_real_]
  
  # make subest per country
  AT <- d1[B_COUNTRY == 'AT',]
  BE <- d1[B_COUNTRY == 'BE',]
  BG <- d1[B_COUNTRY == 'BG',]
  CZ <- d1[B_COUNTRY == 'CZ',]
  DE <- d1[B_COUNTRY == 'DE',]
  ES <- d1[B_COUNTRY == 'ES',]
  FR <- d1[B_COUNTRY == 'FR',]
  IT <- d1[B_COUNTRY == 'IT',]
  LU <- d1[B_COUNTRY == 'LU',]
  NL <- d1[B_COUNTRY == 'NL',]
  PT <- d1[B_COUNTRY == 'PT',]
  SE <- d1[B_COUNTRY == 'SE',]
  SK <- d1[B_COUNTRY == 'SK',]


# euosi - chemical soil functions ----

  # make copy
  results <- copy(d1)
  
  # analysis pH via pH wrapper for all countries
  results[,osi_c_ph := osi_c_ph(B_LU = B_LU, 
                                B_SOILTYPE_AGR = B_SOILTYPE_AGR, 
                                A_CLAY_MI = A_CLAY_MI,
                                A_SAND_MI = A_SAND_MI,
                                A_SOM_LOI = A_SOM_LOI,
                                A_C_OF = A_C_OF,
                                A_CA_CO_PO =A_CA_CO_PO,
                                A_MG_CO_PO = A_MG_CO_PO,
                                A_K_CO_PO = A_K_CO_PO,
                                A_NA_CO_PO = A_NA_CO_PO,
                                A_PH_WA = A_PH_WA,
                                A_PH_CC = A_PH_CC,
                                A_PH_KCL = A_PH_KCL,
                                B_COUNTRY = B_COUNTRY)]
          
  # a few remarks
  # note that you sometimes uses NA. Its better to use NA_real_ for numeric and NA_character for characters. This avoids errors since a single value NA is considered to be a logical by data.table.
  
  # analysis phosphorus via pH wrapper for all countries
  # note that all not-Olsen methods are already defined as NA in function call, so no need to add (though its possible)
  # as example, i only leave one in below (A_P_AL)
  # note that in your file the carbonate is called 'A_CACO_IF' and not 'A_CACO3_IF'
  # when looking to results, osi_c_phosphor is NA when land use is nature or when P_OLSEN is missing
  results[,osi_c_phosphor :=  osi_c_phosphor(B_LU = B_LU,
                                             B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                             A_CLAY_MI = A_CLAY_MI,
                                             A_SAND_MI = A_SAND_MI,
                                             A_SOM_LOI = A_SOM_LOI,
                                             A_C_OF = A_C_OF,
                                             A_PH_WA = A_PH_WA,
                                             A_PH_CC = A_PH_CC,
                                             A_CACO3_IF = A_CACO_IF,
                                             A_P_AL = rep(NA_real_, length(nrow(results))),
                                             A_P_OL = A_P_OL,
                                             B_COUNTRY = B_COUNTRY)]
  # check the output (which countries and land use categories are NA)
  results[is.na(osi_c_phosphor),table(B_COUNTRY,B_LU_cat)]

  # nitrogen
  # note: i changed the max for carbonates, but a percentage of almost 100% seems quite unrealistic to me
  results[,osi_c_nitrogen := osi_c_nitrogen(B_LU = B_LU,
                                            B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                            A_CLAY_MI = A_CLAY_MI,
                                            A_SAND_MI = A_SAND_MI,
                                            A_SOM_LOI = A_SOM_LOI,
                                            A_C_OF = A_C_OF,
                                            A_N_RT = A_N_RT,
                                            A_CACO3_IF = A_CACO_IF,
                                            B_COUNTRY = B_COUNTRY)]
  
  # potassium
  results[,osi_c_potassium := osi_c_potassium(B_LU = B_LU,
                                              B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                              A_SOM_LOI = A_SOM_LOI,
                                              A_C_OF = A_C_OF,
                                              A_CLAY_MI = A_CLAY_MI,
                                              A_SAND_MI = A_SAND_MI,
                                              A_PH_CC = A_PH_CC,
                                              A_PH_WA = A_PH_WA,
                                              A_CACO3_IF = A_CACO_IF,
                                              A_CEC_CO = A_CEC_CO,
                                              A_K_AAA = A_K_AAA,
                                              B_COUNTRY = B_COUNTRY
                                            )]
  
  # check the output (which countries and land use categories are NA)
  results[is.na(osi_c_potassium),table(B_COUNTRY,B_LU_cat)]
  
  
# euosi - physical soil functions ----

  # Water holding capacity
  results[,osi_p_whc := osi_p_whc(A_CLAY_MI = A_CLAY_MI, 
                                  A_SAND_MI = A_SAND_MI, 
                                  A_SOM_LOI = A_SOM_LOI, 
                                  type = "water holding capacity", ptf = "Wosten1999")]
  # check the output (which countries and land use categories are NA)
  results[is.na(osi_p_whc),table(B_COUNTRY,B_LU_cat)]
  
  # Crumbability
  results[,osi_p_crumbleability := osi_p_crumbleability(B_LU = B_LU, 
                                                       A_CLAY_MI = A_CLAY_MI,
                                                       A_SOM_LOI = A_SOM_LOI,
                                                       A_PH_CC = A_PH_CC,
                                                       B_COUNTRY = B_COUNTRY)]
  # check the output (which countries and land use categories are NA)
  results[is.na(osi_p_whc),table(B_COUNTRY,B_LU_cat)]
  
# euosi - biological soil functions ---- 
  

  # biodiversity conditions
  results[, osi_biodiversity :=  osi_biodiversity(A_SOM_LOI = A_SOM_LOI, A_PH_CC = A_PH_CC)]
  
  # potentially mineralable N / microbial activity
  results[,i_b_pmn := osi_b_pmn(B_LU = B_LU,
                                B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                A_CLAY_MI = A_CLAY_MI, 
                                A_N_PMN = NA_real_,
                                A_N_RT = A_N_RT,
                                B_COUNTRY = B_COUNTRY)]
  # check the output (which countries and land use categories are NA)
  results[is.na(i_b_pmn),table(B_COUNTRY,B_LU_cat)]
  

# euosi - environmental soil functions ---- 
  
  # risk for water erosion
  results[,i_e_watererosie := osi_erosion(B_LU = B_LU, A_SOM_LOI = A_SOM_LOI,
                                     A_CLAY_MI = A_CLAY_MI, A_SAND_MI=A_SAND_MI, 
                                     B_RUSL_RE = B_RE,B_RUSL_SE= B_S,B_RUSL_LS= B_LS,B_RUSL_CM= NA_real_,
                                     B_COUNTRY = B_COUNTRY)]
  
  # check the output (which countries and land use categories are NA)
  results[is.na(i_e_watererosie),table(B_COUNTRY,B_LU_cat)]
  
  # carbon
  results[,osi_carbon := osi_carbon(B_LU = B_LU, A_C_OF = A_C_OF, B_BGZ = as.character(B_BGZ), 
                                    A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, B_COUNTRY = B_COUNTRY)]
  
  # check the output (which countries and land use categories are NA)
  results[is.na(osi_carbon),table(B_COUNTRY,B_LU_cat)]

  # nitrate leaching
  results[,osi_gw_nleach := osi_gw_nleach(B_LU = B_LU,
                                          A_CLAY_MI = A_CLAY_MI,
                                          A_SAND_MI = A_SAND_MI,
                                          A_CACO3_IF = A_CACO_IF,
                                          A_N_RT = A_N_RT,
                                          A_C_OF = A_C_OF,
                                          B_PREC_SUM = B_PRECIPITATION_SUM,
                                          B_PREC_WIN = B_PRECIPITATION_WIN,
                                          B_PET_SUM = B_PET_SUM*-1,
                                          B_PET_WIN = B_PET_WIN*-1,
                                          B_TEMP_SUM = B_TEMPERATURE_SUM,
                                          B_TEMP_WIN = B_TEMPERATURE_WIN,
                                          B_COUNTRY = B_COUNTRY
                                        )]
  # check the output (which countries and land use categories are NA)
  results[is.na(osi_gw_nleach),table(B_COUNTRY,B_LU_cat)]
  

  # phosphorus excess 
  results[,osi_nut_p := osi_nut_p(B_LU = B_LU,
                                  B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                                  A_CLAY_MI = A_CLAY_MI,
                                  A_SAND_MI = A_SAND_MI,
                                  A_C_OF = A_C_OF,
                                  A_SOM_LOI = A_SOM_LOI,
                                  A_PH_WA = A_PH_WA,
                                  A_PH_CC = A_PH_CC,
                                  A_CACO3_IF = A_CACO_IF,
                                  A_P_OL = A_P_OL,
                                  B_COUNTRY = B_COUNTRY
                                )]
  
  # check the output (which countries and land use categories are NA)
  results[is.na(osi_nut_p) & !is.na(A_P_OL),table(B_COUNTRY,B_LU_cat)]
  
  # potassium excess risk indicator
  results[,osi_e_kexcess := osi_nut_k(B_LU = B_LU, B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                               A_SOM_LOI = A_SOM_LOI, A_C_OF = A_C_OF, 
                               A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,
                               A_PH_CC = A_PH_CC, 
                               A_CEC_CO = A_CEC_CO, 
                               A_K_AAA = A_K_AAA,
                               B_COUNTRY = B_COUNTRY)]
  
  # check the output (which countries and land use categories are NA)
  results[is.na(osi_e_kexcess) & !is.na(A_K_AAA),table(B_COUNTRY,B_LU_cat)]
  
  