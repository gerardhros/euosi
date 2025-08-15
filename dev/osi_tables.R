# make internal table for bulk density

  # load internal packages
  require(data.table)
  require(usethis)
  
# make table for all countries (https://datahub.io/core/country-list#r)
  
    # get information where to find the location where csv data is stored
    # library("jsonlite")
    # json_file <- 'https://datahub.io/core/country-list/datapackage.json'
    # json_data <- fromJSON(paste(readLines(json_file), collapse=""))
    # json_data$resources$path
    
    # download data
    # d2 <- fread('https://pkgstore.datahub.io/core/country-list/data_csv/data/d7c9d7cfb42cb69f4422dec222dbbaa8/data_csv.csv', encoding = 'UTF-8')
  
  # loaddata
  d1 <- fread('dev/osi_countries.csv',encoding = 'UTF-8')
  
  # update column names
  setnames(d1,c('country_name','country_code','continent_code'))
  
  # set countries to lower case
  d1[, country_name := tolower(country_name)]

  # make copy for package table
  osi_countries <- copy(d1)
  
  # save updated country table
  usethis::use_data(osi_countries,overwrite = TRUE)

# make a table for all soil properties being used for calculating the OSI
  
  # loaddata
  osi_parms <- fread('dev/osi_parameters.csv',encoding = 'UTF-8')

  # save updated osi parameter table
  usethis::use_data(osi_parms,overwrite = TRUE)
  
# make a table for all soil types available, being country dependent
  
  # loaddata
  osi_soiltype <- fread('dev/osi_soiltype.csv',encoding = 'UTF-8')
  
  # save updated crop table
  usethis::use_data(osi_soiltype,overwrite = TRUE)
  
# make a table for all soil indicator evaluation functions, being country and soil function dependent
  
  # load data
  osi_thresholds <- fread('dev/osi_thresholds.csv',encoding = 'UTF-8',na.strings = c(NA_character_, "","NA"))
  
  # round numbers
  osi_thresholds[,osi_st_c1 := round(as.numeric(osi_st_c1),3)]
  osi_thresholds[,osi_st_c2 := round(as.numeric(osi_st_c2),3)]
  osi_thresholds[,osi_st_c3 := round(as.numeric(osi_st_c3),3)]
  
  # save updated threshold table
  usethis::use_data(osi_thresholds,overwrite = TRUE)

# make crop table with crop data for France, Belgium, Finland and the Netherlands
  
  # load crop data with crop codes per country
  osi_crops1 <- fread('dev/osi_crops_countrycode.csv',encoding = 'UTF-8',na.strings = c(NA_character_, "","NA"))
  
  # load crop data with crop codes standardized from IACS data
  osi_crops2 <- fread('dev/osi_crops_iacs.csv',encoding = 'UTF-8',na.strings = c(NA_character_, "","NA"))
  
  # combine both
  osi_crops <- rbind(osi_crops1[,.(osi_country,crop_code,
                                   crop_name = tolower(crop_name),
                                   crop_cat1 = tolower(crop_cat1),
                                   crop_cat2 = tolower(crop_cat2),
                                   crop_n,crop_p,crop_k,crop_c,crop_s,crop_crumbleability)],
                     osi_crops2[,.(osi_country,crop_code,
                                   crop_name = tolower(crop_name),
                                   crop_cat1 = tolower(crop_cat1),
                                   crop_cat2 = tolower(crop_cat2),
                                   crop_n,crop_p,crop_k,crop_c,crop_s,crop_crumbleability)])
  # save updated crop table
  usethis::use_data(osi_crops,overwrite = TRUE)

# make weather data table per country
  
  # load datasets
  prec <- as.data.table(readxl::read_xlsx('dev/osi_eu_weather.xlsx',sheet='b_prec_m'))
  pet <- as.data.table(readxl::read_xlsx('dev/osi_eu_weather.xlsx',sheet='b_pet_m'))
  temp <- as.data.table(readxl::read_xlsx('dev/osi_eu_weather.xlsx',sheet='b_temp_m'))

  # get country
  prec[, osi_country := substr(NUTS2,1,2)]
  pet[, osi_country := substr(NUTS2,1,2)]
  temp[, osi_country := substr(NUTS2,1,2)]
  
  # reformat to input in OSI functions
  osi_prec <- prec[,.(osi_country,
                      B_PREC_Y = round(Total),
                      B_PREC_SUM = round(M3 + M4 + M5 + M6 + M7 + M8),
                      B_PREC_WIN = round(M1 + M2 + M9 + M10 + M11 + M12))]
  osi_pet <- pet[,.(osi_country,
                      B_PET_Y = round(Total),
                      B_PET_SUM = round(M3 + M4 + M5 + M6 + M7 + M8),
                      B_PET_WIN = round(M1 + M2 + M9 + M10 + M11 + M12))]
  osi_temp <- temp[,.(osi_country,
                     B_TEMP_Y = round(average,2),
                     B_TEMP_SUM = round((M3 + M4 + M5 + M6 + M7 + M8)/6,2),
                     B_TEMP_WIN = round((M1 + M2 + M9 + M10 + M11 + M12)/6,2))]
  
  # calculate man per country
  osi_prec <- osi_prec[,lapply(.SD,mean),.SDcols = c('B_PREC_Y', 'B_PREC_SUM', 'B_PREC_WIN'),by='osi_country']
  osi_pet <- osi_pet[,lapply(.SD,mean),.SDcols = c('B_PET_Y', 'B_PET_SUM', 'B_PET_WIN'),by='osi_country']
  osi_temp <- osi_temp[,lapply(.SD,mean),.SDcols = c('B_TEMP_Y', 'B_TEMP_SUM', 'B_TEMP_WIN'),by='osi_country']
  
  # combine weather file
  osi_clim <- merge(osi_prec,osi_pet,by ='osi_country',all.x = TRUE)
  osi_clim <- merge(osi_clim,osi_temp,by ='osi_country',all.x = TRUE)
  
  # set to lower case
  setnames(osi_clim,tolower(colnames(osi_clim)))
  
  # save updated weather data table
  usethis::use_data(osi_clim,overwrite = TRUE)
  
# make output and input overview of variables
  
  # make oject with EU OSI input variables for main OSI field function
  dp <- osi_parms[,.(code = osi_parm_name,
                     parameter = osi_parm_description,
                     unit = osi_parm_unit,
                     data_type = osi_parm_data_type,	
                     value_min = osi_parm_min,
                     value_max = osi_parm_max,
                     categories = osi_parm_options)]
  
  # copy as bln_column_description
  osi_vars_input <- copy(dp)
  
  # save output variables
  usethis::use_data(osi_vars_input,overwrite = TRUE)
  
  # make object with EU OSI output variables
  
  cols <- c("i_b_biodiv","i_b_pmn","i_c_b","i_c_k","i_c_mg" ,         
            "i_c_n","i_c_p","i_c_ph","i_c_zn","i_e_carbon","i_e_kexcess" ,    
            "i_e_nleach","i_e_pexcess","i_e_watererosie","i_p_cr","i_p_dens","i_p_ksat" ,       
            "i_p_paw","i_p_wef","i_p_whc","s_euosi_ess_env","s_euosi_ess_prod", 
            "s_euosi_prod_b","s_euosi_prod_c","s_euosi_prod_p",
            "s_euosi_clim","s_euosi_nutcycle", "s_euosi_water","s_euosi_total")
  
  colsnames <- c('The soil indicator value reflecting distance to target for soil biodiversity in view of crop production',
                 'The soil indicator value reflecting distance to target for soil microbial activity in view of crop production',
                 'The soil indicator value reflecting distance to target for boron supply in view of crop production',
                 'The soil indicator value reflecting distance to target for potassium supply in view of crop production',
                 'The soil indicator value reflecting distance to target for magnesium supply in view of crop production',
                 'The soil indicator value reflecting distance to target for nitrogen supply in view of crop production',
                 'The soil indicator value reflecting distance to target for phosphorus supply in view of crop production',
                 'The soil indicator value reflecting distance to target for soil pH in view of crop production',
                 'The soil indicator value reflecting distance to target for zinc supply in view of crop production',
                 'The soil indicator value reflecting distance to target for carbon saturation in mineral soils in view carbon sequestration potential',
                 'The soil indicator value reflecting distance to target for potassium use efficiency in view of nutrient recycling and reuse',
                 'The soil indicator value reflecting distance to target for nitrogen leaching risks in view of groundwater regulation and purification',
                 'The soil indicator value reflecting distance to target for phosphorus use efficiency in view of nutrient recycling and reuse',
                 'The soil indicator value reflecting distance to target for water erodibility',
                 'The soil indicator value reflecting distance to target for crumbleability in view of crop production',
                 'The soil indicator value reflecting distance to target for subsoil compaction in view of crop production',
                 'The soil indicator value reflecting distance to target for water permeability in view of crop production',
                 'The soil indicator value reflecting distance to target for water stress in view of crop production',
                 'The soil indicator value reflecting distance to target for wind erodibility in view of crop production',
                 'The soil indicator value reflecting distance to target for water retention in view of crop production',
                 'The aggregated soil quality score for the ecosystem service Environment',
                 'The aggregated soil quality score for the ecosystem service Crop Production',
                 'The aggregated soil indicator value for biological soil functions supporting crop production',
                 'The aggregated soil indicator value for chemical soil functions supporting crop production',
                 'The aggregated soil indicator value for physical soil functions supporting crop production',
                 'The aggregated soil indicator value for soil functions supporting carbon sequestration and climate regulation',
                 'The aggregated soil indicator value for soil functions supporting nutrient recycling and reuse',
                 'The aggregated soil indicator value for soil functions supporting water regulation and purification',
                 'The total EU OSI soil quality score')
  
  # make output description
  dp <- data.table(code = cols,
                   parameter = colsnames)
  
  # copy as bln_column_description
  osi_vars_output <- copy(dp)
  
  # save output variables
  usethis::use_data(osi_vars_output,overwrite = TRUE)
  
  