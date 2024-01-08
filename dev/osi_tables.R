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
  osi_thresholds <- fread('dev/osi_thresholds.csv',encoding = 'UTF-8',dec=',')
  
  # save updated threshold table
  usethis::use_data(osi_thresholds,overwrite = TRUE)

# make crop table with crop data for France and the Netherlands
  
  # load crop datat
  osi_crops <-  fread('dev/osi_crops.csv',encoding = 'UTF-8',na.strings = c(NA_character_, "","NA"))
  
  # select only selected categories
  osi_crops <- osi_crops[,.(osi_country,crop_code,crop_name,crop_cat1,crop_cat2,crop_n,crop_p,crop_k)]
  
  # save updated crop table
  usethis::use_data(osi_crops,overwrite = TRUE)
  