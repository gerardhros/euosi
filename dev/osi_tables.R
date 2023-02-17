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
  
  # save updated crop table
  usethis::use_data(osi_countries,overwrite = TRUE)
          