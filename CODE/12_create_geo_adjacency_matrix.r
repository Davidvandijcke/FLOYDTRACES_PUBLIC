#**********************************************************************
# Compile Graph Data
#**********************************************************************

redo = 0 # redo data compilation?


if (redo ==1) {
  ## Get Data and Stack It
  geo_data_directory <- file.path(datainBlm, "safe_graph_geos", "2020")
  processed_data_directory <- dataout
  fig_directory <- figs
  
  # Specify the set of months used to generate geo-adj matrix. Only use
  # jan and feb, bc pre-COVID
  months <- c('05')
  
  # Cycle through months
  data_wrapped <- foreach(i = months, .combine='rbind') %do% {
      # Get the relevant directory
      directory = paste(geo_data_directory, i,sep="/")
      
      # How many days in each month?
      n_days = case_when(i == '05' ~ 29)
      
      # Loop through the days
      data_aggregate_month <- foreach(j = seq(19, n_days, 1), .combine='rbind') %do% { # start one week before protests
          # Specify a date string, and read in the relevant file
          day_string <- str_pad(as.character(j), 2, '0', side='left')
          file = str_c(directory, '/2020', as.character(i), as.character(day_string), '_cbg_graph.txt')
          data = read_tsv(file, 
                          col_types = cols(
                              X1 = col_integer(),
                              `origin-cbg` = col_integer(),
                              `dest-cbg` = col_integer(),
                              `num-devices` = col_integer(),
                              `cbg_adj_factor` = col_integer()
                          ))
          # Add the days date to the dataframe
          data <- data %>% 
            mutate(ds = str_c('2020-', i, '-', day_string))
          
          # Rename df so its easy to read
          names(data) <- c('rn', 'origin_cbg', 'destination_cbg', 'num_devices', 'cbg_adj_factor', 'ds')
  
          # Remove row number and return
          data <- data %>% dplyr::select(-rn)
      }
  }
  
  
  # Filter out any origin or destination counties
  # not in the 50 states (i.e., in US terrritories)
  geo_adj <- data_wrapped %>%
      dplyr::filter(origin_cbg <= 57000 & destination_cbg <= 57000)
  
  data <- NULL 
  data_wrapped <- NULL
  data_aggregate_month <- NULL
  
  
  
  # get city fips
  cit <- read.csv(file.path(dataout, "cbgProtestCities.csv"))
  colnames(cit)[colnames(cit)=="CBGFIPS"] <- "destination_cbg"
  cit <- cit %>% dplyr::select(city, destination_cbg)
  cit$destination_cbg <- str_pad(cit$destination_cbg, 12, side= "left", pad = 0)
 
  # left join
  geo_adj <- sqldf("SELECT origin_cbg, destination_cbg, num_devices, cbg_adj_factor
                   FROM geo_adj
                   LEFT JOIN cit USING(destination_cbg)")

   #geo_adj <- plyr::join(geo_adj, cit) # merge to adjacency data to get city names
  
  
  
  fwrite(geo_adj, file.path(dataout, "geo_adj2_stacked.csv.gz"), compress="gzip")
  

  ## Calculate Average # of Devices in Week Before
  avg <- geo_adj[(geo_adj$ds < as.Date("2020-05-26")) & (geo_adj$ds > as.Date("2020-05-18")),] %>%
    group_by(destination_cbg,ds) %>% dplyr::summarise(num_devices = sum(num_devices, na.rm = T))
  
  avg <- avg %>% group_by(destination_cbg) %>% dplyr::summarise(avg_devices = mean(num_devices,na.rm = T))

  fwrite(avg, file.path(dataout, "avg.csv.gz"), compress='gzip')

} 




#**********************************************************************
# MINNEAPOLIS
#**********************************************************************


# Minneapolis first day
first <- geo_adj[(as.Date(geo_adj$ds) == as.Date("2020-05-26")),]

first <- first %>% group_by(destination_cbg) %>% dplyr::summarise(num_devices = sum(num_devices, na.rm = T))
first <- plyr::join(first, avg)
first$surge <- 100*(first$num_devices / first$avg_devices) 


# get city CBG 
cit <- read.csv(file.path(dataout, "cbgProtestCities.csv"))
colnames(cit)[colnames(cit)=="CBGFIPS"] <- "destination_cbg"
cit <- cit %>% dplyr::select(city, destination_cbg)
test <- plyr::join(test, cit)
test <- first[first$surge > 400,]




test <- plyr::join()

hist(first$surge, breaks=100)

cutoff <- 500






test <- fread(file.path("/home/antonvocalis/Google_Drive2/Documents/Corona/blm/raw/out/geo_adj_stacked.csv.gz"))


