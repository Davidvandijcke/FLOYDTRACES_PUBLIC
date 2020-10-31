#****************************************************************************************************************************************************

# Get List of Cities and Protests

# Austin L Wright, David Van Dijcke
# University of Oxford / UChicago
# david.vandijcke@economics.ox.ac.uk

#****************************************************************************************************************************************************


## Code to match cities to CBGs

# cbg codes
zips <- fread(file.path(datain, "TRACT_ZIP_032020.csv"))
zips <- zips %>% group_by(TRACT) %>% dplyr::filter(RES_RATIO == max(RES_RATIO, na.rm = T))
zips <- zips[!duplicated(zips$TRACT),] # drop the ones with perfect match
zips <- zips %>% dplyr::select(TRACT, ZIP)
colnames(zips) <- c("TRACTFIPS", "zip")
zips$TRACTFIPS <- str_pad(zips$TRACTFIPS, 11, "left", "0")

data("zipcode")
zipcode <- zipcode %>% dplyr::select(zip, city, state)

zips <- plyr::join(zips, zipcode)
zips$zip <- NULL


fwrite(zips, file.path(dataout, "tractToCity.csv"))


# # get city-specific CBG FIPS codes
# cit <- fread(file.path(datain, "weekly-patterns", "main-file", "2020-02-03-weekly-patterns.csv.gz"))
# #cit <- cit[!duplicated(cit$city),] # take every city once
# cit <- cit %>% dplyr::select(city, safegraph_place_id)
# cit <- plyr::join(cit, cbg) # join by safegraph place ids
# cit <- cit[!duplicated(cit$CBGFIPS),] # keep onl unique census block groups
# write.csv(cit, file.path(dataout, "cityCBG.csv"))



  
#### Get Protest Cities #### 

# get Jazeera protests data
jaz <- read.csv(file.path(datain, "protestsJazeera.csv"), header = T)
jaz <- as.data.frame(jaz)
temp <- jaz[nchar(jaz$list)==2,]
jaz <- jaz[!nchar(jaz$list)==2,]
jaz <- cbind(jaz, temp)
jaz <- as.data.frame(jaz)
colnames(jaz) <- c("city", "state")


# get Wiki protests data
protests <- read.csv(file.path(datain, "protestsWiki.csv"),sep=",")
protests$city <- gsub("\\,.*","",protests$city) # drop state name, will cross-ref with jazeera

# get Elephrame protests data
ele <- read.csv(file.path(datain, "protestsElephrame.csv"))
ele <- ele %>% dplyr::select(City, State)
colnames(ele) <- c("city", "state")


# cross-ref protests data
protests <- plyr::join(protests, jaz) # NB: a few cities w same name in different state
protests <- protests[!is.na(protests$state),]
protests <- protests[!duplicated(protests %>% dplyr::select(city, state)),] # drop duplicates

protests <- rbind(protests, ele)
protests <- protests[!duplicated(protests %>% dplyr::select(city, state)),] # drop duplicates

protests$city <- gsub("St.", "Saint", protests$city)
zips$city <- gsub("St.", "Saint", zips$city)



# take matches from cbg data
df <- plyr::join(zips, protests, type = "inner")
  
write.csv(df, file.path(datain, "cbgProtestCities.csv"), row.names=F)



