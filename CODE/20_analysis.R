#**********************************************************************
# Compile Graph Data
#**********************************************************************
    
    
processed_data_directory <- dataout
fig_directory <- figs



months <-  c('04', '05', '06')

geo_adj <- foreach(i = months, .combine='rbind') %do% {
  
  geo_data_directory <- file.path(datain, "safe_graph_geos_adj", "2020", i)
  
  # How many days in each month?
  n_days = case_when(i == '06' ~ 6, # change 
                     i == '05' ~ 31,
                     i == '04' ~ 30)
  
  # Loop through the days, in April start at 25
  if (i == '04') { firstDay <- 25 } else { firstDay <- 1 }
  
  geo_adj <- foreach(j = seq(firstDay, n_days, 1), .combine='rbind') %do% {
  
    ## Get Data and Stack It
    day_string <- str_pad(as.character(j), 2, '0', side='left')

    file = str_c(geo_data_directory, '/2020', as.character(i), as.character(day_string), '_cbg_graph_adj.txt')
    data <- fread(file, colClasses = c("numeric", "character", "character", "numeric", "numeric"))
    
    data$V1 <- NULL
    data$cbg_adj_factor <- NULL
    
    colnames(data)[1:3] <- c("origin_cbg", "dest_cbg", "num_devices")
    
    data <- data %>% group_by(dest_cbg) %>% dplyr::summarise(num_devices = sum(num_devices, na.rm = T))
    
    # Add the days date to the dataframe
    data <- data %>% 
      mutate(ds = str_c('2020-', i, '-', day_string))
    
  }
}



data <- NULL

geo_adj$ds <- as.Date(geo_adj$ds)


# Calculate Average # of Devices in Week(s) Before

start <- as.Date("2020-05-26")

maxWksBack <- 4

for (i in 1:4) { # loop over various weeks back and create pct and level surge variables
  
  wk <- start - i*8 # farthest back for average
  
  nam <- paste("avg", i, sep="_")
  
  avg <- geo_adj[(geo_adj$ds < start) & (geo_adj$ds > wk),] %>%
    group_by(dest_cbg) %>% dplyr::summarise(avg = mean(num_devices, na.rm = T))
  
  geo_adj <- plyr::join(geo_adj, avg)
  
  geo_adj[[paste("outside_perc_wk", i, sep = "")]] <- 100 * geo_adj$num_devices / geo_adj$avg
  geo_adj[[paste("outside_level_wk", i, sep = "")]] <- geo_adj$num_devices - geo_adj$avg
  
  # get max for all protests days for each cbg
  geo_adj$temp <- geo_adj[[paste("outside_level_wk", i, sep = "")]]
  max <- geo_adj[geo_adj$ds >= start,] %>% group_by(dest_cbg) %>% dplyr::summarise(max = max(temp, na.rm = T))
  geo_adj <- plyr::join(geo_adj, max)
  colnames(geo_adj)[colnames(geo_adj)=="max"] <- str_c("maxByCbg", i)
  
  colnames(geo_adj)[colnames(geo_adj) == "avg"] <- paste("avg_wk", i,sep="")

}


# write long to file
fwrite(geo_adj, file.path(dataout, "geo_stacked_long_all.csv"))

# pad strings with zeros
geo_adj$dest_cbg <- str_pad(geo_adj$dest_cbg, 12, "left", "0")
geo_adj$TRACTFIPS <- substr( geo_adj$dest_cbg, 1,11)


# merge in protests data
prots <- fread(file.path(datain, "protestsMerged.csv"))
colnames(prots)[1:3] <- c("city", "State", "ds")
prots$city <- gsub("St.", "Saint", prots$city)
zips <- fread(file.path(dataout, "tractToCity.csv"))
zips$city <- gsub("St.", "Saint", zips$city)
states <- data.frame(State=state.name,state=state.abb) # these are built in datasets
prots <- plyr::join(prots, states)

zips$TRACTFIPS <- str_pad(zips$TRACTFIPS, 11, "left", "0")

geo_adj$ds <- as.Date(geo_adj$ds)
prots$ds <- as.Date(prots$ds)
prots <- plyr::join(prots, zips)
prots$State <- NULL

geo_adj <- plyr::join(geo_adj, prots)
geo_adj$city <- NULL
geo_adj$state <- NULL

# ever a protest?
temp <- geo_adj %>% group_by(dest_cbg) %>% dplyr::summarise(protEver = max(Protest, na.rm = T))
geo_adj <- plyr::join(geo_adj,temp)

# merge in all city names
geo_adj <- plyr::join(geo_adj, zips)

# drop non-protest dates
geo_adj <- geo_adj[geo_adj$ds >= as.Date("2020-05-25"),]


for (i in 1:4) {
  for (j in seq(200,500,100)) {
    geo_adj[[str_c("outside_perc_wk", i, "_above", j)]] <- as.numeric(geo_adj[[str_c("outside_perc_wk",i)]] > j)
  }
}

# get centroids
latlong <- read.csv(file.path(datain, "CBG_2017", "all_cbg_centroids.csv"))
colnames(latlong)[colnames(latlong)=="GEOID"] <- "dest_cbg"
latlong$dest_cbg <- str_pad(latlong$dest_cbg, 12, "left", "0")

geo_adj <- plyr::join(geo_adj, latlong)

# get city names
geo_adj$city <- NULL
geo_adj$state <- NULL
tracts <- read.csv(file.path(dataout, "tractToCity.csv"))
tracts$TRACTFIPS <- str_pad(tracts$TRACTFIPS, 11, "left", "0")
geo_adj <- plyr::join(geo_adj, tracts)


# write long to file
fwrite(geo_adj, file.path(dataout, "geo_stacked_long.csv"))





#### pivot to wide format 

geo_wide <- geo_adj %>% pivot_wider(names_from = ds, values_from = -c(dest_cbg, ds,city, state, protEver,
                                                                      maxByCbg1, maxByCbg2, maxByCbg3, maxByCbg4,
                                                                      xcoord, ycoord))
colnames(geo_wide)[colnames(geo_wide)!=c("dest_cbg", "maxByCbg")] <- gsub("2020-", "", colnames(geo_wide)[colnames(geo_wide)!=c("dest_cbg", "maxByCbg")])




fwrite(geo_wide, file.path(dataout, "geo_stacked_wide.csv"))




    

#**********************************************************************
#  SCATTERPLOT
#**********************************************************************

set.seed(13)

# get data
geo_adj <- fread(file.path(dataout, "geo_stacked_long.csv"))

# keep only protest days
geo_adj$ds <- as.Date(geo_adj$ds)
geo_adj <- geo_adj[geo_adj$ds > as.Date("2020-05-25"),]

# filter out territories
geo_adj$dest_cbg <- as.numeric(geo_adj$dest_cbg)
geo_adj <- geo_adj %>%
  dplyr::filter(dest_cbg <= 570000000000)

# filter out Alaska
geo_adj <- geo_adj[!dplyr::between(geo_adj$dest_cbg, 20000000000, 30000000000),]

geo_adj <- geo_adj[geo_adj$xcoord < 150,] # mistake?
geo_adj <- geo_adj[geo_adj$xcoord > -150,] # mistake?

geo_adj$outside_perc_wk1 <- geo_adj$outside_perc_wk1 
geo_adj$perc_breaks <- cut(geo_adj$outside_perc_wk1, breaks = c(-Inf, seq(-300,2000,50), Inf))

geo_adj$x <- runif(dim(geo_adj)[1], min = 0.4, max = 0.6) # manual jitter

#colnames(geo_adj)[colnames(geo_adj)=="outside_perc_wk1"] <- "Percentage Surge"
heatcols <- heat.colors(length(levels(geo_adj$perc_breaks)) +10 , rev = T)[10:(length(levels(geo_adj$perc_breaks))+10)]

#geo_adj <- geo_adj[geo_adj$outside_perc_wk1 < 3000,]






# get max for each day 
temp <- geo_adj[geo_adj$city!="" & !is.na(geo_adj$city) & geo_adj$ds != as.Date("2020-05-26"),] %>% group_by(ds) %>% dplyr::summarise(maxProt = max(outside_perc_wk1, na.rm = T))
geo_adj <- plyr::join(geo_adj, temp)
geo_adj$labs <- as.numeric(geo_adj$outside_perc_wk1 == geo_adj$maxProt)

# add Minneapolis on first day
geo_adj[geo_adj$city == "Minneapolis" & geo_adj$ds == as.Date("2020-05-26") & geo_adj$outside_perc_wk1 > 450,]$labs <- 1


# add second largest protests
geo_adj <- geo_adj[!is.na(geo_adj$outside_perc_wk1),] # about 50 NAs
temp <- geo_adj[geo_adj$ds == as.Date("2020-05-28") & geo_adj$city != "" & 
                  !(geo_adj$city %in% unique(geo_adj[geo_adj$labs == 1,]$city)),]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n-5)[n-5]
geo_adj[nd == geo_adj$outside_perc_wk1,]$labs <- 1

geo_adj[geo_adj$ds == as.Date("2020-05-30"),]$labs <- 0

temp <- geo_adj[geo_adj$ds == as.Date("2020-05-30") & geo_adj$city != "" & 
                  !(geo_adj$city %in% unique(geo_adj[geo_adj$labs == 1,]$city)),]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n-2)[n-2]
geo_adj[nd == geo_adj$outside_perc_wk1,]$labs <- 1

nd <- max(geo_adj[geo_adj$ds == as.Date("2020-05-30") & geo_adj$city == "Houston",]$outside_perc_wk1)
geo_adj[nd  == geo_adj$outside_perc_wk1,]$labs <- 1


temp <- geo_adj[geo_adj$ds == as.Date("2020-05-31") & geo_adj$city != "" & 
                  !(geo_adj$city %in% unique(geo_adj[geo_adj$labs == 1,]$city)),]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n-21)[n-21]
geo_adj[nd < geo_adj$outside_perc_wk1 & geo_adj$city == "Philadelphia" & geo_adj$ds == as.Date("2020-05-31"),]$labs <- 1


temp <- geo_adj[geo_adj$ds == as.Date("2020-06-01") & geo_adj$city != "" & 
                  !(geo_adj$city %in% unique(geo_adj[geo_adj$labs == 1,]$city)),]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n-20)[n-20]
geo_adj[nd < geo_adj$outside_perc_wk1 & geo_adj$city == "Chicago" & geo_adj$ds == as.Date("2020-06-01"),]$labs <- 1

temp <- geo_adj[geo_adj$ds == as.Date("2020-05-29") & geo_adj$city != "" & 
                  !(geo_adj$city %in% unique(geo_adj[geo_adj$labs == 1,]$city)),]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n-20)[n-20]
geo_adj[nd < geo_adj$outside_perc_wk1 & geo_adj$city == "Portland"  & geo_adj$ds == as.Date("2020-05-29"),]$labs <- 1

temp <- geo_adj[geo_adj$ds == as.Date("2020-06-02") & geo_adj$city != "" & 
                  !(geo_adj$city %in% unique(geo_adj[geo_adj$labs == 1,]$city)),]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n-20)[n-20]
geo_adj[nd < geo_adj$outside_perc_wk1 & geo_adj$city == "Miami"  & geo_adj$ds == as.Date("2020-06-02"),]$labs <- 1



# San Antonio appears too often, me no like
geo_adj[(geo_adj$ds == as.Date("2020-05-29") | 
           geo_adj$ds == as.Date("2020-06-01")) & geo_adj$city == "San Antonio",]$labs <- 0 



temp <- geo_adj[geo_adj$ds == as.Date("2020-05-27") & geo_adj$city != "" &
                  !(geo_adj$city %in% unique(geo_adj[geo_adj$labs == 1,]$city)),]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n-30)[n-30]
geo_adj[nd < geo_adj$outside_perc_wk1 & geo_adj$city == "Bronx"  & geo_adj$ds == as.Date("2020-05-27"),]$labs <- 1

temp <- geo_adj[geo_adj$ds == as.Date("2020-05-29") & geo_adj$city != "" &
                  !(geo_adj$city %in% unique(geo_adj[geo_adj$labs == 1,]$city)),]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n-5)[n-5]
geo_adj[nd < geo_adj$outside_perc_wk1 & geo_adj$city == "Detroit"  & geo_adj$ds == as.Date("2020-05-29"),]$labs <- 1



temp <- geo_adj[geo_adj$ds == as.Date("2020-06-01") & geo_adj$city != "" & 
                  !(geo_adj$city %in% unique(geo_adj[geo_adj$labs == 1,]$city)),]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n-9)[n-9]
geo_adj[nd < geo_adj$outside_perc_wk1 & geo_adj$city == "Los Angeles"  & geo_adj$ds == as.Date("2020-06-01"),]$labs <- 1

temp <- geo_adj[geo_adj$ds == as.Date("2020-06-02") & geo_adj$city != "" & 
                  !(geo_adj$city %in% unique(geo_adj[geo_adj$labs == 1,]$city)),]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n-60)[n-60]
geo_adj[nd < geo_adj$outside_perc_wk1 & geo_adj$city == "Saint Paul"  & geo_adj$ds == as.Date("2020-06-02"),]$labs <- 1

temp <- geo_adj[geo_adj$ds == as.Date("2020-06-03") & geo_adj$city != "" & 
                  !(geo_adj$city %in% unique(geo_adj[geo_adj$labs == 1,]$city)),]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n-40)[n-40]
geo_adj[nd < geo_adj$outside_perc_wk1 & geo_adj$city == "Washington"  & geo_adj$ds == as.Date("2020-06-03"),]$labs <- 1

temp <- geo_adj[geo_adj$ds == as.Date("2020-06-03") & geo_adj$city != "" & 
                  !(geo_adj$city %in% unique(geo_adj[geo_adj$labs == 1,]$city)),]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n-10)[n-10]
geo_adj[nd < geo_adj$outside_perc_wk1 & geo_adj$city == "Dallas"  & geo_adj$ds == as.Date("2020-06-03"),]$labs <- 1



temp <- geo_adj[geo_adj$ds == as.Date("2020-06-01") & geo_adj$city != "",]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n)[n]
geo_adj[nd == geo_adj$outside_perc_wk1,]$labs <- 1



temp <- geo_adj[geo_adj$ds == as.Date("2020-06-02")  & geo_adj$city != "",]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n)[n]
geo_adj[nd == geo_adj$outside_perc_wk1,]$labs <- 1



temp <- geo_adj[geo_adj$ds == as.Date("2020-06-03") & geo_adj$city != "",]$outside_perc_wk1
n <- length(temp[!is.na(temp)])
nd <- sort(temp,partial=n)[n]
geo_adj[nd == geo_adj$outside_perc_wk1,]$labs <- 1




# date labels
geo_adj$ds <- as.Date(geo_adj$ds)
sortDs <- sort(format(unique(geo_adj$ds), "%b %d"))
geo_adj$date <- factor(format(geo_adj$ds, "%b %d"), levels = c(sortDs[grepl("May", sortDs)],
                                                               sortDs[grepl("Jun", sortDs)]))

# get Minneap first day coord (do it like this so it only appears in first facet window)
geo_adj$xMin <- as.numeric(NA)
geo_adj$yMin <- as.numeric(NA)
geo_adj[geo_adj$ds == as.Date("2020-05-26") & geo_adj$city == "Minneapolis" & geo_adj$outside_perc_wk1 > 350,]$xMin <- 
  geo_adj[geo_adj$ds == as.Date("2020-05-26") & geo_adj$city == "Minneapolis" & geo_adj$outside_perc_wk1 > 350,]$x
geo_adj[geo_adj$ds == as.Date("2020-05-26") & geo_adj$city == "Minneapolis" & geo_adj$outside_perc_wk1 > 350,]$yMin <- 
  geo_adj[geo_adj$ds == as.Date("2020-05-26") & geo_adj$city == "Minneapolis" & geo_adj$outside_perc_wk1 > 350,]$outside_perc_wk1


p <- ggplot(geo_adj, aes(x=x, y = outside_perc_wk1)) + 
  geom_scattermore(aes(colour = geo_adj$perc_breaks), pointsize = 6.1) + 
  geom_point(aes(x=xMin, y=yMin), colour=heat.colors(2)[1], size=3) +
  #  geom_point(data = subset(geo_adj, labs == 1), aes(x=x, y=outside_perc_wk1)) +
  geom_text_repel(data = subset(geo_adj, labs == 1), aes(label = city),  size = 4,
                  segment.color = "black", ylim = c(1400,3600), force = 5,
                  min.segment.length = 0.0, point.padding = 0, family = "CM Roman",
                  arrow = arrow(length = unit(0.03, "npc"))) +
  facet_wrap(~date, nrow = 2) + 
  scale_colour_manual(name = "perc_breaks",
                      values = 
                        c(heatcols)
  ) +
  theme(legend.position = "none", 
        text = element_text(family = 'CM Roman'), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + ylab("Device Surge (Ratio)") + xlab("") + ylim(-100,5000)

p


plotname <- "scatterByDay.pdf"
ggsave(file.path(figs, plotname), height=10, width=10)

embed_fonts(file.path(figs, plotname))





    
    
    
    #*******************************************************************
    # ANIMATED MAP
    #*******************************************************************
    
    # get data
    geo_adj <- fread(file.path(dataout, "geo_stacked_long.csv"))
    geo_adj$ds <- as.Date(geo_adj$ds)
    
    geo_adj <- geo_adj[geo_adj$avg_wk1 > 5,]
    
    
    # set nice ggplot maps theme
    theme_set(theme_bw()) 
    
    geo_adj$dest_cbg <- as.numeric(geo_adj$dest_cbg)
    
    # filter out territories
    geo_adj <- geo_adj %>%
      dplyr::filter(dest_cbg <= 570000000000)
    
    # filter out Alaska
    geo_adj <- geo_adj[!dplyr::between(geo_adj$dest_cbg, 20000000000, 30000000000),]
    
    geo_adj <- geo_adj[geo_adj$xcoord < 150,] # mistake?
    geo_adj <- geo_adj[geo_adj$xcoord > -150,] # mistake?
    
    # take only protest locations
    toPlot <- geo_adj[geo_adj$outside_perc_wk1 > 350,] # protest location threshold
    
    
    # convert to different date format
    #toPlot$ds <- as.POSIXct(toPlot$ds)
    toPlot$ds <- as.Date(toPlot$ds)
    toPlot <- toPlot[!is.na(toPlot$ds),]
    
    
    # set 25th to zero
    toPlot[toPlot$ds == as.Date("2020-05-25"),]$outside_level_wk1 <- 0
    
    #### Dynamic Levels Map ####
    
    
    # get blue from density plot
    nb.cols <- length(unique(geo_adj$ds))
    mycolor1 <- colorRampPalette(brewer.pal(8, "Blues"))(nb.cols)[4]
    mycolor1 <- t_col(mycolor1, 40)
    
    # get map for us
    world <- ggplot() +
      borders("state", colour = "gray81", fill = mycolor1) +
      theme_map() 
    
    
    
    #### Static Maps
    
    
    # date labels
    toPlot$ds <- as.Date(toPlot$ds)
    sortDs <- sort(format(unique(toPlot$ds), "%b %d"))
    toPlot$date <- factor(format(toPlot$ds, "%b %d"), levels = c(sortDs[grepl("May", sortDs)],
                                                                   sortDs[grepl("Jun", sortDs)]))
    
    for (i in unique(toPlot[toPlot$date != "May 25",]$date)) {
      temp <- toPlot[toPlot$outside_level_wk1 > 0,]
      map <- world + 
      geom_point(aes(x = xcoord, y = ycoord, size = outside_level_wk1),
                 # data = subset(toPlot, ds == as.Date("2020-05-30")), 
                 data = subset(temp, date == i), 
                 colour = 'red', alpha = .2) +
        scale_size_identity(trans="sqrt",guide="legend",  breaks = c(100,500,5000)) +
      # scale_size_continuous(limits=c(1,15), "Crowd Device Count",
      #                      ) + 
        labs(legend.title = "Crowd Size")
        theme(plot.title = element_text(size = 16, face = "bold", vjust = 0.9),
              legend.text = element_text(size = 16), 
              legend.title = element_text(size = 18) )
      map
      plotname <- str_c("countMap_", substr(i,1,3), 
                        str_pad(substr(i,5,nchar(i)),2,"left", "0"), ".pdf")
      ggsave(file.path(figs, plotname), height=9, width=16)
      
      embed_fonts(file.path(figs, plotname))
      
    }
    
    
    #### Dynamic Map 
    
    # static map 
    temp <- toPlot[toPlot$outside_level_wk1 > 0 | toPlot$ds == as.Date("2020-05-25"),]
    map <- world + 
      geom_point(aes(x = xcoord, y = ycoord, size = outside_level_wk1, 
                     group = seq_along(ds)),
                # data = subset(toPlot, ds == as.Date("2020-05-30")), 
                 data = temp, 
                 colour = 'red', alpha = .2) +
      scale_size_continuous(range = c(0,15), "Crowd Device Count",
                            breaks = c(100,500,5000)) 
    map
    
    
    # make dynamic
    
    anim <- map + 
      transition_states(ds, transition_length = 2, state_length = 1) +
       labs(title = "George Floyd Protests, Date: {closest_state}")  + 
      theme(plot.title = element_text(size = 16, face = "bold", vjust = 0.9),
            legend.text = element_text(size = 10), 
            legend.title = element_text(size = 12) ) +
      enter_fade() + enter_grow() + exit_fade()  # + transition_components(ds, enter_length = as.Date(1/2))
    
    # anim <- map + 
    #   transition_time(ds) + labs(title = "Date: {frame_time}")  
    
    numDates <- length(unique(toPlot$ds))
    animOut <- animate(anim, duration = 2*numDates, nframes = numDates*30, width = 800, height = 800*9/16)
    animOut
    anim_save("dynamicLevels350.gif", animOut, path = figs)
    





#***************************************************************
# KERNEL DENSITY MAPS
#***************************************************************

# get data
geo_adj <- fread(file.path(dataout, "geo_stacked_long.csv"))
geo_adj$ds <- as.Date(geo_adj$ds)

geo_adj <- geo_adj[geo_adj$avg_wk1 > 5,]

geo_adj$dFact <- factor(geo_adj$ds)


# filter out territories
geo_adj$dest_cbg <- as.numeric(geo_adj$dest_cbg)
geo_adj <- geo_adj %>%
  dplyr::filter(dest_cbg <= 570000000000)

# filter out Alaska
geo_adj <- geo_adj[!dplyr::between(geo_adj$dest_cbg, 20000000000, 30000000000),]

geo_adj <- geo_adj[geo_adj$xcoord < 150,] # mistake?
geo_adj <- geo_adj[geo_adj$xcoord > -150,] # mistake?

# sort dates
geo_adj$ds <- as.Date(geo_adj$ds)
sortDs <- sort(format(unique(geo_adj$ds), "%b %d"))
geo_adj$Date <- factor(format(geo_adj$ds, "%b %d"), levels = c(sortDs[grepl("May", sortDs)],
                                                               sortDs[grepl("Jun", sortDs)]))

# get more colours
nb.cols <- length(unique(geo_adj$ds))
mycolors <- colorRampPalette(brewer.pal(8, "Blues"))(nb.cols)

# plot
ggplot(geo_adj) +
  geom_density(data = geo_adj, aes(outside_perc_wk1, group  = Date, color = Date), size = 0.7) + 
  scale_colour_manual(values = mycolors) +
  geom_vline(xintercept = 300, color = "black", linetype = "dashed", alpha = 0.2) + 
  xlab("Device Surge (Ratio)") + ylab("Density") + 
  xlim(0, 500) + 
  annotate("rect", xmin = 300, xmax = Inf, ymin = -Inf, ymax = Inf,
           alpha = .4, fill = mycolors[4]) + 
  #annotate("text", x = 210,y = 0.02, label = "Protest Locations -->", hjust = 0, family = "CM Roman") + 
  theme(text = element_text(family = 'CM Roman'))


plotname <- "densityPlots.pdf"
ggsave(file.path(figs, plotname), height=4, width=6)

embed_fonts(file.path(figs, plotname))






#***************************************************************
# EVOLUTION OF THE NUMBER OF CBGS
#***************************************************************

# get data
geo_adj <- fread(file.path(dataout, "geo_stacked_long.csv"))
geo_adj$ds <- as.Date(geo_adj$ds)

geo_adj <- geo_adj[geo_adj$avg_wk1 > 5,]

geo_adj$dFact <- factor(geo_adj$ds)


# filter out territories
geo_adj$dest_cbg <- as.numeric(geo_adj$dest_cbg)
geo_adj <- geo_adj %>%
  dplyr::filter(dest_cbg <= 570000000000)

# filter out Alaska
geo_adj <- geo_adj[!dplyr::between(geo_adj$dest_cbg, 20000000000, 30000000000),]

geo_adj <- geo_adj[geo_adj$xcoord < 150,] # mistake?
geo_adj <- geo_adj[geo_adj$xcoord > -150,] # mistake?

# sort dates
geo_adj$ds <- as.Date(geo_adj$ds)
sortDs <- sort(format(unique(geo_adj$ds), "%b %d"))
geo_adj$Date <- factor(format(geo_adj$ds, "%b %d"), levels = c(sortDs[grepl("May", sortDs)],
                                                               sortDs[grepl("Jun", sortDs)]))


# create data frame w count of # cbgs per day, for different thresholds
start <- 300
for (i in seq(start,600,100)) {
  nam <- str_c("num", i)
  temp <- geo_adj %>% group_by(Date) %>% 
    dplyr::summarise(count = sum(outside_perc_wk1 > i, na.rm = T))
  temp$thres <- i 
  if (i == start) { dracula <- temp } else { dracula <- rbind(dracula, temp)}
}

mycolors <- hcl.colors(length(unique(dracula$thres)), palette = "Plasma")

dracula$Threshold <- factor(dracula$thres)

ggplot(dracula) + 
  geom_line(aes(x = Date, y = count, colour = Threshold, group = Threshold),
            size =1.5, alpha=0.6) + 
  scale_colour_manual(values = mycolors) + ylab("Number of Protest CBGs") + 
  theme(text = element_text(family = 'CM Roman'))


plotname <- "numberCbgs.pdf"
ggsave(file.path(figs, plotname), height=4, width=6)

embed_fonts(file.path(figs, plotname))
  



# geo_adj <- fread(file.path(dataout, "geo_adj2_stacked.csv.gz"))
# 
# # Minneapolis first day
# first <- geo_adj[(geo_adj$ds == as.Date("2020-05-26")),]
# geo_adj <- NULL
# 
# 
# 
# first <- first %>% group_by(destination_cbg) %>% dplyr::summarise(num_devices = sum(num_devices, na.rm = T))
# first <- plyr::join(first, avg)
# first$surge <- 100*(first$num_devices / first$avg_devices) 
# 
# 
# # get city CBG 
# cit <- read.csv(file.path(dataout, "cbgProtestCities.csv"))
# colnames(cit)[colnames(cit)=="CBGFIPS"] <- "destination_cbg"
# cit <- cit %>% dplyr::select(city, destination_cbg)
# test <- plyr::join(test, cit)
# test <- first[first$surge > 400,]
# 
# 
# 
# date <- as.Date("2020-05-30")
# 
# 
# prot <- geo_adj[geo_adj$ds == date,]
# 
# 
# # get city fips
# cit <- read.csv(file.path(dataout, "cbgProtestCities.csv"))
# colnames(cit)[colnames(cit)=="CBGFIPS"] <- "dest_cbg"
# cit <- cit %>% dplyr::select(city, dest_cbg)
# cit$dest_cbg <- str_pad(cit$dest_cbg, 12, side= "left", pad = 0)
# 
# # left join
# prot <- plyr::join(prot, cit)




