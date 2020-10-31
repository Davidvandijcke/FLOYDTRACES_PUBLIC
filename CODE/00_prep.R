
#****************************************************************************************************************************************************

# PREPARE FOR ANALYSIS

# David Van Dijcke
# University of Oxford, 2019-20
# david.vandijcke@economics.ox.ac.uk
# https://davidvandijcke.github.io

#****************************************************************************************************************************************************

# To Install DirichletMultinomial: First install bioconducts, then install 
# package using BiocManage::install("DirichletMultinomial") 
# https://bioconductor.org/install/ 


#### LOAD LIBRARIES ####

suppressPackageStartupMessages({
  wants <- c("srvyr", "tibble", "gdata", "reshape2", "dplyr", "stargazer", "zoo", "lubridate", "datetime",  "lubridate", "plyr", "foreach", "ggplot2", "extrafont",
             "data.table",  "here", "devtools",  "urca", "dplyr", "openintro", "RcmdrMisc", "fpc","combinations","purrr",  "pracma",  "pbmcapply",  "RStata", "fread", 
             "xlsx", "tidyr", "scales", "grid", "schoolmath", "gridExtra", "AER", "tigris",  "stringdist", "crunch", "sqldf",  "shiny", "leaflet", "leaflet.extras", "rsconnect", "hash",
             "dplyr", "stringr", "foreach", "jsonlite", "readr", "DirichletMultinomial", "broom", "scales", "usmap", "flexmix",  "xts", "zipcode", "doParallel", "cowplot",
             "scattermore", "viridis", "RColorBrewer", "ggrepel", "gganimate", "googleway", "ggspatial",  "ggthemes",
             "magick")
  lapply(wants, function(wants) {if (!require(wants, character.only=T)) {install.packages(wants, dependencies=TRUE)}; require(wants) })
})

rsconnect::setAccountInfo(name='davidvandijcke',
                          token='74632A623530C754F97052E66D161478',
                          secret='24yXoWR6IJNy8/vDR0zgX+faGi3s5a56CUrypkO6')

font_install("fontcm")
options("RStata.StataPath"='/usr/local/stata16/stata')
options("RStata.StataVersion" = stataVersion)


suppressPackageStartupMessages({
  wants <- c("class", "foreign", "KernSmooth", "MASS", "nlme", "nnet", "spatial")
  lapply(wants, function(wants) {if (!require(wants, character.only=T)) {install.packages(wants, dependencies=TRUE)}; require(wants) })
})


# load necessary packages for importing the function
library(RCurl)

# import the function from repository
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
install.packages("remotes")
remotes::install_github("Brattle/BrattleExtras")






 