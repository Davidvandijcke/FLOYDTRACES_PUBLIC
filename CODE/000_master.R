  
  #****************************************************************************************************************************************************
  
  # MASTER FILE: "Using Mobile Device Traces to Improve Near-Real Time Data Collection During the George Floyd Protests"
  
  # Austin Wright, David Van Dijcke
  # University of Oxford / University of Chicago
  # david.vandijcke@economics.ox.ac.uk
  
  #****************************************************************************************************************************************************
  
  #### SET PARAMETERS <- SET YOURSELF ####
  
  stataVersion <- 16 # set to whichever stata version you're using
  
  rm(list = ls()) # clear
  
  
  #### SET PATHS #### 
  
  if (!require("here", character.only=T)) {install.packages("here", dependencies=TRUE)}; require("here")
  mkdir=sub("/CODE", "", here::here())
  setwd(file.path(mkdir,"CODE"))
  #datain = file.path("/home/antonvocalis/Google_Drive2/Documents/Corona/Corona/raw")
  datain = file.path(mkdir, "DATA/RAW") # alter this if different file structure
  dataout = file.path(mkdir, "DATA/PROCESSED")
  tabs = file.path(mkdir, "RESULTS/TABS")
  figs = file.path(mkdir, "RESULTS/FIGS")
  
  
  
  #### SOURCE SCRIPTS ####
  
  source("00_prep.R") # load libraries etc
  source("01_fts.R") # load self-written functions



