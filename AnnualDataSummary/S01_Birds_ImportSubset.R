#This script loops through all of the records in tlu_EcoSiteInformationToReport and creates an annual report for each

#-----------------------------------------------------------------------
#load necessary packages - installs automatically if needed
pkgList <- c("gridExtra",
             "ggplot2",
             "stringr",
             "htmlwidgets",
             "DT",
             "RODBC",
             "leaflet",
             "rgdal",
             "png",
             "dplyr",
             "tidyr")


inst <- pkgList %in% installed.packages()
if (length(pkgList[!inst]) > 0) install.packages(pkgList[!inst],dep=TRUE)
lapply(pkgList, library, character.only = TRUE, quietly=TRUE)

#-----------------------------------------------------------------------
#when using knitr, the working directory is set to the location of this file
#if you are going to be reviewing the output of this script you can temporarily set the working directory
#knitr will ignore this, so it doesn't matter if it stays
setwd("C:\\Users\\LMcCoy\\Documents\\Projects\\Birds\\AnnualDataSummary")


#source the code to create the graphics
  source("S02_Birds_Table&FigureFunctions.R")
#-----------------------------------------------------------------------
#set an output path - this is handy if your output directory is different than your working directory
outputpath <- "./"

#-----------------------------------------------------------------------
#Import the report table, which is txt right now because we don't have a place to put it
report <- read.table("tlu_EcoSiteInformationToReport.txt", sep="|", header = TRUE, stringsAsFactors = FALSE)

#-----------------------------------------------------------------------
vcp2014 <- read.csv("C:\\Users\\LMcCoy\\Documents\\Projects\\Birds\\2014\\VCP_Counts_AllData2014.csv", stringsAsFactors = F, na="")
vcp2015 <- read.csv("C:\\Users\\LMcCoy\\Documents\\Projects\\Birds\\2015\\VCP_Counts_AllData2015.csv", stringsAsFactors = F, na="")
vcp2017 <- read.csv("C:\\Users\\LMcCoy\\Documents\\Projects\\Birds\\2017\\VCP_Counts_AllData2017.csv", stringsAsFactors = F, na="")
functional <- read.csv("C:\\Users\\LMcCoy\\Documents\\Projects\\Birds\\2017\\Sub_FoliarCover_Quantiles_All.csv", stringsAsFactors = F)

#-----------------------------------------------------------------------
#Import the spatial data needed for this report - these need to be in WGS84 to map in leaflet
  #import the network boundary
     scpnboundary <- readOGR(dsn="OtherFilesandGIS", layer = "SCPNBoundary", GDAL1_integer64_policy = TRUE) #reads the shapefile information
     parkcenter <- readOGR(dsn="OtherFilesandGIS", layer = "ParkCentroid", GDAL1_integer64_policy = TRUE) #reads the shapefile information
     parkboundary <- readOGR(dsn="OtherFilesandGIS", layer = "ParkBoundary", GDAL1_integer64_policy = TRUE) #reads the shapefile information
#     ecositeboundary <- readOGR(dsn="OtherFilesandGIS", layer = "EcoSiteBoundary", GDAL1_integer64_policy = TRUE) #reads the shapefile information
#     plotpositions <- readOGR(dsn="OtherFilesandGIS", layer = "PlotPositions", GDAL1_integer64_policy = TRUE) #reads the shapefile information
  
#-----------------------------------------------------------------------
#Loop through each park ecosite and year and create a report for each
for(i in 1:dim(report)[1]) { #for each record in the report table
#for(i in 1) { #for each record in the report table
  tempreport <- report[i,] #subset to ecosite i
  tempparkcenter <- parkcenter[parkcenter$UNIT_CODE %in% tempreport$Park,] #subset park centerpoint
  tempparkboundary <- parkboundary[parkboundary$UNIT_CODE %in% tempreport$Park,] #subset park boundary
  tempvcp <- vcp[vcp$Unit_Code %in% tempreport$Park & vcp$Habitat %in% tempreport$Habitat,] #subset the vcp data
  tempfunctional <- functional[functional$Unit_Code %in% tempreport$Park & functional$Habitat %in% tempreport$Habitat,] #subset to functional data
  
  rmarkdown::render('S01_ReportGenerator_Birds2017.Rmd', output_file = paste0(outputpath, 'OutputDataSummary/BirdDataSummary_', tempreport$Park, "_", tempreport$Habitat, "_", tempreport$EventYear, '.html'))

}

