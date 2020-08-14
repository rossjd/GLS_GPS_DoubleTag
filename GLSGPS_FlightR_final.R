#                  ****** GLS-GPS FLightR script ******   
#                           Luke Halpin & Jeremy Ross         

#  This script uses TwGeos to initially define twilights from Migrate Technology GLS tags
#  And FLightR to calculate positions from twilights

#### -------- INSTALL REQUIRED PACKAGES AND LOAD LIBRARIES #########
# Check to make sure the required packages are installed
# If not, they will be installed

reqPackages <- c("devtools","digest","GeoLight","geosphere","raster","fields","forecast",
                 "circular","truncnorm","parallel","bit","rgdal","CircStats","Rcpp",
                 "RcppArmadillo","ggmap", "ggplot2", "ggsn","sp","maptools","rgeos","MASS")


get.packages <- reqPackages[!(reqPackages %in% installed.packages()[,"Package"])]

if(length(get.packages)>0){
  install.packages(get.packages,repos = "https://cloud.r-project.org/", dependencies = TRUE)
}

# Install necessary packages from Github using the devtools library #
library(devtools)
install_github("SLisovski/TwGeos") 
install_github("eldarrak/FLightR")

#For latest version of ggmap that is required by FLightR use the following:
install.packages("digest")
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")

#Load libraries
library(GeoLight)
library(TwGeos)
library(SGAT)
library(FLightR)
library(MASS)  #needed for fitting distributions
library(maptools)
library(ggmap) #may need to point to specific directory if the build was made from GitHub: library("ggmap", lib.loc="~/R/win-library/3.4")
library(ggplot2)
library(dplyr)
library(tidyr)

#IMPORTANT NOTE: USE CLOCK DRIFT ADJUSTED GLS DATA DENOTED BY THE END OF FILENAME: "driftadj.lux"

########### Set the Project Folder location #################
project_folder <- "~R/project_folder"
project_folder <- "C:/Users/Jeremy/Dropbox/Research/Geolocators/GPS_Geoloc_comps_Halpin/twilight_tags_data/outputs"

########### -------------- IMPORT THE DATASET METADATA ##############

setwd(project_folder) #IMPORTANT: edit for YOUR local directory!!!
# Folders within this working directory should include "light_data" and "SST" that within each have a separate folder for each site within the study
# Proper folder structure and filename consistency will be absolutely necessary to call the proper folders/files using the metadata as a guide !!

#RECOMMENDED: Load the metadata file (it will have other uses, too!):
library(readr)
GLSGPS_metadata <- read_csv("GLSGPS_metadata.csv") #Be sure to check date and time formatting and use POSIX format for datetime!!

##ALTERNATIVE: if you want to build the list directly from the actual files, rather than from the metadata,
#make a list of all the population folders within the data directory

regions <- list.files(path = "/light_data", include.dirs = TRUE,recursive = FALSE)

#Make a dataframe that lists all "tags" files in individual rows, regardless of population origin
temp<-do.call("rbind", lapply(seq_along(regions), function (i){
  data.frame(files=do.call(rbind, lapply(
    list.files(path = paste0(regions[[i]],"/tags/"), pattern = "*_tags", recursive = FALSE, full.names = TRUE),paste)))
}))

tag.list <- as.list(as.data.frame(t(temp))) #convert df to list
#Iteratively import each tag file into FlightR compatible format
for(i in tag.list){
  tag <- paste0(gsub("_tags.csv","",basename(paste(i))),".data")
  print(tag)
  assign(tag, get.tags.data(paste0("./",i)))
}

#**********************************************************
#### ----------- DEFINE TWILIGHTS USING TwGeos ----- #######
#**********************************************************

#Make sure we're working in GMT
Sys.setenv(TZ='GMT')
Sys.timezone()

#The following function will cycle through the list of files indicated in the metadata for each bird's calibration period
#If any changes are made WITHIN the function, be sure to run it again so that it is properly set prior to execution

twlts <- function(row = 1:nrow(GLSGPS_metadata)) {
  setwd(project_folder)
  for(i in row) {
    ### Convert tags file into FLightR-compatible format
    if(GLSGPS_metadata$gls_calibration_id[i] == "included_on_tag"){
      light <- GLSGPS_metadata$gls_id[i]
      folder <- "/"
    } else {
      light <- paste0("external_calibration_files/",GLSGPS_metadata$gls_calibration_id[i])
      folder <- "/external_calibration_files/"
    }
    print(light) #readout to be sure you've selected the right file
    id <- GLSGPS_metadata$indiv[i]
    print(id) #Optional readout to check which individual you're analyzing
    #View(tag.data) #Comment/Uncomment to use as checkpoint
    
    ## Read in the files using the format-appropriate approach
    if(gsub(".*\\.(l[a-z]{2})","\\1",light) == "lig") {
      lt.data <-ligTrans(paste0("raw_calibration_data/",gsub("_([0-9]|R).*","",id),"/", light))
      colnames(lt.data) <- c("Date","Light")
      #print(str(lt.data$Date)) #Comment/Uncomment to use as checkpoint
    } else {
      lt.data <-readMTlux(paste0("raw_calibration_data/",gsub("_([0-9]|R).*","",id),"/", light))
      #print(str(lt.data$Date)) #Comment/Uncomment to use as checkpoint
    }
    #log transform for easier twilight annotation
    bump = 1
    lt.data$Light<- log(lt.data$Light+bump)
    #View(lt.data) #If you want to check the resulting light levels

    #Produce image of data (adjust "offset" as needed so night is in middle) 
    lightImage( tagdata = lt.data, offset = offset,     
                zlim = c(0, 5),dt = 300)
    lon.calib <- GLSGPS_metadata$gls_cal_lon[i]
    lat.calib <- GLSGPS_metadata$gls_cal_lat[i]
    tsimageDeploymentLines(lt.data$Date, lon = lon.calib, lat = lat.calib,
                           offset = offset, lwd = 3, col = adjustcolor("orange", alpha.f = 0.5))
    
    # Plot the light levels for the first few days to see how the transitions look relative to the threshold
    col = colorRampPalette(c('black',"purple",'orange'))(50)[as.numeric(cut(lt.data[1:6000,2],breaks = 50))]
    par(mfrow = c(1, 1), mar = c(2, 2, 2, 2) )
    with(lt.data[1:6000,], plot(Date, Light, type = "o", pch=16,  col = col, cex = 0.5)) 
    abline(h=threshold, col="orange", lty = 2, lwd = 2)
    
    ##Define twilights and save (this option can be turned off if you want to read the prior result back in)
    if(define.twl == TRUE){
      #for instructions for preprocessLight see: https://slisovski.github.io/TheGeolocationManual/twilight.html
      #do not press 'a' when finished annotation as this will not save twilights to the object
      #make sure to press 'q' when finished annotation
      lt.data.twl <- preprocessLight(lt.data, 
                                     threshold = threshold,
                                     offset = offset, 
                                     lmax = threshold*2,              # max. light value
                                     gr.Device = "default")       
      
      #Write the twilight annotation as csv
      lt.data.twl$Twilight <- as.POSIXct(lt.data.twl$Twilight, tz = "GMT")
      write.csv(lt.data.twl, paste0(project_folder,"light_data/", gsub("_([0-9]|R).*","",id),folder,"twilights/", id,"_twl.csv"), 
                row.names = F)
    } else {
      lt.data.twl <- read.csv(paste0(project_folder,"light_data/", gsub("_([0-9]|R).*","",id),folder,"twilights/", id,"_twl.csv"))
      lt.data.twl$Twilight <- as.POSIXct(lt.data.twl$Twilight, tz = "GMT")#, origin = "1970-01-01")
      #View(lt.data.twl) #If you need to check resulting datatable
    }
    #Create a TAGS format file for geolocation analysis
    lt.data$Light<- round(exp(lt.data$Light)-bump) #First need to reverse log-transformation
    lt.data.tags <- twGeos2TAGS(lt.data, lt.data.twl, 
                                filename=NULL, 
                                threshold = exp(threshold)-bump)
    #Write the TAGS file for later reading in FLightR
    write.csv(lt.data.tags, paste0(project_folder,"light_data/", gsub("_([0-9]|R).*","",id),folder,"tags/", id, "_tags.csv"), row.names = F)
  }
}

define.twl = T
threshold <- 1 #Use a threshold value just above the log-transformed peak nighttime noise level.
offset = 0 #Change this so that the offset of day/night periods best displays midnight in middle
twlts(row=c(1:length(unique(GLSGPS_metadata$id))))

### Calculate percentage of twilights that were excluded from each individual and append to metadata ####

GLSGPS_metadata$pct_twl_excl <- NA

for(i in 1:nrow(GLSGPS_metadata)) {
  ### Convert tags file into FLightR-compatible format
  file <- GLSGPS_metadata$TWL_file[i]
  print(paste(i,file,paste0("light_data/",gsub("_([0-9]|R).*","",file))))
  #tag <- paste0(gsub("_tags.csv","",file),".data") #In case you want to add it as dataframe using "assign" function
  temp_twl <- read.csv(paste0("light_data/",gsub("_([0-9]|R).*","",file),"/twilights/",file))
  pct_twl_excl <- 100*(sum(temp_twl$Deleted == TRUE)/(sum(temp_twl$Deleted == TRUE)+sum(temp_twl$Deleted == FALSE)))
  print(pct_twl_excl)
  GLSGPS_metadata$pct_twl_excl[i] <- pct_twl_excl
  }

write.csv(GLSGPS_metadata, "GLSGPS_metadata.csv",row.names = F)


#************************************************************
####### FlightR Geolocator twilight analysis loop ###########
#************************************************************

#Note: be sure that the GLSGPS_metadata is loaded into the workspace (see above)

complete <- function(rw = 1:nrow(GLSGPS_metadata),o,m) {
  if(m==F){inland <- -Inf} else {inland <- -1}
  print(c(o,m))
  for(i in rw) {
    ### Convert tags file into FLightR-compatible format
    file <- GLSGPS_metadata$TAGS_file[i]
    print(paste(i,file,paste0("light_data/",gsub("_([0-9]|R).*","",file))))
    #tag <- paste0(gsub("_tags.csv","",file),".data") #In case you want to add it as dataframe using "assign" function
    if(GLSGPS_metadata$calibration_type[i] == "external_stationary"){
      print('external')
      tag.data.calib <- get.tags.data(paste0("light_data/",gsub("_([0-9]|R).*","",file),"/external_calibration_files/tags/",file))
    } else {
      print('internal')
      tag.data.calib <- get.tags.data(paste0("light_data/",gsub("_([0-9]|R).*","",file),"/tags/",file))
    }
    #View(tag.data) #If needed to check that its working
    
    ### Calibration ###
    calib.lon <- GLSGPS_metadata$gls_cal_lon[i]
    calib.lat <- GLSGPS_metadata$gls_cal_lat[i]
    calib.start <- as.POSIXct(c(GLSGPS_metadata$gls_cal_start[i]),tz="GMT")
    calib.end <- as.POSIXct(c(GLSGPS_metadata$gls_cal_end[i]+1),tz="GMT") #+1 day because at least the sunrise incl from end day
    if(find.loc == TRUE){
      Location<-find.stationary.location(tag.data.calib, calib.start, calib.end,
                                         initial.coords=c(calib.lon, calib.lat),
                                         plot = TRUE)
      #View(Location)
    } else {
      Location <- c(calib.lon,calib.lat)
    }
    if(Read.calib == FALSE){
        Calibration.periods <- data.frame(calibration.start=calib.start, 
                                      calibration.stop=calib.end, 
                                        lon=Location[1], lat=Location[2])
        print(Calibration.periods) #to check
        Calibration <- make.calibration(tag.data.calib, Calibration.periods, model.ageing = FALSE, 
                                      plot.each = FALSE, plot.final = TRUE,
                                      likelihood.correction = calib.corr,
                                      fixed.logSlope = logSlope) #Ageing not necessary since all have only one brief calibration period
        saveRDS(Calibration, file = paste0("calibration_files/", gsub("_tags.csv","",file),".Fcal")) #Save the Calibration file for future use
        
    } else {
      if(file.exists(paste0("calibration_files/", gsub("_tags.csv","",file),".Fcal"))){
        Calibration <- readRDS(paste0("calibration_files/", gsub("_tags.csv","",file),".Fcal"))
      } else { #In case the calibration file is missing
        Calibration.periods <- data.frame(calibration.start=calib.start, 
                                          calibration.stop=calib.end, 
                                          lon=Location[1], lat=Location[2])
        print(Calibration.periods) #to check
        Calibration <- make.calibration(tag.data.calib, Calibration.periods, model.ageing = FALSE, 
                                        plot.each = FALSE, plot.final = TRUE,
                                        likelihood.correction = calib.corr,
                                        fixed.logSlope = logSlope) #Ageing not necessary since all have only one brief calibration period
        saveRDS(Calibration, file = paste0("calibration_files/", gsub("_tags.csv","",file),".Fcal")) #Save the Calibration file for future use
      }
    }
    
    ### Movement grid for particles ###
    captr.lon <- GLSGPS_metadata$colony_lon[i]  #capture longitude
    captr.lat <- GLSGPS_metadata$colony_lat[i]  #capture latitude
    wLimit <- if(captr.lon - lontol < -180){captr.lon + (360-lontol)} else {captr.lon - lontol} #define relative spatial extents
    eLimit <- if(captr.lon + lontol > 180){captr.lon - (360-lontol)} else {captr.lon + lontol}
    sLimit <- if(captr.lat > 0){captr.lat - (lattol*2)} else {captr.lat - lattol}
    nLimit <- if(captr.lat > 0){captr.lat + lattol} else {captr.lat + (lattol*2)}
    print(paste0("Lon: ",wLimit,", ",eLimit," Lat: ",sLimit,", ",nLimit))
    Grid<-make.grid(left= wLimit, bottom=sLimit, right= eLimit, top=nLimit,
                    distance.from.land.allowed.to.use=c(inland, Inf), 
                    distance.from.land.allowed.to.stay=c(inland, Inf), plot = F) #Change plot to TRUE to check grid map
    
    ### All-in ###
    deply.start <- as.POSIXct(c(GLSGPS_metadata$dbl_tag_start[i]),tz="GMT")
    deply.end <- as.POSIXct(c(GLSGPS_metadata$dbl_tag_end[i]),tz="GMT")
    tag.data.dbl <- get.tags.data(filename = paste0("light_data/",gsub("_([0-9]|R).*","",file),"/tags/",file),
                                  start.date = deply.start, end.date = deply.end)
    print("tag.data.dbl loaded")
    allin.filename <- paste0("all_ins/",gsub("_tags.csv","",file),".AI_i",inland,"_lim",wLimit,".",eLimit,".",sLimit,".",nLimit)
    
    if(Read.allin == FALSE){
      a<-Sys.time()
      all.in<-make.prerun.object(tag.data.dbl, Grid, start=c(captr.lon, captr.lat), 
                               Calibration=Calibration, threads = -1,
                               likelihood.correction = calib.corr)
      print(paste("Elasped time for all-in: ",Sys.time()-a))
      saveRDS(all.in, file=allin.filename)
    } else {
      if(file.exists(allin.filename)){
        print('Reading All-In')
        all.in <- readRDS(allin.filename)
      } else {
        #In case you asked to load a file that didn't yet exist
        a<-Sys.time()
        all.in<-make.prerun.object(tag.data.dbl, Grid, start=c(captr.lon, captr.lat), 
                                   Calibration=Calibration, threads = -1,
                                   likelihood.correction = calib.corr)
        print(paste("Elasped time for all-in: ",Sys.time()-a))
        saveRDS(all.in, file=allin.filename)
      }
    }
    
    ### Main Particle filter ###
    k <- GLSGPS_metadata$battery_OK_recovery[i]

    result.name <- paste0("results/",gsub("_tags.csv","",file),".pSD",p.sd,"_o",o,"_inland",inland,"_b",daily.max,"_known",k,"_",nParticles)
    print(result.name)
    if(Read.result == FALSE){
      #For "threads" "-1" retains one core for other processes, while "0" uses all cores
      a<-Sys.time()
      Result<-run.particle.filter(all.in, threads=-1,
                                nParticles=nParticles, known.last=k,
                                b=daily.max, check.outliers=o, precision.sd= p.sd)
      print(paste("Elasped time for PF of ",nParticles, ": ", Sys.time()-a))
      saveRDS(Result, file=result.name)
      print("Result saved")
    } else {
      if(file.exists(result.name)){
        Result <- readRDS(result.name)} else {
          print("skipping")
          next}
    }
    
    #Plot on map
    if(plot.map == TRUE){
      dates<-data.frame(start=deply.start,end=deply.end)
      map.FLightR.ggmap(Result, dates = dates, plot.cloud = TRUE,
                      map.options=list(zoom=zoom,scale=2,maptype="satellite",
                                       color="color"), plot.options = NULL,
                      save.options = list(filename = paste0("maps/",gsub("_tags.csv","",file),".pSD",p.sd,"_o",o,"_inland",inland,"_b",daily.max,"_known",k,"_",nParticles,".png"),
                                          device = "png"),
                      zoom = zoom, return.ggobj = FALSE, seasonal.donut.location = NULL,
                      save = TRUE)
    }
    #Plot and save Lat Lon probabilities
    png(file=paste0("LatLon/",gsub("_tags.csv","",file),".pSD",p.sd,"_o",o,"_inland",inland,"_b",daily.max,"_known",k,"_",nParticles,".png"),
        units="in",width=7,height=5,res=700)

    plot_lon_lat(Result)
    dev.off()
    
    ## Extract the daily positional data and save
    locations<-as.data.frame(cbind(Result$Results$Quantiles$time,Result$Results$Quantiles$Meanlat,
                                   Result$Results$Quantiles$Meanlon,Result$Results$Quantiles$Medianlat,
                                   Result$Results$Quantiles$Medianlon, Result$Results$Quantiles$UCI.lon,
                                   Result$Results$Quantiles$UCI.lat, Result$Results$Quantiles$FstQu.lon,
                                   Result$Results$Quantiles$FstQu.lat, Result$Results$Quantiles$TrdQu.lon, 
                                   Result$Results$Quantiles$TrdQu.lat, Result$Results$Quantiles$LCI.lon, 
                                   Result$Results$Quantiles$LCI.lat)) # extract mean, median, and quantile estimates 
    # add new variables and rename columns
    colnames(locations) <- c("datetime","meanLat","meanLon","medianLat","medianLon","UCI.lon",
                             "UCI.lat","FstQu.lon","FstQu.lat","TrdQu.lon","TrdQu.lat","LCI.lon","LCI.lat") # rename columns in dataframe
    locations$datetime<-as.POSIXct(locations$datetime, origin = "1970-01-01", tz="GMT") # convert date number to correct format
    locations$year<-as.factor(substr(locations$datetime,1,4)) # new variable "year"
    locations$month<-as.factor(substr(locations$datetime,6,7)) # new variable "month"
    locations$day<-as.factor(substr(locations$datetime,9,10)) # new variable "day"
    locations$tag<-gsub("_tags.csv","",file) # new variable "tag" id
    
    #Write table with mean lat, mean lon positions, the final product for each tag analysis
    write.csv(locations, file = paste0("positions/",gsub("_tags.csv","",file),
                                       ".pSD",p.sd,"_o",o,"_inland",inland,"_b",daily.max,"_known",k,"_",nParticles,"positions_wQ.csv"))

    #Plot use distribution
    if(plot.util == TRUE){
      plot_util_distr(Result, dates=data.frame(deply.start, deply.end), map.options = list(location = c(lon = captr.lon, lat = captr.lat)),
      save.options = list(filename = paste0("util/",gsub("_tags.csv","",file),
                                            ".pSD",p.sd,"_o",o,"_inland",inland,"_b",daily.max,"_known",k,"_",nParticles,".png"),device = "png"),
      add.scale.bar=FALSE, percentiles=c(0.2,0.4,0.6,0.8), zoom=zoom,scale=zoom,save = TRUE)
    }
  }
}

#Load these libraries if you haven't already at the top of the code
library(grid)
library(ggplot2)
library("ggmap", lib.loc="~/R/win-library/3.4")
library(maptools)
library(FLightR)
library(lubridate)
register_google("------------------------") #IMPORTANT: You will need personal Google Key to plot the output maps
has_google_key()
Sys.setenv(TZ="UTC") #This or Sys.setenv(tz="GMT") will prevent R from converting time to local in analysis

########## GLS loop settings ########
setwd(project_folder)

#Calibration options
calib.corr = T #Might need to set to FALSE for early Biotrack/BAS tags
find.loc = F #If unsure about the calibration location
logSlope = c(NA,NA) #If you want to manually set the mean and sd for the calibration slope

#Read in existing files?
Read.calib = T
Read.allin = T
Read.result = F

#Plotting options
plot.map = F #Turn off to save Google API usage if you already have generated this map
plot.util = F #Turn off to save Google API usage if you already have generated this distribution
zoom = "auto" #For plotted maps. Can use 'auto', but that really chews through API quota

#Grid settings:
inland <- -Inf #Number of kilometers inland movement is allowed. Use -Inf for no mask
lontol = 35 #Degrees of longitude +/- from capture location bird is allowed to fly
lattol = 25 #Degrees of latitude from capture location bird is allowed to fly toward pole (double toward Equator)
#PF variables
daily.max <- 1500 # Max distance allowed between twilights. Seabirds known for large daily travels (up to 1500km) 
p.sd <- 2.5 #For PRECISION.SD
nParticles=1e4  # 4 to test (~1 min), 6 for real data and refinement (~5 min)
outliers <- c(TRUE,FALSE) #Should outliers be checked?
mask <- c(TRUE,FALSE)

#run the function:
FlightR.runs <- function(outliers,mask) {
  for (o in outliers){
    for (m in mask){
      #complete(rw=c(1:nrow(GLSGPS_metadata)),o,m) # or select specific rows for 'rw')
      complete(rw=c(1:3),o,m)
    }
  }
}

FlightR.runs(outliers,mask)
        
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Calculate daily distance from GPS point  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(readr)
gps <- read_csv("GPS_data/Interpolated_GPS/InterpolatedGPSTrackingData_allbirds.csv") #Check date and datetime format!
#scrub NA data
gps <<- gps[!is.na(gps$Longitude),]
gps <<- gps[!is.na(gps$Latitude),]


library(geosphere)
library(probGLS)
FlightR.positions <- function(outliers,mask){
  for (o in outliers){
    for (m in mask){
      path <- c("C:/Users/Jeremy/Dropbox/Research/Geolocators/GPS_Geoloc_comps_Halpin/twilight_tags_data/outputs/positions")
      filenames <- grep(list.files(path=path),
                        pattern='Inf', inv=m, value=T)
      filenames <- Filter(function(x) grepl(paste0("o",as.character(o)), x), filenames)
      filenames <- Filter(function(x) grepl('b150', x), filenames)
      filenames <- Filter(function(x) grepl('SD2.5', x), filenames)
      filenames <- Filter(function(x) grepl('inland',x), filenames)
      #filenames <- Filter(function(x) grepl(paste0("known",as.character(k)),x), filenames) #If separating known from unknown recovery locations
      filenames <- Filter(function(x) grepl('wQ',x), filenames)
      fullpath = file.path(path,filenames)
      options(stringsAsFactors = FALSE)
      FR_results <- do.call("rbind",lapply(fullpath,FUN=function(files){read.csv(files)}))
      if(is.null(FR_results)){next} #In case you hadn't run all of the known-mask-outlier combinations
      FR_results[,1]<-NULL
      FR_results$datetime <- as.POSIXct(FR_results$datetime)
      write.csv(FR_results,paste0(project_folder,"FlightR_outputs/FR_results_o",o,"_mask",m,".csv"))
      frr3 <- NULL
      Sys.setenv(TZ='GMT') #Make sure we're working in GMT
      #Note: reload FR_results and gps2 using read.csv() and the appropriate row names, if returning to this point
      
      for(i in c(1:length(unique(FR_results$tag)))){
        print(paste(i,length(unique(FR_results$tag)),sep=" / "))
        
        identity <- unique(FR_results$tag)[i]
        FRRi     <- FR_results[FR_results$tag == identity,]
        
        for(j in c(1:(nrow(FRRi)-1))){
          tmp <- FRRi[j,]
          
          tmp$gps.lon.mean  <- NA
          tmp$gps.lat.mean  <- NA
          
          tmp$mean.median.distance   <- NA
          tmp$mean.distance          <- NA
          
          # check if any GPS position for this time and id are available
          tmp2 <- as.data.frame(FRRi[j+1,])
          if(!is.na(tmp2$datetime)){
            gps_FR   <- gps[gps$Datetime >= tmp$datetime-1800 & gps$Datetime <= tmp2$datetime+1800 & gps$indiv==tmp$tag,]
            gps_FR   <- gps_FR[gps_FR$Datetime <= tmp$datetime+1800 | gps_FR$Datetime >= tmp2$datetime-1800,]
            gps_FR   <- gps_FR[!is.na(gps_FR$Longitude),]
            
            if(nrow(gps_FR)>0){
              
              if(nrow(gps_FR)==1){
                  tmp$gps.lon.midpoint.mean <- mean(gps_FR$Longitude)
                  tmp$gps.lat.midpoint.mean <- mean(gps_FR$Latitude)
                } else {
                GeoMean <- geomean(cbind(x=gps_FR$Longitude, y=gps_FR$Latitude))
                
                tmp$gps.lon.midpoint.mean <- GeoMean[,1]
                tmp$gps.lat.midpoint.mean <- GeoMean[,2]
                }

                tmp$midpoint.median.distance      <- spDistsN1(matrix(c(tmp$gps.lon.midpoint.mean,tmp$gps.lat.midpoint.mean),ncol=2),matrix(c(tmp$medianLon,tmp$medianLat),ncol=2),longlat=T)
                tmp$midpoint.min.distance          <- min(spDistsN1(matrix(c(FRRi$medianLon,FRRi$medianLat),ncol=2),matrix(c(tmp$gps.lon.midpoint.mean,tmp$gps.lat.midpoint.mean),ncol=2),longlat=T))[1]
              } else {
                tmp$gps.lon.midpoint.mean <- NA
                tmp$gps.lat.midpoint.mean <- NA
                tmp$midpoint.median.distance <- NA
                tmp$midpoint.min.distance    <- NA
              }
            } else {next}
          
          #need to change column name of datetime to indicate that this is the tFirst
          colnames(tmp)[colnames(tmp)=="datetime"] <- "tFirst"
          tmp$tSecond <- tmp2$datetime #Notate which is the tSecond
          if(is.null(frr3)==T) frr3 <- tmp else frr3 <- rbind(frr3,tmp)
        }
      }
      frr3$dtime <- do.call(c, Map(function(x, y) mean(c(x, y)), frr3$tFirst, frr3$tSecond))
      
      # Save the resulting table, as it contains the final distance comparisons between GLS and GPS tracks!
      write.csv(frr3,paste0(project_folder,"FlightR_outputs/frr3_allrows_o",o,"_mask",m,"_wQ.csv"))
      
      #NOTE: if row numbers are non-sequential use the following to list ALL rows:
      #write.csv(frr3,paste0("frr3_rows",paste(rows,collapse = "_"),".csv"))
    }
  }
}

### Run the GLS-GPS distance caluculations ###
outliers <- c(TRUE) #Can add FALSE if outliers were not checked
mask <- c(TRUE) #Can add FALSE if a landmask was not used

FlightR.positions(outliers,mask)


### Plot the daily patterns of GLS-interpolated latitude and longitude relative to GPS positions ###

plottit <- function(rw = 1:nrow(GLSGPS_metadata),o,m,scheme = c("vertical", "horizontal")) {
    if(m==F){inland <- -Inf} else {inland <- -1}
    setwd(project_folder)
    for (o in outliers){
      for (m in mask){
        ## Load the frr3 file
        frr3_allrows_wQ <- read_csv("FlightR_outputs/frr3_allrows_o",o,"_mask",m,"_wQ.csv")
        for(i in rw) {
          file <- GLSGPS_metadata$TAGS_file[i]
          tag.name <- gsub("_tags.csv","",file)
          print(paste(i,tag.name))
    
          result.name <- paste0("results/",tag.name,".pSD",p.sd,"_o",o,"_inland",inland,"_b",daily.max,"_known",k,"_",nParticles)
          if(file.exists(result.name)){
            Result <- readRDS(result.name)} else {
              print("skipping")
              next}
      
          gps.data <- subset(frr3_allrows_wQ, tag == tag.name & !is.na(gps.lon.mean) & !is.na(gps.lat.mean))
          
        ###########################################################
        Quantiles <- Result$Results$Quantiles
        
        # Function checks if spatial grid crosses the dateline and converts to lon 360 if so
        overdateline <- ifelse(attr(Result$Spatial$Grid, "left") > 
                                 attr(Result$Spatial$Grid, "right"), TRUE, FALSE)
        if (overdateline) {
          Quantiles$Medianlon[Quantiles$Medianlon < 0] <- Quantiles$Medianlon[Quantiles$Medianlon < 
                                                                                0] + 360
          Quantiles$LCI.lon[Quantiles$LCI.lon < 0] <- Quantiles$LCI.lon[Quantiles$LCI.lon < 
                                                                          0] + 360
          Quantiles$UCI.lon[Quantiles$UCI.lon < 0] <- Quantiles$UCI.lon[Quantiles$UCI.lon < 
                                                                          0] + 360
          Quantiles$TrdQu.lon[Quantiles$TrdQu.lon < 0] <- Quantiles$TrdQu.lon[Quantiles$TrdQu.lon < 
                                                                                0] + 360
          Quantiles$FstQu.lon[Quantiles$FstQu.lon < 0] <- Quantiles$FstQu.lon[Quantiles$FstQu.lon < 
                                                                                0] + 360
          gps.data$gps.lon.mean[gps.data$gps.lon.mean < 0] <- gps.data$gps.lon.mean[gps.data$gps.lon.mean < 0] + 360
        }
        
        png(filename=paste0("maps/FlightR_plots/",tag.name,"_o",o,"_m",m,"_latlon.png"))
        if (scheme[1] == "horizontal") {
          graphics::par(mfrow = c(1, 2))
        }
        else {
          graphics::par(mfrow = c(2, 1))
        }
        graphics::par(mar = c(2, 4, 3, 1), cex = 1)
        suppressWarnings(Sys.setlocale("LC_ALL", "English"))
        Years <- unique(format(Quantiles$time, format = "%Y"))
        eq <- c(as.POSIXct(paste(Years, "-09-22 00:00:00 GMT", 
                                 sep = "")), as.POSIXct(paste(Years, "-03-20 12:00:00 GMT", 
                                                              sep = "")))
        eq <- eq[eq > min(Quantiles$time) & eq < max(Quantiles$time)]
        Vert_grid <- seq(as.POSIXct("2000-01-01"), as.POSIXct("2030-01-01"), 
                         by = "day")
        Vert_grid <- Vert_grid[Vert_grid >= min(Quantiles$time) & 
                                 Vert_grid <= max(Quantiles$time)]
        graphics::plot(Quantiles$Medianlon ~ Quantiles$time, las = 1, 
                         col = grDevices::grey(0.1), pch = 16, ylab = "Longitude", 
                         xlab = "", lwd = 2, ylim = range(c(Quantiles$LCI.lon,
                                                            Quantiles$UCI.lon,gps.data$gps.lon.mean)), type = "n", axes = FALSE)
        graphics::axis(2, las = 1)
        graphics::axis.POSIXct(1, x = Quantiles$time, format = "%b %d")
        graphics::box()
        graphics::abline(v = Vert_grid, col = grDevices::grey(0.5), 
                         lty = 2)
        graphics::abline(h = seq(-180, 360, by = 10), col = grDevices::grey(0.5), 
                         lty = 2)
        graphics::abline(v = eq, col = 2, lwd = 2, lty = 1)
        graphics::polygon(x = c(Quantiles$time, rev(Quantiles$time)), 
                          y = c(Quantiles$LCI.lon, rev(Quantiles$UCI.lon)), col = grDevices::grey(0.9), 
                          border = grDevices::grey(0.5))
        
        graphics::polygon(x = c(Quantiles$time, rev(Quantiles$time)), 
                          y = c(Quantiles$TrdQu.lon, rev(Quantiles$FstQu.lon)), 
                          col = grDevices::grey(0.7), border = grDevices::grey(0.5))
        #graphics::lines(gps.data$gps.lon.mean ~ gps.data$dtime, lty = 1, lwd = 2, col =  4) #This works, too
        graphics::lines(x= gps.data$dtime,
                        y = gps.data$gps.lon.mean,  lty = 1, lwd = 2, col =  4)
        graphics::lines(Quantiles$Medianlon ~ Quantiles$time, col = grDevices::grey(0.1), 
                        lwd = 2)
        title(main = paste("Bird ID:",GLSGPS_metadata$indiv[i],"\n", "Species:", GLSGPS_metadata$species_com[i]), cex.main = 0.8, font.main = 1)
        
        graphics::par(mar = c(3, 4, 1, 1))
        graphics::plot(Quantiles$Medianlat ~ Quantiles$time, las = 1, 
                       col = grDevices::grey(0.1), pch = 16, ylab = "Latitude", 
                       xlab = "", lwd = 2, ylim = range(c(Quantiles$UCI.lat, 
                                                          Quantiles$LCI.lat,gps.data$gps.lat.mean)), type = "n", axes = FALSE)
        graphics::axis(2, las = 1)
        graphics::axis.POSIXct(1, x = Quantiles$time, format = "%b %d")
        graphics::box()
        graphics::abline(v = Vert_grid, col = grDevices::grey(0.5), 
                         lty = 2)
        graphics::abline(h = seq(-80, 80, by = 10), col = grDevices::grey(0.5), 
                         lty = 2)
        graphics::abline(v = eq, col = 2, lwd = 2, lty = 1)
        graphics::polygon(x = c(Quantiles$time, rev(Quantiles$time)), 
                          y = c(Quantiles$LCI.lat, rev(Quantiles$UCI.lat)), col = grDevices::grey(0.9), 
                          border = grDevices::grey(0.5))
        graphics::polygon(x = c(Quantiles$time, rev(Quantiles$time)), 
                          y = c(Quantiles$TrdQu.lat, rev(Quantiles$FstQu.lat)), 
                          col = grDevices::grey(0.7), border = grDevices::grey(0.5))
        #graphics::lines(gps.data$gps.lat.mean ~ gps.data$dtime, lty = 1, lwd = 2, col =  4) #This works, too
        graphics::lines(x= gps.data$dtime,
                        y = gps.data$gps.lat.mean,  lty = 1, lwd = 2, col =  4)
        graphics::lines(Quantiles$Medianlat ~ Quantiles$time, col = grDevices::grey(0.1), 
                        lwd = 2)
        dev.off()
        }
      }
    }
}


row.nos=c(1:nrow(GLSGPS_metadata))
outliers <- c(TRUE) #Should outliers be checked?
mask <- c(TRUE)

plottit(row.nos,outliers,mask)