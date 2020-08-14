#                  ****** GLS-GPS Sea Surface Temperature analysis script ******
#                             ****** probGLS *******
#                           Luke Halpin & Jeremy Ross         

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load libraries ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(probGLS)
library(ncdf4)
library(rowr)
library(curl)
library(maptools)
library(readr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load metadata and GPS track data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("~/R/project_folder") #IMPORTANT: edit for YOUR local directory!!!
# Folders within this working directory should include "light_data" and "SST" that within each have a separate folder for each site within the study
# Proper folder structure and filename consistency will be absolutely necessary to call the proper folders/files using the metadata as a guide !!

GLSGPS_metadata <- read_csv("GLSGPS_metadata.csv") #Be sure to check datetime formatting!

#Reduce metadata to only those birds with valid Wet-Dry and SST data
GLSGPS_metadata_SST <- GLSGPS_metadata[!(is.na(GLSGPS_metadata$gls_temp) | is.na(GLSGPS_metadata$gls_wet_dry) | is.na(GLSGPS_metadata$gls_temp_start)),]
rm(GLSGPS_metadata)

#Add information about flight speeds based on taxonomic group (if available)
GLSGPS_metadata_SST$flight_speed <- ifelse(GLSGPS_metadata_SST$flight_group == "gadfly",13.9,
                                           ifelse(GLSGPS_metadata_SST$flight_group == "shearwater",14.0,
                                                  ifelse(GLSGPS_metadata_SST$flight_group == "booby",17.1,ifelse(GLSGPS_metadata_SST$flight_group == "tropicbird",16.5,NA))))

gps <- read_csv("local_directory/InterpolatedGPSTrackingData.csv") #Ensure that datetime columns are in POSIX format

#*******************************************************#
#### Function to load twilight, sensor, and GPS data ####
#*******************************************************#

probGLS.run <- function(row, particles) {
  
  newt2<-NULL
  Sys.setenv(TZ='GMT') #Make sure you're working in GMT

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### Load and format the twilight data ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (make.trn2 == T){
    tag.data.table <- data.frame(0)
    j <- 0
    for(i in row) {
      j <- j+1
      
      # Retrieve twilights from TAGS files
      tag.file <- GLSGPS_metadata_SST$TAGS_file[i]
      deply.start <- as.POSIXct(GLSGPS_metadata_SST$dbl_tag_start[i],tz="GMT")
      deply.end <- as.POSIXct(GLSGPS_metadata_SST$dbl_tag_end[i],tz="GMT")
      print(paste("Row",i,tag.file, deply.start, deply.end))
      tag.data.dbl <- get.tags.data(filename = paste0("light_data/",gsub("_([0-9]|R).*","",tag.file),"/tags/",tag.file),
                                    start.date = deply.start, end.date = deply.end)
      tag.data.table <- subset(tag.data.dbl[["FLightR.data"]][["twilights"]], select = c(datetime,type))
      tag.data.table <- cbind.fill(tag.data.table, tag.data.table$datetime[2:nrow(tag.data.table)],fill=NA)
      colnames(tag.data.table) <- c("tFirst","type","tSecond")
      tag.data.table <- tag.data.table[,c("tFirst","tSecond","type")]
      tag.data.table <- tag.data.table[1:(nrow(tag.data.table)-1),] #remove the last row, since there's no tSecond for it
      tag.data.table$tSecond <- as.POSIXct(tag.data.table$tSecond)
      tag.data.table$id <- GLSGPS_metadata_SST$indiv[i]
      if (j==1) trn2 <- tag.data.table else trn2<-rbind(trn2,tag.data.table)
    }
    trn2$daylength  <- abs(as.numeric(difftime(trn2$tFirst, trn2$tSecond, units="hours")))
    trn2            <- trn2[trn2$daylength < 24,]
    rownames(trn2) <- seq(length=nrow(trn2))
    saveRDS(trn2,"probGLS_outputs/trn2.out")
  } else {trn2 <- readRDS("probGLS_outputs/trn2.out")}

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load and format the wet-dry & SST data ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (use.sst == T){
    if (load.sst == F){
      k <- 0
      for (i in row){
        k <- k+1
        folder <- paste0("SST/",gsub("_([0-9]|R).*","",GLSGPS_metadata_SST$TAGS_file[i]))
        
        #>>>>>>>>>>> Wet-Dry data <<<<<<<<<<<<<<<<< #
        wd.file <- GLSGPS_metadata_SST$gls_wet_dry[i]
        wd.format <- gsub(".*\\.([a-z]{3})","\\1",wd.file)
        #print(wd.format) #Comment/Uncomment to use as checkpoint
        if(wd.format == "deg"){
          ad  <- read.table(paste0(folder,"/",wd.file),sep="\t",skip=19,header=T,quote="")
          } else if(wd.format == "act"){
            ad <- read.csv(paste0(folder,"/",wd.file),header=F,colClasses=c("NULL", NA, "NULL", NA, NA))
            #Use NULL to omit "ok" and decimal time columns
            } else {
              print(paste("Invalid WD format:",wd.format))
              next
            }
        colnames(ad) <- c("datetime","duration","wet.dry")
        ad$dtime       <- as.POSIXct(strptime(ad$datetime, format = "%d/%m/%Y %H:%M:%S"),tz='UTC') #to ensure proper formatting
        if(gsub("([0-2]{2}).*","\\1",as.Date(ad$dtime[1])) == "00"){
          ad$dtime       <- as.POSIXct(strptime(ad$datetime, format = "%d/%m/%y %H:%M:%S"),tz='UTC')
        }
        ad$date        <- as.Date(ad$dtime)
        ad$id          <- GLSGPS_metadata_SST$indiv[i]
        ad             <- ad[ad$date >= GLSGPS_metadata_SST$gls_temp_start[i] & ad$date <= GLSGPS_metadata_SST$gls_temp_end[i],]
        ad$WetDryState <- ad$duration/60/60
        ad             <- ad[ad$wet.dry=="wet",]
  
        #>>>>>>>>>>>>>> Sea-surface temperature data <<<<<<<<<<<<<<<<<<#
        sst.file <- GLSGPS_metadata_SST$gls_temp[i]
        if(is.na(sst.file) == TRUE){
          sst.format <- NA
          next
          } else {
          sst.format <- gsub(".*\\.([a-z]{3})","\\1",sst.file)
          if (sst.format == "sst"){
            td             <- read.table(paste0(folder,"/",sst.file),sep="\t",skip=19,header=T,quote="")
            colnames(td) <- c("datetime","wet_min","wet_max","wet_mean","samples")
            td$dtime       <- as.POSIXct(strptime(td$datetime, format = "%d/%m/%Y %H:%M:%S"),tz='UTC') #to ensure proper formatting
          } else if (sst.format == "tem"){
              td <- read.csv(paste0(folder,"/",sst.file),header = F, colClasses=c("NULL", NA, "NULL", NA))
              colnames(td) <- c("datetime","wet_mean")
              td$dtime       <- as.POSIXct(strptime(td$datetime, format = "%d/%m/%y %H:%M:%S"),tz='UTC') #to ensure proper formatting
              } else {
                print(paste('Invalid SST format:',sst.format))
                next
                }
  
          sensor         <- sst_deduction(datetime = td$dtime,temp = td$wet_mean,temp.range = c(0,35))
          sensor$doy     <- as.numeric(strftime(sensor$date, format = "%j"))
          sensor$month   <- as.numeric(strftime(sensor$date, format = "%m"))
          sensor$year    <- as.numeric(strftime(sensor$date, format = "%Y"))
          sensor$jday    <- as.numeric(julian(sensor$date))
          sensor$id      <- GLSGPS_metadata_SST$indiv[i]
          }
        if(k==1){ act   <- ad} else {act <- rbind(act,ad)}
        if(k==1){ sen   <- sensor} else {sen <- rbind(sen,sensor)}
        }
      #Save the files for reloading
      saveRDS(act, paste0("probGLS_outputs/act_rows",min(rows),"to",max(rows),".out"))
      saveRDS(sen, paste0("probGLS_outputs/sen_rows",min(rows),"to",max(rows),".out"))
    } else {
      act <- readRDS(paste0("probGLS_outputs/act_rows",min(rows),"to",max(rows),".out"))
      sen <- readRDS(paste0("probGLS_outputs/sen_rows",min(rows),"to",max(rows),".out"))
    }
  } else {
      act <- NULL
      sen <- NULL
    }

  #~~~~~~~~~~~~~~~~~~~
  # Load GPS data ----
  #~~~~~~~~~~~~~~~~~~~
  gps$i.id <- NA
  for(i in row){
    start            <- as.POSIXct(GLSGPS_metadata_SST$dbl_tag_start[i],tz="GMT")
    end              <- as.POSIXct(GLSGPS_metadata_SST$dbl_tag_end[i],tz="GMT")
    id               <- GLSGPS_metadata_SST$indiv[i]
    gps$i.id[gps$dtime >= start & gps$dtime <= end & gps$indiv == id]      <- paste(i,id,sep='.')
  }
  gps2 <<- gps[!is.na(gps$i.id),]
   
  rm(ad,sensor,td)
  rm(end,i,id,start)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Run model with 200 iterations and desired number of particles ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  l = 0
  for(i in row){
    l = l+1
    
    # Filter the data according to the individual being analyzed (i.e., row "i")
    start            <- as.POSIXct(strptime(GLSGPS_metadata_SST$dbl_tag_start[i], format = "%Y-%m-%d"),tz='UTC')
    end              <- as.POSIXct(strptime(GLSGPS_metadata_SST$dbl_tag_end[i], format = "%Y-%m-%d"),tz='UTC')
    id               <- GLSGPS_metadata_SST$indiv[i]
    print (paste("Row",i, "-",id)) #Comment/Uncomment to use as checkpoint
    
    # Populate the parameters for the particle filter run
    trn              <- trn2[trn2$id==id,]
    if(nrow(trn)>1){
      trn$id           <- id
      trn$i            <- i
      trn$i.id         <- paste(trn$i,trn$id,sep='.')
      trn              <- trn[trn$tFirst >= (start) & trn$tSecond <= (end),]
      if (use.sst == T){
        sensor           <- sen[sen$id==id,]
        sensor           <- sensor[sensor$date >= as.Date(start) & sensor$date <= as.Date(end),]
        sensor$id        <- id
        sensor$i         <- i
        sensor$i.id      <- paste(sensor$i,sensor$id,sep='.')
        act2             <- act[act$id==id,]
        act2$i           <- i
        act2$i.id        <- paste(act2$i,act2$id,sep='.')
      } else {
          sensor <- NULL
          act2 <- NULL}
  
      
      #Make the dynamic boundary box to limit unrealistic movements using lat/lon tolerances set by user
      captr.lon <- GLSGPS_metadata_SST$colony_lon[i]
      captr.lat <- GLSGPS_metadata_SST$colony_lat[i]
      wLimit <- if(captr.lon - lontol < -180){captr.lon + (360-lontol)} else {captr.lon - lontol}
      wLimit <- round(wLimit/0.25)*0.25
      eLimit <- if(captr.lon + lontol > 180){captr.lon - (360-lontol)} else {captr.lon + lontol}
      eLimit <- round(eLimit/0.25)*0.25
      #Latitude tolerances twice as large toward equator
      sLimit <- if(captr.lat > 0){captr.lat - (lattol*2)} else {captr.lat - lattol} 
      sLimit <- round(sLimit/0.25)*0.25
      nLimit <- if(captr.lat > 0){captr.lat + lattol} else {captr.lat + (lattol*2)}
      nLimit <- round(nLimit/0.25)*0.25
      #print(paste0("Lon: ",wLimit,", ",eLimit," Lat: ",sLimit,", ",nLimit)) #Comment/Uncomment to use as checkpoint
      
  
        # Load and run the particle filter
        p = 0
        for(part in particles){
          p = p+1
          a<-Sys.time()
          mmlist.name <- paste0("mm_lists/",id,"_SSD_",use.sst,"_mask_",landmask,"_particles",particles)
          #print(mmlist.name) #Comment/Uncomment to use as checkpoint
          if(Read.mmlist == TRUE){
            if(file.exists(mmlist.name)){
              Result <- readRDS(mmlist.name)} else {
                print("No mm.list file found")
                next}
          } else {
  
          ##### Alter less-salient parameters of the probGLS particle filter #######
            mm.list <- prob_algorithm(particle.number             = part, 
                                     iteration.number            = iterations,
                                     trn                         = trn, 
                                     sensor                      = sensor, 
                                     act                         = act2, 
                                     loess.quartile              = NULL, 
                                     sunrise.sd                  = tw,
                                     sunset.sd                   = tw,
                                     tagging.location            = c(captr.lon,captr.lat),
                                     tagging.date                = start, 
                                     retrieval.date              = end, 
                                     speed.wet                   = c(1,1.3,5), #optimal speed, speed standard deviation and max speed allowed if logger is wet in m/s
                                     speed.dry                   = c(GLSGPS_metadata_SST$flight_speed[i],5,27.75), #optimal speed, speed standard deviation and max speed allowed if logger is dry in m/s
                                     boundary.box                = c(wLimit,eLimit,sLimit,nLimit),
                                     days.around.spring.equinox  = c(20,20), 
                                     days.around.fall.equinox    = c(20,20),
                                     ice.conc.cutoff             = 0.25, 
                                     land.mask                   = landmask,
                                     med.sea                     = F,
                                     black.sea                   = F,
                                     baltic.sea                  = F,
                                     caspian.sea                 = F,
                                     wetdry.resolution           = 6,
                                     NOAA.OI.location = "local_directory/NOAA_SST") #Needs to be downloaded from https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html
          
          print(paste("Elasped time for run of ",id, ": ", Sys.time()-a))
          
          # Plot the results and format & combined the outputs for the computed tracks
          png(file=paste0("maps/probGLS_plots/",id,"_SST_",use.sst,"_mask_",landmask,"_particles",particles,".png"), 
              units="in",width=7,height=5,res=700)
          plot_timeline(mm.list,degElevation = NULL)
          dev.off()
          plot_map(mm.list)
          saveRDS(mm.list, file=mmlist.name)
          
          newt3      <- mm.list[[1]]
          newt3$id   <- id
          newt3$i    <- i
          newt3$i.id <- paste(newt3$i,newt3$id,sep='.')
          newt3$available.particles <- part
          #print('newt3 produced') #Comment/Uncomment to use as checkpoint
          if(p==1){newt4 <- newt3} else {newt4 <- rbind(newt4,newt3)}
          }
        }
    } else {next}
    print("pf runs complete")
    if(l==1) newt5 <- newt4 else newt5 <- rbind(newt5,newt4) #changed from spRbind() as this was causing undiagnosible error
  }
  # dataframe containing all computed tracks for each individual using 200 iterations and the set number of particles
  newt5$identity <- paste(newt5$available.particles, newt5$step, newt5$i.id, sep=".")
  newt5          <<- data.frame(newt5)
  
  # Save the output files to reload them in the future
  write.csv(newt5,paste0("probGLS_outputs/newt5_rows",min(rows),"thru",max(rows),".SST_",use.sst,".mask_",!is.null(landmask),".csv"))
  write.csv(gps2,paste0("probGLS_outputs/gps2_rows",min(rows),"thru",max(rows),".SST_",use.sst,".mask_",!is.null(landmask),".csv"))
  
  #NOTE: if row numbers are non-sequential use the following to list ALL rows:
  # write.csv(newt5,paste0("newt5_rows",paste(rows,collapse = "_"),".csv"))
  # write.csv(gps2,paste0("gps2_rows",paste(rows,collapse = "_"),".csv"))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Calculate daily median geographic position across the 200 iterations  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

calc.positions.new <- function(newt5,SST,mask) {
  nag3 <- NULL
  polyf <<- NULL
  Sys.setenv(TZ='GMT') #Make sure we're working in GMT
  setwd("local_directory/outputs")

  # Revise the "plot_timeline" function of probGLS to include the ability to superimpose GPS tracks
  plot_timeline_wGPS <- function(pr,degElevation,s){
   
    if(as.character(pr[[4]]$chosen[pr[[4]]$parameter=="sensor.data"])=="TRUE") SST=T else SST=F
      #to shortcut the SST sensor status just use:   SST <- s
    
    if(!is.null(degElevation)){
     ho2 <- data.frame(pr[[2]])
     ho2$tFirst  <- ho2$tFirst  + ho2$tFirst.err
     ho2$tSecond <- ho2$tSecond + ho2$tSecond.err
     ho2$lons <- coord(tFirst=ho2$tFirst,tSecond=ho2$tSecond,type=ho2$type,degElevation = degElevation,note=F)[,1]
     ho2$lats <- coord(tFirst=ho2$tFirst,tSecond=ho2$tSecond,type=ho2$type,degElevation = degElevation,note=F)[,2]
    }
    
    se <- as.numeric(unlist(strsplit(as.character(pr[[4]][14,2]),'[ ]'))) #days.around.spring.equinox
    fe <- as.numeric(unlist(strsplit(as.character(pr[[4]][15,2]),'[ ]'))) #days.around.fall.equinox
    #to shortcut setting the days around equinox function to 20 days +/- use:
    # se <- as.numeric(c(20,20))
    # fe <- as.numeric(c(20,20))
     
    se <- c(79  - se[1], 79  + se[2])    # doy 79  = 20 March; doy 265 = 22 September
    fe <- c(265 - fe[1], 265 + fe[2])
    
    years <- unique(pr[[1]]$year)
    
    jday  <- floor(as.numeric(julian(ISOdate(years,1,1))))
    
    jse1 <- jday+se[1]
    jse2 <- jday+se[2]
    jfe1 <- jday+fe[1]
    jfe2 <- jday+fe[2]
    
    poly.frame<-function(data1,data2,prob1,prob2){
     polyf<<-data.frame(c(unique(data1[order(data1)]),unique(data1[order(data1,decreasing=T)])),c(tapply(data2,data1,quantile, probs = prob1,na.rm=T),tapply(data2,data1,quantile, probs = prob2,na.rm=T)[order(as.numeric(names(tapply(data2,data1,quantile, probs = prob2,na.rm=T))),decreasing=T)]))
     return(polyf)
    }
    
    #Account for plotting tracks with dateline crossings
    
    if(max(pr[[1]]$lon)-min(pr[[1]]$lon) > 300){
     x1 <- data.frame(pr[[1]])
     x1$lon[x1$lon<0] <- x1$lon[x1$lon<0]+360
     x2 <- data.frame(pr[[2]])
     x2$lon[x2$lon<0] <- x2$lon[x2$lon<0]+360
     if(!is.null(degElevation)) {
       x3 <-ho2
       x3$lons[x3$lons<0] <- x3$lons[x3$lons<0]+360
     }
      long.label = "Longitude [0 to 360]"
    } else {
     x1 <- pr[[1]]
     x2 <- pr[[2]]
     if(!is.null(degElevation)) x3 <-ho2
     long.label = "Longitude [-180 to 180]"
    }
    
    
    if(SST==T){
     
     opar <- par(mfrow=c(3,1),mar=c(0,4,2,0),oma=c(2,0,0,0))
     
     plot(x1$jday,x1$lon,col='white',xaxt="n",ylab=long.label,
          main = paste("Bird ID:",x1$id[1],"\n", "Species:", x1$species_com[1]))
     polygon(poly.frame(x1$jday,x1$lon,0.75,0.25),col=rgb(1,0,0,alpha=0.3) ,border=NA)
     polygon(poly.frame(x1$jday,x1$lon,0.95,0.05),col=rgb(1,0,0,alpha=0.3) ,border=NA)
     colnames(polyf) <- c("jday","Lon.CI")
     CI90.lon <<- polyf
     lines(x2$jday,x2$lon,col='darkred',lwd=1,type="o",cex=1)
     lines(x2$jday,x2$gps.lon.midpoint.mean,col='blue',lwd=1,type="o",cex=1) #adding the line of GPS midpoints between twilights
     if(!is.null(degElevation)) lines(x3$jday,x3$lons,lwd=1,type="o",cex=1)
     
     plot(pr[[1]]$jday,pr[[1]]$lat,col='white',xaxt="n",ylab="Latitude")
     polygon(poly.frame(pr[[1]]$jday,pr[[1]]$lat,0.75,0.25),col=rgb(1,0,0,alpha=0.3) ,border=NA)
     polygon(poly.frame(pr[[1]]$jday,pr[[1]]$lat,0.95,0.05),col=rgb(1,0,0,alpha=0.3) ,border=NA)
     colnames(polyf) <- c("jday","Lat.CI")
     CI90.lat <<- polyf
     lines(pr[[2]]$jday,pr[[2]]$lat,col='darkred',lwd=1,type="o",cex=1)
     lines(pr[[2]]$jday,pr[[2]]$gps.lat.midpoint.mean,col='blue',lwd=1,type="o",cex=1) #adding the line of GPS midpoints between twilights
     if(!is.null(degElevation)) lines(ho2$jday,ho2$lats,lwd=1,type="o",cex=1)
     abline(v=c(jse1,jse2),lty=3,col="green")
     abline(v=c(jfe1,jfe2),lty=3,col="blue")
     
     plot  (pr[[1]]$jday,pr[[1]]$sat.sst,col="white",ylab="SST",xaxt="n")
     polygon(poly.frame(pr[[1]]$jday,pr[[1]]$sat.sst,0.75,0.25),col=rgb(1,0,0,alpha=0.3) ,border=NA)
     polygon(poly.frame(pr[[1]]$jday,pr[[1]]$sat.sst,0.95,0.05),col=rgb(1,0,0,alpha=0.3) ,border=NA)
     colnames(polyf) <- c("jday","SST.CI")
     CI90.sst <<- polyf
     points(pr[[2]]$jday,pr[[2]]$median.sat.sst,type='o',lwd=1,col="darkred",cex=1)
     points(pr[[2]]$jday,pr[[2]]$tag.sst,type='o',lwd=1,cex=1)
     axis(1,at=floor(pr[[2]]$jday),labels=as.Date(floor(pr[[2]]$jday),origin="1970-01-01"))
    }
    if(SST==F){
     opar <- par(mfrow=c(2,1),mar=c(0,4,2,0),oma=c(2,0,0,0))
    
     plot(x1$jday,x1$lon,col='white',xaxt="n",ylab=long.label,
          main = paste("Bird ID:",x1$id[1],"\n", "Species:", x1$species_com[1]))
     polygon(poly.frame(x1$jday,x1$lon,0.75,0.25),col=rgb(1,0,0,alpha=0.3) ,border=NA)
     polygon(poly.frame(x1$jday,x1$lon,0.95,0.05),col=rgb(1,0,0,alpha=0.3) ,border=NA)
     colnames(polyf) <- c("jday","Lon.CI")
     CI90.lon <<- polyf
     lines(x2$jday,x2$lon,col='darkred',lwd=1,type="o",cex=1)
     lines(x2$jday,x2$gps.lon.midpoint.mean,col='blue',lwd=1,type="o",cex=1) #adding the line of GPS midpoints between twilights
     if(!is.null(degElevation)) lines(x3$jday,x3$lons,lwd=1,type="o",cex=1)
     axis(1,at=floor(pr[[2]]$jday),labels=as.Date(floor(pr[[2]]$jday),origin="1970-01-01"))
     
     par(mar=c(2,4,0,0))
     plot(pr[[1]]$jday,pr[[1]]$lat,col='white',xaxt="n",ylab="Latitude")
     polygon(poly.frame(pr[[1]]$jday,pr[[1]]$lat,0.75,0.25),col=rgb(1,0,0,alpha=0.3) ,border=NA)
     polygon(poly.frame(pr[[1]]$jday,pr[[1]]$lat,0.95,0.05),col=rgb(1,0,0,alpha=0.3) ,border=NA)
     colnames(polyf) <- c("jday","Lat.CI")
     CI90.lat <<- polyf
     lines(pr[[2]]$jday,pr[[2]]$lat,col='darkred',lwd=1,type="o",cex=1)
     lines(pr[[2]]$jday,pr[[2]]$gps.lat.midpoint.mean,col='blue',lwd=1,type="o",cex=1) #adding the line of GPS midpoints between twilights
     if(!is.null(degElevation)) lines(ho2$jday,ho2$lats,lwd=1,type="o",cex=1)
     abline(v=c(jse1,jse2),lty=3,col="green")
     abline(v=c(jfe1,jfe2),lty=3,col="blue")
     
    }
    par(opar) 
    
    }
  
  #Iterate through the various combinations of SST and landmask use to calculate a summary file for each
  for(s in SST){
    for(m in mask){
      for(i in rows){
        nag2 <- NULL
        id      <- unique(newt5$id)[i]
        newt5b   <- newt5[newt5$id == id,]
        meta.row <- GLSGPS_metadata_SST[GLSGPS_metadata_SST$id == id,]
        newt5b$species_sci <- meta.row$species_sci[1]
        newt5b$species_com <- meta.row$species_com[1]
        for(g in c(1:length(unique(newt5b$identity)))){
          print(paste(id,g,length(unique(newt5b$identity)),sep=" / ")) #Comment/Uncomment to use as progress monitor
          
          identity <- unique(newt5b$identity)[g]
          newt3     <- newt5b[newt5b$identity == identity,]
          
          # Calculate median location for each twilight using all 200 available iterations
          sf                  <- data.frame(spDists(as.matrix(newt3[,1:2],ncol=2),longlat=T),ncol=length(newt3$step)) #Note the 1:2 column selection. This will be thrown if R adds an identifier column when re-opening newt5
          sa                  <- data.frame(sum.dist=rowMeans(sf),bot=seq(1,length(newt3$step),1))
          gmp                 <- newt3 [sa$sum.dist==min(sa$sum.dist),] [1,]
          gmp$median.sat.sst  <- median(newt3$sat.sst)
          gmp$median.sun.elev <- median(newt3$sun.elev)
          gmp$median.wrel     <- median(newt3$wrel,na.rm = T)
          gmp$mean.wrel       <- mean  (newt3$wrel,na.rm=T)
          gmp$sd.wrel         <- sd    (newt3$wrel,na.rm=T)
          
          nag <- gmp
          nag$gps.lon.mean  <- NA
          nag$gps.lat.mean  <- NA
          
          nag$mean.median.distance   <- NA
          nag$mean.distance          <- NA
  
          # check if any GPS position for this time and id are available
          gps3b   <- gps2[gps2$dtime >= nag$tFirst-1800 & gps2$dtime <= nag$tSecond+1800 & gps2$i.id==nag$i.id,] #Trim to 30 minutes outside of either twilight
          gps3b   <- gps3b[gps3b$dtime <= nag$tFirst+1800 | gps3b$dtime >= nag$tSecond-1800,] #Trim to 30 minutes before either twilight
          gps3b   <- gps3b[!is.na(gps3b$lon),]
          
          if(nrow(gps3b)>0){
            if(nrow(gps3b)==1){
            nag$gps.lon.midpoint.mean <- mean(gps3b$lon)
            nag$gps.lat.midpoint.mean <- mean(gps3b$lat)
            } else {
              GeoMean <- geomean(cbind(x=gps3b$lon, y=gps3b$lat))
              nag$gps.lon.midpoint.mean  <- GeoMean[,1]
              nag$gps.lat.midpoint.mean  <- GeoMean[,2]          
            }
            
            nag$midpoint.median.distance      <- spDistsN1(matrix(c(nag$gps.lon.midpoint.mean,nag$gps.lat.midpoint.mean),ncol=2),matrix(c(nag$lon,nag$lat),ncol=2),longlat=T)
            nag$midpoint.min.distance          <- min(spDistsN1(matrix(c(newt3$lon,newt3$lat),ncol=2),matrix(c(nag$gps.lon.midpoint.mean,nag$gps.lat.midpoint.mean),ncol=2),longlat=T))[1]
          } else {
            nag$gps.lon.midpoint.mean <- NA
            nag$gps.lat.midpoint.mean <- NA
            nag$midpoint.median.distance <- NA
            nag$midpoint.min.distance    <- NA
          }
          
          if(is.null(nag2)==T) nag2 <- nag else nag2 <- rbind(nag2,nag)
        }

        #Plot the latitude, longitude, and (if applicable) the SST estimates with confidence intervals relative to the GPS data
        png(file=paste0("maps/probGLS_plots/",id,"_SST_",use.sst,"_mask_",landmask,"_particles",particles,".png"), 
            units="in",width=7,height=5,res=700)
        plot_timeline_wGPS(mm.list,degElevation=NULL,s)
        dev.off()
        
        # Add confidence interval data and calculate how many GPS points fall outside these bounds
        
        nag2$lon.UCI <- NA
        nag2$lon.LCI <- NA
        nag2$lat.UCI <- NA
        nag2$lat.LCI <- NA
        nag2$sst.UCI <- NA
        nag2$sst.LCI <- NA
        nag2$Sig.lat <- NA
        nag2$Sig.lon <- NA
        nag2$Sig.sst <- NA
        
        for (r in 1:nrow(nag2)){
          r.lon <- subset(CI90.lon, jday == nag2$jday[r], select = Lon.CI)
          min.lon <- min(r.lon)
          max.lon <- max(r.lon)
          if((max.lon - min.lon) > 270){ #Check this as it may be easily violated by tracks near the poles!
            nag2$lon.UCI[r] <- min.lon
            nag2$lon.LCI[r] <- max.lon
            if(!is.na(nag2$gps.lon.midpoint.mean[r])){
              if((nag2$gps.lon.midpoint.mean[r] < max.lon) || (nag2$gps.lon.midpoint.mean[r] > min.lon)){
                nag2$Sig.lon[r] <- TRUE
              } else {nag2$Sig.lon[r] <- FALSE}
            }
          } else {
            nag2$lon.UCI[r] <- max.lon
            nag2$lon.LCI[r] <- min.lon            
            if(!is.na(nag2$gps.lon.midpoint.mean[r])){
              if((nag2$gps.lon.midpoint.mean[r] > max.lon) || (nag2$gps.lon.midpoint.mean[r] < min.lon)){
                nag2$Sig.lon[r] <- TRUE
              } else {nag2$Sig.lon[r] <- FALSE}
            }
          }
          r.lat <- subset(CI90.lat, jday == nag2$jday[r], select = Lat.CI)
          nag2$lat.UCI[r] <- max(r.lat)
          nag2$lat.LCI[r] <- min(r.lat)
          if(!is.na(nag2$gps.lat.midpoint.mean[r])){
            if((nag2$gps.lat.midpoint.mean[r] > max(r.lat)) || (nag2$gps.lat.midpoint.mean[r] < min(r.lat))){
              nag2$Sig.lat[r] <- TRUE
            } else {nag2$Sig.lat[r] <- FALSE}
          }
          if(SST==T){
            r.sst <- subset(CI90.sst, jday == nag2$jday[r], select = SST.CI)
            nag2$sst.UCI[r] <- max(r.sst)
            nag2$sst.LCI[r] <- min(r.sst)
            if(!is.na(nag2$tag.sst[r])){
              if((nag2$tag.sst[r] > max(r.sst)) || (nag2$tag.sst[r] < min(r.sst))){
                nag2$Sig.sst[r] <- TRUE
              } else { nag2$Sig.sst[r] <- FALSE}
            }
          }
        }
        
        
        if(is.null(nag3)==T) nag3 <- nag2 else nag3 <- rbind(nag3,nag2)

      }
        
        nag3$daylength  <- abs(as.numeric(difftime(nag3$tFirst, nag3$tSecond, units="hours")))
        
        nag3$sst.used <- 0
        nag3$sst.used[!is.na(nag3$sst.diff)] <- 1
        
        nag3$available.bootstraps <- iterations
        
        # Save the resulting table, as it contains the final distance comparisons between GLS and GPS tracks!
        setwd("C:/Users/Jeremy/Dropbox/Research/Geolocators/GPS_Geoloc_comps_Halpin/twilight_tags_data/outputs/probGLS_outputs")
        write.csv(nag3,paste0("nag3_rows",min(rows),"thru",max(rows),".SST_",s,".mask_",m,".csv"))
        
        #NOTE: if row numbers are non-sequential use the following to list ALL rows:
        # write.csv(nag3,paste0("nag3_rows",paste(rows,collapse = "_"),".csv"))
        
        nag3 <<- nag3 #If you want to inspect as a dataframe right afterward
    }
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# probGLS.run function settings----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make.trn2 = T     #TRUE If you need to load and convert the TwGeo-derived twilights into a probGLS format; 
                  #FALSE if these can be reloaded from previous conversion
use.sst = T       #TRUE If you need to create the SST parameters of "act" and "tem";
                  #FALSE if you want to exclude SST from model
load.sst = F      #TRUE if you want to reload previously calculated sensor and wet-dry data
tw            <-  twilight_error_estimation()
lontol = 35       #Degrees of longitude +/- from capture location bird is allowed to fly
lattol = 25       #Degrees of latitude from capture location bird is allowed to fly toward pole (double toward Equator)
landmask = T      #TRUE keeps positions over water; FALSE keeps positions over land; NULL allows any placement of positons
iterations = 200  #Number of bootstrap iterations to conduct
particles = 10000 #Number of particles to simulate movement of at each step. NOTE: Run with ten particles FIRST, so that you can be sure of clean run
Read.mmlist = F   #False if calculating new, True if you just want to replot

#*********************************
#### RUN THE probGLS FUNCTION ####
#*********************************

rows <- c(1:length(unique(GLSGPS_metadata_SST$id))) # you can also run a specific set of rows
probGLS.run(row=rows,particles = c(10000)) 

#*********************************
#### Summarize the probGLS outputs for each of the SST-landmask combinations ####
#*********************************
rows <- c(1:length(unique(newt5$id))) # you can also run a specific set of rows
SST<- c(TRUE,FALSE)
mask<- c(TRUE,FALSE)
calc.positions.new(newt5,SST,mask)