library(tidyverse)

## read in geolocation results with distance and uncertainty calculations

#####  PROBGLS results #####
## Model Parameter set 1
PG1_SST_FALSE_mask_FALSE <- read_csv("probGLS_parameter_set_1_SST_FALSE_mask_FALSE.csv")
## Model Parameter set 2
PG2_SST_FALSE_mask_TRUE <- read_csv("probGLS_parameter_set_2_SST_FALSE_mask_TRUE.csv")
## Model Parameter set 3
PG3_SST_TRUE_mask_TRUE <- read_csv("probGLS_parameter_set_3_SST_TRUE_mask_TRUE.csv")

##### FLIGHTR results #####
## Model Parameter set 1
flightr_oFALSE_maskFALSE_knownTRUE <- read_csv("flightr_parameter_set_1_oFALSE_maskFALSE_knownTRUE.csv")
## Model Parameter set 2
flightr_oFALSE_maskTRUE_knownTRUE <- read_csv("flightr_parameter_set_2_oFALSE_maskTRUE_knownTRUE.csv")
## Model Parameter set 3
flightr_oTRUE_maskFALSE_knownTRUE <- read_csv("flightr_parameter_set_3_oTRUE_maskFALSE_knownTRUE.csv")
## Model Parameter set 4
flightr_oTRUE_maskTRUE_knownTRUE <- read_csv("flightr_parameter_set_4_oTRUE_maskTRUE_knownTRUE.csv")


#read in metadata
metadata <- read_csv("~/analyses/2018/gps-gls/PAPER/Supplementary Data/Supplementary - S1 - Tracking Metadata.csv", 
                   col_types = cols(dbl_tag_end = col_date(format = "%d/%m/%Y"), 
                                    dbl_tag_start = col_date(format = "%d/%m/%Y"), 
                                    gls_temp_end = col_date(format = "%d/%m/%Y"), 
                                    gls_temp_start = col_date(format = "%d/%m/%Y")))

# Ensure FLIGHTR results have an id identifier the same as in the metadata and probGLS results
flightr_oFALSE_maskFALSE_knownTRUE$id <- flightr_oFALSE_maskFALSE_knownTRUE$tag
flightr_oFALSE_maskTRUE_knownTRUE$id <- flightr_oFALSE_maskTRUE_knownTRUE$tag
flightr_oTRUE_maskTRUE_knownTRUE$id <- flightr_oTRUE_maskTRUE_knownTRUE$tag
flightr_oTRUE_maskFALSE_knownTRUE$id <- flightr_oTRUE_maskFALSE_knownTRUE$tag

# select columns necessary for GAMs analysis from metadata columns
metadata$id <- metadata$indiv
metadata_cols <- metadata %>% dplyr::select(id, bird_id, species_com, gls_type, colony_name, stage, colony_lat, colony_lon)


#join metadata columns for each FlightR outliers/mask combination dataframe
flightr_oFALSE_maskFALSE_knownTRUE <- dplyr::left_join(flightr_oFALSE_maskFALSE_knownTRUE, metadata_cols, by = "id")
flightr_oFALSE_maskTRUE_knownTRUE <- dplyr::left_join(flightr_oFALSE_maskTRUE_knownTRUE, metadata_cols, by = "id")
flightr_oTRUE_maskFALSE_knownTRUE <- dplyr::left_join(flightr_oTRUE_maskFALSE_knownTRUE, metadata_cols, by = "id")
flightr_oTRUE_maskTRUE_knownTRUE <- dplyr::left_join(flightr_oTRUE_maskTRUE_knownTRUE, metadata_cols, by = "id")


# ensure relevant strings are factors for later analyses
flightr_oFALSE_maskFALSE_knownTRUE$species_com <- as.factor(flightr_oFALSE_maskFALSE_knownTRUE$species_com)
flightr_oFALSE_maskFALSE_knownTRUE$gls_type <- as.factor(flightr_oFALSE_maskFALSE_knownTRUE$gls_type)
flightr_oFALSE_maskFALSE_knownTRUE$id <- as.factor(flightr_oFALSE_maskFALSE_knownTRUE$id)
flightr_oFALSE_maskTRUE_knownTRUE$species_com <- as.factor(flightr_oFALSE_maskTRUE_knownTRUE$species_com)
flightr_oFALSE_maskTRUE_knownTRUE$gls_type <- as.factor(flightr_oFALSE_maskTRUE_knownTRUE$gls_type)
flightr_oFALSE_maskTRUE_knownTRUE$id <- as.factor(flightr_oFALSE_maskTRUE_knownTRUE$id)
flightr_oTRUE_maskFALSE_knownTRUE$species_com <- as.factor(flightr_oTRUE_maskFALSE_knownTRUE$species_com)
flightr_oTRUE_maskFALSE_knownTRUE$gls_type <- as.factor(flightr_oTRUE_maskFALSE_knownTRUE$gls_type)
flightr_oTRUE_maskFALSE_knownTRUE$id <- as.factor(flightr_oTRUE_maskFALSE_knownTRUE$id)
flightr_oTRUE_maskTRUE_knownTRUE$species_com <- as.factor(flightr_oTRUE_maskTRUE_knownTRUE$species_com)
flightr_oTRUE_maskTRUE_knownTRUE$gls_type <- as.factor(flightr_oTRUE_maskTRUE_knownTRUE$gls_type)
flightr_oTRUE_maskTRUE_knownTRUE$id <- as.factor(flightr_oTRUE_maskTRUE_knownTRUE$id)


# Join metadata to probGLS models
PG1_SST_FALSE_mask_FALSE <- left_join(PG1_SST_FALSE_mask_FALSE, metadata_cols, by = "id")
PG2_SST_FALSE_mask_TRUE <- left_join(PG2_SST_FALSE_mask_TRUE, metadata_cols, by = "id")
PG3_SST_TRUE_mask_TRUE <- left_join(PG3_SST_TRUE_mask_TRUE, metadata_cols, by = "id")

PG3_SST_TRUE_mask_TRUE$species_com <- as.factor(PG3_SST_TRUE_mask_TRUE$species_com)
PG3_SST_TRUE_mask_TRUE$gls_type <- as.factor(PG3_SST_TRUE_mask_TRUE$gls_type)
PG3_SST_TRUE_mask_TRUE$id <- as.factor(PG3_SST_TRUE_mask_TRUE$id)


#Calculate table with average % of excluded twilights
species_twl <- metadata %>%
  group_by(species_com) %>%
  summarise(pct_delete = mean(pct_twl_excl))

ungroup(metadata)

#######                                                                  #######
###     Determine whether GPS locations fall within uncertainty estimates    ###
#######                                                                  #######

# Using FlightR results with SST and land mask
flightr_oTRUE_maskTRUE_knownTRUE = flightr_oTRUE_maskTRUE_knownTRUE %>% 
  dplyr::mutate(inUClat = if_else(gps.lat.midpoint.mean >= LCI.lat  & gps.lat.midpoint.mean <= UCI.lat, TRUE, FALSE),
                inUClon = if_else(gps.lon.midpoint.mean >= LCI.lon  & gps.lon.midpoint.mean <= UCI.lon,  TRUE, FALSE)
                )
## calculate percentage inside geolocation uncertainty for FLightR
# latitude
round((sum(flightr_oTRUE_maskTRUE_knownTRUE$inUClat)/length(flightr_oTRUE_maskTRUE_knownTRUE$inUClat)*100),1)
# [1] 38.6
# 38.6% of latitudinal GPS locations fall within the FLightR uncertainty estimates
# longitude
round((sum(flightr_oTRUE_maskTRUE_knownTRUE$inUClon)/length(flightr_oTRUE_maskTRUE_knownTRUE$inUClon)*100),1)
# [[1] 23.7
# 23.7% of longitudinal GPS locations fall within the FLightR uncertainty estimates


PG3_SST_TRUE_mask_TRUE = PG3_SST_TRUE_mask_TRUE %>% 
  dplyr::mutate(inUClat = if_else(gps.lat.midpoint.mean >= lat.LCI  & gps.lat.midpoint.mean <= lat.UCI, TRUE, FALSE),
                inUClon = if_else(gps.lon.midpoint.mean >= lon.LCI  & gps.lon.midpoint.mean <= lon.UCI,  TRUE, FALSE),
                inUCsst = if_else(tag.sst >= sst.LCI & tag.sst <= sst.UCI, TRUE, FALSE)
  )

## calculate percentage inside geolocation uncertainty for probGLS
# latitude
round((sum(PG3_SST_TRUE_mask_TRUE$inUClat)/length(PG3_SST_TRUE_mask_TRUE$inUClat)*100),1)
# [1] 84.5
# 84.5% of latitudinal GPS locations fall within the probGLS uncertainty estimates
# longitude
round((sum(PG3_SST_TRUE_mask_TRUE$inUClon)/length(PG3_SST_TRUE_mask_TRUE$inUClon)*100),1)
# [1] 88.8
# 88.8% of longitudinal GPS locations fall within the FLightR uncertainty estimates

  
###########                           ###########
###           DISTANCE CALCULATIONS           ###
###########                           ###########

# Calculate the displacement distance between first and second twilights per individual deployment
# for each of the package results using no landmask and no SST
# FLightR:
library(raster)
flightr_oFALSE_maskFALSE_knownTRUE$Displacement <- NA
for(i in 1:nrow(flightr_oFALSE_maskFALSE_knownTRUE)){
  ActiveID<-flightr_oFALSE_maskFALSE_knownTRUE$id[i]
  ActivetFirst<-flightr_oFALSE_maskFALSE_knownTRUE$tFirst[i]
  ActivetSecond<-flightr_oFALSE_maskFALSE_knownTRUE$tSecond[i]
  
  Subset<-InterpolatedTrackingData[which(InterpolatedTrackingData$id == ActiveID),]
  Point1DiffTime<-abs(difftime(time1 = ActivetFirst, time2 = Subset$dtime, units = "secs"))
  Point1Index<-which(Point1DiffTime == min(Point1DiffTime))
  Point1Index<-Point1Index[1]
  Point1<-Subset[Point1Index, c("lon", "lat")]
  
  Point2DiffTime<-abs(difftime(time1 = ActivetSecond, time2 = Subset$dtime, units = "secs"))
  Point2Index<-which(Point2DiffTime == min(Point2DiffTime))
  Point2Index<-Point2Index[length(Point2Index)]
  Point2<-Subset[Point2Index, c("lon", "lat")]
  
  flightr_oFALSE_maskFALSE_knownTRUE$Displacement[i]<-pointDistance(p1 = Point1, p2 = Point2, lonlat = TRUE)
}

flightr_oFALSE_maskFALSE_knownTRUE$Displacement <- flightr_oFALSE_maskFALSE_knownTRUE$Displacement*0.001

#probGLS:
PG1_SST_FALSE_mask_FALSE$Displacement <- NA
for(i in 1:nrow(PG1_SST_FALSE_mask_FALSE)){
  ActiveID<-PG1_SST_FALSE_mask_FALSE$id[i]
  ActivetFirst<-PG1_SST_FALSE_mask_FALSE$tFirst[i]
  ActivetSecond<-PG1_SST_FALSE_mask_FALSE$tSecond[i]
  
  Subset<-InterpolatedTrackingData[which(InterpolatedTrackingData$id == ActiveID),]
  Point1DiffTime<-abs(difftime(time1 = ActivetFirst, time2 = Subset$dtime, units = "secs"))
  Point1Index<-which(Point1DiffTime == min(Point1DiffTime))
  Point1Index<-Point1Index[1]
  Point1<-Subset[Point1Index, c("lon", "lat")]
  
  Point2DiffTime<-abs(difftime(time1 = ActivetSecond, time2 = Subset$dtime, units = "secs"))
  Point2Index<-which(Point2DiffTime == min(Point2DiffTime))
  Point2Index<-Point2Index[length(Point2Index)]
  Point2<-Subset[Point2Index, c("lon", "lat")]
  
  PG1_SST_FALSE_mask_FALSE$Displacement[i]<-pointDistance(p1 = Point1, p2 = Point2, lonlat = TRUE)
}

PG1_SST_FALSE_mask_FALSE$Displacement <- PG1_SST_FALSE_mask_FALSE$Displacement*0.001



# Remove rows in the distance calculation column with NA. 
# These were times when geolocation and gps tracking did not overlap:
PG1_SST_FALSE_mask_FALSE <- PG1_SST_FALSE_mask_FALSE[!is.na(PG1_SST_FALSE_mask_FALSE$midpoint.median.distance),]
PG2_SST_FALSE_mask_TRUE <- PG2_SST_FALSE_mask_TRUE[!is.na(PG2_SST_FALSE_mask_TRUE$midpoint.median.distance),]
PG3_SST_TRUE_mask_TRUE <- PG3_SST_TRUE_mask_TRUE[!is.na(PG3_SST_TRUE_mask_TRUE$midpoint.median.distance),]
flightr_oFALSE_maskFALSE_knownTRUE <- flightr_oFALSE_maskFALSE_knownTRUE[!is.na(flightr_oFALSE_maskFALSE_knownTRUE$midpoint.median.distance),]
flightr_oFALSE_maskTRUE_knownTRUE <- flightr_oFALSE_maskTRUE_knownTRUE[!is.na(flightr_oFALSE_maskTRUE_knownTRUE$midpoint.median.distance),]
flightr_oTRUE_maskFALSE_knownTRUE <- flightr_oTRUE_maskFALSE_knownTRUE[!is.na(flightr_oTRUE_maskFALSE_knownTRUE$midpoint.median.distance),]
flightr_oTRUE_maskTRUE_knownTRUE <- flightr_oTRUE_maskTRUE_knownTRUE[!is.na(flightr_oTRUE_maskTRUE_knownTRUE$midpoint.median.distance),]

#arrange results rows by the columns id and dtime
PG1_SST_FALSE_mask_FALSE <- dplyr::arrange(PG1_SST_FALSE_mask_FALSE,id,dtime,desc(dtime))
PG2_SST_FALSE_mask_TRUE <- dplyr::arrange(PG2_SST_FALSE_mask_TRUE,id,dtime,desc(dtime))
PG3_SST_TRUE_mask_TRUE <- dplyr::arrange(PG3_SST_TRUE_mask_TRUE,id,dtime,desc(dtime))
flightr_oFALSE_maskFALSE_knownTRUE <- dplyr::arrange(flightr_oFALSE_maskFALSE_knownTRUE,id,dtime,desc(dtime))
flightr_oFALSE_maskTRUE_knownTRUE <- dplyr::arrange(flightr_oFALSE_maskTRUE_knownTRUE,id,dtime,desc(dtime))
flightr_oTRUE_maskFALSE_knownTRUE <- dplyr::arrange(flightr_oTRUE_maskFALSE_knownTRUE,id,dtime,desc(dtime))
flightr_oTRUE_maskTRUE_knownTRUE <- dplyr::arrange(flightr_oTRUE_maskTRUE_knownTRUE,id,dtime,desc(dtime))



###########         ###########
###         METRICS         ###
###########         ###########


  
########### --- calculate daylength --- ###########
# daylength is already produced by the probGLS package
# calculate in FLightR results

flightr_oFALSE_maskFALSE_knownTRUE$daylength <- difftime(flightr_oFALSE_maskFALSE_knownTRUE$tSecond, flightr_oFALSE_maskFALSE_knownTRUE$tFirst, tz = "UTC", units = "hours")
flightr_oFALSE_maskTRUE_knownTRUE$daylength <- difftime(flightr_oFALSE_maskTRUE_knownTRUE$tSecond, flightr_oFALSE_maskTRUE_knownTRUE$tFirst, tz = "UTC", units = "hours")
flightr_oTRUE_maskFALSE_knownTRUE$daylength <- difftime(flightr_oTRUE_maskFALSE_knownTRUE$tSecond, flightr_oTRUE_maskFALSE_knownTRUE$tFirst, tz = "UTC", units = "hours")
flightr_oTRUE_maskTRUE_knownTRUE$daylength <- difftime(flightr_oTRUE_maskTRUE_knownTRUE$tSecond, flightr_oTRUE_maskTRUE_knownTRUE$tFirst, tz = "UTC", units = "hours")


# Calculate time from equinoxes
TrackingData<-data.frame(DateTime = as.POSIXct(x = c("2015-02-09 10:00:00", "2016-02-09 10:00:00", "2015-03-09 10:00:00"), format = "%Y-%m-%d %H:%M:%S", origin = "1970-01-01"))
EquinoxDates<-c("2010-03-20", "2010-09-23", "2011-03-20", "2011-09-23", "2012-03-20", "2012-09-22", "2013-03-20", "2013-09-22", 
               "2014-03-20", "2014-09-23", "2015-03-20", "2015-09-23", "2016-03-20", "2016-09-22", "2017-03-20", "2017-09-22", "2018-03-20", "2018-09-23", "2019-03-20", "2019-09-23")
EquinoxDates<-as.Date(x = EquinoxDates, format = "%Y-%m-%d", origin = "1970-01-01")


EquinoxTime<-vector()
for(i in 1:nrow(PG3_SST_TRUE_mask_TRUE)){
 EquinoxDiff<-min(abs(difftime(time1 = PG3_SST_TRUE_mask_TRUE$dtime[i], time2 = EquinoxDates, units = "days")))
 EquinoxTime<-c(EquinoxTime, EquinoxDiff)
}

flightr_oFALSE_maskFALSE_knownTRUE$days_since_equinox<-EquinoxTime
flightr_oFALSE_maskTRUE_knownTRUE$days_since_equinox<-EquinoxTime
flightr_oTRUE_maskFALSE_knownTRUE$days_since_equinox<-EquinoxTime
flightr_oTRUE_maskTRUE_knownTRUE$days_since_equinox<-EquinoxTime
PG1_SST_FALSE_mask_FALSE$days_since_equinox<-EquinoxTime
PG2_SST_FALSE_mask_TRUE$days_since_equinox<-EquinoxTime
PG3_SST_TRUE_mask_TRUE$days_since_equinox<-EquinoxTime

#calculate package-specific spatial accuracies
library(dplyr)
probGLS1 <- PG1_SST_FALSE_mask_FALSE %>% summarise(
  name = "probGLS1",
  alldata_mean = mean(midpoint.median.distance),
  alldata_sd = sd(midpoint.median.distance),
  mean_not_equinox = mean(midpoint.median.distance[days_since_equinox>=21]),
  sd_not_equinox = sd(midpoint.median.distance[days_since_equinox>=21]),
  mean_equinox = mean(midpoint.median.distance[days_since_equinox<=21]),
  sd_equinox = sd(midpoint.median.distance[days_since_equinox<=21])
  )
probGLS1 <- round(probGLS1[2:7], 0)

probGLS2 <- PG2_SST_FALSE_mask_TRUE %>% summarise(
  name = "probGLS2",
  alldata_mean = mean(midpoint.median.distance, na.rm = T),
  alldata_sd = sd(midpoint.median.distance, na.rm = T),
  mean_not_equinox = mean(midpoint.median.distance[days_since_equinox>=21], na.rm = T),
  sd_not_equinox = sd(midpoint.median.distance[days_since_equinox>=21], na.rm = T),
  mean_equinox = mean(midpoint.median.distance[days_since_equinox<=21], na.rm = T),
  sd_equinox = sd(midpoint.median.distance[days_since_equinox<=21], na.rm = T)
)
probGLS2 <- data.frame(round(probGLS2[2:7], 0))

probGLS3 <- PG3_SST_TRUE_mask_TRUE %>% summarise(
  name = "probGLS3",
  alldata_mean = mean(midpoint.median.distance, na.rm = T),
  alldata_sd = sd(midpoint.median.distance, na.rm = T),
  mean_not_equinox = mean(midpoint.median.distance[days_since_equinox>=21], na.rm = T),
  sd_not_equinox = sd(midpoint.median.distance[days_since_equinox>=21], na.rm = T),
  mean_equinox = mean(midpoint.median.distance[days_since_equinox<=21], na.rm = T),
  sd_equinox = sd(midpoint.median.distance[days_since_equinox<=21], na.rm = T)
)
probGLS3 <- data.frame(round(probGLS3[2:7], 0))

probGLSmodels <- rbind(probGLS1,probGLS2, probGLS3)

probGLSmodels


FLightR1 <- flightr_oFALSE_maskFALSE_knownTRUE %>% summarise(
  name = "FLightR1",
  alldata_mean = mean(midpoint.median.distance, na.rm = T),
  alldata_sd = sd(midpoint.median.distance, na.rm = T),
  mean_not_equinox = mean(midpoint.median.distance[days_since_equinox>=21], na.rm = T),
  sd_not_equinox = sd(midpoint.median.distance[days_since_equinox>=21], na.rm = T),
  mean_equinox = mean(midpoint.median.distance[days_since_equinox<=21], na.rm = T),
  sd_equinox = sd(midpoint.median.distance[days_since_equinox<=21], na.rm = T)
)
FLightR1 <- data.frame(round(FLightR1[2:7], 0))
FLightR1


FLightR2 <- flightr_oFALSE_maskTRUE_knownTRUE %>% summarise(
  name = "FLightR2",
  alldata_mean = mean(midpoint.median.distance, na.rm = T),
  alldata_sd = sd(midpoint.median.distance, na.rm = T),
  mean_not_equinox = mean(midpoint.median.distance[days_since_equinox>=21], na.rm = T),
  sd_not_equinox = sd(midpoint.median.distance[days_since_equinox>=21], na.rm = T),
  mean_equinox = mean(midpoint.median.distance[days_since_equinox<=21], na.rm = T),
  sd_equinox = sd(midpoint.median.distance[days_since_equinox<=21], na.rm = T)
)
FLightR2 <- data.frame(round(FLightR2[2:7], 0))
FLightR2


FLightR3 <- flightr_oTRUE_maskFALSE_knownTRUE %>% summarise(
  name = "FLightR3",
  alldata_mean = mean(midpoint.median.distance, na.rm = T),
  alldata_sd = sd(midpoint.median.distance, na.rm = T),
  mean_not_equinox = mean(midpoint.median.distance[days_since_equinox>=21], na.rm = T),
  sd_not_equinox = sd(midpoint.median.distance[days_since_equinox>=21], na.rm = T),
  mean_equinox = mean(midpoint.median.distance[days_since_equinox<=21], na.rm = T),
  sd_equinox = sd(midpoint.median.distance[days_since_equinox<=21], na.rm = T)
)
FLightR3 <- data.frame(round(FLightR3[2:7], 0))
FLightR3


FLightR4 <- flightr_oTRUE_maskTRUE_knownTRUE %>% summarise(
  name = "FLightR4",
  alldata_mean = mean(midpoint.median.distance, na.rm = T),
  alldata_sd = sd(midpoint.median.distance, na.rm = T),
  mean_not_equinox = mean(midpoint.median.distance[days_since_equinox>=21], na.rm = T),
  sd_not_equinox = sd(midpoint.median.distance[days_since_equinox>=21], na.rm = T),
  mean_equinox = mean(midpoint.median.distance[days_since_equinox<=21], na.rm = T),
  sd_equinox = sd(midpoint.median.distance[days_since_equinox<=21], na.rm = T)
)
FLightR4 <- data.frame(round(FLightR4[2:7], 0))
FLightR4


FLightRmodels <- rbind(FLightR1, FLightR2, FLightR3, FLightR4)
FLightRmodels


####                                    #### 
#### ---- Species-specific metrics ---- ####
####                                    ####

#ProbGLS GEOLOCATION MODEL 1: SST FALSE LANDMASK FALSE
# sum geolocation errors in km by species from models with no land mask or SST interpolation

# all data overall error
mean(PG1_SST_FALSE_mask_FALSE$midpoint.median.distance, na.rm = TRUE)
sd(PG1_SST_FALSE_mask_FALSE$midpoint.median.distance, na.rm = TRUE)

# all data < 21 days from equinox
mean(PG1_SST_FALSE_mask_FALSE$midpoint.median.distance[PG1_SST_FALSE_mask_FALSE$days_since_equinox<=21], na.rm = TRUE)
sd(PG1_SST_FALSE_mask_FALSE$midpoint.median.distance[PG1_SST_FALSE_mask_FALSE$days_since_equinox<=21], na.rm = TRUE)

# all data > 21 days from equinox
mean(PG1_SST_FALSE_mask_FALSE$midpoint.median.distance[PG1_SST_FALSE_mask_FALSE$days_since_equinox>=21], na.rm = TRUE)
sd(PG1_SST_FALSE_mask_FALSE$midpoint.median.distance[PG1_SST_FALSE_mask_FALSE$days_since_equinox>=21], na.rm = TRUE)

# means, SDs and twilight_distance by species
species_probgls_model_1 <- PG1_SST_FALSE_mask_FALSE %>%
  group_by(species_com) %>% 
  summarize(model_no = "PG1",
            Number_of_deployments = length(unique(id)),
            #Average_Displacement = mean(Displacement, na.rm = TRUE),
            #SD_Displacement = sd(Displacement, na.rm = TRUE),
            #Max_Displacement = max(Displacement, na.rm = TRUE),
            mean_error_all = mean(midpoint.median.distance, na.rm = TRUE),
            sd_error_all = sd(midpoint.median.distance, na.rm = TRUE),
            Mean_Error_non_equinox = mean(midpoint.median.distance[PG1_SST_FALSE_mask_FALSE$days_since_equinox>=21], na.rm = TRUE),
            sd_non_equinox = sd(midpoint.median.distance[PG1_SST_FALSE_mask_FALSE$days_since_equinox>=21], na.rm = TRUE),
            Mean_equinox = mean(midpoint.median.distance[PG1_SST_FALSE_mask_FALSE$days_since_equinox<=21], na.rm = TRUE),
            sd_equinox = sd(midpoint.median.distance[PG1_SST_FALSE_mask_FALSE$days_since_equinox<=21], na.rm = TRUE),
            package = "probGLS"
            ) %>%  
            arrange(desc(species_com))

# round digits
species_probgls_model_1 <- species_probgls_model_1 %>% 
  mutate_if(is.numeric, round, digits = 1)

species_probgls_model_1


#ProbGLS GEOLOCATION MODEL 2: SST FALSE LANDMASK TRUE
# sum geolocation errors in km by species from models with no land mask or SST interpolation
# overall error
mean(PG2_SST_FALSE_mask_TRUE$midpoint.median.distance, na.rm = TRUE)
sd(PG2_SST_FALSE_mask_TRUE$midpoint.median.distance, na.rm = TRUE)

# all data < 21 days from equinox
mean(PG2_SST_FALSE_mask_TRUE$midpoint.median.distance[PG2_SST_FALSE_mask_TRUE$days_since_equinox<=21], na.rm = TRUE)
sd(PG2_SST_FALSE_mask_TRUE$midpoint.median.distance[PG2_SST_FALSE_mask_TRUE$days_since_equinox<=21], na.rm = TRUE)

# all data > 21 days from equinox
mean(PG2_SST_FALSE_mask_TRUE$midpoint.median.distance[PG2_SST_FALSE_mask_TRUE$days_since_equinox>=21], na.rm = TRUE)
sd(PG2_SST_FALSE_mask_TRUE$midpoint.median.distance[PG2_SST_FALSE_mask_TRUE$days_since_equinox>=21], na.rm = TRUE)

# means, SDs and twilight_distance by species
species_probgls_model_2 <- PG2_SST_FALSE_mask_TRUE %>%
  group_by(species_com) %>% 
  summarize(model_no = "PG2",
            Number_of_deployments = length(unique(id)),
            #Average_Displacement = mean(Displacement, na.rm = TRUE),
            #SD_Displacement = sd(Displacement, na.rm = TRUE),
            #Max_Displacement = max(Displacement, na.rm = TRUE),
            mean_error_all = mean(midpoint.median.distance, na.rm = TRUE),
            sd_error_all = sd(midpoint.median.distance, na.rm = TRUE),
            Mean_Error_non_equinox = mean(midpoint.median.distance[PG2_SST_FALSE_mask_TRUE$days_since_equinox>=21], na.rm = TRUE),
            sd_non_equinox = sd(midpoint.median.distance[PG2_SST_FALSE_mask_TRUE$days_since_equinox>=21], na.rm = TRUE),
            Mean_equinox = mean(midpoint.median.distance[PG2_SST_FALSE_mask_TRUE$days_since_equinox<=21], na.rm = TRUE),
            sd_equinox = sd(midpoint.median.distance[PG2_SST_FALSE_mask_TRUE$days_since_equinox<=21], na.rm = TRUE),
            package = "probGLS"
  ) %>%  
  arrange(desc(species_com))

# round digits
species_probgls_model_2 <- species_probgls_model_2 %>% 
  mutate_if(is.numeric, round, digits = 1)

species_probgls_model_2


#######GEOLOCATION MODEL 3: SST TRUE LANDMASK TRUE

# sum geolocation errors in km by species from models with no land mask or SST interpolation
# overall error
mean(PG3_SST_TRUE_mask_TRUE$midpoint.median.distance, na.rm = TRUE)
sd(PG3_SST_TRUE_mask_TRUE$midpoint.median.distance, na.rm = TRUE)

# all data < 21 days from equinox
mean(PG3_SST_TRUE_mask_TRUE$midpoint.median.distance[PG3_SST_TRUE_mask_TRUE$days_since_equinox<=21], na.rm = TRUE)
sd(PG3_SST_TRUE_mask_TRUE$midpoint.median.distance[PG3_SST_TRUE_mask_TRUE$days_since_equinox<=21], na.rm = TRUE)

# all data > 21 days from equinox
mean(PG3_SST_TRUE_mask_TRUE$midpoint.median.distance[PG3_SST_TRUE_mask_TRUE$days_since_equinox>=21], na.rm = TRUE)
sd(PG3_SST_TRUE_mask_TRUE$midpoint.median.distance[PG3_SST_TRUE_mask_TRUE$days_since_equinox>=21], na.rm = TRUE)


# means, SDs and twilight_distance by species
species_probgls_model_3 <- PG3_SST_TRUE_mask_TRUE %>%
  group_by(species_com) %>% 
  summarize(model_no = "PG3",
            Number_of_deployments = length(unique(id)),
            #Average_Displacement = mean(Displacement, na.rm = TRUE),
            #SD_Displacement = sd(Displacement, na.rm = TRUE),
            #Max_Displacement = max(Displacement, na.rm = TRUE),
            mean_error_all = mean(midpoint.median.distance, na.rm = TRUE),
            sd_error_all = sd(midpoint.median.distance, na.rm = TRUE),
            Mean_Error_non_equinox = mean(midpoint.median.distance[PG3_SST_TRUE_mask_TRUE$days_since_equinox>=21], na.rm = TRUE),
            sd_non_equinox = sd(midpoint.median.distance[PG3_SST_TRUE_mask_TRUE$days_since_equinox>=21], na.rm = TRUE),
            Mean_equinox = mean(midpoint.median.distance[PG3_SST_TRUE_mask_TRUE$days_since_equinox<=21], na.rm = TRUE),
            sd_equinox = sd(midpoint.median.distance[PG3_SST_TRUE_mask_TRUE$days_since_equinox<=21], na.rm = TRUE),
            package = "probGLS"
  ) %>%  
  arrange(desc(species_com))

# round digits
species_probgls_model_3 <- species_probgls_model_3 %>% 
  mutate_if(is.numeric, round, digits = 1)

species_probgls_model_3

species_measures_probgls_table <- rbind(species_probgls_model_1, species_probgls_model_2, species_probgls_model_3)
species_measures_probgls_table

#   species_com           model_no Number_of_deployments mean_error_all sd_error_all Mean_Error_non_equinox sd_non_equinox Mean_equinox sd_equinox package
#   <fct>                 <chr>                    <dbl>          <dbl>        <dbl>                  <dbl>          <dbl>        <dbl>      <dbl> <chr>  
#    1 White-necked Petrel   PG1                          9           534.        369.                    536.          370.          522.      371.  probGLS
#    2 Scopoli's Shearwater  PG1                         61           233.        168.                    234.          172.          226.      127.  probGLS
#    3 Red-billed Tropicbird PG1                          7           317.        201.                    317.          201.          NaN        NA   probGLS
#    4 Cory's Shearwater     PG1                         97           393.        289.                    390.          295.          403.      269.  probGLS
#    5 Cape Verde Shearwater PG1                         11           411.        271.                    443.          279.          274.      174.  probGLS
#    6 White-necked Petrel   PG2                          9           627.        378.                    648.          377.          514.      367.  probGLS
#    7 Scopoli's Shearwater  PG2                         61           274.        395.                    272.          399.          284.      354.  probGLS
#    8 Red-billed Tropicbird PG2                          7           354.        249.                    354.          249.          NaN        NA   probGLS
#    9 Cory's Shearwater     PG2                         97           488.        356.                    498.          373.          454.      285.  probGLS
#   10 Cape Verde Shearwater PG2                         11           442.        300.                    475.          313.          300.      181.  probGLS
#   11 White-necked Petrel   PG3                          9           236.        182.                    241.          178.          209.      199.  probGLS
#   12 Scopoli's Shearwater  PG3                         61           161.         90.2                   159.           89.3         172.       97.5 probGLS
#   13 Red-billed Tropicbird PG3                          7           489.        229.                    489.          229.          NaN        NA   probGLS
#   14 Cory's Shearwater     PG3                         97           361.        522.                    384.          569.          277.      285   probGLS
#   15 Cape Verde Shearwater PG3                         11           452.        272.                    464.          251.          405.      350.  probGLS

#####  FLIGHTR GEOLOCATION SPECIES-SPECIFIC RESULTS

#FLightR Model 1: frr3_oFALSE_maskFALSE_knownTRUE
# sum geolocation errors in km by species from models with no land mask or SST interpolation

# overall error
mean(flightr_oFALSE_maskFALSE_knownTRUE$midpoint.median.distance, na.rm = TRUE)
sd(flightr_oFALSE_maskFALSE_knownTRUE$midpoint.median.distance, na.rm = TRUE)

# all data < 21 days from equinox
mean(flightr_oFALSE_maskFALSE_knownTRUE$midpoint.median.distance[flightr_oFALSE_maskFALSE_knownTRUE$days_since_equinox<=21], na.rm = TRUE)
sd(flightr_oFALSE_maskFALSE_knownTRUE$midpoint.median.distance[flightr_oFALSE_maskFALSE_knownTRUE$days_since_equinox<=21], na.rm = TRUE)

# all data > 21 days from equinox
mean(flightr_oFALSE_maskFALSE_knownTRUE$midpoint.median.distance[flightr_oFALSE_maskFALSE_knownTRUE$days_since_equinox>=21], na.rm = TRUE)
sd(flightr_oFALSE_maskFALSE_knownTRUE$midpoint.median.distance[flightr_oFALSE_maskFALSE_knownTRUE$days_since_equinox>=21], na.rm = TRUE)

# means, SDs and twilight_distance by species
species_flightr_1<- flightr_oFALSE_maskFALSE_knownTRUE %>%
  group_by(species_com) %>% 
  summarize(model_no = "FR1",
            Number_of_deployments = length(unique(id)),
            #Average_Displacement = mean(Displacement, na.rm = TRUE),
            #SD_Displacement = sd(Displacement, na.rm = TRUE),
            #Max_Displacement = max(Displacement, na.rm = TRUE),
            mean_error_all = mean(midpoint.median.distance, na.rm = TRUE),
            sd_error_all = sd(midpoint.median.distance, na.rm = TRUE),
            Mean_Error_non_equinox = mean(midpoint.median.distance[flightr_oFALSE_maskFALSE_knownTRUE$days_since_equinox>=21], na.rm = TRUE),
            sd_non_equinox = sd(midpoint.median.distance[flightr_oFALSE_maskFALSE_knownTRUE$days_since_equinox>=21], na.rm = TRUE),
            Mean_equinox = mean(midpoint.median.distance[flightr_oFALSE_maskFALSE_knownTRUE$days_since_equinox<=21], na.rm = TRUE),
            sd_equinox = sd(midpoint.median.distance[flightr_oFALSE_maskFALSE_knownTRUE$days_since_equinox<=21], na.rm = TRUE),
            package = "FLightR"
  ) %>%  
  arrange(desc(species_com))

# round digits
species_flightr_1 <- species_flightr_1 %>% 
  mutate_if(is.numeric, round, digits = 1)

species_flightr_1


#FLightR Model 2: 
# sum geolocation errors in km by species from models with no land mask or SST interpolation
# overall error
mean(flightr_oFALSE_maskTRUE_knownTRUE$midpoint.median.distance, na.rm = TRUE)
sd(flightr_oFALSE_maskTRUE_knownTRUE$midpoint.median.distance, na.rm = TRUE)

# all data < 21 days from equinox
mean(flightr_oFALSE_maskTRUE_knownTRUE$midpoint.median.distance[flightr_oFALSE_maskTRUE_knownTRUE$days_since_equinox<=21], na.rm = TRUE)
sd(flightr_oFALSE_maskTRUE_knownTRUE$midpoint.median.distance[flightr_oFALSE_maskTRUE_knownTRUE$days_since_equinox<=21], na.rm = TRUE)

# all data > 21 days from equinox
mean(flightr_oFALSE_maskTRUE_knownTRUE$midpoint.median.distance[flightr_oFALSE_maskTRUE_knownTRUE$days_since_equinox>=21], na.rm = TRUE)
sd(flightr_oFALSE_maskTRUE_knownTRUE$midpoint.median.distance[flightr_oFALSE_maskTRUE_knownTRUE$days_since_equinox>=21], na.rm = TRUE)


# means, SDs and twilight_distance by species
species_flightr_2 <- flightr_oFALSE_maskTRUE_knownTRUE %>%
  group_by(species_com) %>% 
  summarize(model_no = "FR2",
            Number_of_deployments = length(unique(id)),
            #Average_Displacement = mean(Displacement, na.rm = TRUE),
            #SD_Displacement = sd(Displacement, na.rm = TRUE),
            #Max_Displacement = max(Displacement, na.rm = TRUE),
            mean_error_all = mean(midpoint.median.distance, na.rm = TRUE),
            sd_error_all = sd(midpoint.median.distance, na.rm = TRUE),
            Mean_Error_non_equinox = mean(midpoint.median.distance[flightr_oFALSE_maskTRUE_knownTRUE$days_since_equinox>=21], na.rm = TRUE),
            sd_non_equinox = sd(midpoint.median.distance[flightr_oFALSE_maskTRUE_knownTRUE$days_since_equinox>=21], na.rm = TRUE),
            Mean_equinox = mean(midpoint.median.distance[flightr_oFALSE_maskTRUE_knownTRUE$days_since_equinox<=21], na.rm = TRUE),
            sd_equinox = sd(midpoint.median.distance[flightr_oFALSE_maskTRUE_knownTRUE$days_since_equinox<=21], na.rm = TRUE),
            package = "FLightR"
  ) %>%  
  arrange(desc(species_com))

# round digits
species_flightr_2 <- species_flightr_2 %>% 
  mutate_if(is.numeric, round, digits = 1)

species_flightr_2


#FLightR Model 3
# sum geolocation errors in km by species from models with no land mask or SST interpolation
# overall error
mean(flightr_oTRUE_maskFALSE_knownTRUE$midpoint.median.distance, na.rm = TRUE)
sd(flightr_oTRUE_maskFALSE_knownTRUE$midpoint.median.distance, na.rm = TRUE)

# all data < 21 days from equinox
mean(flightr_oTRUE_maskFALSE_knownTRUE$midpoint.median.distance[flightr_oTRUE_maskFALSE_knownTRUE$days_since_equinox<=21], na.rm = TRUE)
sd(flightr_oTRUE_maskFALSE_knownTRUE$midpoint.median.distance[flightr_oTRUE_maskFALSE_knownTRUE$days_since_equinox<=21], na.rm = TRUE)

# all data > 21 days from equinox
mean(flightr_oTRUE_maskFALSE_knownTRUE$midpoint.median.distance[flightr_oTRUE_maskFALSE_knownTRUE$days_since_equinox>=21], na.rm = TRUE)
sd(flightr_oTRUE_maskFALSE_knownTRUE$midpoint.median.distance[flightr_oTRUE_maskFALSE_knownTRUE$days_since_equinox>=21], na.rm = TRUE)

# means, SDs and twilight_distance by species
species_flightr_3 <- flightr_oTRUE_maskFALSE_knownTRUE %>%
  group_by(species_com) %>% 
  summarize(model_no = "FR3",
            Number_of_deployments = length(unique(id)),
            #Average_Displacement = mean(Displacement, na.rm = TRUE),
            #SD_Displacement = sd(Displacement, na.rm = TRUE),
            #Max_Displacement = max(Displacement, na.rm = TRUE),
            mean_error_all = mean(midpoint.median.distance, na.rm = TRUE),
            sd_error_all = sd(midpoint.median.distance, na.rm = TRUE),
            Mean_Error_non_equinox = mean(midpoint.median.distance[flightr_oTRUE_maskFALSE_knownTRUE$days_since_equinox>=21], na.rm = TRUE),
            sd_non_equinox = sd(midpoint.median.distance[flightr_oTRUE_maskFALSE_knownTRUE$days_since_equinox>=21], na.rm = TRUE),
            Mean_equinox = mean(midpoint.median.distance[flightr_oTRUE_maskFALSE_knownTRUE$days_since_equinox<=21], na.rm = TRUE),
            sd_equinox = sd(midpoint.median.distance[flightr_oTRUE_maskFALSE_knownTRUE$days_since_equinox<=21], na.rm = TRUE),
            package = "FLightR"
  ) %>%  
  arrange(desc(species_com))

# round digits
species_flightr_3 <- species_flightr_3 %>% 
  mutate_if(is.numeric, round, digits = 1)

species_flightr_3

#FLightR Model 4: 
# sum geolocation errors in km by species from models with no land mask or SST interpolation
# overall error
mean(flightr_oTRUE_maskTRUE_knownTRUE$midpoint.median.distance, na.rm = TRUE)
sd(flightr_oTRUE_maskTRUE_knownTRUE$midpoint.median.distance, na.rm = TRUE)

# all data < 21 days from equinox
mean(flightr_oTRUE_maskTRUE_knownTRUE$midpoint.median.distance[flightr_oTRUE_maskTRUE_knownTRUE$days_since_equinox<=21], na.rm = TRUE)
sd(flightr_oTRUE_maskTRUE_knownTRUE$midpoint.median.distance[flightr_oTRUE_maskTRUE_knownTRUE$days_since_equinox<=21], na.rm = TRUE)

# all data > 21 days from equinox
mean(flightr_oTRUE_maskTRUE_knownTRUE$midpoint.median.distance[flightr_oTRUE_maskTRUE_knownTRUE$days_since_equinox>=21], na.rm = TRUE)
sd(flightr_oTRUE_maskTRUE_knownTRUE$midpoint.median.distance[flightr_oTRUE_maskTRUE_knownTRUE$days_since_equinox>=21], na.rm = TRUE)

# means, SDs and twilight_distance by species
species_flightr_4 <- flightr_oTRUE_maskTRUE_knownTRUE %>%
  group_by(species_com) %>% 
  summarize(model_no = "FR4",
            Number_of_deployments = length(unique(id)),
            #Average_Displacement = mean(Displacement, na.rm = TRUE),
            #SD_Displacement = sd(Displacement, na.rm = TRUE),
            #Max_Displacement = max(Displacement, na.rm = TRUE),
            mean_error_all = mean(midpoint.median.distance, na.rm = TRUE),
            sd_error_all = sd(midpoint.median.distance, na.rm = TRUE),
            Mean_Error_non_equinox = mean(midpoint.median.distance[flightr_oTRUE_maskTRUE_knownTRUE$days_since_equinox>=21], na.rm = TRUE),
            sd_non_equinox = sd(midpoint.median.distance[flightr_oTRUE_maskTRUE_knownTRUE$days_since_equinox>=21], na.rm = TRUE),
            Mean_equinox = mean(midpoint.median.distance[flightr_oTRUE_maskTRUE_knownTRUE$days_since_equinox<=21], na.rm = TRUE),
            sd_equinox = sd(midpoint.median.distance[flightr_oTRUE_maskTRUE_knownTRUE$days_since_equinox<=21], na.rm = TRUE),
            package = "FLightR"
  ) %>%  
  arrange(desc(species_com))

# round digits
species_flightr_4 <- species_flightr_4 %>% 
  mutate_if(is.numeric, round, digits = 1)

species_flightr_4

species_measures_flightr_table <- rbind(species_flightr_1, species_flightr_2, species_flightr_3, species_flightr_4)

species_measures_flightr_table

#   species_com           model_no Number_of_deployments mean_error_all sd_error_all Mean_Error_non_equinox sd_non_equinox Mean_equinox sd_equinox package
#   <fct>                 <chr>                    <dbl>          <dbl>        <dbl>                  <dbl>          <dbl>        <dbl>      <dbl> <chr>  
#    1 White-necked Petrel   FR1                         21           764.         550.                   748.           551.         919.       520. FLightR
#    2 Scopoli's Shearwater  FR1                         61           173.         169.                   171.           168          195.       176. FLightR
#    3 Red-billed Tropicbird FR1                          7           152          244.                   152            244.         NaN         NA  FLightR
#    4 Cory's Shearwater     FR1                         71           332.         350.                   333            355.         323.       304. FLightR
#    5 Cape Verde Shearwater FR1                         11           342.         285.                   324.           270          413        335. FLightR
#    6 White-necked Petrel   FR2                         21           812.         625.                   789.           618.        1045.       651. FLightR
#    7 Scopoli's Shearwater  FR2                         61           144.         142                    142.           142.         165.       137. FLightR
#    8 Red-billed Tropicbird FR2                          7           153          246.                   153            246.         NaN         NA  FLightR
#    9 Cory's Shearwater     FR2                         71           292.         355.                   295.           354.         266        360. FLightR
#   10 Cape Verde Shearwater FR2                         11           403.         304.                   396.           304.         428.       306. FLightR
#   11 White-necked Petrel   FR3                         21           750.         568                    724.           553.        1016.       645. FLightR
#   12 Scopoli's Shearwater  FR3                         61           144.         134.                   139.           127.         181        183. FLightR
#   13 Red-billed Tropicbird FR3                          7           117.         129.                   117.           129.         NaN         NA  FLightR
#   14 Cory's Shearwater     FR3                         71           330          385.                   333.           394.         298.       280. FLightR
#   15 Cape Verde Shearwater FR3                         11           290          255.                   286.           257.         306        249  FLightR
#   16 White-necked Petrel   FR4                         21           802.         570.                   784.           555.         979.       685. FLightR
#   17 Scopoli's Shearwater  FR4                         61           117.         135.                   110.           124.         187.       201. FLightR
#   18 Red-billed Tropicbird FR4                          7           148.         161.                   148.           161.         NaN         NA  FLightR
#   19 Cory's Shearwater     FR4                         71           274.         290.                   275.           292.         259.       271. FLightR
#   20 Cape Verde Shearwater FR4                         11           307.         272.                   308            283.         304.       223. FLightR

######################                                     ######################
########### ---------- GENERALIZED ADDITIVE MIXED MODEL    ----------############ 
######################                                     ######################


# set up a Generalized Additive Model (GAM) with a smoothing parameter on the true latitude (i.e. from GPS tags)
library(mgcv)
gam_mod_probgls <- mgcv::gam(midpoint.median.distance ~ s(daylength, bs = "tp") +
                             s(Displacement, bs = "tp")+
                             factor(gls_type) +
                             s(species_com, id, bs = "re"),
                             family = Gamma(link = "log"), 
                             data = PG1_SST_FALSE_mask_FALSE, method = "REML", select = TRUE)

#Flightr with displacement
gam_mod_flightr <- mgcv::gam(midpoint.median.distance ~ s(daylength, bs = "tp") +
                               s(Displacement, bs = "tp")+
                               s(species_com, id, bs = "re") +
                               factor(gls_type),
                               family = Gamma(link = "log"), 
                               data = flightr_oFALSE_maskFALSE_knownTRUE, method = "REML", select = TRUE)


## Model summaries 

summary(gam_mod_flightr, all.terms = TRUE)

#    Family: Gamma 
#    Link function: log 
#    
#    Formula:
#    midpoint.median.distance ~ s(daylength, bs = "tp") + 
#                               s(Displacement, bs = "tp") + 
#                               s(species_com, id, bs = "re") + 
#                               factor(gls_type)
#    
#    Parametric coefficients:
#                                      Estimate   Std. Error    t value     Pr(>|t|)    
#    (Intercept)                       5.93065    0.37729        15.719     <2e-16 ***
#      factor(gls_type)BAS MK3005      0.20098    0.50585        0.397      0.6912    
#    factor(gls_type)MT C250          -0.47607    0.38724       -1.229      0.2190    
#    factor(gls_type)MT C330          -0.72147    0.39026       -1.849      0.0646 .  
#    factor(gls_type)MT C65            0.02851    0.42288        0.067      0.9463    
#    ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#    
#    Approximate significance of smooth terms:
#                           edf        Ref.df     F          p-value    
#      s(daylength)         6.085      9          1281.79    3.94e-15 ***
#      s(Displacement)      2.288      9          52.73      4.53e-16 ***
#      s(species_com,id)    155.441    166        26.49      < 2e-16  ***
#      ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#    
#    R-sq.(adj) =  0.595   Deviance explained = 63.2%
#    -REML =  20433  Scale est. = 0.36411   n = 3081



anova(gam_mod_flightr)
#    Family: Gamma 
#    Link function: log 
#    
#    Formula:
#    midpoint.median.distance ~ s(daylength, bs = "tp") + 
#                               s(Displacement, bs = "tp") + 
#                               s(species_com, id, bs = "re") + 
#                               factor(gls_type)
#    
#    Parametric Terms:
#                       df     F        p-value
#    factor(gls_type)   4      4.659    0.000949
#    
#    Approximate significance of smooth terms:
#                         edf       Ref.df      F          p-value
#    s(daylength)         6.085     9.000       1281.79    3.94e-15
#    s(Displacement)      2.288     9.000       52.73      4.53e-16
#    s(species_com,id)    155.441   166.000     26.49      < 2e-16


summary(gam_mod_probgls, all.terms = TRUE)
#    Family: Gamma 
#    Link function: log 
#    
#    Formula:
#      midpoint.median.distance ~ s(daylength, bs = "tp") + 
#                                  s(Displacement, bs = "tp") +
#                                 factor(gls_type) + 
#                                 s(species_com, id, bs = "re")
#    
#    Parametric coefficients:
#                                  Estimate   Std. Error    t value    Pr(>|t|)    
#      (Intercept)                 6.22806    0.07942       78.416     < 2e-16 ***
#      factor(gls_type)BAS MK3005 -0.36425    0.15042       -2.422     0.01551 *  
#      factor(gls_type)MT C250    -0.64452    0.09250       -6.968     3.93e-12 ***
#      factor(gls_type)MT C330    -0.59262    0.09616       -6.163     8.09e-10 ***
#      factor(gls_type)MT C65     -0.67182    0.21402       -3.139     0.00171 ** 
#      ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#    
#    Approximate significance of smooth terms:
#                           edf         Ref.df       F      p-value    
#      s(daylength)         6.6561      9         300.540   1.11e-14 ***
#      s(Displacement)      0.6919      9         0.861     0.0495   *  
#      s(species_com,id)    158.6889    180       11.946    < 2e-16  ***
#      ---
#      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#    
#    R-sq.(adj) =  0.573   Deviance explained = 51.3%
#    -REML =  20874  Scale est. = 0.23684   n = 3194

anova(gam_mod_probgls)
#    Family: Gamma 
#    Link function: log 
#    
#    Formula:
#      midpoint.median.distance ~ s(daylength, bs = "tp") + 
#                                  s(Displacement, bs = "tp") +
#                                 factor(gls_type) + 
#                                 s(species_com, id, bs = "re") 
#    Parametric Terms:
#                       df     F        p-value
#    factor(gls_type)   4      13.25    1.06e-10
#    
#    Approximate significance of smooth terms:
#                         edf         Ref.df       F          p-value
#    s(daylength)         6.6561      9.0000       300.540    1.11e-14
#    s(Displacement)      0.6919      9.0000       0.861      0.0495
#    s(species_com,id)    158.6889    180.0000     11.946     < 2e-16  



# Calculate the relative error for each geolocation in a new column 
PG1_SST_FALSE_mask_FALSE$rel_error <- relative_error(d = PG1_SST_FALSE_mask_FALSE$daylength)
flightr_oFALSE_maskFALSE_knownTRUE$rel_error <- relative_error(d = flightr_oFALSE_maskFALSE_knownTRUE$daylength)



#plot and check the model results
par(mfrow=c(1,1))
plot(gam_mod_probgls, all.terms = TRUE, rug = T, pages = 1,shade = T, shade.col = "lightgrey")
plot(gam_mod_flightr, all.terms = TRUE, rug = T, pages = 1,shade = T, shade.col = "lightgrey")


# Diagnostics
par(mfrow=c(2,2))
gam.check(gam_mod_probgls)
k.check(gam_mod_probgls)
gam.check(gam_mod_flightr)
k.check(gam_mod_flightr)

#Check for colinearity 
mgcv::concurvity(gam_mod_probgls, full = TRUE)
conc <- concurvity(gam_mod_probgls, full = F)$estimate
conc <- data.frame(round(conc, 2))
conc

#######                           #######
####   Relative Accuracy Function    ####
#######                           #######

## Estimate relative spatial accuracy based on the daylength observed by loggers
relative_error <- function(d){
  exp(-0.5 * ((d-12) / 1.2) ^ 2)  
}


#### Plot results
# Plotting GAMM results probGLS
tiff(file="Figure_1.tiff",
     width=9.3, height=3.5, units="in", res=600)
par(oma=c(1,1,1,1)) # all sides have 3 lines of space c(bottom, left, top, right))
par(mar=c(4,4,4,2) + 0)
par(mfcol=c(2,2))
#layout(matrix(1:3, ncol = 3))
plot(gam_mod_probgls, select = 1, main = "Daylength", cex.main =1.5, xlab = "Hours", cex.lab = 1, cex.axis = 1, rug = T, shade = T, shade.col = "lightgrey", scheme=1)+ text(locator(1), "A", cex = 2.5)
plot.gam(gam_mod_probgls, select = 3, main = "Species (Individual Identity)", cex.main =1.5, cex.lab = 1, cex.axis = 1) +text(locator(1), "C", cex = 2.5)
plot(gam_mod_probgls, select = 2, main = "Displacement", cex.main =1.5, xlab = "Kilometers",cex.lab = 1, cex.axis = 1, rug = T, shade = T) +text(locator(1), "B", cex = 2.5)
termplot(gam_mod_probgls, rug = T, se=T, xlabs = "", ylabs = NULL, main = "Geolocator Model", cex.main =1.5, cex.lab = 1, cex.axis = 1,
         col.term = 1, lwd.term = 2, col.se = "grey", lty.se = 2, lwd.se = 1, col.res = "gray", cex = 1, pch = par("pch"))+text(locator(1), "D", cex = 2.5)
dev.off()

getwd()

# Plotting GAMM results FLIGHTR
tiff(file="Figure_2.tiff",
     width=8, height=8, units="in", res=600)
par(oma=c(1,1,1,1)) # all sides have 3 lines of space c(bottom, left, top, right))
par(mar=c(4,4,4,2) + 0)
par(mfcol=c(2,2))
#layout(matrix(1:3, ncol = 3))
plot(gam_mod_flightr, select = 1, main = "Daylength", cex.main =1.5, xlab = "Hours", cex.lab = 1, cex.axis = 1, rug = T, shade = T, shade.col = "lightgrey")+ text(locator(1), "A", cex = 2.5)
plot(gam_mod_flightr, select = 3, main = "Species (Individual Identity)", cex.main =1.5, cex.lab = 1, cex.axis = 1) +text(locator(1), "C", cex = 2.5)
plot(gam_mod_flightr, select = 2, main = "Displacement", cex.main =1.5, xlab = "Kilometers",cex.lab = 1, cex.axis = 1, rug = T, shade = T) +text(locator(1), "B", cex = 2.5)
termplot(gam_mod_flightr, rug = T, se=T, xlabs = "", ylabs = NULL, main = "Geolocator Model", cex.main =1.5, cex.lab = 1, cex.axis = 1,
         col.term = 1, lwd.term = 1, col.se = "grey", lty.se = 2, lwd.se = 1, col.res = "gray", cex = 1, pch = par("pch"))+text(locator(1), "D", cex = 2.5)

dev.off()



# bind results of the two dataframes to make a bar plot 
PG3_SST_TRUE_mask_TRUE$model <- "probGLS"
flightr_oTRUE_maskTRUE_knownTRUE$model <- "FLightR"
pg3 <- dplyr::select(PG3_SST_TRUE_mask_TRUE, species_com, midpoint.median.distance, model)
fr4 <- dplyr::select(flightr_oTRUE_maskTRUE_knownTRUE, species_com, midpoint.median.distance, model)
sp_errors <- rbind(pg3, fr4)


library(ggpubr)
library(ggplot2)
tiff(file="Figure_3.tiff",
     width=7, height=3, units="in", res=600)

p <- ggbarplot(sp_errors, x = "species_com", y = "midpoint.median.distance", color = "black",
          add = "mean_ci", ci = 0.95, label = F, 
          fill = "model",  palette = "Paired", 
          position = position_dodge(), legend = "none",
          ylim = c(0,800), facet.by = "model",
          order = c("Scopoli's Shearwater", 
                    "White-necked Petrel", 
                    "Cory's Shearwater", 
                    "Cape Verde Shearwater", 
                    "Red-billed Tropicbird"),
          xlab = "",
          ylab = "Mean spatial accuracy (km)")+
  theme(strip.text.x = element_text(size = 15, family = "mono"))+
  theme(axis.text=element_text(size=13))+
  theme(axis.title = element_text(size = 13,  vjust = -1))

p + ggpubr::rotate() 

dev.off()

