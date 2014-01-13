## $Id$
## Part IV - Geographically filter the GBIF data
## Author: Dan McGlinn
## Contact: danmcglinn@gmail.com
## Date: 10/2011
## Description:
## This script takes a quick cut of the best data.
## It simply examines if the given lat/long of the record
## match the country or continent that was recorded for
## the record. If the country matches then the species
## gets a geoCode of 0 and if the country does not match
## but the continent does then the record recieves a
## geoCode of 1

library(foreach)
library(doSNOW) ##needed to initialize the cluster with foreach
library(snowfall) ##needed to create cluster

setwd('~/plant_evol/gbif')

source('./gbif_processing_scripts/GBIF_functions.R')

inputDir = './gbif_data_chunks/'
outputDir = './filtered_results/'
dir.create(outputDir)

load('gbif_geog.Rdata')
fileNames = dir(inputDir)

## read in change names file
names.tr = read.table('../forest/db/names-tr.txt', sep='\t', header=TRUE,comment.char='',
           as.is=TRUE, strip.white=TRUE, quote = '')

sfInit(parallel=TRUE, cpus=8, type="SOCK")
sfLibrary(raster)
registerDoSNOW(sfGetCluster())

foreach(i = 1:length(fileNames), .inorder = FALSE) %dopar% {
  dat = read.delim(paste(inputDir,fileNames[i],sep=''),sep=',',
        colClasses='character',header=TRUE)
  gc()
  ## fix the Namibia - NA country code
  if(sum(dat$Country == 'Namibia' | dat$Country == 'NAMIBIA', na.rm=TRUE)>0){
    ## the actual two letter code is NA but this creates problems in R    
    dat$Country_interpreted[which(dat$Country == 'Namibia')] = 'NAm' 
    dat$Country_interpreted[which(dat$Country == 'NAMIBIA')] = 'NAm'
  }
  ## identify rows that have appropriately structured species names
  goodRow = is.spname(dat$Scientific)
  ## strsplit names down to binomial
  spNames = makeBinomial(dat$Scientific[goodRow])
  ## synonymize names according to 'names-tr.txt'
  nameChange = match(spNames, names.tr$originalName)
  spNames = ifelse(is.na(nameChange), spNames, names.tr$newName[nameChange])
  dat$Scientific_name_interpreted[goodRow] = spNames
  ## Begin checking for coordinate issues
  ## first simply only pull rows that have numeric information for lat/lon 
  goodRow[goodRow] = is.coord(dat$Longitude[goodRow], dat$Latitude[goodRow]) 
  ## drop rows that are duplicates accounding to the 'occurance id field'
  goodRow[goodRow] = !duplicated(dat$occurrence_id[goodRow]) 
  ## check that not near gbif HQ
  goodRow[goodRow] = notGBIFhq(dat$Longitude[goodRow], dat$Latitude[goodRow],
                     cutoff=50)
  ## check that lat not equal lon
  goodRow[goodRow] = dat$Latitude[goodRow] != dat$Longitude[goodRow]
  ## check that Country_interpreted field matches coordinate at country and
  ## continent scales
  goodRow[goodRow] = !is.na(dat$Country_interpreted[goodRow])
  recCountry = dat$Country_interpreted[goodRow]
  recContinent = as.character(countryDat$continent[
                 match(recCountry, countryDat$code)])
  true = !is.na(recCountry) & !is.na(recContinent)
  goodRow[goodRow] = true
  recCountry = recCountry[true]
  recContinent = recContinent[true]
  pts = cbind(as.numeric(dat$Longitude[goodRow]),
              as.numeric(dat$Latitude[goodRow]))
  ## now we ask if the recorded country and continent match what the lat/lon
  ## indicate (i.e., the pts object)
  gc()
  gbifGeog = overlay(countryDat,SpatialPoints(pts)) # gbif geography based upon lat/lon
  gc()
  goodCountry = as.character(gbifGeog$code) == recCountry
  goodContinent = as.character(gbifGeog$continent) == recContinent
  ## define a code to describe degree of confidence in coordinate smaller is better  
  geoCode = rep(NA,sum(goodRow)) 
  ## if country matches this is the highest level of validation
  geoCode = ifelse(goodCountry,0,geoCode)
  ## if continent matches this is next best level
  geoCode = ifelse(!goodCountry&goodContinent,1,geoCode)
  goodRow[goodRow] = !is.na(geoCode)
  pts = pts[!is.na(geoCode),2:1]
  cont = recContinent[!is.na(geoCode)]
  geoCode = geoCode[!is.na(geoCode)]
  ## begin process of outputing data
  gc()
  fileStub = strsplit(fileNames[i],'gbif')[[1]][2]
  dat = dat[goodRow,c(1:5,7,11)]
  dat = cbind(dat,pts,geoCode,cont)
  names(dat) = c(names(dat)[1:7],'Latitude','Longitude','geoCode','continent')
  write.csv(dat[,c(1:5,10,6,11,8:9,7)],
            file=paste(outputDir,'gbif.fast.filter',fileStub,sep=''),
            row.names = FALSE)
  rm(dat,goodRow,spNames,goodSp,recCountry,recContinent,true,pts,gbifGeog,geoCode)
  gc()
  print(paste('file',i,'of',length(fileNames),sep=' '))
}

sfStop()

rm(list=ls(all=TRUE))
gc()