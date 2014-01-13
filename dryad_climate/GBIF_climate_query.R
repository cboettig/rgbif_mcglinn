## Part V - Query climate and ecosystem data
## Author: Dan McGlinn
## Contact: danmcglinn@gmail.com
## Date: 11/2011
## Description:
## This script reads in the filtered GBIF data product and queries
## the remotely sensed data layers at each of the GBIF coordinates
## Output is a seperate file for each genus that is written to the 
## 'genus_sort' directory

library(raster)


setwd('~/plant_evol/gbif')

inputDir = './filtered_results/'
outputDir = './genus_sort/'
dir.create(outputDir)

## bioenvio vars
load('bioStack.Rdata')

## ndvi raster data
ndviAvg = stack('~/gis/GlobalVegData/Mean&Std/ndviAvg.grd')
fixNDVI = function(x){
  x2 = ifelse(x ==1,NA,x)
  (x2-128)/128
}
ndviAvg = calc(ndviAvg, fixNDVI)


## load wwf ecoregions
load('wwfeco.Rdata')

fileNames = dir(inputDir)[grep('gbif.fast.filter',dir(inputDir))]

## create a seperate file for each genus
for (i in seq_along(fileNames)) {
  dat = read.csv(file.path(inputDir,fileNames[i]))
  ## pull genus out of name column
  genus = sapply(strsplit(as.character(dat$Scientific),' '),
          function(x) unlist(x)[1])
  genusList = sort(unique(genus))
  ## go through genus list and pull all records for each species together
  ## extract climate data and then export the information
  for(j in seq_along(genusList)){ 
    datTemp = dat[genus == genusList[j],]
    colNames = names(datTemp)
    if(nrow(datTemp) == 1){
      datTemp = data.frame(c(datTemp,
                extract(bioStack, cbind(datTemp$Longitude,datTemp$Latitude))))
      datTemp = data.frame(c(datTemp,
                extract(ndviAvg, cbind(datTemp$Longitude,datTemp$Latitude))))
      names(datTemp) = c(colNames,names(bioStack),'ndviAvg')
    }            
    else{
      datTemp = data.frame(datTemp,
                extract(bioStack, cbind(datTemp$Longitude,datTemp$Latitude)))                
      datTemp = data.frame(datTemp,
                ndviAvg = extract(ndviAvg, cbind(datTemp$Longitude,datTemp$Latitude)))
    }
    datTemp = data.frame(datTemp, eco_code = 
                         overlay(wwfeco, SpatialPoints(cbind(datTemp$Longitude,datTemp$Latitude)))$eco_code)
    ## order by species name prior to outputing
    datTemp = datTemp[order(datTemp$Scientific),]
    write.csv(datTemp,file=paste(outputDir,genusList[j],
              strsplit(fileNames[i],'filter')[[1]][2],sep=''),row.names=FALSE)
 }
 rm(dat,datTemp)
 gc()
 print(paste('file',i,'of',length(fileNames),sep=' '))
}

rm(list=ls(all=TRUE))
gc()

