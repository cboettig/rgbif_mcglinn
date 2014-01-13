## Part III - Setup the geographical data 
## Purpose: is to setup the goegraphical databases and create the file
## "gbif_geog.Rdata" which is an easy to access datafile which contains
## two country lookup tables and a raster file of global altitude
## Author: Dan McGlinn
## Contact: danmcglinn@gmail.com
## Date: 10/2011

library(sp)
library(raster)
library(maptools)
library(maps)
library(rgdal)

setwd('~/plant_evol/gbif')
home_path = path.expand('~')

## the relevant country files are available here:
## http://code.google.com/p/gbif-dataportal/wiki/ConfiguringGeoserver

country = readOGR(file.path(home_path, 'gis/country'),
                  'country')

## the relevant continent file is available here:
## http://pubs.usgs.gov/of/2006/1187/basemaps/continents/
continent = readOGR(file.path(home_path, 'gis/continent'),
                    'continent')

## compute the the continent of each country
cc = overlay(continent, SpatialPoints(coordinates(country))) 
country$CNTRY_NAME[is.na(cc)]
#[1] Papua New Guinea Gambia           Christmas Island Bermuda
## manually fix the few mistakes
cc[is.na(cc)] = c("Asia","Africa","Asia","North America") 

countryCode = read.table("./country_codes.csv", header=TRUE, sep=',',
              colClasses="character")

sum(tolower(country$CNTRY_NAME) %in% tolower(countryCode[,1]))
country$CNTRY_NAME[!tolower(country$CNTRY_NAME) %in% tolower(countryCode[,1])]

countryDat = cbind(country,cc,countryCode[
              match(tolower(country$CNTRY_NAME), tolower(countryCode[,1])),2])
names(countryDat) = c(names(country), 'continent', 'code')
countryDat = SpatialPolygonsDataFrame(Sr = country, data = countryDat,
             match.ID = TRUE)
## now we can go to the GBIF data and based upon the 2 letter country 
## interpreted field look up the proposed continent of the record and check
## if their coordinates generally match with that. We can also check the
## proposed country name if is available

## add a continent designation to the countryCode object as well
countryCode = cbind(countryCode, as.character(
              countryDat$continent[
              match(tolower(countryCode[,1]), tolower(countryDat$CNTRY_NAME))]))
names(countryCode) = c(names(countryCode)[1:2], 'continent')
countryCode[,3] = as.character(countryCode[,3])
countryCode[is.na(countryCode$continent),] 
## fix unknown continents in countryCode
continentFix = read.csv('./continent_manual_lookup.csv',header=TRUE,
               colClasses='character')
countryCode[is.na(countryCode$continent),] = continentFix


nations = map("world", fill=TRUE, col="transparent", plot=FALSE)
nations = map2SpatialPolygons(nations, IDs=nations$names,
          proj4string = CRS("+proj=longlat"))

tst = map("world")$names
tst = unique(sub(":.*$","", tst))
tst[!(tolower(tst)%in%tolower(countryCode[,1]))]

## load altitude data
## this file is available here:
## http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/alt_5m_bil.zip
load('~/gis/WorldClimData/alt_5m.Rdata')

save(countryDat, countryCode, alt, 
     file="./gbif_geog.Rdata")

## load bioclim, alt, and geographic information
## these files are located here: 
## http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio_5m_bil.zip
load('~/gis/WorldClimData/bioclim_5m.Rdata')
class(bioStack)

## combine bioclim and alt data
bioStack = addLayer(bioStack,alt)

## save raster stack
save(bioStack, file='./bioStack.Rdata')

## import wwf ecoregions
wwfeco = readOGR(file.path(home_path, 'gis/wwf/official/'), 'wwf_terr_ecos')
eco_lkup = read.csv('./biome/wwf_eco_lookup.csv')
eco_key = read.csv('./biome/biome_categories_key.csv')
indices = match(wwfeco@data$eco_code, eco_lkup$eco_code)
wwfeco@data$Fine_biome = eco_lkup$Fine_biome[indices]
indices = match(wwfeco@data$Fine_biome, eco_key$Fine)
wwfeco@data$Code = eco_key$Code[indices]
save(wwfeco, file='./wwfeco.Rdata')

rm(list=ls(all=TRUE))
gc()




