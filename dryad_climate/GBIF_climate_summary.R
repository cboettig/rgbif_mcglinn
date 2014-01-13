## Part VI - Compute Averages and Variances
## Author: Dan McGlinn
## Contact: danmcglinn@gmail.com
## Date: 11/2011
## Description: 
## for each genus import all files that apply to that genus from the genus_sort
## directory compute means and variances of the bioclim, alt, and ndiv variables
## on a per species basis for that genus
## compute the primary biome and ecosystem based upon which count is the 
## highest on a per species basis
## Output a serate summary file for each genus to the 'genus_results' directory

library(methods)

library(foreach)
library(doSNOW) ##needed to initialize the cluster with foreach
library(snowfall) ##needed to create cluster

setwd('~/plant_evol/gbif/')

source('./gbif_processing_scripts/GBIF_functions.R')

inputDir = './genus_sort/'
outputDir = './genus_results/'
dir.create(outputDir)

fileNames = dir(inputDir)
genusFiles = sapply(strsplit(fileNames,'[[:digit:]]'),function(x)unlist(x)[[1]])
genusList = sort(unique(genusFiles))

load('wwfeco.Rdata')
load('gbif_geog.Rdata')
biome_lkup = read.csv('./biome/wwf_eco_lookup.csv')
biomeNames = sort(unique(as.character(wwfeco@data$Code)))
contNames = sub(" ",".",sort(unique(as.character(countryDat@data$continent))))
biomeContTemp = expand.grid(biomeNames,contNames)
biomeContNames = paste(biomeContTemp[,1],biomeContTemp[,2],sep='_')
rm(wwfeco,alt,countryDat,biomeContTemp); gc()

sfInit(parallel=TRUE, cpus=6, type="SOCK")
registerDoSNOW(sfGetCluster())

foreach(i = 1:length(genusList), .inorder = FALSE) %dopar% {
  for(j in which(genusFiles %in% genusList[i]) ){
    datTemp = read.table(paste(inputDir,fileNames[j],sep=''),
              sep=',',header=TRUE, colClasses=c(rep('character',5),'numeric',
              rep('character',2),rep('numeric',24),'character'))
    if(!exists('dat'))
      dat = datTemp[,-c(1:2)]
    else
      dat = rbind(dat,datTemp[,-c(1:2)])
  }
  rm(datTemp)
  ## drop duplicates
  dat = dat[!duplicated(dat$occurrence_id),]
  ## convert 'Date_collected' to just year of collection
  dat$Date_collected = as.numeric(sapply(strsplit(dat$Date_collected,
                       '-'),function(x) unlist(x)[[1]]))
  spNames = dat$Scientific
  spList = sort(unique(spNames))
  absLat = abs(dat$Latitude)
  dat = data.frame(dat,absLat)
  ## now compute n, means and variances
  colnames = c("Date_collected", "Latitude", "Longitude", "absLat",
               "mat", "mdr", "iso", "tseas", "tmax", "tmin", "tar",
               "twetq", "tdryq", "twarmq", "tcoldq", "ap", "pwet",
               "pdry", "pseas", "pwetq", "pdryq", "pwarmq", "pcoldq",
               "alt", "ndviAvg")
  datSub = dat[ , colnames]
  ns = tapply(dat$mat,spNames,function(x) sum(!is.na(x)))
  quant = apply(datSub,2, function(x){
          aggregate(x,by = list(spNames),quantile,c(.025,.5,.975),na.rm=TRUE)})
  quantList = sapply(1:ncol(datSub),function(x) matrix(unlist(quant[[x]][2]),
              nrow=length(spList),ncol=3),simplify=FALSE) 
  quantMatrix = matrix(unlist(quantList),nrow=length(spList),ncol=ncol(datSub)*3)
  ## now examine biome & biogeographic realm variables
  nBiome = matrix(0,nrow=length(spList),ncol=length(biomeNames))
  rownames(nBiome) = spList
  colnames(nBiome) = biomeNames
  ## convert eco_code to biomeNames
  dat$Code = biome_lkup$Code[match(dat$eco_code, biome_lkup$eco_code)]
  ## convert continent to code
  
  for(p in seq_along(spList)){
    tabBiome = table(dat$Code[spNames == spList[p]])  
    nBiome[p,match(names(tabBiome),biomeNames)] = tabBiome
  }
  countBiome = apply(nBiome,1,sum)
  primaryBiome = unlist(apply(nBiome,1,function(x)names(which(x == max(x))[1])))
  primaryBiome = ifelse(countBiome == 0,NA,primaryBiome)
  ## biome continent variable
  ## first fix continent formating
  true = !is.na(dat$eco_code)
  nBiomeCont = matrix(0,nrow=length(spList),ncol=length(biomeContNames))
  rownames(nBiomeCont) = spList
  colnames(nBiomeCont) = biomeContNames
  primaryBiomeCont = rep(NA,length(spList))  
  if(sum(true) > 0){
    biomeCont = paste(dat$Code, sub(' ', '.', dat$continent), sep='_')[true]
    for(p in seq_along(spList)){
      tabBiomeCont = table(biomeCont[spNames[true] == spList[p]])
      nBiomeCont[p,match(names(tabBiomeCont),biomeContNames)] = tabBiomeCont
    }
    primaryBiomeCont = unlist(apply(nBiomeCont,1,function(x)names(
                       which(x == max(x))[1])))
  }  
  ## output climate and primary biome status
  climFileHeader = c('species','count', paste(rep(names(datSub),each=3),
                   c('.lo','.me','.hi'),sep=''),'primaryBiome','primaryBiomeCont')
  if(nrow(dat) == 1){
    climOut = c(spList,ns,quantMatrix,primaryBiome,primaryBiomeCont)
    names(climOut) = climFileHeader
    climOut = data.frame(t(climOut))
  }
  else{
    climOut = data.frame(spList,ns,quantMatrix,primaryBiome,primaryBiomeCont)
    names(climOut) = climFileHeader
  }
  write.csv(climOut,file=paste(outputDir,'clim.',genusList[i],'.csv',sep=''),
            row.names=FALSE)
  write.csv(nBiome,file=paste(outputDir,'biome.',genusList[i],'.csv',sep=''))
  write.csv(nBiomeCont,file=paste(outputDir,'biome.cont.',genusList[i],'.csv',
            sep=''))
  rm(dat)
}

sfStop()

rm(list=ls(all=TRUE))
gc()


