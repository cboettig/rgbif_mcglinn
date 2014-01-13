## Part VIII - Repackage results for distribution
## Author: Dan McGlinn
## Contact: danmcglinn@gmail.com
## Date: 11/2011
## Description:
## read in the genus level summary datafiles on the biome and climate variables
## rbind them together and then export two final files
## 1) 'spclimate.csv' : climate summary for every species
## 2) 'spbiomes.csv' : biome summary for every species

inputDir = '../genus_results/'
outputDir = '../data_products/'
fileNames = dir(inputDir)
                 
##pull out file names for each of the three classes of results
climFiles = fileNames[grep('clim.',fileNames,fixed=TRUE)]
biomeFiles = fileNames[grep('biome.',fileNames,fixed=TRUE)]
biomeContFiles = biomeFiles[grep('biome.cont.',biomeFiles,fixed=TRUE)]
biomeFiles = biomeFiles[-grep('biome.cont.',biomeFiles,fixed=TRUE)]


length(climFiles)
length(biomeFiles)
length(biomeContFiles)


for(i in seq_along(climFiles)){ 
  clim = read.table(paste(inputDir,climFiles[i],sep=''),header=TRUE,sep=',')
  biome = read.table(paste(inputDir,biomeFiles[i],sep=''),header=TRUE,sep=',')
  biomeCont = read.table(paste(inputDir,biomeContFiles[i],sep=''),header=TRUE,sep=',')
  row.names(biome) = biome[,1]
  row.names(biomeCont) = biomeCont[,1]
  biome = biome[,-1]
  biomeCont = biomeCont[,-1]
  ##
  if(i == 1 ){
    climOut = clim
    biomeOut = biome
    biomeContOut = biomeCont
  }
  else{
    climOut = rbind(climOut,clim)
    biomeOut = rbind(biomeOut,biome)
    biomeContOut = rbind(biomeContOut,biomeCont)
  }
}

write.csv(climOut,file=file.path(outputDir,'spclimate.csv'),row.names=FALSE)
write.csv(biomeOut,file=file.path(outputDir,'spbiomes.csv'),row.names=TRUE)
write.csv(biomeContOut,file=file.path(outputDir,'spbiomecontinent.csv'),row.names=TRUE)

rm(list=ls(all=TRUE))
gc()