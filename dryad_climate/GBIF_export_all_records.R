## Part VII - Export dataframe with the raw coordinates of every species
## Author: Dan McGlinn
## Contact: danmcglinn@gmail.com
## Description:
## read in the genus level datafiles and rbind them together without summarizing
## then export two files:
## 1) 'gbif_all_remote_data.csv' which has information on all the relevant variables
## 2) 'gbif_cords_alt.csv' which only has the fields: spname, long, lat, and alt

inputDir = '../genus_sort/'
outputDir = '../data_products/'
fileNames = dir(inputDir)
genusFiles = sapply(strsplit(fileNames,'[[:digit:]]'),function(x)unlist(x)[[1]])
genusList = sort(unique(genusFiles))

for(i in 1:length(genusList)){
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
  ## now begin exporting process
  fields = c('Scientific_name_interpreted','Longitude','Latitude','alt')
  if(i == 1){
    write.csv(dat, file=file.path(outputDir, 'gbif_all_remote_data.csv'),
              row.names=F)
    write.csv(dat[ , fields], file=file.path(outputDir, 'gbif_cords_alt.csv'),
              row.names=F)
  }  
  else{
    write.csv(dat, file=file.path(outputDir,'gbif_all_remote_data.csv'),
              row.names=F, append=TRUE,col.names=FALSE)
    write.csv(dat[ , fields], file=file.path(outputDir, 'gbif_cords_alt.csv'),
              row.names=F, append=TRUE, col.names=FALSE)                
  }
  rm(dat)
}

rm(list=ls(all=TRUE))
gc()