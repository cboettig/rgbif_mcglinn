## Part II - Generate a GBIF species list
## Author: Dan McGlinn
## Contact: danmcglinn@gmail.com
## Date: 11/2011
## Description
## this will be used to compare with the database species list for the project
## eventually we will only want to pull out climate information for species that 
## we have in the global dataset (see Part III below).  

start_time = proc.time()[3]

setwd('~/plant_evol/gbif')

startDir = getwd()
source('./gbif_processing_scripts/GBIF_functions.R')

inputDir = './gbif_data_chunks/'
outputDir = './batch_results/'
dir.create(outputDir, showWarnings=FALSE) 
fileNames = dir(inputDir)

## Bring in data chunks, count species occurances, and export

for(i in seq_along(fileNames)){
  dat = read.delim(paste(inputDir,fileNames[i],sep=''),sep=',',
        colClasses='character',header=TRUE)
  gc()
  spName = dat$Scientific_name_interpreted
  spTrue = is.spname(spName)
  spList = unique(spName[spTrue])
  spSum = sapply(spList,function(spName) countSpCoords(spName,dat))
  fileStub = strsplit(fileNames[i],'gbif')[[1]][2]
  write.csv(spSum,file=paste(outputDir,'sp.sum.batch',fileStub,sep=''))
  rm(dat,spTrue,spList,spSum)
  gc()
  print(paste('file',i,'of',length(fileNames),'files',sep=' '))
}

## Import species counts

setwd(outputDir)
fileNames = dir()
for(i in 1:length(fileNames)){
  if(i == 1){
    dat = read.table(fileNames[i],sep=',',header=F,skip=1,
          colClasses=c('character','integer'))
  }
  else{
    dat = rbind(dat,read.table(fileNames[i],sep=',',header=F,skip=1,
          colClasses=c('character','integer')))
  }
}

## Create a unique species list with count information

spList = unique(dat[,1])
spCnts = tapply(as.numeric(dat[,2]),as.character(dat[,1]),sum)

## note that
identical(sort(spList),names(spCnts))

tab = table(spCnts)
tab = tab[-1]
brks = seq(0,1000,50)
cnts = sapply(brks,function(x) sum(tab[as.numeric(names(tab))>=x]))

## drop varieties and subsp and the like

spNames = makeBinomial(spList)

## ok now spNames has only genus and species name
spCntsNoTrinomials = tapply(spCnts,spNames,sum)

tabNoTrinomials  = table(spCntsNoTrinomials)
tabNoTrinomials  = tabNoTrinomials [-1]
brks = seq(0,1000,50)
cntsNoTrinomials = sapply(brks,function(x){
                   sum(tabNoTrinomials[as.numeric(names(tabNoTrinomials))>=x])})

setwd(startDir)
pdf('./figs/speciesCumulCoordCutoff.pdf')
  plot(brks,cnts,ylim=c(0,max(cnts)),type='o',col='red',lwd=2,
       ylab='Number of species',xlab='Minimum # of coordinates for a species')
  points(brks,cntsNoTrinomials,type='o',col='blue',lwd=2)
  legend('topright',c('Trinomials','Binomials only'),col=c('red','blue'),
         pch=1,lwd=2,bty='n')
dev.off()

## output the species lists with count information

write.csv(spCnts,file='./data_products/gbifSpCnts.csv')
write.csv(spCntsNoTrinomials,file='./data_products/gbifSpCntsNoTrinomials.csv')

rm(list=ls(all=TRUE))
gc()
## compare GBIF species list to full ANGIOPHYLO database species list

## read in GBIF species list
gbifSplist = read.table('./data_products/gbifSpCntsNoTrinomials.csv',header=TRUE,
                        sep=',', colClasses=c('character','numeric'))
colnames(gbifSplist) = c('spname','ncords')

## read in ANGIPYHLO trait database
traitSplist = as.character(read.csv('../forest/export/speciesTraitData.csv')$gs)

## read in change names file
names.tr = read.table('../forest/db/names-tr.txt', sep='\t', header=TRUE,comment.char='',
           as.is=TRUE, strip.white=TRUE, quote = '')

##change GBIF and ANGIPYHLO names per names.tr 
nameChange = match(gbifSplist$spname, names.tr$originalName)
sum(!is.na(nameChange))
gbifSplist$spname = ifelse(is.na(nameChange), gbifSplist$spname,
                    names.tr[nameChange,2])
temp = tapply(gbifSplist$ncords,gbifSplist$spname,sum)
gbifSplist = data.frame(spname = rownames(temp), ncords = as.numeric(temp))

nameChange = match(traitSplist, names.tr$orginialName)
sum(!is.na(nameChange)) # no names need to be changed


traitINgbif = match(traitSplist,gbifSplist$spname)
sum(!is.na(traitINgbif))

gbifINtrait = match(gbifSplist$spname,traitSplist)
sum(!is.na(gbifINtrait))
sum(gbifSplist$ncords[!is.na(gbifINtrait)] > 500, is.na=TRUE)

spNotMatched = gbifSplist[is.na(gbifINtrait),]
spMatched = gbifSplist[!is.na(gbifINtrait),] 

write.csv(spMatched,file='./data_products/gbifCrossTrait.csv',row.names=FALSE) 

end_time = proc.time()[3]
print(paste('Time elapased = ', end_time - start_time, sep=''))

setwd(startDir)
rm(list=ls(all=TRUE))
gc()
