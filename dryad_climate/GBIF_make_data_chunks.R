## Part I: Chunk the GBIF dataset into smaller files
## Author: Dan McGlinn
## Contact: danmcglinn@gmail.com
## Description:
## the purpose of this code is to read in each of the 6 files
## and chunk them down into smaller file sizes we can more easily manipulate

setwd('~/plant_evol/gbif')

inputDir = './gbif_raw_data/'
outputDir = './gbif_data_chunks/'
dir.create(outputDir, showWarnings = FALSE)

fileNames = dir(inputDir)[grep('AZanne_20110905',dir(inputDir))]
nRows = 1e6 
hder = read.csv('headers_commadelim_rerun.txt',header=FALSE,colClasses='character')

start_time = proc.time()[3]

for(i in seq_along(fileNames)){ 
  print(paste('reading ', file.path(inputDir,fileNames[i]), sep=''))
  dat = read.delim(file.path(inputDir,fileNames[i]),na.string='\\N',sep='\t',
                   colClasses='character',as.is=TRUE,quote='',header=FALSE)
  gc()
  ## cut the dataset down to size and export
  names(dat) = hder
  duplicateObs = duplicated(dat$occurrence_id) 
  ## drop duplicated obs and the image_URL column
  dat = dat[!duplicateObs,-5] 
  gc()
  nparts = ceiling(nrow(dat) / nRows)
  for(j in 1:nparts){
    if(j == nparts){
      write.csv(dat[(1+(j-1)*nRows):nrow(dat),],file=paste(outputDir,'gbif',i,
                '.part',j,'.csv',sep=''),row.names=FALSE)
      gc()
    }
    else{
      write.csv(dat[(1+(j-1)*nRows):(j*nRows),],file=paste(outputDir,'gbif',i,
                '.part',j,'.csv',sep=''),row.names=FALSE)
      gc()
    }
  }
  rm(dat)
  gc()
}

end_time = proc.time()[3]
print(paste('Run time = ', end_time - start_time, sep=''))

write.csv(pinus, file='./pinus_list.csv', row.names=F)
write.csv(quercus, file='./quercus_list.csv', row.names=F)

rm(list=ls(all=TRUE))
gc()