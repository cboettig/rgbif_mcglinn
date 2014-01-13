
## calculate which species were exposed to freezing temperatures over 
## a portion of their species range

spclim = read.csv('./data_products/spclimate.csv')


Freeze.tmin.lo = ifelse(spclim$tmin.lo <= 0, "FreezingExposed", "FreezingUnexposed")

out = data.frame(species = spclim$species, Freeze.tmin.lo)

write.csv(out, file='./data_products/MinimumFreezingExposure.csv', row.names=F)