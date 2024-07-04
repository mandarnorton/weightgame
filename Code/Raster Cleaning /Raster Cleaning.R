#Clean up Rasters in R
install.packages('spatialEco')

library(spatialEco)
library(raster)

#Stack rasters 
#Read in all rasters in file 
fempath<-'/Users/amandanorton/Desktop/female 65PLS pop'
allrasters_fem <- stack(list.files(fempath,full.names=T))

malpath<-'/Users/amandanorton/Desktop/male 65PLS pop'
allrasters_mal <- stack(list.files(malpath,full.names=T))

#Now that rasters are in stacks, stack female & male 
all_rasters<-stack(allrasters_fem,allrasters_mal)

#Now run a summary function
raster_ages <- calc(all_rasters,fun=sum,na.rm=T)

#Export 
writeRaster(raster_ages,'aged65pls.tif')

#Take Log of Rasters 

#Read in Rasters
pop_20 <- raster("/Users/amandanorton/Desktop/total pop/fra_ppp_2020_UNadj_constrained.tif")

rast_log<-log(pop_20)
writeRaster(rast_log,'rast_log.tif')

#Population is fine as is. 







