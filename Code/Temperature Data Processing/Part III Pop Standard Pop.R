#Part III: Population Weighting 
#ExactExtract

#Load Libraries 
library(climateExtract)
library(raster)
library(exactextractr)
library(sf)
library(sp)
library(fasterize)
library(tidyverse)

#Read in POP TIF 
global_pop_23 <- raster("/Users/amandanorton/Desktop/total pop/fra_ppp_2020_UNadj_constrained.tif")

#Read in Temp Data
#Pull France Border
fr_border = sf::st_as_sf(geodata::gadm("GADM", country = "FRA", level = 0))

#Extract NC Values 
climate_data <- extract_nc_value(first_year = 2011, 
                                 last_year = 2023,
                                 local_file = TRUE,
                                 file_path = '/Users/amandanorton/Desktop/tg_ens_mean_01deg_reg_2011-2023_v280e.nc',
                                 sml_chunk = "2011-2023",
                                 spatial_extent = fr_border,
                                 clim_variable = "mean temp",
                                 statistic = "mean",
                                 grid_size = 0.1,
                                 ecad_v = NULL,
                                 write_raster = TRUE,
                                 out = "test_dat.tiff",
                                 return_data = TRUE)

#Ok this worked Better
#Read in the exported tiff
rbk <- terra::rast("test_dat.tiff")
format(object.size(rbk), "MB")

#Read in NUTS
NUTS_3<- st_read("/Users/amandanorton/Desktop/NUTS3/NUTS_RG_20M_2021_3035/NUTS_RG_20M_2021_3035.shp")

#Project it 
NUTS_3P <- st_transform(NUTS_3, 'EPSG:4326')%>%
  filter(CNTR_CODE == 'FR' & LEVL_CODE == 3)

#Temperature Raster (do first day of year of interest)
r23<-raster(rbk["2023-01-01"])
raster::crs(r23) <- "EPSG:4326"

#Project so they are the same. 
global_pop_23p<-projectRaster(global_pop_23, crs = '+proj=longlat +datum=WGS84 +no_defs') 
global_pop_23pe<-resample(global_pop_23p, r23)

#workaround if this breaks not sure we need it where we're going. 
r23t<-r23*1
global_pop_23pet<-global_pop_23pe*1

NUTS_3P_23<- NUTS_3P

#Now Try Running Exact_Extract
NUTS_3P_23$eu_wgt_tmp <- exact_extract(r23, NUTS_3P, 'weighted_mean', weights = global_pop_23pe)

#Now Rename and add Date Column 
NUTS_3b<-as.data.frame(NUTS_3P_23)
NUTS_3b$date<-'2023-01-01'

#NOW make a list of dates
dates23<-seq(as.Date("2023-01-02"), as.Date("2023-12-31"), by="days")

#Write a Loop! 
for (i in 1:length(dates23)){
  tryCatch({
    #Subset Raster Data 
    date<-as.character(dates23[i])
    
    r<-raster(rbk[date])
    
    #Add CRS
    raster::crs(r) <- "EPSG:4326"
    
    #Ok aggregate
    #Summarize 
    nuts_a<-NUTS_3P
    nuts_a$eu_wgt_tmp<-exact_extract(r, NUTS_3P, 'weighted_mean', weights = global_pop_23pe)
    
    nuts_a$date<-date
    nuts_b<-as.data.frame(nuts_a)
    
    #Bind together by Year 
    NUTS_3b<-rbind(NUTS_3b, nuts_b)
    
    #Remove
    remove(nuts_b)}, error=function(e){print("skip")})
}

summary(NUTS_3b$Temp)

#Try renaming and formatting columns
names(NUTS_3b)<-c('NUTSID','LEVL','CNTR','NAME_LATIN','NAME','MOUNT','URBN','COAST','FID','geo','Temp_wgt60pls','Date')

#try formatting temp as numeric 
NUTS_3b$Temp<-as.numeric(NUTS_3b$Temp)

#Remove Geo Column and format as data frame
NUTS_3b_df<-as.data.frame(NUTS_3b)

#Remove Geo
NUTS_3b_dfa<-subset(NUTS_3b_df, select=c(NUTSID,LEVL,CNTR,NAME_LATIN,NAME,MOUNT,URBN,COAST,FID,Temp,Date))

#Export to csv 
write.csv(NUTS_3b_dfa, file = '/Users/amandanorton/Desktop/temp_wgtavg_2023_a.csv')
remove(NUTS_3b)
