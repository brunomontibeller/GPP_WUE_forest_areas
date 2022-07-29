
#### Pre-processing of MODIS GPP and ET data

#### The MODIS products were downloaded for the entire Europe from the website 
#### "https://lpdaacsvc.cr.usgs.gov/appeears/" 
#### for the period from 01/01/2000 to 
#### 12/31/2020 
#### Before starting analyzing the MODIS data, we have to apply pre-processing steps. For example:
#### mask the data only for area of interest. 


########### Processing GPP .tif files

require(terra)
require(raster)
require(sf)

shp<-vect("D:/Bruno/shp_base/europe_final_rep_rasterized.shp")
shp
shp<-st_make_valid(shp)
shp
st_is_valid(shp)

## matrix with the reclassification scheme
## it represents the range of values defined on the GPP user guide document for
## non-vegetated pixels and therefore there is no GPP calculation
reclass_df_gpp <- c(-32767,0.0, NA, 
                    32760,32768,NA) 
rclmat_gpp <- matrix(reclass_df_gpp, ncol=3, byrow=TRUE)



lr_gpp<-list.files("D:/paper_3_europe/GPP_modis/orig", pattern = "cut.tif", full.names = T)
x<-read.table("D:/paper_3_europe/GPP_modis/orig/gpp_dates.txt")
x<-as.list(x$x)

## mask the GPP product
for (i in 1:length(lr_gpp)){
  r<-rast(lr_gpp[i])
  r<-crop(r, shp) 
  r<-mask(r,shp,updatevalue=NA) ## mask the rasters exactly at the countries border and assign NA values for pixels outside the border
  r <- classify(r, rclmat_gpp)
  r<-r*0.0001 ### scale factor
  writeRaster(r, filename=file.path("D:/Bruno/gpp_cut_europe/",paste0(gsub(".tif","_cut.tif",x[i]))),
              overwrite=T,  wopt= list(gdal=c("COMPRESS=LZW", "of=COG")))
  print(names(r))
  print(x[i])
  print(i)
  rm(r)
}

#remove files from winter months
y<-list.files("D:/Bruno/gpp_cut_europe",pattern = "_01_0|_02_0|_12_0|_01_1|_02_1|_12_1|_01_2|_02_2|_12_2|_01_3|_02_3|_12_3",full.names = T)
y
targetdir<-"D:/Bruno/gpp_cut_europe/winter"
file.copy(from=y, to=targetdir, copy.mode = TRUE)
file.remove(y)

rm(list=ls())
## list the GPP file recent reclassified and masked for Europe 
ll<-list.files("D:/Bruno/gpp_cut_europe/", full.names = T, pattern = "_cut_rep.tif")
ll

## list the GPP file names 
ll_names<-list.files("D:/Bruno/gpp_cut_europe", pattern = "_cut_rep.tif")
ll_names
new_dates <-substr(ll_names,11,20)
new_dates

##generate the indexes that will be used in the calculation of the monthly GPP mean later
indices <- format(as.Date(new_dates, format = "%Y_%m_%d"), format = "%Y%m")
indices
indices<-as.numeric(indices)
indices1<- as.data.frame(indices)
indices1
class(indices1)
indices1_uni<-unique(indices1$indices)
indices1_uni<- paste("MODIS_GPP_MEAN",indices1_uni, sep = "_")
indices1_uni


## calculate the mean GPP per month in all the years 
require(dplyr)
require(terra)
indices$id <-  indices1 %>% group_indices(indices1$indices)
indices$id
ss_total<-rast(ll)
ss_total
st_evap_month<-tapp(ss_total, indices$id, fun = mean)#na.rm=TRUE
st_evap_month
names(st_evap_month)<-indices1_uni
st_evap_month

writeRaster(st_evap_month, filename=file.path("D:/Bruno/gpp_cut_europe/month_gpp_mean/GPP_MEAN_STACK.tif"),
            overwrite=T,  wopt= list(gdal=c("COMPRESS=LZW", "of=COG")))

### convert the layers of the stack in indivudal .tis files
for (i in 1:nlyr(st_evap_month)){ #st_evap_month
  x<-subset(st_evap_month,i)
  writeRaster(x, filename=file.path("D:/Bruno/gpp_cut_europe/month_gpp_mean",
                                    paste0(indices1_uni[[i]],".tif")), #band_GPP_
              format="GTiff", overwrite=T) # D:/paper_3/ET/
  print(i)
}



#############################################
######   Processing ET
rm(list=ls())
require(terra)
require(raster)
require(sf)

## matrix with the reclassification scheme
## it represents the range of values defined on the GPP user guide document for
## non-vegetated pixels and therefore there is no GPP calculation
reclass_df_et <- c(-32767,0.0, NA, 
                    32760,32768,NA) 
rclmat_et <- matrix(reclass_df_gpp, ncol=3, byrow=TRUE)

### read European countries bounderies shapefile
#shp<-vect("D:/paper_3_europe/shp_base/europe_final_rep.shp")
shp<-vect("D:/Bruno/shp_base/europe_final_rep_rasterized.shp")

shp
shp<-st_make_valid(shp)
shp
st_is_valid(shp)

###list files 
lr_et<-list.files("D:/paper_3_europe/et_modis/orig", pattern = "cut.tif", full.names = T)
x<-read.table("D:/paper_3_europe/et_modis/orig/et_dates.txt")
x<-as.list(x$x)


## mask the GPP product
for (i in 1:length(lr_et)){
  r<-rast(lr_et[i])
  r<-crop(r, shp) 
  r<-mask(r,shp,updatevalue=NA) ## mask the rasters exactly at the countries border and assign NA values for pixels outside the border
  r <- classify(r, rclmat_et)
  r<-r*0.1
  writeRaster(r, filename=file.path("D:/Bruno/et_cut_europe/",paste0(gsub("_cut.tif","_cut.tif",x[i]))),
              overwrite=T,  wopt= list(gdal=c("COMPRESS=LZW", "of=COG")))
  print(names(r))
  print(x[i])
  print(i)
  rm(r)
}

#remove files from winter months
y<-list.files("D:/Bruno/et_cut_europe/",pattern="_01_0|_02_0|_12_0|_01_1|_02_1|_12_1|_01_2|_02_2|_12_2|_01_3|_02_3|_12_3",full.names = T)
y
targetdir<-"D:/Bruno/et_cut_europe/winter"
file.copy(from=y, to=targetdir, copy.mode = TRUE)
file.remove(y)

## list the ET file to read in the loopping
rm(list=ls())

ll<-list.files("D:/Bruno/et_cut_europe", full.names = T, pattern = "cut.tif")
ll

## list the ET file names
ll_names<-list.files("D:/Bruno/et_cut_europe", pattern = "cut.tif")
ll_names
new_dates <-substr(ll_names,10,19)
new_dates

##generate the indexes that will be used in the ET mean calculation later
indices <- format(as.Date(new_dates, format = "%Y_%m_%d"), format = "%Y%m")
indices
indices<-as.numeric(indices)
indices1<- as.data.frame(indices)
indices1
class(indices1)
indices1_uni<-unique(indices1$indices)
indices1_uni<- paste("MODIS_ET_MEAN",indices1_uni, sep = "_") ##
indices1_uni

## calculate the ET mean per month in all the years 
require(dplyr)
require(terra)
indices$id <-  indices1 %>% group_indices(indices1$indices)
indices$id
ss_total<-rast(ll)
ss_total
st_evap_month<-tapp(ss_total, indices$id, fun = mean)#na.rm=TRUE
st_evap_month
names(st_evap_month)<-indices1_uni
st_evap_month

writeRaster(st_evap_month, filename=file.path("D:/Bruno/et_cut_europe/month_et_mean/ET_MEAN_STACK.tif"),
            overwrite=T,  wopt= list(gdal=c("COMPRESS=LZW", "of=COG")))

### convert the layers of the stack in indivudal .tis files
for (i in 1:nlyr(st_evap_month)){ #st_evap_month
  x<-subset(st_evap_month,i)
  writeRaster(x, filename=file.path("D:/Bruno/gpp_cut_europe/month_gpp_mean",
                                    paste0(indices1_uni[[i]],".tif")), #band_GPP_
              format="GTiff", overwrite=T) # D:/paper_3/ET/
  print(i)
}


#########################################
## This is step is crucial to assure that the MODIS pixels have
## consistent values trhought the whole period for both variables (GPP and ET)
rm(list=ls())
require(terra)

et_ll<-rast("D:/Bruno/et_cut_europe/month_et_mean/ET_MEAN_STACK.tif")
et_ll
gpp_ll<-rast("D:/Bruno/gpp_cut_europe/month_gpp_mean/GPP_MEAN_STACK.tif")


x<-as.data.frame(rep(1:1,189))
head(x)
nrow(x)

et_sum<-tapp(et_ll, x$`rep(1:1, 189)`, fun = sum)
et_sum
gpp_sum<-tapp(gpp_ll, x$`rep(1:1, 189)`, fun = sum)
gpp_sum

gpp_et_sum<-sum(et_sum, gpp_sum)
gpp_et_sum

writeRaster(gpp_et_sum, filename=file.path("D:/Bruno/et_gpp_sum_base_git.tif"),
            overwrite=T,  wopt= list(gdal=c("COMPRESS=LZW", "of=COG")))

library(stars)
require(sf)

tifpath=system.file("D:/Bruno/et_gpp_sum_base.tif", package = "stars")
tif=read_stars("D:/Bruno/et_gpp_sum_base_git.tif")
sf=st_as_sf(tif)
sf
st_write(sf, "D:/Bruno/MODIS_pixels_cmplt.shp",delete_layer = TRUE)



