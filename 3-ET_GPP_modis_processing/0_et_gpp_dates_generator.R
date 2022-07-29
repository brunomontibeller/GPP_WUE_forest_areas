#create a list of outputs names by convert DOY to actuall dates


## et MODIS processing
## list the original images 
lr_gpp<-list.files("D:/paper_3_europe/GPP_modis/orig", pattern = "001.tif", full.names = T)
st_gpp<-list.files("D:/paper_3_europe/GPP_modis/orig", pattern = "001.tif")

date_mods <-as.data.frame(substr(st_gpp,28,34))
dj2date <- function(x) {as.Date(as.numeric(substr(x,5,7)), origin=(paste0(substr(x,1,4), "-01-01"))) -1}
x <- dj2date(date_mods$`substr(st_gpp, 28, 34)`)
x<-gsub('-', '_', x)
x<- paste("MODIS_GPP",x, sep = "_")
x<-paste( x,"_cut.tif", sep = "")
x<-as.data.frame(x)
head(x)
write.table(x, "D:/paper_3_europe/GPP_modis/orig/gpp_dates.txt")

### ET dates processing
rm(list=ls())
lr_gpp<-list.files("D:/Bruno/et_raw", pattern = "001.tif", full.names = T)
st_gpp<-list.files("D:/Bruno/et_raw", pattern = "001.tif")


date_mods <-as.data.frame(substr(st_gpp,26,32))
dj2date <- function(x) {as.Date(as.numeric(substr(x,5,7)), origin=(paste0(substr(x,1,4), "-01-01"))) -1}
x <- dj2date(date_mods$`substr(st_gpp, 26, 32)`)
x<-gsub('-', '_', x)
x<- paste("MODIS_ET",x, sep = "_")
x<-paste( x,"_cut.tif", sep = "")
x<-as.data.frame(x)
head(x)
write.table(x, "D:/Bruno/et_raw/et_dates.txt")
