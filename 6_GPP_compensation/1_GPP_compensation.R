##### COMEPNSATION CALCULATION 


df_gpp_trend<-read.table("D:/paper_3_europe/GPP_stack/GPP_MEAN_STACK_FC_trend_qgis.txt")
head(df_gpp_trend)


require(sf)
pp<-read_sf("D:/paper_3_europe/MODIS_pixels_50_intersected_points.shp")
head(pp)
sum(pp$area_ha)

ggp_pp<-merge(df_gpp_trend, pp, by=c("id"))
head(ggp_pp)
ggp_pp$ss_total<-(ggp_pp$sen.slope*ggp_pp$area_ha*10000)/1000000 #convert to tonnes
min(ggp_pp$ss_total)
max(ggp_pp$ss_total)

library(dplyr)
df_gpp_ss_sum<-ggp_pp %>% 
  group_by(id) %>% 
  summarise(ss_sum_ton_gpp = sum(ss_total))

head(df_gpp_ss_sum)

df_gpp_ss_sum$gpp_sign<-ifelse(df_gpp_ss_sum$ss_sum_ton_gpp<0,"Deficit","Surplus")
unique(df_gpp_ss_sum$gpp_sign)
table(df_gpp_ss_sum$gpp_sign)

tst_sum<-df_gpp_ss_sum %>% 
  group_by(gpp_sign) %>% 
  summarise(ton_gpp = sum(ss_sum_ton_gpp))
tst_sum$ton_gpp 

ggp_pp_sum<-merge(df_gpp_ss_sum, pp, by=c("id"))
head(ggp_pp_sum)
ggp_pp_sum<-st_as_sf(ggp_pp_sum)
write_sf(ggp_pp_sum,"D:/paper_3_europe/GPP_stack/gpp_points_surplus_deficit.shp")

area_sum<-ggp_pp_sum %>% 
  group_by(gpp_sgn) %>% 
  summarise(area_sum = sum(area_ha))
area_sum$area_sum

