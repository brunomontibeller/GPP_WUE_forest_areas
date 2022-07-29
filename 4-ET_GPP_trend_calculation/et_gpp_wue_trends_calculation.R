
#############################################
#### GPP trend calculation

### import the table that contains the GPP extracted using the QGIS software
ggp<-read.csv("D:/paper_3_europe/GPP_stack/GPP_MEAN_STACK_FC_trend_qgis.csv")
head(ggp)
nrow(ggp)
colnames(ggp)

### as the name of the columns are not meaningful, we rename the columns using the name of the layers from the GPP composite
### created previously
require(terra)
r_gpp<-rast("D:/paper_3_europe/GPP_stack/GPP_MEAN_STACK.tif")
x_gpp<-names(r_gpp)
colnames(ggp)[4:192] <- x_gpp
head(ggp)

require(tidyr)

gpp_df <- ggp %>% 
  gather(date, gpp_value_mean_monthly, "MODIS_GPP_MEAN_200003":"MODIS_GPP_MEAN_202011")
head(gpp_df)

gpp_df$month<-as.integer(substr(gpp_df$date,20,21))
gpp_df$year<-as.integer(substr(gpp_df$date,16,19))
head(gpp_df)
gpp_df$gpp_value_mean_monthly<-gpp_df$gpp_value_mean_monthly/8 ##generate a daily mean

### Get the total monthly GPP
gpp_df$gpp_total_monthly<-ifelse(gpp_df$month == 4, gpp_df$gpp_value_mean_monthly*30,
                                 ifelse(gpp_df$month == 6, gpp_df$gpp_value_mean_monthly*30,
                                        ifelse(gpp_df$month == 9, gpp_df$gpp_value_mean_monthly*30,
                                               ifelse(gpp_df$month == 11, gpp_df$gpp_value_mean_monthly*30,
                                                      gpp_df$gpp_value_mean_monthly*31))))

gpp_df$gpp_total_monthly<-gpp_df$gpp_total_monthly*1000 ## convert to grams
head(gpp_df)
gpp_df<-gpp_df[with(gpp_df, order(id, month,year)),]
### check the results
gpp_df[1:25,]

### double checking the values distribution
require(ggplot2)
ggplot(gpp_df, aes(x = factor(year), y = gpp_total_monthly)) + #, colour = month
  geom_boxplot() +
  facet_wrap( ~ month)+ theme_bw()+
  labs(y="GPP (gC/m²/month)", x = "Months")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        legend.position = "none",text = element_text(size = 16))

### now we need to import the .shp points used to extract the GPP values
require(sf)
pp<-read_sf("D:/paper_3_europe/MODIS_pixels_50_intersected_points.shp")
pp

### merge the GPP dataframe with the .shp points
geo_gpp<-merge(gpp_df,pp, by=c("id"))
head(geo_gpp)

##### calculation of monthly trends for each id (for each forest core area) GPP
require(wql)
coef.fcn = function(newdata) {
  res<-mannKen(newdata$gpp_total_monthly)
  return(as.data.frame(res))
}

require(dplyr)
df_gpp_trend = gpp_df %>% 
  group_by(month, id)%>%
  do(coef.fcn(.))
df_gpp_trend

df_gpp_trend$gpp_trend<-ifelse(df_gpp_trend$sen.slope < 0 & 
                                 df_gpp_trend$p.value<0.05, "Decrease", 
                               ifelse(df_gpp_trend$sen.slope >0 & 
                                        df_gpp_trend$p.value<0.05, "Increase", "No trend"))

table(df_gpp_trend$gpp_trend)
require(dplyr)
gpp_trend_count<-df_gpp_trend %>% 
  group_by(month) %>% count(gpp_trend)
head(gpp_trend_count)

head(df_gpp_trend)
write.table(df_gpp_trend,"D:/paper_3_europe/GPP_stack/GPP_MEAN_STACK_FC_trend_qgis.txt")


#############################################
#### ET trend calculation
et<-read.csv("D:/paper_3_europe/ET_stack/ET_MEAN_STACK_FC_qgis.csv")
head(et)
colnames(et)


require(terra)
r_et<-rast("D:/paper_3_europe/ET_stack/ET_MEAN_STACK.tif")
x_et<-names(r_et)
colnames(et)[3:191] <- x_et
head(et)

require(tidyr)
et_df <- et %>% 
  gather(date, et_value_mean_monthly, "MODIS_ET_MEAN_200003": "MODIS_ET_MEAN_202011")
head(et_df)

et_df$month<-as.integer(substr(et_df$date,19,20))
et_df$year<-as.integer(substr(et_df$date,15,18))
head(et_df)
et_df$et_value_mean_monthly<-et_df$et_value_mean_monthly/8 ##generate a daily mean

### Get the total montlhy ET
et_df$et_total_monthly<-ifelse(et_df$month == 4, et_df$et_value_mean_monthly*30,
                               ifelse(et_df$month == 6, et_df$et_value_mean_monthly*30,
                                      ifelse(et_df$month == 9, et_df$et_value_mean_monthly*30,
                                             ifelse(et_df$month == 11, et_df$et_value_mean_monthly*30,
                                                    et_df$et_value_mean_monthly*31))))
head(et_df)
et_df<-et_df[with(et_df, order(id, month,year)),]
et_df[1:25,]

require(ggplot2)
ggplot(et_df, aes(x = factor(year), y = et_total_monthly)) + #, colour = month
  geom_boxplot() +
  facet_wrap( ~ month)+ theme_bw()+
  labs(y="ET (mm/month)", x = "Months")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        legend.position = "none",text = element_text(size = 16))


##### calculation of monthly trends for ET
require(wql)
coef.fcn = function(newdata) {
  res<-mannKen(newdata$et_total_monthly)
  return(as.data.frame(res))
}

require(dplyr)
df_et_trend = et_df %>% 
  group_by(month, id)%>%
  do(coef.fcn(.))
df_et_trend

df_et_trend$et_trend<-ifelse(df_et_trend$sen.slope < 0 & 
                               df_et_trend$p.value<0.05, "Decrease", 
                             ifelse(df_et_trend$sen.slope >0 & 
                                      df_et_trend$p.value<0.05, "Increase", "No trend"))

table(df_et_trend$et_trend)
write.table(df_et_trend,"D:/paper_3_europe/ET_stack/ET_MEAN_STACK_FC_trend_qgis.txt")

###### calculation of WUE trends
df_wue<-merge(et_df,gpp_df, by=c("id","month", "year"))
head(df_wue)

df_wue$wue<-df_wue$gpp_total_monthly/df_wue$et_total_monthly

coef.fcn = function(newdata) {
  res<-mannKen(newdata$wue)
  return(as.data.frame(res))
}

df_wue_trend = df_wue %>% 
  group_by(month, id)%>%
  do(coef.fcn(.))
df_wue_trend

df_wue_trend$wue_trend<-ifelse(df_wue_trend$sen.slope < 0 & 
                                 df_wue_trend$p.value<0.05, "Decrease", 
                               ifelse(df_wue_trend$sen.slope >0 & 
                                        df_wue_trend$p.value<0.05, "Increase", "No trend"))

write.table(df_wue_trend,"D:/paper_3_europe/ET_stack/WUE_STACK_FC_trend_qgis.txt")



