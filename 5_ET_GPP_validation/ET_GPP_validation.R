
#### ET Validation
rm(list=ls())
df_complt<-read.csv("D:/paper_3_europe/fluxnet/et_points_values_towers.csv", sep=",")
df_complt$station<-df_complt$tower_id
head(df_complt)
unique(df_complt$station)
#df_complt<-df_complt[df_complt$year!=2000,]
head(df_complt)
#View(df_complt)

towers<-read.csv("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/dd/et_towers_dd.csv", 
                 sep = ",", stringsAsFactors = F)
head(towers)
unique(towers$station)
towers$station <- gsub('-', '_', towers$station)
unique(towers$station)
unique(towers$year_month)
towers$year<-as.integer(substr(towers$year_month,1,4))
towers$month<-as.integer(substr(towers$year_month,6,7))

nrow(towers)
unique(towers$year_month)
head(towers)
boxplot(towers$et_day_mean_mu_per_month~towers$station)
boxplot(towers$ET_CORR_day_mean_mu_per_month~towers$station)
summary(towers)

df<-merge(towers, df_complt, by=c("month","station", "year"),duplicates=FALSE)
head(df)
unique(df$station)
df <- df[order(df$station),]
head(df)
nrow(df)
require(dplyr)
df <- df %>% 
  mutate(n_days_calendar = if_else(month == 03, 31, if_else(month == 05, 31, 
                                                            if_else(month == 07, 31,
                                                                    if_else(month == 08, 31,
                                                                            if_else(month == 10, 31,30))))))

head(df)

date_count_per_station <- df %>%                              # Applying group_by & summarise
  group_by(station,year) %>%
  summarise(uni_date = unique(year), count=n_distinct(year_month))

### CORRELATION BETWEEN MODIS AND et_day_mean_mu_per_month
require(hydroGOF)
nrow(df)
cor.val <-round(cor(df$et_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis, method = "pearson",use = "complete.obs"),2)
cor.label <- paste0("R: ", cor.val)
rmse_label<-round(rmse(df$et_day_mean_mu_per_month*df$n_days_calendar, df$et_monthly_modis,na.rm = TRUE),2)
rmse_label <- paste0("RMSE: ±", rmse_label,"mm")
#bias_label<-round(bias(df$et_month_total,df$et_monthly_modis),2)
#bias_label <- paste0("Bias: ", bias_label)
mae_label<-round(mae(df$et_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis),2)
mae_label <- paste0("MAE: ±", mae_label, "mm")
r2_label<-round(cor(df$et_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis,use = "complete.obs")^2,2)
r2_label = paste0("R²: ",r2_label)


unique(df$station)
library(ggplot2)
library(hrbrthemes)
ggplot(df, aes(x=et_day_mean_mu_per_month*df$n_days_calendar, y=et_monthly_modis)) +
  geom_point() +#geom_text(aes(label=station))+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid.minor = element_blank(),
        legend.position = "none",text = element_text(size = 18))+
  xlab("Towers ET (mm/month)")+ylab("MODIS - ET (mm/month)")+
  annotate( geom = "text", y = c(200,190,180,170), x = c(25,25,25,25),
            label = c(cor.label,r2_label,rmse_label,mae_label), size = 6)+
  ggtitle("ET ")

### CORRELATION BETWEEN MODIS AND ET_CORR_day_mean_mu_per_month
require(hydroGOF)
head(df)
nrow(df)
cor.val <-round(cor(df$ET_CORR_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis, method = "pearson",use = "complete.obs"),2)
cor.label <- paste0("R: ", cor.val)
rmse_label<-round(rmse(df$ET_CORR_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis,na.rm = TRUE),2)
rmse_label <- paste0("RMSE: ±", rmse_label,"mm")
#bias_label<-round(bias(df$ET_CORR_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis),2)
#bias_label <- paste0("Bias: ", bias_label)
mae_label<-round(mae(df$ET_CORR_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis),2)
mae_label <- paste0("MAE: ±", mae_label, "mm")
r2_label<-round(cor(df$ET_CORR_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis,use = "complete.obs")^2,2)
r2_label = paste0("R²: ",r2_label)

library(ggplot2)
library(hrbrthemes)
ggplot(df, aes(x=df$ET_CORR_day_mean_mu_per_month*df$n_days_calendar, y=et_monthly_modis)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid.minor = element_blank(),
        legend.position = "none",text = element_text(size = 18))+
  xlab("Towers ET (mm/month)")+ylab("MODIS - ET (mm/month)")+
  annotate( geom = "text", y = c(200,190,180,170), x = c(25,25,25,25),
            label = c(cor.label,r2_label,rmse_label,mae_label), size = 6)+
  ggtitle("ET corrected")

### CORRELATION BETWEEN MODIS AND ET_day_mean_fao_per_month
# require(hydroGOF)
# head(df)
# nrow(df)
# cor.val <-round(cor(df$ET_day_mean_fao_per_month*df$n_days_calendar,df$et_monthly_modis, method = "pearson",use = "complete.obs"),2)
# cor.label <- paste0("R: ", cor.val)
# rmse_label<-round(rmse(df$ET_day_mean_fao_per_month*df$n_days_calendar,df$et_monthly_modis,na.rm = TRUE),2)
# rmse_label <- paste0("RMSE: ±", rmse_label,"mm")
# #bias_label<-round(bias(df$et_month_total,df$et_monthly_modis),2)
# #bias_label <- paste0("Bias: ", bias_label)
# mae_label<-round(mae(df$ET_day_mean_fao_per_month*df$n_days_calendar,df$et_monthly_modis),2)
# mae_label <- paste0("MAE: ±", mae_label, "mm")
# r2_label<-round(cor(df$ET_day_mean_fao_per_month*df$n_days_calendar,df$et_monthly_modis,use = "complete.obs")^2,2)
# r2_label = paste0("R²: ",r2_label)
# 
# library(ggplot2)
# library(hrbrthemes)
# ggplot(df, aes(x=ET_day_mean_fao_per_month*df$n_days_calendar, y=et_monthly_modis)) +
#   geom_point() +
#   geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
#   theme(axis.text.x = element_text(angle = 0),
#         panel.grid.minor = element_blank(),
#         legend.position = "none",text = element_text(size = 18))+
#   xlab("Towers ET (mm/month)")+ylab("MODIS - ET (mm/month)")+
#   annotate( geom = "text", y = c(200,190,180,170), x = c(25,25,25,25),
#             label = c(cor.label,r2_label,rmse_label,mae_label), size = 5)+
#   ggtitle("Monthly comparison - ET FAO")
# 
# ### CORRELATION BETWEEN MODIS AND ET_CORR_mean_fao_per_month
# require(hydroGOF)
# head(df)
# nrow(df)
# cor.val <-round(cor(df$ET_CORR_mean_fao_per_month*df$n_days_calendar,df$et_monthly_modis, method = "pearson",use = "complete.obs"),2)
# cor.label <- paste0("R: ", cor.val)
# rmse_label<-round(rmse(df$ET_CORR_mean_fao_per_month*df$n_days_calendar,df$et_monthly_modis,na.rm = TRUE),2)
# rmse_label <- paste0("RMSE: ±", rmse_label,"mm")
# #bias_label<-round(bias(df$et_month_total,df$et_monthly_modis),2)
# #bias_label <- paste0("Bias: ", bias_label)
# mae_label<-round(mae(df$ET_CORR_mean_fao_per_month*df$n_days_calendar,df$et_monthly_modis),2)
# mae_label <- paste0("MAE: ±", mae_label, "mm")
# r2_label<-round(cor(df$ET_CORR_mean_fao_per_month*df$n_days_calendar,df$et_monthly_modis,use = "complete.obs")^2,2)
# r2_label = paste0("R²: ",r2_label)
# 
# library(ggplot2)
# library(hrbrthemes)
# ggplot(df, aes(x=ET_CORR_mean_fao_per_month*df$n_days_calendar, y=et_monthly_modis)) +
#   geom_point() +#geom_text(aes(label=station))+
#   geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
#   theme(axis.text.x = element_text(angle = 0),
#         panel.grid.minor = element_blank(),
#         legend.position = "none",text = element_text(size = 18))+
#   xlab("Towers ET (mm/month)")+ylab("MODIS - ET (mm/month)")+
#   annotate( geom = "text", y = c(200,190,180,170), x = c(25,25,25,25),
#             label = c(cor.label,r2_label,rmse_label,mae_label), size = 5)+
#   ggtitle("Monthly comparison - ET corrected FAO")



#### GPP Validation
rm(list=ls())
df_complt<-read.csv("D:/paper_3_europe/fluxnet/gpp_modis_values_from_towers.csv", sep = ",")
df_complt$gpp_monthly_modis<-df_complt$gpp_monthly_modis*1000
df_complt$daily_gpp<-df_complt$daily_gpp*1000
head(df_complt)
unique(df_complt$station)
str(df_complt)


towers_gpp<-read.csv("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/dd/gpp_towers_dd.csv", sep = ",", stringsAsFactors = F)
head(towers_gpp)
unique(towers_gpp$station)
towers_gpp$station <- gsub('-', '_', towers_gpp$station)
unique(towers_gpp$station)
unique(towers_gpp$year_month)
towers_gpp$year<-as.integer(substr(towers_gpp$year_month,1,4))
towers_gpp$month<-as.integer(substr(towers_gpp$year_month,6,7))
nrow(towers_gpp)
unique(towers_gpp$year_month)
head(towers_gpp)
table(towers_gpp$GPP_DT_VUT_REF_day_mean_per_month)

df<-merge(towers_gpp, df_complt, by=c("month","station", "year"))
head(df)
unique(df$station)
df <- df[order(df$station),]
head(df)
nrow(df)
unique(df$year_month)
require(dplyr)
df <- df %>% 
  mutate(n_days_calendar = if_else(month == 03, 31, if_else(month == 05, 31, 
                                                            if_else(month == 07, 31,
                                                                    if_else(month == 08, 31,
                                                                            if_else(month == 10, 31,30))))))

head(df)

#####  GPP COMPARISON FOR GPP_DT_VUT_REF_day_mean_per_month
require(hydroGOF)
nrow(df)
head(df)
cor.val <-round(cor(df$GPP_DT_VUT_REF_day_mean_per_month*df$n_days_calendar,df$gpp_monthly_modis, method = "pearson",use = "complete.obs"),2)
cor.label <- paste0("R: ", cor.val)
rmse_label<-round(rmse(df$GPP_DT_VUT_REF_day_mean_per_month*df$n_days_calendar,df$gpp_monthly_modis,na.rm = TRUE),2)
rmse_label <- paste0("RMSE: ±", rmse_label," GPP (gC/m²/month)")
mae_label<-round(mae(df$GPP_DT_VUT_REF_day_mean_per_month*df$n_days_calendar,df$gpp_monthly_modis),2)
mae_label <- paste0("MAE: ±", mae_label, " GPP (gC/m²/month)")
r2_label<-round(cor(df$GPP_DT_VUT_REF_day_mean_per_month*df$n_days_calendar,df$gpp_monthly_modis,use = "complete.obs")^2,2)
r2_label = paste0("R²: ",r2_label)
table(df$GPP_DT_VUT_REF_day_mean_per_month)

#install.packages("hrbrthemes")
# library(ggplot2)
# library(hrbrthemes)
# ggplot(df, aes(x=GPP_DT_VUT_REF_day_mean_per_month*df$n_days_calendar , y=gpp_monthly_modis)) +
#   geom_point() +
#   geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
#   theme(axis.text.x = element_text(angle = 0),
#         panel.grid.minor = element_blank(),
#         legend.position = "none",text = element_text(size = 18))+
#   xlab("Tower - GPP (gC/m²/month)")+ylab("MODIS - GPP (gC/m²/month)")+
#   annotate( geom = "text", y = c(355,340,325,310), x = c(90,90,90,90),
#             label = c(cor.label,r2_label,rmse_label,mae_label), size = 6)
# 
# 
# #####  GPP COMPARISON FOR GPP_NT_VUT_REF_day_mean_per_month
# require(hydroGOF)
# nrow(df)
# head(df)
# cor.val <-round(cor(df$GPP_NT_VUT_REF_day_mean_per_month*df$n_days_calendar,df$gpp_monthly_modis, method = "pearson",use = "complete.obs"),2)
# cor.label <- paste0("R: ", cor.val)
# rmse_label<-round(rmse(df$GPP_NT_VUT_REF_day_mean_per_month*df$n_days_calendar,df$gpp_monthly_modis,na.rm = TRUE),2)
# rmse_label <- paste0("RMSE: ±", rmse_label," GPP (gC/m²/month)")
# mae_label<-round(mae(df$GPP_NT_VUT_REF_day_mean_per_month*df$n_days_calendar,df$gpp_monthly_modis),2)
# mae_label <- paste0("MAE: ±", mae_label, " GPP (gC/m²/month)")
# r2_label<-round(cor(df$GPP_NT_VUT_REF_day_mean_per_month*df$n_days_calendar,df$gpp_monthly_modis,use = "complete.obs")^2,2)
# r2_label = paste0("R²: ",r2_label)
# table(df$GPP_NT_VUT_REF_day_mean_per_month)
# 
# #install.packages("hrbrthemes")
# library(ggplot2)
# library(hrbrthemes)
# ggplot(df, aes(x=GPP_NT_VUT_REF_day_mean_per_month*df$n_days_calendar , y=gpp_monthly_modis)) +
#   geom_point() +
#   geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
#   theme(axis.text.x = element_text(angle = 0),
#         panel.grid.minor = element_blank(),
#         legend.position = "none",text = element_text(size = 18))+
#   xlab("Tower - GPP (gC/m²/month)")+ylab("MODIS - GPP (gC/m²/month)")+
#   annotate( geom = "text", y = c(355,340,325,310), x = c(120,120,120,120),
#             label = c(cor.label,r2_label,rmse_label,mae_label), size = 6)



