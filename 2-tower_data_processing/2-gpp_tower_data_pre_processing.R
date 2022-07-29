### TOWER GPP estimation
rm(list=ls())

#### make a list of the tables with  GPP values
lst_csv<-list.files("D:/paper_3_europe/fluxnet/dd", pattern = "4.csv", full.names = T)


x<-list.files("D:/paper_3_europe/fluxnet/dd", pattern = "4.csv")

data_list_day = list()
data_list_month = list()

for (i in 1:length(lst_csv)) {
  gpp<-read.csv(lst_csv[i])
  head(gpp)
  names(gpp)
  unique(gpp$NEE_VUT_REF_QC)
  
  ### select only the columns:TIMESTAMP,GPP_DT_VUT_REF, GPP_NT_VUT_REF, 
  ### NEE_VUT_REF_QC
  require(dplyr)
  gpp_sub <- gpp %>% select(TIMESTAMP,GPP_DT_VUT_REF, GPP_NT_VUT_REF, 
                            NEE_VUT_REF_QC) 
  head(gpp_sub)
  str(gpp_sub)
  
  ### Remove the winter months. For that convert the time column into dates and times
  gpp_sub$year<-as.integer(substr(gpp_sub$TIMESTAMP, 1,4))
  head(gpp_sub)
  gpp_sub$month<-substr(gpp_sub$TIMESTAMP, 5,6)
  head(gpp_sub)
  str(gpp_sub)
  gpp_sub<-gpp_sub[!(gpp_sub$year<2000),]
  gpp_sub<-gpp_sub %>% 
    mutate_each(funs(replace(., .<0, NA)))
  
  require(lubridate)
  gpp_sub$date<-format(as.Date(as.character(gpp_sub$TIMESTAMP), format="%Y%m%d"))
  gpp_sub$date
  head(gpp_sub)
  
  
  ### eliminate those observations with any QC values >=2
  gpp_sub_qc<-subset(gpp_sub, subset=(NEE_VUT_REF_QC>0.75 ))
  head(gpp_sub_qc)
  
  ### we can remove the days with less than 60% of the total observations (>20 out of 30) with valid observations
  gpp_sub_qc_p_LE_res<-gpp_sub_qc
  summary(gpp_sub_qc_p_LE_res)
  
  gpp_sub_qc_p_LE_res$year_month<-substr(gpp_sub_qc_p_LE_res$date, 1,7)
  gpp_sub_qc_p_LE_res<-gpp_sub_qc_p_LE_res %>% group_by(year_month) %>% 
    mutate(count_obs_day = n())%>%
    ungroup()
  gpp_sub_qc_p_LE_res<-as.data.frame(gpp_sub_qc_p_LE_res)
  head(gpp_sub_qc_p_LE_res)
  unique(gpp_sub_qc_p_LE_res$count_obs_day)
  
  gpp_sub_qc_p_LE_res<-gpp_sub_qc_p_LE_res[gpp_sub_qc_p_LE_res$count_obs_day >= 21,]
  unique(gpp_sub_qc_p_LE_res$year_month)
  unique(gpp_sub_qc_p_LE_res$count_obs_day)
  head(gpp_sub_qc_p_LE_res)
  
  #### sum GPp per month 
  head(gpp_sub_qc_p_LE_res)
  gpp_sub_qc_p_LE_res_day_sum<-gpp_sub_qc_p_LE_res %>% 
    group_by(year_month) %>% 
    summarise(GPP_DT_VUT_REF_day_mean_per_month = mean(GPP_DT_VUT_REF),
              GPP_NT_VUT_REF_day_mean_per_month = mean(GPP_NT_VUT_REF))###originial fluxnet balance correction)
  gpp_sub_qc_p_LE_res_day_sum<-as.data.frame(gpp_sub_qc_p_LE_res_day_sum)
  head(gpp_sub_qc_p_LE_res_day_sum)
  table(gpp_sub_qc_p_LE_res_day_sum$GPP_DT_VUT_REF_day_mean_per_month)
  
  ## if after all filtering, we have valid observations, add it to the list of data frames
  if (nrow(gpp_sub_qc_p_LE_res_day_sum)>0){
    
    gpp_sub_qc_p_LE_res_day_sum$station<-substr(x[i],5,10)
    
    data_list_day[[i]]<-as.data.frame(gpp_sub_qc_p_LE_res_day_sum)
    
    
  }
}
class(data_list_day)
big_df_day = do.call(rbind, data_list_day)
big_df_day
unique(big_df_day$station)
write.csv(big_df_day,
          "C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/dd/gpp_towers_dd.csv")
