### TOWER ET estimation
rm(list=ls())

#### make a list of the tables with latent heat (LE) values
#### the ET were estimated using the LE values
lst_csv<-list.files("D:/paper_3_europe/fluxnet/dd", pattern = "4.csv", full.names = T)


x<-list.files("D:/paper_3_europe/fluxnet/dd", pattern = "4.csv")
data_list_day = list()
data_list_month = list()

for (i in 1:length(lst_csv)) {
  BE_Bra<-read.csv(lst_csv[i])
  head(BE_Bra)
  names(BE_Bra)
  unique(BE_Bra$LE_F_MDS_QC)
  
  ### select ont the columns:TIMESTAMP_END; TA_F_MDS; P; LE_F_MDS; LE_CORR;
  ### LE_F_MDS_QC
  require(dplyr)
  BE_Bra_sub <- BE_Bra %>% select(TIMESTAMP,TA_F_MDS,LE_F_MDS,LE_CORR, LE_F_MDS_QC) 
  head(BE_Bra_sub)
  str(BE_Bra_sub)
  
  ### Remove the winter months. For that, first we convert the time column into dates and times
  BE_Bra_sub$year<-as.integer(substr(BE_Bra_sub$TIMESTAMP, 1,4))
  head(BE_Bra_sub)
  BE_Bra_sub$month<-substr(BE_Bra_sub$TIMESTAMP, 5,6)
  head(BE_Bra_sub)
  str(BE_Bra_sub)
  BE_Bra_sub<-BE_Bra_sub[!(BE_Bra_sub$year<2000),]
  
  require(lubridate)
  BE_Bra_sub$date<-format(as.Date(as.character(BE_Bra_sub$TIMESTAMP), format="%Y%m%d"))
  BE_Bra_sub$date
  head(BE_Bra_sub)
  
  
  ### eliminate those observations with any QC values >=0.75
  BE_Bra_sub_qc<-subset(BE_Bra_sub, subset=(LE_F_MDS_QC>0.75 ))
  head(BE_Bra_sub_qc)

  
  ### we can remove the months with less than 60% of the total number of days with valid observations (>20 out 30) 
  BE_Bra_sub_qc_p_LE_res<-BE_Bra_sub_qc
  summary(BE_Bra_sub_qc_p_LE_res)
  
  BE_Bra_sub_qc_p_LE_res$year_month<-substr(BE_Bra_sub_qc_p_LE_res$date, 1,7)
  BE_Bra_sub_qc_p_LE_res<-BE_Bra_sub_qc_p_LE_res %>% group_by(year_month) %>% 
    mutate(count_obs_day = n())%>%
    ungroup()
  BE_Bra_sub_qc_p_LE_res<-as.data.frame(BE_Bra_sub_qc_p_LE_res)
  head(BE_Bra_sub_qc_p_LE_res)
  unique(BE_Bra_sub_qc_p_LE_res$count_obs_day)
  
  BE_Bra_sub_qc_p_LE_res<-BE_Bra_sub_qc_p_LE_res[BE_Bra_sub_qc_p_LE_res$count_obs_day >= 21,]
  unique(BE_Bra_sub_qc_p_LE_res$year_month)
  
  
  ### apply MU approach for LE conversion
  BE_Bra_sub_qc_p_LE_res_day_gap<-BE_Bra_sub_qc_p_LE_res
  head(BE_Bra_sub_qc_p_LE_res_day_gap)
  BE_Bra_sub_qc_p_LE_res_day_gap$lambda<-(2.501-0.002361*BE_Bra_sub_qc_p_LE_res_day_gap$TA_F_MDS)*1000000
  BE_Bra_sub_qc_p_LE_res_day_gap$et_mu_daily<-(BE_Bra_sub_qc_p_LE_res_day_gap$LE_F_MDS*86400)/
    BE_Bra_sub_qc_p_LE_res_day_gap$lambda
  summary(BE_Bra_sub_qc_p_LE_res_day_gap)
  str(BE_Bra_sub_qc_p_LE_res_day_gap)
  sum(is.na(BE_Bra_sub_qc_p_LE_res_day_gap$LE_CORR))
  BE_Bra_sub_qc_p_LE_res_day_gap$et_corre_mu_daily<-((BE_Bra_sub_qc_p_LE_res_day_gap$LE_CORR*86400)/
                                                       BE_Bra_sub_qc_p_LE_res_day_gap$lambda)
  
  head(BE_Bra_sub_qc_p_LE_res_day_gap)
  
  #### sum LE per month
  BE_Bra_sub_qc_p_LE_res_day_sum<-BE_Bra_sub_qc_p_LE_res_day_gap %>% 
    group_by(year_month) %>% 
    summarise(LE_day_mean_per_month = mean(LE_F_MDS),
              LE_CORR_day_mean_per_month = mean(LE_CORR),
              et_day_mean_mu_per_month = mean(et_mu_daily),
              ET_CORR_day_mean_mu_per_month = mean(et_corre_mu_daily))###original fluxnet balance correction)
  BE_Bra_sub_qc_p_LE_res_day_sum<-as.data.frame(BE_Bra_sub_qc_p_LE_res_day_sum)
  head(BE_Bra_sub_qc_p_LE_res_day_sum)

  ### convert from wats to mm using the FAO approach
  # BE_Bra_sub_qc_p_LE_res_day_sum$ET_day_mean_fao_per_month<-BE_Bra_sub_qc_p_LE_res_day_sum$LE_day_mean_per_month*0.035
  # BE_Bra_sub_qc_p_LE_res_day_sum$ET_CORR_mean_fao_per_month<-BE_Bra_sub_qc_p_LE_res_day_sum$LE_CORR_day_mean_per_month*0.035
  # head(BE_Bra_sub_qc_p_LE_res_day_sum)
  # 
  # BE_Bra_sub_qc_p_LE_res_day_sum<-BE_Bra_sub_qc_p_LE_res_day_sum %>% 
  #   mutate_each(funs(replace(., .<0, NA)))
  # BE_Bra_sub_qc_p_LE_res_day_sum<-as.data.frame(BE_Bra_sub_qc_p_LE_res_day_sum)
  # head(BE_Bra_sub_qc_p_LE_res_day_sum)
  
  ## if after all filtering, we have valid observations, add it to the list of data frames
  if (nrow(BE_Bra_sub_qc_p_LE_res_day_sum)>0){

    BE_Bra_sub_qc_p_LE_res_day_sum$station<-substr(x[i],5,10)
    
    data_list_day[[i]]<-as.data.frame(BE_Bra_sub_qc_p_LE_res_day_sum)
  }
}

class(data_list_day)
big_df_day = do.call(rbind, data_list_day)
big_df_day
unique(big_df_day$station)
write.csv(big_df_day,
          "C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/dd/et_towers_dd.csv")
