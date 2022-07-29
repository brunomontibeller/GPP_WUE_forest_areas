###################################
##### PLOT FIGURE 1
##########
##### Plot figure 1A 

# Figure 1A was made in ArcGis

##### Plot figure 1B
require(ggplot2)
library(grid)
#https://coderedirect.com/questions/174677/two-horizontal-bar-charts-with-shared-axis-in-ggplot2-similar-to-population-pyr
df<-read.csv("D:/paper_3_europe/europe_fcover_stats.csv")
head(df)
df<-df[order(-df$fcore_area_ha),]


ggplot(data = df, aes(x = reorder(NAME_0, -fcore_area_ha), y = fcore_area_ha*0.01)) +
  ylab("Undisturbed forest core area (km²)")+
  geom_bar(position=position_dodge(),stat = "identity",fill = "darkgreen",color="darkgreen" ,alpha=0.5) + #ggtitle("") +
  coord_flip()+#ylim(0,1200)+ expand = FALSE
  scale_y_reverse(limits=c(1200,0))+
  #coord_cartesian()+
  scale_x_discrete(name = "", position = "top")+
  # scale_y_continuous(breaks = seq(0, 1200, by = 200),  # y axis values (before coord_flip) 
  #                   labels = seq(0,  1200, by =  200))+
  theme(#axis.title.x = element_blank(),
    #panel.background = element_blank(),
    axis.title.y = element_blank(), #panel.grid.major.x = element_line(colour = "grey"),
    axis.line.y.right = element_line(colour = "black"),
    #axis.ticks.y = element_blank(),
    axis.line.x.bottom = element_line(colour = "black"),
    axis.text=element_text(size=14),
    axis.title.x.bottom = element_text(size=14),
    plot.margin = unit(c(0.1,0.1,0.2,0.5), "cm")) +
  geom_text(aes(label=round(fcore_area_ha*0.01,1)), hjust=1.01,size=3.8)
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/barplot_fcore_1b.png", dpi=350)


###################################
##### PLOT FIGURE 2
##########
##### Plot figure 2A
require(dplyr)
library(purrr)
require(sf)

df_gpp_trend<-read.table("D:/paper_3_europe/GPP_stack/GPP_MEAN_STACK_FC_trend_qgis.txt")
head(df_gpp_trend)

pp<-read_sf("D:/paper_3_europe/MODIS_pixels_50_intersected_points.shp")
pp<-st_transform(pp, 4326)
head(pp)

shp_euro<-read_sf("D:/paper_3_europe/shp_base/europe_final_rep_rasterized.shp")

world<-read_sf("D:/shp_base/World_Countries.shp")

pp_gpp<-merge(df_gpp_trend, pp, by="id")
head(pp_gpp)
table(pp_gpp$id)

pp_gpp_1 <- pp_gpp %>%
  mutate(long =unlist(map(pp_gpp$geometry,1)),
         lat = unlist(map(pp_gpp$geometry,2)))

str(pp_gpp_1)
pp_gpp_1<-pp_gpp_1[order(pp_gpp_1$month),]
pp_gpp_1$month_str<-month.abb[pp_gpp_1$month]
pp_gpp_1$month_str<-factor(pp_gpp_1$month_str,
                           levels =unique(pp_gpp_1$month_str), ordered = T)
pp_gpp_2<-pp_gpp_1[pp_gpp_1$gpp_trend!="No trend",]
unique(pp_gpp_2$gpp_trend)

require(ggplot2)
ggplot() +facet_wrap( ~ month_str)+ #panel.grid.minor = element_blank(),panel.grid.major = element_blank()
  theme_bw()+ 
  geom_sf(data = world, colour = "grey", fill = "lightgrey")+
  geom_sf(data = shp_euro, colour = "darkgrey", fill = "grey")+
  geom_point(data=pp_gpp_2, aes(x=long, y=lat,col=gpp_trend),
             size=0.75)+#color="darkgreen" , 
  scale_color_manual(values=c("darkorange2","darkolivegreen"))+
  coord_sf(xlim = c(-12, 42), ylim = c(35, 72), expand = FALSE)+
  #scale_x_continuous(breaks = c(-15, 20, 30)) + 
  #scale_y_continuous(breaks = c(40, 50, 60,70)) +
  theme(legend.position = "bottom",legend.title = element_blank(), 
        legend.text = element_text(size = 18),panel.background = element_rect(fill = "lightskyblue1") ,
        axis.title.x=element_blank(),axis.title.y=element_blank(), 
        #axis.text.y = element_text(angle = 90,hjust = 0.5, size=12),
        #axis.text.x = element_text(size=12),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =   element_blank(),
        #panel.grid.minor.y =  element_blank(),
        strip.text = element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=4)))
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/fig_2a.png", dpi=350)

##########
##### Plot figure 2B,C and D
df_gpp_et_wue<-read.csv("D:/paper_3_europe/df_gpp_et_wue_percentages.csv")
str(df_gpp_et_wue)
df_gpp_et_wue$month_str<-month.abb[df_gpp_et_wue$month]
df_gpp_et_wue$month_str<-factor(df_gpp_et_wue$month_str,
                                levels =unique(df_gpp_et_wue$month_str), ordered = T)
df_gpp_et_wue<-df_gpp_et_wue[df_gpp_et_wue$cat=="gpp"& df_gpp_et_wue$trend!="No trend",]
df_gpp_et_wue$perc_area<-df_gpp_et_wue$area_sum/360149.6*100

require(ggplot2)
y<-ggplot(data=df_gpp_et_wue)+
  geom_bar(aes(x=month_str,y=area_sum*0.01/100, fill=trend),
           position="dodge", stat="identity",colour="black")+theme_bw()+ylab("FCA (x100 km²)")+
  xlab("")+scale_y_continuous(position = "right", limits = c(0,6))+
  scale_fill_manual(values=c("darkorange2","darkolivegreen"))+#ylim(0,600)+
  theme_bw()+
  theme(panel.grid.major.x  = element_blank(),axis.title.y.right = element_text(size = 16), axis.title.x.bottom = element_text(size = 16),
    text = element_text(size = 18),legend.title = element_blank(), legend.text = element_blank(),legend.position = "none",
    axis.text.y =element_text(angle = 0, hjust = 0.5))


df_trend<- read.table("D:/paper_3_europe/GPP_stack/GPP_MEAN_STACK_FC_trend_qgis.txt")#change GPP WUE ET #WUE_STACK_FC_trend_qgis.txt ET_MEAN_STACK_FC_trend_qgis.txt
head(df_trend)
unique(df_trend$gpp_trend)
df_trend$month_str<-month.abb[df_trend$month]
df_trend$month_str<-factor(df_trend$month_str,
                           levels =unique(df_trend$month_str), ordered = T)
df_trend<-df_trend[df_trend$gpp_trend!="No trend",]
head(df_trend)
min(df_trend$sen.slope)
p<-ggplot() +
  geom_boxplot(data=df_trend, mapping = aes(x = month_str, y=sen.slope, fill=gpp_trend))+#fill="grey",color="grey",
  theme_bw()+#facet_wrap( ~ gpp_trend,nrow = 3)+
  scale_fill_manual(values=c("darkorange2","darkolivegreen"))+
  ylab(expression("TS-GPP gC/m²/year"))+scale_y_continuous(position = "right", limits = c(-8,8), breaks = seq(-8,8,by=4))+xlab("")+# (SS-WUE gC"*'/L H'[2]*'O'*'/year')
  theme(#panel.grid.minor = element_blank(),
    panel.grid.major.x  = element_blank(),axis.title.y.right = element_text(size = 16), axis.title.x.bottom = element_text(size = 16),  #axis.text.x = element_text(angle = -45) #"SS-WUE (gC/L H2O/year
    text = element_text(size = 18),legend.title = element_blank(), legend.text = element_blank(),legend.position = "none",
    axis.text.y =element_text(angle = 0, hjust = 0.5),#axis.text.x =element_blank(),
    #panel.background = element_rect(fill = "lightgrey", color="black")#axis.text.x =element_blank(),axis.ticks.x=element_blank()
  )


require(sf)
pp<-read_sf("D:/paper_3_europe/MODIS_pixels_50_intersected_points.shp")

df_trend_geo<-merge(df_trend, pp, by=c("id"))
df_trend_geo$ss_total<-((df_trend_geo$sen.slope)*(df_trend_geo$area_ha*10000))/1000
unique(df_trend_geo$gpp_trend)
table(df_trend_geo$month_str)

require(dplyr)
df_trend_geo<-df_trend_geo%>%
  group_by(month_str,gpp_trend)%>% 
  summarise(sum_ss=sum(ss_total))
df_trend_geo$sum_ss<-df_trend_geo$sum_ss/1000000
max(df_trend_geo$sum_ss)
z<-ggplot(data=df_trend_geo,mapping = aes(x=month_str,y=sum_ss, fill=gpp_trend))+
  geom_bar(position="dodge", stat="identity",colour="black")+theme_bw()+ylab(expression("Mt C/year"))+#####'m'^3*'H'[2]*'O'*'/year(x1000)'
  xlab("Months")+scale_fill_manual(values=c("darkorange2","darkolivegreen"))+
  scale_y_continuous(position = "right",limits = c(-1.5, 1.5), breaks = seq(-1.5, 1.5, by = 0.5))+
  theme(panel.grid.major = element_blank(),axis.text.x = element_blank(),#panel.grid.minor = element_blank()
        text = element_text(size = 18),legend.title = element_blank(), legend.text = element_blank(),legend.position = "none",
        axis.title.y.right = element_text(size = 16), axis.title.x.bottom = element_text(size = 16),
        axis.text.y =element_text(angle = 0, hjust = 0.5), axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))

require("ggpubr")
ggarrange(y,p,z,
          labels = c("B", "C","D"), label.x = 0.01 , label.y = 0.99,heights  = c(1,1,1),align = "v", #labels = c("B", "C","D"), heights = c(0.9,1,0.9)
          ncol = 1, nrow = 3)
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/fig_2bcd.png", dpi=350)


###################################
##### PLOT FIGURE 3
##########
##### Plot figure 3A

df_et_trend<- read.table("D:/paper_3_europe/ET_stack/ET_MEAN_STACK_FC_trend_qgis.txt")

pp_et<-merge(df_et_trend, pp, by="id")
head(pp_et)
pp_et_1 <- pp_et %>%
  mutate(long =unlist(map(pp_et$geometry,1)),
         lat = unlist(map(pp_et$geometry,2)))

pp_et_1<-pp_et_1[order(pp_et_1$month),]
pp_et_1$month_str<-month.abb[pp_et_1$month]
pp_et_1$month_str<-factor(pp_et_1$month_str,
                          levels =unique(pp_et_1$month_str), ordered = T)
head(pp_et_1)

pp_et_2<-pp_et_1[pp_et_1$et_trend!="No trend",]

require(ggplot2)
ggplot() +facet_wrap( ~ month_str)+ #panel.grid.minor = element_blank(),panel.grid.major = element_blank()
  theme_bw()+ 
  geom_sf(data = world, colour = "grey", fill = "lightgrey")+
  geom_sf(data = shp_euro, colour = "darkgrey", fill = "grey")+
  geom_point(data=pp_et_2, aes(x=long, y=lat,col=et_trend),
             size=0.75,alpha=0.4)+#color="darkgreen" , 
  scale_color_manual(values=c("darkorange2","darkolivegreen"))+
  coord_sf(xlim = c(-12, 42), ylim = c(35, 72), expand = FALSE)+
  #scale_x_continuous(breaks = c(-15, 20, 30)) + 
  #scale_y_continuous(breaks = c(40, 50, 60,70)) +
  theme(legend.position = "bottom",legend.title = element_blank(), 
        legend.text = element_text(size = 18),panel.background = element_rect(fill = "lightskyblue1") ,
        axis.title.x=element_blank(),axis.title.y=element_blank(), 
        #axis.text.y = element_text(angle = 90,hjust = 0.5, size=12),
        #axis.text.x = element_text(size=12),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =   element_blank(),
        #panel.grid.minor.y =  element_blank(),
        strip.text = element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha=0.5)))
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/fig_3a.png", dpi=350)


##########
##### Plot figure 3B,C and D
df_gpp_et_wue<-read.csv("D:/paper_3_europe/df_gpp_et_wue_percentages.csv")
str(df_gpp_et_wue)
df_gpp_et_wue$month_str<-month.abb[df_gpp_et_wue$month]
df_gpp_et_wue$month_str<-factor(df_gpp_et_wue$month_str,
                                levels =unique(df_gpp_et_wue$month_str), ordered = T)
df_gpp_et_wue<-df_gpp_et_wue[df_gpp_et_wue$cat=="et"& df_gpp_et_wue$trend!="No trend",]
df_gpp_et_wue$perc_area<-df_gpp_et_wue$area_sum/360149.6*100

require(ggplot2)
y<-ggplot(data=df_gpp_et_wue)+
  geom_bar(aes(x=month_str,y=area_sum*0.01/100, fill=trend), 
           position="dodge", stat="identity", colour="black")+theme_bw()+ylab("FCA (x100 km²)")+ #alpha=0.7
  xlab("")+scale_y_continuous(position = "right", limits = c(0,10))+
  scale_fill_manual(values=c("darkorange2","darkolivegreen"))+#ylim(0,600)+
  theme_bw()+
  theme(panel.grid.major.x  = element_blank(),
        text = element_text(size = 18),legend.title = element_blank(), legend.text = element_blank(),legend.position = "none",
        axis.title.y.right = element_text(size = 16), axis.title.x.bottom = element_text(size = 16),
        axis.text.y =element_text(angle = 0, hjust = 0.5))


df_trend<- read.table("D:/paper_3_europe/ET_stack/ET_MEAN_STACK_FC_trend_qgis.txt")#change GPP WUE ET #WUE_STACK_FC_trend_qgis.txt ET_MEAN_STACK_FC_trend_qgis.txt
head(df_trend)
unique(df_trend$et_trend)
df_trend$month_str<-month.abb[df_trend$month]
df_trend$month_str<-factor(df_trend$month_str,
                           levels =unique(df_trend$month_str), ordered = T)
df_trend<-df_trend[df_trend$et_trend!="No trend",]
head(df_trend)

p<-ggplot() +
  geom_boxplot(data=df_trend, mapping = aes(x = month_str, y=sen.slope, fill=et_trend))+#fill="grey",color="grey",
  theme_bw()+#facet_wrap( ~ gpp_trend,nrow = 3)+
  scale_fill_manual(values=c("darkorange2","darkolivegreen"))+
  ylab(expression("TS-ET (mm/year)"))+scale_y_continuous(position = "right", limits = c(-6,6))+xlab("")+# (SS-WUE gC"*'/L H'[2]*'O'*'/year')
  theme(#panel.grid.minor = element_blank(),
    panel.grid.major.x  = element_blank(),#axis.text.x = element_text(angle = -45) #"SS-WUE (gC/L H2O/year
    text = element_text(size = 18),legend.title = element_blank(), legend.text = element_blank(),legend.position = "none",
    axis.title.y.right = element_text(size = 16), axis.title.x.bottom = element_text(size = 16),
    axis.text.y =element_text(angle = 0, hjust = 0.5),#axis.text.x =element_blank(),
    #panel.background = element_rect(fill = "lightgrey", color="black")#axis.text.x =element_blank(),axis.ticks.x=element_blank()
  )


require(sf)
pp<-read_sf("D:/paper_3_europe/MODIS_pixels_50_intersected_points.shp")

df_trend_geo<-merge(df_trend, pp, by=c("id"))
df_trend_geo$ss_total<-((df_trend_geo$sen.slope)*(df_trend_geo$area_ha*10000))/1000
unique(df_trend_geo$et_trend)
min(df_trend_geo$ss_total)

require(dplyr)
df_trend_geo<-df_trend_geo%>%
  group_by(month_str,et_trend)%>% 
  summarise(sum_ss=sum(ss_total))
max(df_trend_geo$sum_ss)/1000
z<-ggplot(data=df_trend_geo,mapping = aes(x=month_str,y=sum_ss/1000, fill=et_trend))+
  geom_bar(position="dodge", stat="identity", colour="black")+theme_bw()+ylab(expression("m"^3*' H'[2]*'O '*'/ year (x1000)'))+#####
  xlab("Months")+scale_fill_manual(values=c("darkorange2","darkolivegreen"))+
  scale_y_continuous(position = "right",limits = c(-1000, 1000), breaks = seq(-1000, 1000, by = 500))+
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),#panel.grid.minor = element_blank()
        text = element_text(size = 18),legend.title = element_blank(), legend.text = element_blank(),legend.position = "none",
        axis.title.y.right = element_text(size = 16), axis.title.x.bottom = element_text(size = 16),
        axis.text.y =element_text(angle = 0, hjust = 0.5), axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))

require("ggpubr")
ggarrange(y,p,z,
          labels = c("B", "C","D"), label.x = 0.01 , label.y = 0.99,heights  = c(1,1,1),align = "v", #labels = c("B", "C","D"), heights = c(0.9,1,0.9)
          ncol = 1, nrow = 3)
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/fig_3bcd.png", dpi=350)

###################################
##### PLOT FIGURE 4A

df_wue_trend<- read.table("D:/paper_3_europe/ET_stack/WUE_STACK_FC_trend_qgis.txt")
pp_wue<-merge(df_wue_trend, pp, by="id")
pp_wue_1 <- pp_wue %>%
  mutate(long =unlist(map(pp_wue$geometry,1)),
         lat = unlist(map(pp_wue$geometry,2)))

pp_wue_1<-pp_wue_1[order(pp_wue_1$month),]
pp_wue_1$month_str<-month.abb[pp_wue_1$month]
pp_wue_1$month_str<-factor(pp_wue_1$month_str,
                           levels =unique(pp_wue_1$month_str), ordered = T)
head(pp_wue_1)
pp_wue_2<-pp_wue_1[pp_wue_1$wue_trend!="No trend",]
head(pp_wue_2)

require(ggplot2)
ggplot() +facet_wrap( ~ month_str)+ #panel.grid.minor = element_blank(),panel.grid.major = element_blank()
  theme_bw()+ 
  geom_sf(data = world, colour = "grey", fill = "lightgrey")+
  geom_sf(data = shp_euro, colour = "darkgrey", fill = "grey")+
  geom_point(data=pp_wue_2, aes(x=long, y=lat,col=wue_trend),
             size=0.75,alpha=0.4)+#color="darkgreen" , 
  scale_color_manual(values=c("darkorange2","darkolivegreen"))+
  coord_sf(xlim = c(-12, 42), ylim = c(35, 72), expand = FALSE)+
  #scale_x_continuous(breaks = c(-15, 20, 30)) + 
  #scale_y_continuous(breaks = c(40, 50, 60,70)) +
  theme(legend.position = "bottom",legend.title = element_blank(), 
        legend.text = element_text(size = 18),panel.background = element_rect(fill = "lightskyblue1") ,
        axis.title.x=element_blank(),axis.title.y=element_blank(), 
        #axis.text.y = element_text(angle = 90,hjust = 0.5, size=12),
        #axis.text.x = element_text(size=12),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =   element_blank(),
        #panel.grid.minor.y =  element_blank(),
        strip.text = element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=4, alpha=0.5)))
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/fig_4a.png", dpi=350)

##########
##### Plot figure 4B,C and D
df_gpp_et_wue<-read.csv("D:/paper_3_europe/df_gpp_et_wue_percentages.csv")
str(df_gpp_et_wue)
df_gpp_et_wue$month_str<-month.abb[df_gpp_et_wue$month]
df_gpp_et_wue$month_str<-factor(df_gpp_et_wue$month_str,
                                levels =unique(df_gpp_et_wue$month_str), ordered = T)
df_gpp_et_wue<-df_gpp_et_wue[df_gpp_et_wue$cat=="wue"& df_gpp_et_wue$trend!="No trend",]
df_gpp_et_wue$perc_area<-df_gpp_et_wue$area_sum/360149.6*100

require(ggplot2)
y<-ggplot(data=df_gpp_et_wue)+
  geom_bar(aes(x=month_str,y=area_sum*0.01, fill=trend),
           position="dodge", stat="identity",colour="black")+theme_bw()+ylab("FCA (km²)")+
  xlab("")+scale_y_continuous(position = "right", limits = c(0,1500))+
  scale_fill_manual(values=c("darkorange2","darkolivegreen"))+#ylim(0,600)+
  theme_bw()+
  theme(panel.grid.major.x  = element_blank(),
        text = element_text(size = 18),legend.title = element_blank(), legend.text = element_blank(),legend.position = "none",
        axis.title.y.right = element_text(size = 16), axis.title.x.bottom = element_text(size = 16),
        axis.text.y =element_text(angle = 0, hjust = 0.5))


df_trend<- read.table("D:/paper_3_europe/ET_stack/WUE_STACK_FC_trend_qgis.txt")#change GPP WUE ET #WUE_STACK_FC_trend_qgis.txt ET_MEAN_STACK_FC_trend_qgis.txt
head(df_trend)
unique(df_trend$wue_trend)
df_trend$month_str<-month.abb[df_trend$month]
df_trend$month_str<-factor(df_trend$month_str,
                           levels =unique(df_trend$month_str), ordered = T)
df_trend<-df_trend[df_trend$wue_trend!="No trend",]
head(df_trend)

p<-ggplot() +
  geom_boxplot(data=df_trend, mapping = aes(x = month_str, y=sen.slope, fill=wue_trend))+#fill="grey",color="grey",
  theme_bw()+#facet_wrap( ~ gpp_trend,nrow = 3)+
  scale_fill_manual(values=c("darkorange2","darkolivegreen"))+ #"darkorange2","darkolivegreen"
  ylab(expression("TS-WUE gC"*'/L H'[2]*'O'*'/year'))+scale_y_continuous(position = "right", limits = c(-.1,.1))+xlab("")+# (SS-WUE gC"*'/L H'[2]*'O'*'/year')
  theme(#panel.grid.minor = element_blank(),
    panel.grid.major.x  = element_blank(),#axis.text.x = element_text(angle = -45) #"SS-WUE (gC/L H2O/year
    text = element_text(size = 18),legend.title = element_blank(), legend.text = element_blank(),legend.position = "none",
    axis.title.y.right = element_text(size = 16), axis.title.x.bottom = element_text(size = 16),
    axis.text.y =element_text(angle = 0, hjust = 0.5),#axis.text.x =element_blank(),
    #panel.background = element_rect(fill = "lightgrey", color="black")#axis.text.x =element_blank(),axis.ticks.x=element_blank()
  )

require("ggpubr")
ggarrange(y,p,
          labels = c("B", "C"), label.x = 0.01 , label.y = 0.99,heights  = c(1,1),align = "v", #labels = c("B", "C","D"), heights = c(0.9,1,0.9)
          ncol = 1, nrow = 2)
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/fig_4bc.png", dpi=350)


###################################
##### PLOT FIGURE 5

df_wue_trend<- read.table("D:/paper_3_europe/ET_stack/WUE_STACK_FC_trend_qgis.txt")
head(df_wue_trend)
names(df_wue_trend)[3] <- "sen.slope_wue"

df_et_trend<- read.table("D:/paper_3_europe/ET_stack/ET_MEAN_STACK_FC_trend_qgis.txt")
head(df_et_trend)
names(df_et_trend)[3] <- "sen.slope_et"

df_gpp_trend<-read.table("D:/paper_3_europe/GPP_stack/GPP_MEAN_STACK_FC_trend_qgis.txt")
head(df_gpp_trend)
names(df_gpp_trend)[3] <- "sen.slope_gpp"


df_list <- list(df_wue_trend, df_et_trend, df_gpp_trend)
df<-Reduce(function(x, y) merge(x, y, by=c("id","month")), df_list, accumulate=FALSE)
head(df)


df$gpp_signs<-ifelse(df$sen.slope_gpp<0,"-","+")
df$et_signs<-ifelse(df$sen.slope_et<0,"-","+")
df$wue_signs<-ifelse(df$sen.slope_wue<0,"-","+")

df$n_signs<-ifelse(df$gpp_signs=="+"& df$et_signs=="+" & df$wue_signs=="+" , 1,
                   ifelse(df$gpp_signs=="+"& df$et_signs=="-" & df$wue_signs=="-", 2,
                          ifelse(df$gpp_signs=="+"& df$et_signs=="+" & df$wue_signs=="-" ,3,
                                 ifelse(df$gpp_signs=="+"& df$et_signs=="-" & df$wue_signs=="+" ,4,
                                        ifelse(df$gpp_signs=="-"& df$et_signs=="-" & df$wue_signs=="-" ,5,
                                               ifelse(df$gpp_signs=="-"& df$et_signs=="+" & df$wue_signs=="+" ,6,
                                                      ifelse(df$gpp_signs=="-"& df$et_signs=="+" & df$wue_signs=="-" ,7,
                                                             ifelse(df$gpp_signs=="-"& df$et_signs=="-" & df$wue_signs=="+" ,8,9))))))))
head(df)
table(df$n_signs)

require(dplyr)
gpp_et_wue_coun_signs<-df %>% 
  group_by(month) %>% count(n_signs) #%>% summarise(n_opty = sum(n))
gpp_et_wue_coun_signs

gpp_et_wue_coun_signs<-gpp_et_wue_coun_signs %>% 
  group_by(month) %>% mutate(n_sum = sum(n)) %>%
  ungroup() #%>% summarise(n_opty = sum(n))
gpp_et_wue_coun_signs


gpp_et_wue_coun_signs$perc<-round(gpp_et_wue_coun_signs$n/gpp_et_wue_coun_signs$n_sum*100,2)
class(gpp_et_wue_coun_signs)
head(gpp_et_wue_coun_signs)

gpp_et_wue_coun_signs$month_str<-month.abb[gpp_et_wue_coun_signs$month]
gpp_et_wue_coun_signs$month_str<-factor(gpp_et_wue_coun_signs$month_str,
                                        levels =unique(gpp_et_wue_coun_signs$month_str), ordered = T)
unique(gpp_et_wue_coun_signs$n_signs)
gpp_et_wue_coun_signs$sign_str<-ifelse(gpp_et_wue_coun_signs$n_signs==1,"GPP (+) ET (+) WUE(+)",
                                       ifelse(gpp_et_wue_coun_signs$n_signs==2,"GPP (+) ET (-) WUE(-)",
                                              ifelse(gpp_et_wue_coun_signs$n_signs==3,"GPP (+) ET (+) WUE(-)",
                                                     ifelse(gpp_et_wue_coun_signs$n_signs==4,"GPP (+) ET (-) WUE(+)",
                                                            ifelse(gpp_et_wue_coun_signs$n_signs==5,"GPP (-) ET (-) WUE(-)",
                                                                   ifelse(gpp_et_wue_coun_signs$n_signs==6,"GPP (-) ET (+) WUE(+)",
                                                                          ifelse(gpp_et_wue_coun_signs$n_signs==7,"GPP (-) ET (+) WUE(-)",
                                                                                 "GPP (-) ET (-) WUE(+)")))))))
unique(gpp_et_wue_coun_signs$n_sum)
require(ggplot2)
library(viridis)
library(RColorBrewer)

ggplot(gpp_et_wue_coun_signs, aes(y=sign_str, x=month_str, fill= perc)) + 
  geom_tile(colour = "lightgrey", size=0.5)  + theme_bw()+
  geom_text(colour = "black",aes(label = round(perc, 1))) +
  #scale_fill_viridis(discrete=FALSE, direction=-1,limits=c(0,100))+
  scale_fill_gradientn("perc", colours = brewer.pal(9, "Greens"),limits=c(0,100))+#YlOrRd
  #scale_fill_gradient(low = "grey", high = "darkorange",limits=c(0,100)) +#aes(color=group_n),size=1
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(angle = -45), 
        axis.title.x=element_blank(),axis.title.y=element_blank(),text = element_text(size = 16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.ticks=element_blank())#text = element_text(size = 18)
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/fig_5.png", dpi = 350)

###################################
##### PLOT FIGURE 6

##########
##### Plot figure 6A
df_gpp_trend<-read.table("D:/paper_3_europe/GPP_stack/GPP_MEAN_STACK_FC_trend_qgis.txt")
head(df_gpp_trend)

require(sf)
pp<-read_sf("D:/paper_3_europe/MODIS_pixels_50_intersected_points.shp")
head(pp)
sum(pp$area_ha)

ggp_pp<-merge(df_gpp_trend, pp, by=c("id"))
head(ggp_pp)
ggp_pp$ss_total<-(ggp_pp$sen.slope*ggp_pp$area_ha*10000)/1000 #convert to tonnes
ggp_pp$ss_total<-ggp_pp$ss_total/1000000
min(ggp_pp$ss_total)
max(ggp_pp$ss_total)

library(dplyr)
df_gpp_ss_sum<-ggp_pp %>% 
  group_by(month) %>% 
  summarise(ss_sum_monthly_ton_gpp = sum(ss_total))

head(df_gpp_ss_sum)

df_gpp_ss_sum$gpp_sign<-ifelse(df_gpp_ss_sum$ss_sum_monthly_ton_gpp<0,"Loss","Gain")
unique(df_gpp_ss_sum$gpp_sign)

df_gpp_ss_sum$month_str<-month.abb[df_gpp_ss_sum$month]
df_gpp_ss_sum$month_str<-factor(df_gpp_ss_sum$month_str,
                                levels =unique(df_gpp_ss_sum$month_str), ordered = T)

require(ggplot2)
ggplot(df_gpp_ss_sum, aes(x=month_str, y=ss_sum_monthly_ton_gpp, fill=gpp_sign)) +
  theme_bw()+
  geom_col(position="stack",colour = "black")+
  labs(y="GPP - (Mt C/year)", x = "Months")+
  ylim(-2.5,5)+
  scale_fill_manual(values = c("darkolivegreen","darkorange2"), guide = FALSE)+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),#axis.text.x = element_text(angle = -45)
        text = element_text(size = 18),legend.position = "none",legend.title=element_blank(),
        axis.text.y =element_text(angle = 90, hjust = 0.5), axis.text.x.bottom = element_text(size = 16),axis.text.y.left = element_text(size = 16))+
  annotate("text", x = 9.3, y = 5, label = "A", size=7)
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/fig_6a.png", dpi = 350)

##########
##### Plot figure 6B
ggp_pp<-merge(df_gpp_trend, pp, by=c("id"))
head(ggp_pp)
ggp_pp$ss_total<-(ggp_pp$sen.slope*ggp_pp$area_ha*10000)/1000000 #convert to tonnes
min(ggp_pp$ss_total)
max(ggp_pp$ss_total)

library(dplyr)
df_gpp_ss_sum<-ggp_pp %>% 
  group_by(id) %>% 
  summarise(ss_sum_ton_gpp = sum(ss_total))
library(dplyr)
df_gpp_ss_sum<-ggp_pp %>% 
  group_by(id) %>% 
  summarise(ss_sum_ton_gpp = sum(ss_total))

df_gpp_ss_sum$gpp_sign<-ifelse(df_gpp_ss_sum$ss_sum_ton_gpp<0,"Loss","Gain")
unique(df_gpp_ss_sum$gpp_sign)
table(df_gpp_ss_sum$gpp_sign)

max(df_gpp_ss_sum$ss_sum_ton_gpp)
require(ggplot2)
ggplot(df_gpp_ss_sum, aes(x = gpp_sign, y = ss_sum_ton_gpp)) + #, colour = month
  geom_violin(aes(fill=gpp_sign), alpha=0.5)+
  geom_boxplot(width = 0.2,aes(fill=gpp_sign)) + ylim(-5,5)+
  theme_bw()+scale_fill_manual(values=c("darkolivegreen","darkorange2"))+
  labs(y="GPP (t /month)", x="")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        legend.position = "none",text = element_text(size = 18),
        axis.text.y =element_text(angle = 90, hjust = 0.5), axis.text.x.bottom = element_text(size = 16),axis.text.y.left = element_text(size = 16))+
  annotate("text", x = 2.5, y = 5, label = "B", size=6)
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/fig_6b.png", dpi = 350)
df_gpp_ss_sum_neg<-df_gpp_ss_sum[df_gpp_ss_sum$gpp_sign=="Deficit",]
unique(df_gpp_ss_sum_neg$gpp_sign)
sum(df_gpp_ss_sum_neg$ss_sum_ton_gpp)

df_gpp_ss_sum_pos<-df_gpp_ss_sum[df_gpp_ss_sum$gpp_sign=="Surplus",]
unique(df_gpp_ss_sum_pos$gpp_sign)
sum(df_gpp_ss_sum_pos$ss_sum_ton_gpp)


###################################
##### PLOT FIGURE S2
df_complt<-read.csv("D:/paper_3_europe/fluxnet/gpp_modis_values_from_towers.csv", sep = ",")
df_complt$gpp_monthly_modis<-df_complt$gpp_monthly_modis*1000 #convert to grams
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
nrow(df)
x<-cor.test(df$GPP_DT_VUT_REF_day_mean_per_month*df$n_days_calendar,df$gpp_monthly_modis)
N<-paste0("N: ",x$parameter)

#install.packages("hrbrthemes")
library(ggplot2)
library(hrbrthemes)
ggplot(df, aes(x=GPP_DT_VUT_REF_day_mean_per_month*df$n_days_calendar , y=gpp_monthly_modis)) +
  geom_point() + theme_bw()+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid.minor = element_blank(),
        legend.position = "none",text = element_text(size = 18))+
  xlab("Tower - GPP (gC/m²/month)")+ylab("MODIS - GPP (gC/m²/month)")+
  annotate( geom = "text", y = c(370,347, 325), x = c(15,125,15),
            label = c(r2_label,rmse_label,N), size = 5.5)+
  annotate("text", x = 450, y = 370, label = "A", size=6)
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/s2A.png",dpi=350)

###################################
##### PLOT FIGURE S3
rm(list=ls())
df_complt<-read.csv("D:/paper_3_europe/fluxnet/et_points_values_towers.csv", sep=",")
df_complt$station<-df_complt$tower_id
head(df_complt)
unique(df_complt$station)
head(df_complt)

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
summary(df)

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
mae_label<-round(mae(df$et_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis),2)
mae_label <- paste0("MAE: ±", mae_label, "mm")
r2_label<-round(cor(df$et_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis,use = "complete.obs")^2,2)
r2_label = paste0("R²: ",r2_label)
x<-cor.test(df$et_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis)
N<-paste0("N: ",x$parameter)
nrow(df)
unique(df$station)
library(ggplot2)
library(hrbrthemes)
ggplot(df, aes(x=et_day_mean_mu_per_month*df$n_days_calendar, y=et_monthly_modis)) +
  geom_point()+theme_bw() +#geom_text(aes(label=station))+ 
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid.minor = element_blank(),
        legend.position = "none",text = element_text(size = 18))+
  xlab("Towers ET (mm/month)")+ylab("MODIS - ET (mm/month)")+
  annotate( geom = "text", y = c(205,192,180), x = c(4.5,16,4.5),
            label = c(r2_label,rmse_label, N), size = 5.5)+
  annotate("text", x = 140, y = 190, label = "B", size=6)

ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/s2B.png",dpi=350)



### CORRELATION BETWEEN MODIS AND ET_CORR_day_mean_mu_per_month
head(df)
nrow(df)
cor.val <-round(cor(df$ET_CORR_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis, method = "pearson",use = "complete.obs"),2)
cor.label <- paste0("R: ", cor.val)
rmse_label<-round(rmse(df$ET_CORR_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis,na.rm = TRUE),2)
rmse_label <- paste0("RMSE: ±", rmse_label,"mm")
mae_label<-round(mae(df$ET_CORR_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis),2)
mae_label <- paste0("MAE: ±", mae_label, "mm")
r2_label<-round(cor(df$ET_CORR_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis,use = "complete.obs")^2,2)
r2_label = paste0("R²: ",r2_label)
x<-cor.test(df$ET_CORR_day_mean_mu_per_month*df$n_days_calendar,df$et_monthly_modis)
N<-paste0("N: ",x$parameter)

library(ggplot2)
library(hrbrthemes)
ggplot(df, aes(x=df$ET_CORR_day_mean_mu_per_month*df$n_days_calendar, y=et_monthly_modis)) +
  geom_point() + theme_bw()+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid.minor = element_blank(),
        legend.position = "none",text = element_text(size = 18))+
  xlab("Towers ET (mm/month)")+ylab("MODIS - ET (mm/month)")+
  #ggtitle("ET corrected")
  annotate( geom = "text", y = c(205,192,180), x = c(4.5,21,4.5),
            label = c(r2_label,rmse_label, N), size = 5.5)+
  annotate("text", x = 140, y = 190, label = "B", size=6)
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/s2B.png",dpi=350)

###################################
##### PLOT FIGURE S4
rm(list=ls())
df_gpp_trend<-read.table("D:/paper_3_europe/GPP_stack/GPP_MEAN_STACK_FC_trend_qgis.txt")
head(df_gpp_trend)

require(sf)
pp<-read_sf("D:/paper_3_europe/MODIS_pixels_50_intersected_points.shp")
pp<-st_transform(pp, 4326)
head(pp)

pp_gpp<-merge(df_gpp_trend, pp, by="id")
head(pp_gpp)

table(pp_gpp$id)

shp_euro<-read_sf("D:/paper_3_europe/shp_base/europe_final_rep_rasterized.shp")

world<-read_sf("D:/shp_base/World_Countries.shp")

require(dplyr)
library(purrr)

pp_gpp_1 <- pp_gpp %>%
  mutate(long =unlist(map(pp_gpp$geometry,1)),
         lat = unlist(map(pp_gpp$geometry,2)))

str(pp_gpp_1)
pp_gpp_1<-pp_gpp_1[order(pp_gpp_1$month),]
pp_gpp_1$month_str<-month.abb[pp_gpp_1$month]
pp_gpp_1$month_str<-factor(pp_gpp_1$month_str,
                           levels =unique(pp_gpp_1$month_str), ordered = T)
mid=0
min(pp_gpp_1$sen.slope)
max(pp_gpp_1$sen.slope)
require(ggplot2)
ggplot() +facet_wrap( ~ month_str)+theme_bw()+ 
  geom_sf(data = world, colour = "grey", fill = "lightgrey")+
  geom_sf(data = shp_euro, colour = "darkgrey", fill = "grey")+
  geom_point(data=pp_gpp_1, aes(x=long, y=lat, col=sen.slope),
             shape = 19, size=0.75)+#color="darkgreen"
  scale_color_gradient2(midpoint = mid, low = "darkmagenta", mid = "white", high = "darkgreen",
                        limits = c(-5, 5))+
  coord_sf(xlim = c(-12, 42), ylim = c(35, 72), expand = FALSE)+
  theme(legend.position = "bottom", #legend.title = element_text("TS-GPP gC/m²/year",size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, angle = -45),panel.background = element_rect(fill = "lightskyblue1") ,
        axis.title.x=element_blank(),axis.title.y=element_blank(), 
        #axis.text.y = element_text(angle = 90,hjust = 0.5, size=12),
        #axis.text.x = element_text(size=12),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =   element_blank(),
        #panel.grid.minor.y =  element_blank(),
        strip.text = element_text(size=14))+
  labs(fill = "TS-GPP gC/m²/year")
  #guides(fill=guide_legend(title="TS-GPP gC/m²/year"))
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/s4.png", dpi = 350)

###################################
##### PLOT FIGURE S5
rm(list=ls())
df_et_trend<-read.table("D:/paper_3_europe/ET_stack/ET_MEAN_STACK_FC_trend_qgis.txt")
head(df_et_trend)

require(sf)
pp<-read_sf("D:/paper_3_europe/MODIS_pixels_50_intersected_points.shp")
pp<-st_transform(pp, 4326)
head(pp)

pp_et<-merge(df_et_trend, pp, by="id")
head(pp_et)

table(pp_et$id)

shp_euro<-read_sf("D:/paper_3_europe/shp_base/europe_final_rep_rasterized.shp")

world<-read_sf("D:/shp_base/World_Countries.shp")

require(dplyr)
library(purrr)

pp_et_1 <- pp_et %>%
  mutate(long =unlist(map(pp_et$geometry,1)),
         lat = unlist(map(pp_et$geometry,2)))

str(pp_et_1)
pp_et_1<-pp_et_1[order(pp_et_1$month),]
pp_et_1$month_str<-month.abb[pp_et_1$month]
pp_et_1$month_str<-factor(pp_et_1$month_str,
                           levels =unique(pp_et_1$month_str), ordered = T)
mid=0
min(pp_et_1$sen.slope)
max(pp_et_1$sen.slope)
require(ggplot2)
ggplot() +facet_wrap( ~ month_str)+theme_bw()+ 
  geom_sf(data = world, colour = "grey", fill = "lightgrey")+
  geom_sf(data = shp_euro, colour = "darkgrey", fill = "grey")+
  geom_point(data=pp_et_1, aes(x=long, y=lat, col=sen.slope),
             shape = 19, size=0.75)+#color="darkgreen"
  scale_color_gradient2(midpoint = mid, low = "darkmagenta", mid = "white", high = "darkgreen",
                        limits = c(-6, 6))+
  coord_sf(xlim = c(-12, 42), ylim = c(35, 72), expand = FALSE)+
  theme(legend.position = "bottom", #legend.title = element_text("TS-GPP gC/m²/year",size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, angle = -45),panel.background = element_rect(fill = "lightskyblue1") ,
        axis.title.x=element_blank(),axis.title.y=element_blank(), 
        #axis.text.y = element_text(angle = 90,hjust = 0.5, size=12),
        #axis.text.x = element_text(size=12),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =   element_blank(),
        #panel.grid.minor.y =  element_blank(),
        strip.text = element_text(size=14))
  #labs(fill = "TS-GPP gC/m²/year")
#guides(fill=guide_legend(title="TS-GPP gC/m²/year"))
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/s5.png", dpi = 350)

###################################
##### PLOT FIGURE S6
rm(list=ls())
df_wue_trend<-read.table("D:/paper_3_europe/ET_stack/WUE_STACK_FC_trend_qgis.txt")
head(df_wue_trend)

require(sf)
pp<-read_sf("D:/paper_3_europe/MODIS_pixels_50_intersected_points.shp")
pp<-st_transform(pp, 4326)
head(pp)

pp_wue<-merge(df_wue_trend, pp, by="id")
head(pp_wue)

table(pp_wue$id)

shp_euro<-read_sf("D:/paper_3_europe/shp_base/europe_final_rep_rasterized.shp")

world<-read_sf("D:/shp_base/World_Countries.shp")

require(dplyr)
library(purrr)

pp_wue_1 <- pp_wue %>%
  mutate(long =unlist(map(pp_wue$geometry,1)),
         lat = unlist(map(pp_wue$geometry,2)))

str(pp_wue_1)
pp_wue_1<-pp_wue_1[order(pp_wue_1$month),]
pp_wue_1$month_str<-month.abb[pp_wue_1$month]
pp_wue_1$month_str<-factor(pp_wue_1$month_str,
                          levels =unique(pp_wue_1$month_str), ordered = T)
mid=0
min(pp_wue_1$sen.slope)
max(pp_wue_1$sen.slope)
require(ggplot2)
ggplot() +facet_wrap( ~ month_str)+theme_bw()+ 
  geom_sf(data = world, colour = "grey", fill = "lightgrey")+
  geom_sf(data = shp_euro, colour = "darkgrey", fill = "grey")+
  geom_point(data=pp_wue_1, aes(x=long, y=lat, col=sen.slope),
             shape = 19, size=0.75)+#color="darkgreen"
  scale_color_gradient2(midpoint = mid, low = "darkmagenta", mid = "white", high = "darkgreen",
                        limits = c(-0.1, 0.1))+
  coord_sf(xlim = c(-12, 42), ylim = c(35, 72), expand = FALSE)+
  theme(legend.position = "bottom", #legend.title = element_text("TS-GPP gC/m²/year",size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, angle = -45),panel.background = element_rect(fill = "lightskyblue1") ,
        axis.title.x=element_blank(),axis.title.y=element_blank(), 
        #axis.text.y = element_text(angle = 90,hjust = 0.5, size=12),
        #axis.text.x = element_text(size=12),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =   element_blank(),
        #panel.grid.minor.y =  element_blank(),
        strip.text = element_text(size=14))
#labs(fill = "TS-WUE gC"*'/L H'[2]*'O'*'/year'")
#guides(fill=guide_legend(title="TS-GPP gC/m²/year"))
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/s6.png", dpi = 350)


###################################
##### PLOT FIGURE S7
rm(list=ls())
require(sf)
shp_euro<-read_sf("D:/paper_3_europe/shp_base/europe_final_rep_rasterized.shp")

world<-read_sf("D:/shp_base/World_Countries.shp")

ggp_pp_sum<-read_sf("D:/paper_3_europe/GPP_stack/gpp_points_surplus_deficit.shp")
ggp_pp_sum<-st_transform(ggp_pp_sum, 4326)
head(ggp_pp_sum)

require(dplyr)
area_sum<-ggp_pp_sum %>% 
  group_by(gpp_sgn) %>% 
  summarise(area_sum = sum(area_ha))
area_sum$area_sum

library(purrr)
ggp_pp_sum_1 <- ggp_pp_sum %>%
  mutate(long =unlist(map(ggp_pp_sum$geometry,1)),
         lat = unlist(map(ggp_pp_sum$geometry,2)))

require(ggplot2)
ggplot()+theme_bw()+ facet_wrap( ~ gpp_sgn,
                                 labeller = labeller(gpp_sgn = 
                                                       c("Surplus" = "Gain",
                                                         "Deficit" = "Loss")))+
  #geom_sf()+
  geom_sf(data = world, colour = "grey", fill = "lightgrey")+
  geom_sf(data = shp_euro, colour = "darkgrey", fill = "grey")+
  geom_point(data=ggp_pp_sum_1, aes(x=long, y=lat, col=gpp_sgn),alpha=0.4,
             shape = 19, size=0.75)+#color="darkgreen"
  scale_color_manual(values=c("darkorange2","darkolivegreen"))+
  coord_sf(xlim = c(-12, 42), ylim = c(35, 72), expand = FALSE)+
  theme(legend.position = "None",
        legend.title = element_blank(),
        legend.text = element_text(size = 14, angle = -45),panel.background = element_rect(fill = "lightskyblue1") ,
        axis.title.x=element_blank(),axis.title.y=element_blank(), 
        #axis.text.y = element_text(angle = 90,hjust = 0.5, size=12),
        #axis.text.x = element_text(size=12),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =   element_blank(),
        #panel.grid.minor.y =  element_blank(),
        strip.text = element_text(size=14))
ggsave("C:/Users/bruno/Dropbox/Doc/paper_3/results_europe/s7.png", dpi = 350)
