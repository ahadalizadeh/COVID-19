source("https://raw.githubusercontent.com/ahadalizadeh/COVID-19/master/Download%20data.R")


source("https://raw.githubusercontent.com/ahadalizadeh/utility_fun/master/utility_fun.R")
names(COVID19)

COVID19$Deaths.Recovered<- COVID19$Recovered+COVID19$Deaths
Iran = COVID19[COVID19$`Country/Region`=="Iran",] 
Iran$Confirmed.daily <- c(0,Iran$Confirmed)  %>%  diff  
Iran$Recovered.daily <- c(0,Iran$Recovered)  %>%  diff 
Iran$Deaths.daily <- c(0,Iran$Deaths)  %>%  diff  


######## All countries incidence -----
Inc <-
  ggplot(COVID19, 
         aes(x=date, 
             y=Deaths,
             group=`Country/Region`,
             color=`Country/Region`)) +
  stat_summary(geom="line", fun.y = sum)+
  stat_summary(geom="point", fun.y = sum)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom") +
  annotate("text",
           x=24,
           y=31000,
           label="Designed by Iranian COVID19 Research Group",
           alpha= 0.1)

ggsave("Inc.jpg", Inc, width = 10, height = 12.5)



######## Iran incidence -----
(Inc.Iran <-   reshape2::melt(Iran,measure.vars=c(4:6) ) %>% 
    ggplot(aes(x=date, 
               y=value,
               group=variable ,
               fill=variable,  
               color=variable  
    )) +
    stat_summary(geom="area", fun.y = sum,alpha=0.5  )+
    stat_summary(geom="line", fun.y = sum)+
    stat_summary(geom="point", fun.y = sum)+
    # geom_area()+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position="bottom") +
    annotate("text",
             x=8,
             y=2400,
             label="Designed by Iranian COVID19 Research Group",
             alpha= 0.1)+
    scale_fill_manual(values = c("red","black", "green"))+
    scale_color_manual(values = c("red","black", "green"))+
    labs(fill="",color="",y="Count",x="Date"))


ggsave("Inc.Iran.jpg", Inc.Iran, width = 5, height = 5)



######## Iran daily incidence -----
(Inc.Iran.daily <-   reshape2::melt(Iran,measure.vars=c(10:12) ) %>% 
    ggplot(aes(x=date, 
               y=value,
               group=variable ,
               fill=variable,  
               color=variable  
    )) +
    stat_summary(geom="area", fun.y = sum,alpha=0.5  )+
    stat_summary(geom="line", fun.y = sum)+
    stat_summary(geom="point", fun.y = sum)+
    # geom_area()+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position="bottom") +
    annotate("text",
             x=8,
             y=650,
             label="Designed by Iranian COVID19 Research Group",
             alpha= 0.1)+
    scale_fill_manual(values = c("red","green", "black")
    )+
    scale_color_manual(values = c("red","green", "black")
    )+
    labs(fill="",color="",y="Count",x="Date"))


ggsave("Inc.Iran.daily.jpg", Inc.Iran.daily, width = 5, height = 5)

########### Iran CFR trend ------
 
Iran.CFR.trend=ggplot(Iran[-c(1:3),], aes(x=date, 
           y=Deaths/Confirmed*100, 
           group=1)) +
  stat_summary(geom="line", fun.y = sum,alpha=0.5 ,color="red",size=1 )+
  stat_summary(geom="point", fun.y = sum,alpha=0.5,color="red"  )+
  labs(fill="",color="",y="Daily CFR",x="Date") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_line()) +
  annotate("text",
           x=8,
           y=15,
           label="Designed by Iranian COVID19 Research Group",
           alpha= 0.1)+
  scale_y_continuous(breaks=seq(0,25,5))
ggsave("Iran.CFR.trend.jpg", Iran.CFR.trend, width = 5, height = 5)
