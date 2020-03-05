source("https://raw.githubusercontent.com/ahadalizadeh/COVID-19/master/Download%20data.R")
# COVID19 <- covid19.data(last.day="03-04-2020") 

source("https://raw.githubusercontent.com/ahadalizadeh/utility_fun/master/utility_fun.R")
names(COVID19)

COVID19$Deaths.Recovered<- COVID19$Recovered+COVID19$Deaths
Iran = COVID19[COVID19$`Country/Region`=="Iran",] 
Iran$Confirmed.daily <- c(0,Iran$Confirmed)  %>%  diff  
Iran$Recovered.daily <- c(0,Iran$Recovered)  %>%  diff 
Iran$Deaths.daily <- c(0,Iran$Deaths)  %>%  diff  

Iran$Deaths/Iran$Confirmed  
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

