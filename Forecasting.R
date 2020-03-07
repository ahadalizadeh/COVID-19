### Data Manipulation---------
source("https://raw.githubusercontent.com/ahadalizadeh/COVID-19/master/Download%20data.R")
source("https://raw.githubusercontent.com/ahadalizadeh/utility_fun/master/utility_fun.R")

diff.fun <- function(data){
  if(!is.atomic(data)) stop("The input data must be atomic!")
 list(diff(c(0,data)))
}

# COVID19<- COVID19.main
names(COVID19)[2] <- "Country.Region"

COVID19.Cumulative <- COVID19  %>% 
  group_by(date,Country.Region ) %>% 
  summarise(
         Confirmed.Cumulative= sum(Confirmed, na.rm = TRUE),
         Deaths.Cumulative= sum(Deaths, na.rm = TRUE),
         Recovered.Cumulative= sum(Recovered, na.rm = TRUE),
         day= mean(day, na.rm = TRUE)
         ) %>% 
   as.data.frame() 
COVID19.temp<- COVID19.Cumulative  %>% 
  group_by(Country.Region ) %>% 
  summarise(
         Confirmed.Daily= diff.fun(Confirmed.Cumulative),
         Deaths.Daily= diff.fun(Deaths.Cumulative),
         Recovered.Daily= diff.fun(Recovered.Cumulative),
         date = list(date),
         day=list(day) 
   )

for (i in 1:dim(COVID19.temp)[1]){
  d = COVID19.temp[i,]
  n= length(d$Confirmed.Daily[[1]])
 d.temp2 = data.frame(Country.Region = rep(d$Country.Region,n),
             Confirmed.Daily = d$Confirmed.Daily[[1]],
             Deaths.Daily = d$Deaths.Daily[[1]],
             Recovered.Daily = d$Recovered.Daily[[1]],
             day = d$day[[1]],
             date= d$date[[1]])
  

  if( i == 1) d.main <- d.temp2
  if( i != 1) d.main <- rbind(d.main, d.temp2) 
}
d.main <- d.main[with(d.main,order(Country.Region,day)),]

 
COVID19 <-merge(COVID19.Cumulative,d.main, 
                by.x = c("Country.Region","day","date"), 
                by.y =  c("Country.Region","day","date"))
COVID19 <- COVID19[with(COVID19,order(Country.Region,day)),]

