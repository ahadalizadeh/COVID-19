
### Data Manipulation---------
source("https://raw.githubusercontent.com/ahadalizadeh/COVID-19/master/Download%20data.R")
source("https://raw.githubusercontent.com/ahadalizadeh/utility_fun/master/utility_fun.R")

diff.fun <- function(data){
  if(!is.atomic(data)) stop("The input data must be atomic!")
  list(diff(c(0,data)))
}
names(COVID19)[2] <- "day2"
COVID19.Cumulative2 <- COVID19  %>% 
  group_by(date) %>% 
  summarise(
    Confirmed.Cumulative2= sum(Confirmed, na.rm = TRUE),
    Deaths.Cumulative2= sum(Deaths, na.rm = TRUE),
    Recovered.Cumulative2= sum(Recovered, na.rm = TRUE),
    day2= mean(day, na.rm = TRUE)
  ) %>% 
  as.data.frame() 
COVID19.temp2<- COVID19.Cumulative2  %>% 
  group_by(date) %>% 
  summarise(
    Confirmed.Daily2= diff.fun(Confirmed.Cumulative2),
    Deaths.Daily2= diff.fun(Deaths.Cumulative2),
    Recovered.Daily2= diff.fun(Recovered.Cumulative2),
    day2=list(day2) 
  )

for (i in 1:dim(COVID19.temp2)[1]){
  d2 = COVID19.temp2[i,]
  n2= length(d2$Confirmed.Daily2[[1]])
  d.temp22 = data.frame(Confirmed.Daily2 = d2$Confirmed.Daily2[[1]],
                        Deaths.Daily2 = d2$Deaths.Daily2[[1]],
                        Recovered.Daily2 = d2$Recovered.Daily2[[1]],
                        day2 = d2$day2[[1]])
  
  
  if( i == 1) d.main2 <- d.temp22
  if( i != 1) d.main2 <- rbind(d.main2, d.temp22) 
}
d.main2 <- d.main2[with(d.main2,order(day2)),]


COVID192 <-merge(COVID19.Cumulative2,d.main2, 
                 by.x = c("day2"), 
                 by.y =  c("day2"))
COVID192 <- COVID192[with(COVID192,order(day2)),]

