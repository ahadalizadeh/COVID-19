#Data by Countries------------------------
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
COVID19 %>% view


#Global data-----------------------------------------
GCOVID192<- COVID19  %>% 
  group_by(day) %>% 
  summarise(
    Confirmed.Daily= sum(Confirmed.Cumulative, na.rm = TRUE),
    Deaths.Daily= sum(Deaths.Cumulative, na.rm = TRUE),
    Recovered.Daily= sum(Recovered.Cumulative, na.rm = TRUE),
    date = date[1] 
  )
GCOVID19=as.data.frame(cbind(Day=GCOVID192$day,Date=GCOVID192$date,Confirmed.Cumulative=GCOVID192$Confirmed.Daily,
                                    Deaths.Cumulative=GCOVID192$Deaths.Daily,Recovered.Cumulative=GCOVID192$Recovered.Daily,Confirmed.Daily=diff(c(0,COVID19.global$Confirmed.Daily)),
                                    Deaths.Daily=diff(c(0,GCOVID192$Deaths.Daily)),
                                    Recovered.Daily=diff(c(0,GCOVID192$Recovered.Daily))))
GCOVID19  %>% View()


# 10 Countries Daily Data----------------------------
attach(COVID19)
IranCOVID19=COVID19[Country.Region=="Iran",]
ChinaCOVID19=COVID19[Country.Region=="China",]
ItalyCOVID19=COVID19[Country.Region=="Italy",]
SpainCOVID19=COVID19[Country.Region=="Spain",]
GermanyCOVID19=COVID19[Country.Region=="Germany",]
USACOVID19=COVID19[Country.Region=="US",]
SKnCOVID19=COVID19[Country.Region=="South Korea",]

#Models------------------------
#TSGLM
library(forecast)
library(tscount)
library(gtools)
library(glarma)

#The best previous number of obs
GD=ts(GCOVID19$Deaths.Daily,frequency=dim(GCOVID19)[1],start=GCOVID19$Day)
plot.ts(GD)
AI=matrix(0,40,40)
for(i in 1:40){
  for (j in 1:40) {
      GDm=tsglm(GD, link="identity", model=list(past_obs=i, past_mean=j), xreg=NULL, distr="nbinom")
      AI[i,j]=(summary(GDm))$AIC
  }}
which(AI == min(AI[AI>650]), arr.ind = TRUE)
#past_obs=2, past_mean=19) for log link function
#past_obs=1, past_mean=1) for identity link function


#The best link function
m1=tsglm(GD, link="log", model=list(past_obs=2, past_mean=19), xreg=NULL, distr="nbinom")
m2=tsglm(GD, link="identity", model=list(past_obs=1, past_mean=1), xreg=NULL, distr="nbinom")
AIC(m1)
AIC(m2)
plot.ts(GD)
par(new=TRUE)
plot.ts(m1$fitted.values,col="red")
par(new=TRUE)
plot.ts(m2$fitted.values,col="blue")
cbind(GD,round(m1$fitted.values),round(m2$fitted.values))
dist(rbind(GD,round(m1$fitted.values)), method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
dist(rbind(GD,round(m2$fitted.values)), method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
pre1=predict(m1, n.ahead=30)
pre2=predict(m2, n.ahead=60)
plot.ts(pre1$pred)
plot.ts(pre2$pred,Time=1:60)

#------------------------WMC-------------------------------------------------
mydata=scale(GCOVID19$Deaths.Daily) # standardize variables
death=GCOVID19$Deaths.Daily
wss=(nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
fit <- kmeans(mydata, 7,nstart = 100,iter.max = 150000);table(fit$cluster)


aggregate(death,mydata,by=list(fit$cluster),FUN=summary)
mydata <- data.frame(GCOVID19$Deaths.Daily,mydata, fit$cluster)
CL=fit$cluster
table (CL)

CLrename=CL
library(plyr)
#revalue(CLrename, c("1"="1-26","2"="38-55","3"="193-207","4"="85-115","5"="125-160","6"="58-79","7"="245-266"))

n=length(CL)
I=CL
m=rep(0,n-1);d=rep(0,n)
for(i in 1:n-1){m[i]=(I[i]-mean(I))*(I[i+1]-mean(I))}
for(j in 1:n){d[j]=(I[j]-mean(I))^2}
r1=sum(m)/sum(d)
m2=rep(0,n-2);d2=rep(0,n)
for(k in 1:n-2){m2[k]=(I[k]-mean(I))*(I[k+2]-mean(I))}
for(j in 1:n){d2[j]=(I[j]-mean(I))^2}
r2=abs(sum(m2)/sum(d2))
m3=rep(0,n-3);d3=rep(0,n)
for(k in 1:n-3){  m3[k]=(I[k]-mean(I))*(I[k+3]-mean(I))}
for(j in 1:n){d3[j]=(I[j]-mean(I))^2}
r3=abs(sum(m3)/sum(d3))
m4=rep(0,n-4);d4=rep(0,n)
for(k in 1:n-4){m4[k]=(I[k]-mean(I))*(I[k+4]-mean(I))}
for(j in 1:n){  d4[j]=(I[j]-mean(I))^2}
r4=abs(sum(m4)/sum(d4))
m5=rep(0,n-5);d5=rep(0,n)
for(k in 1:n-5){m5[k]=(I[k]-mean(I))*(I[k+5]-mean(I))}
for(j in 1:n){  d5[j]=(I[j]-mean(I))^2}
r5=abs(sum(m5)/sum(d5))
m6=rep(0,n-6);d6=rep(0,n)
for(k in 1:n-6){m6[k]=(I[k]-mean(I))*(I[k+6]-mean(I))}
for(j in 1:n){  d6[j]=(I[j]-mean(I))^2}
r6=abs(sum(m6)/sum(d6))
w1=r1/(r1+r2+r3+r4+r5+r6+r7);w2=r2/(r1+r2+r3+r4+r5+r6+r7);w3=r3/(r1+r2+r3+r4+r5+r6+r7);w4=r4/(r1+r2+r3+r4+r5+r6+r7)
w5=r5/(r1+r2+r3+r4+r5+r6+r7);w6=r6/(r1+r2+r3+r4+r5+r6+r7);w7=r7/(r1+r2+r3+r4+r5+r6+r7)
w=c(w1,w2,w3,w4,w5,w6,w7)
CL=as.factor(CL)
#========one step=============================
p1=matrix(0,7,7)
for(i in 1:n){for(j in 1:7){for(k in 1:7){if(CL[i]==j & CL[i+1]==k) {p1[j,k]=p1[j,k]+1}}}}
for(h in 1:7){p1[h,]=p1[h,]/sum(p1[h,])}
p1
#========Two Steps===================================
p2=matrix(0,7,7)
for(i in 1:n){for(j in 1:7){for(k in 1:7){for(l in 1:7){if(CL[i]==j & CL[i+1]==k & CL[i+2]==l) {p2[j,l]=p2[j,l]+1}}}}}
for(h in 1:7){p2[h,]=p2[h,]/sum(p2[h,])}
p2
#=====Three Steps================================
p3=matrix(0,7,7)
for(i in 1:n){
  for(j in 1:7){
    for(k in 1:7){
      for(l in 1:7){
        for(z in 1:7){
          if(CL[i]==j & CL[i+1]==k & CL[i+2]==l & CL[i+3]==z) {p3[j,z]=p3[j,z]+1}
        }
      }
    }
  }
}
for(h in 1:7){
  p3[h,]=p3[h,]/sum(p3[h,])
}
p3
#=====Four Steps================================
p4=matrix(0,7,7)
for(i in 1:n){
  for(j in 1:7){
    for(k in 1:7){
      for(l in 1:7){
        for(z in 1:7){
          for(q in 1:7){
            if(CL[i]==j & CL[i+1]==k & CL[i+2]==l & CL[i+3]==z & CL[i+4]==q) {p4[j,q]=p4[j,q]+1}
          }
        }
      }
    }
  }
}
for(h in 1:7){
  p4[h,]=p4[h,]/sum(p4[h,])
}
p4
#=====Five Steps================================
p5=matrix(0,7,7)
for(i in 1:n){
  for(j in 1:7){
    for(k in 1:7){
      for(l in 1:7){
        for(z in 1:7){
          for(q in 1:7){
            for(y in 1:7){
              if(CL[i]==j & CL[i+1]==k & CL[i+2]==l & CL[i+3]==z & CL[i+4]==q & CL[i+5]==y) {p5[j,y]=p5[j,y]+1}
            }
          }
        }
      }
    }
  }
}
for(h in 1:7){
  p5[h,]=p5[h,]/sum(p5[h,])
}
p5
#=====Six Steps================================
p6=matrix(0,7,7)
for(i in 1:n){
  for(j in 1:7){
    for(k in 1:7){
      for(l in 1:7){
        for(z in 1:7){
          for(q in 1:7){
            for(y in 1:7){
              for(y6 in 1:7){
                if(CL[i]==j & CL[i+1]==k & CL[i+2]==l & CL[i+3]==z & CL[i+4]==q & CL[i+5]==y & CL[i+6]==y6) {p6[j,y]=p6[j,y]+1}
              }
            }
          }
        }
      }
    }
  }
}
for(h in 1:7){
  p6[h,]=p6[h,]/sum(p6[h,])
}
p6
#=====Seven Steps================================
p7=matrix(0,7,7)
for(i in 1:n){
  for(j in 1:7){
    for(k in 1:7){
      for(l in 1:7){
        for(z in 1:7){
          for(q in 1:7){
            for(y in 1:7){
              for(y6 in 1:7){
                for(y7 in 1:7){
                  if(CL[i]==j & CL[i+1]==k & CL[i+2]==l & CL[i+3]==z & CL[i+4]==q & CL[i+5]==y & CL[i+6]==y6 & CL[i+7]==y7) {p7[j,y]=p7[j,y]+1}
                }
              }
            }
          }
        }
      }
    }
  }
}
for(h in 1:7){
  p7[h,]=p7[h,]/sum(p7[h,])
}
p7



#xx=cbind(CL,x=1:49)
#y=x[CL==7]+7
#yy=y[y<50]
#CL[x=yy]
cc=c(0,0,0,0,0,0,1,
     0,0,0,0,0,1,0,
     0.16,0,0,0,0.5,0.34,0,
     0,0,0.4,0.1,0.3,0.2,0,
     0,0.1,0,0.1,0.3,0.1,0.4,
     0.22,0,0,0.11,0.56,0.11,0,
     0,0.2,0.2,0.2,0.2,0.2,0)
p7=matrix(cc,byrow=TRUE,7)


#=======Prediction===============================
n=49
prob=matrix(0,n,7)
a=matrix(0,7,7)
for(i in 8:n){
  for( j1 in 1:7){
    for( j2 in 1:7){
      for( j3 in 1:7){
        for( j4 in 1:7){
          for( j5 in 1:7){
            for( j6 in 1:7){
              if(CL[i-1]==1 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[1,],p2[1,],p3[1,],p4[1,],p5[1,],p6[1,],p7[1,]);  prob[i,]=t(w)%*%a}
              
              else if(CL[i-1]==2 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[2,],p2[2,],p3[2,],p4[2,],p5[2,],p6[2,],p7[2,]);  prob[i,]=t(w)%*%a}
              
              else if(CL[i-1]==3 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[3,],p2[3,],p3[3,],p4[3,],p5[3,],p6[3,],p7[3,]);  prob[i,]=t(w)%*%a}
              
              else if(CL[i-1]==4 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[4,],p2[4,],p3[4,],p4[4,],p5[4,],p6[4,],p7[4,]);  prob[i,]=t(w)%*%a}
              
              else if(CL[i-1]==5 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[5,],p2[5,],p3[5,],p4[5,],p5[5,],p6[5,],p7[5,]);  prob[i,]=t(w)%*%a}
              
              else if(CL[i-1]==6 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[6,],p2[6,],p3[6,],p4[6,],p5[6,],p6[6,],p7[6,]);  prob[i,]=t(w)%*%a}
              
              else if(CL[i-1]==7 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[7,],p2[7,],p3[7,],p4[7,],p5[7,],p6[7,],p7[7,]);  prob[i,]=t(w)%*%a}
            }
          }
        }
      }
    }
  }
}
result=cbind(prob,CL)
pre=rep(0,n)
for( i in 8:n){
  for(j in 1:7){
    if(max(prob[i,])== prob[i,j]) {pre[i]=j}
  }
}
pre=pre[pre!=0]
library(gmodels)
CrossTable(pre, CL[8:n])
cbind(pre,CL[8:49])


#Forecasting--------------------------------------------
#DD=COVID19.global2$Deaths.Daily
#DD4=DD[46:49]
cLnew=CL[43:49]
prob1=matrix(0,70,7)
a=matrix(0,7,7)
for(i in 8:70){
  for( j1 in 1:7){
    for( j2 in 1:7){
      for( j3 in 1:7){
        for( j4 in 1:7){
          for( j5 in 1:7){
            for( j6 in 1:7){
              if(CL[i-1]==1 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[1,],p2[1,],p3[1,],p4[1,],p5[1,],p6[1,],p7[1,]);  prob1[i,]=t(w)%*%a}
              
              else if(CL[i-1]==2 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[2,],p2[2,],p3[2,],p4[2,],p5[2,],p6[2,],p7[2,]);  prob1[i,]=t(w)%*%a}
              
              else if(CL[i-1]==3 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[3,],p2[3,],p3[3,],p4[3,],p5[3,],p6[3,],p7[3,]);  prob1[i,]=t(w)%*%a}
              
              else if(CL[i-1]==4 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[4,],p2[4,],p3[4,],p4[4,],p5[4,],p6[4,],p7[4,]);  prob1[i,]=t(w)%*%a}
              
              else if(CL[i-1]==5 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[5,],p2[5,],p3[5,],p4[5,],p5[5,],p6[5,],p7[5,]);  prob1[i,]=t(w)%*%a}
              
              
              else if(CL[i-1]==6 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[6,],p2[6,],p3[6,],p4[6,],p5[6,],p6[6,],p7[6,]);  prob1[i,]=t(w)%*%a}
              
              else if(CL[i-1]==7 & CL[i-2]==j1 & CL[i-3]==j2 & CL[i-4]==j3 & CL[i-5]==j4 & CL[i-6]==j5 & CL[i-7]==j6){
                a=rbind(p1[7,],p2[7,],p3[7,],p4[7,],p5[7,],p6[7,],p7[7,]);  prob1[i,]=t(w)%*%a}
            }}}}}}
  pre1=0
  for(u in 1:7){if(max(prob1[i,])== prob1[i,u]) {pre1=u}}
  cLnew=c(cLnew,pre1)
}
prob1
cLnew
CLnewRename=revalue(as.factor(cLnew), c("1"="1-26","2"="38-55","3"="193-207","4"="85-115","5"="125-160","6"="58-79","7"="245-266"))
CLnewRename


