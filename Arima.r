#First section starts here - run from here 

library(quantmod)
library(forecast)
library(xts)

#Select ticker, start date, end date (remember that first 253 obs. will not be included in predictions table)

Ticker<- "AAPL" #Yahoofinance ticker, i.e. "SPY"
Start_date<-  "2019-01-01" #date in format "YYYY-MM-DD"
End_date<-  "2021-09-24" #date in format "YYYY-MM-DD"
  

data<-getSymbols(Ticker,from=Start_date,to=End_date,periodicity="daily",auto.assign = FALSE)
cl<- paste(c(Ticker,"Close"),collapse = ".")
hg<- paste(c(Ticker,"High"),collapse = ".")
returns<- log(data[,cl])-log(lag(data[,cl]))[2:nrow(data)]
High_close<- log(data[,hg])-log(lag(data[,cl]))[254:nrow(data)]
colnames(returns)<- paste(c(Ticker,"returns"),collapse = "_")
k<-nrow(returns)-252
r<-matrix(rep(0,k*4),ncol = 4)
colnames(r)<- c("mean","upper.1","upper.2","aic")
m<-matrix(c(1,1,1,2,2,2,3,3,3,1,2,3,1,2,3,1,2,3,0,0,0,0,0,0,0,0,0),ncol = 3)

for (i in 1:k){
  for (j in 1:9) {
    
    ar_f<- tryCatch(arima(returns[i:(i+251)],order = c(m[j,1],0,m[j,2]),method = "ML"),error=function( err ) FALSE,warning=function( err ) FALSE)
    
    if(!is.logical(ar_f)){
      
      m[j,3]<- ar_f$aic
      
    } else{
      
      m[j,3]<-ar_f
      
    }
  }
  
  if(sum(m[,3]==FALSE)<9){
    a<- m[which(m[,3]==min(m[,3])),1]
    ma<-m[which(m[,3]==min(m[,3])),2]
    a_f<- arima(returns[i:(i+251)],order = c(a,0,ma),method = "ML")
    r[i,1]<- forecast(a_f,h=1,level = c(20,30))$mean[1]
    r[i,2]<- forecast(a_f,h=1,level = c(20,30))$upper[1]
    r[i,3]<- forecast(a_f,h=1,level = c(20,30))$upper[2]
    r[i,4]<- m[which(m[,3]==min(m[,3])),3]
  }else{
    r[i,1]<- returns[(i+252)]
    r[i,2]<- returns[(i+252)]
    r[i,3]<- returns[(i+252)]
    r[i,4]<- returns[(i+252)]
  }
}

r<- cbind(r,returns[253:nrow(returns)])
benchmark<-cumprod(1+r[,5])
ar_mean<- ifelse(r[,1]<0,-r[,5],r[,5])
ar_mean<- cumprod(1+ar_mean)
ar_0.2<- ifelse(r[,2]<0,-r[,5],r[,5])
ar_0.2<- cumprod(1+ar_0.2)
ar_0.3<- ifelse(r[,3]<0,-r[,5],r[,5])
ar_0.3<- cumprod(1+ar_0.3)
results<- cbind(benchmark,ar_mean,ar_0.2,ar_0.3)
plot.zoo(results,col = c("black","red","blue","green"),plot.type = "single",lwd = 2)
legend("topleft",c("benchmark","mean","0.2","0.3"),col = c("black","red","blue","green"),pch = 16)

#First section ends here - run until here. Check the output graph and select the column with best result from table r 
#(1 or 2 or 3 for mean, 0.2 or 0.3, respectively)  

#Second section starts here - run from here
b<- 1 #choose number (1 or 2 or 3 for mean, 0.2 or 0.3, respectively)

SLM<- cbind(r[,b],ifelse(r[,b]<0,-r[,5],r[,5]),High_close,ifelse(r[,b]<0,-r[,5],r[,5]),ifelse(r[,b]<0,-r[,5],r[,5]),ifelse(r[,b]<0,-r[,5],r[,5]),ifelse(r[,b]<0,-r[,5],r[,5]),ifelse(r[,b]<0,-r[,5],r[,5]))
colnames(SLM)<- c("prediction","org","High_close","SL_1","SL_2","SL_3","SL_4","SL_5")

plot(density(SLM[which(SLM[,1]<0 & SLM[,2]<0),2]))
mean(SLM[which(SLM[,1]<0 & SLM[,2]<0),2])

#Second section ends here - run until here. Check the output graph and mean of the loss. Select appropriate levels of stop loss
#according to this distribution. Remember to select levels lower than mean, otherwise this has no sense !!!

#Third section starts here - run from here

SL<- c(0.012,0.01,0.007,0.005,0.003) #Put 5 different values of stop loss into this vector

results_SL<-matrix(rep(0,nrow(SLM)*5),ncol = 5)
results_SL<- cbind(cumprod(1+SLM[,2]),results_SL)
colnames(results_SL)<- c("org",colnames(SLM)[4:8])

for (i in 1:5){
  SLM[which(SLM[,1]<0),(i+3)]<- ifelse(SLM[which(SLM[,1]<0),3]<SL[i],SLM[which(SLM[,1]<0),(i+3)],-SL[i])
  results_SL[,(i+1)]<- cumprod(1+SLM[,(i+3)])
}

plot.zoo(results_SL,col = c("black","red","blue","magenta","orange","green"),screens = "single",lwd = 2)
legend("topleft",c("org","SL_1","SL_2","SL_3","SL_4","SL_5"),col = c("black","red","blue","magenta","orange","green"),pch = 16)

#Third section ends here - run until here. Check the final graph and decide which level of stop loss to use (if any).
#Or try to change values in the SL vector and repeat sec. 3.

#Fourth section starts here - run from here

SL_top<- "SL_5" #Select the column from results table with the best result (i.e. "org" or "SL_1", etc.)
results_SL<- as.data.frame(results_SL)
metrics<- c("mean","sd","VaR_0.95","total_return","p.d.")
values<- c(mean(SLM[,SL_top]),sd(SLM[,SL_top]),quantile(SLM[,SL_top],0.05),(results_SL[nrow(results_SL),SL_top]-results_SL[1,SL_top])/results_SL[1,SL_top],(results_SL[nrow(results_SL),SL_top]/results_SL[1,SL_top])^(1/nrow(results_SL))-1)
final<- rbind(metrics,values)
print(final)
plot.zoo(SLM[,SL_top],ylab = paste(c(SL_top,"returns"),collapse = " "))
plot(density(SLM[,SL_top]),xlab = paste(c(SL_top,"density"),collapse = " "))

#Fourth section ends here - run until here. Inspect the results. Notice there were created also 2 graphs.