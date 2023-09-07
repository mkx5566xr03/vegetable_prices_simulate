library(tidyverse)
rm(ks.pnorm)
#冬季12月到3月：6、7、14-17、23、24、30-33、40-43、49、50、60-62、69、70
#春秋季4、5月和10、11月：4、5、8、12、13、21、22、28、29、34、38、39、47、48、53、54、58、59、63、67、68
#夏季6到9月：1-3、9-11、18-20、25-27、35-37、44-46、51、52、55-57、64-66
winter = c(6,7,14,15,16,17,23,24,30,31,32,33,40,41,42,43,49,50,60,61,62,69,70)
spraut = c(4,5,8,12,13,21,22,28,29,34,38,39,47,48,53,54,58,59,63,67,68)
summer = c(1,2,3,9,10,11,18,19,20,25,26,27,35,36,37,44,45,46,51,52,55,56,57,64,65,66)
par(mfrow = c(3,3)) 
###模擬9組梨山平均溫度樣本
sim_i = c()
sim = matrix(nrow=70,ncol=1)
#先執行1次並存入sim
sim_i[winter] = round(runif(23,7,13),1)
sim_i[spraut] = round(runif(21,11,16.3),1)
sim_i[summer] = round(runif(26,14.6,17.6),1)
sim = as.matrix(sim_i)
#跑剩餘的8次並存入sim
for(i in 1:8){
  sim_i[winter] = round(runif(23,7,13),1)
  sim_i[spraut] = round(runif(21,11,16.3),1)
  sim_i[summer] = round(runif(26,14.6,17.6),1)
  sim = cbind(sim,as.matrix(sim_i))
}
#將9組模擬溫度的直方圖配適出現，好觀察可能的機率分配為何
for(j in 1:9){
   hist(sim[,j],main = "temperature's distribution",xlab = 'temperature')
}

#要檢測這些組是否服從隨機漫步模型，使用adf test H0:資料具非隨機性，具單位根
library("urca")
library("tseries")
adf.test(sim[,j])$p.value

adf.pvalue = c()
for(j in 1:9){
  adf.pvalue = c(adf.pvalue,adf.test(sim[,j])$p.value)
}
data.frame(adf.pvalue)
###

###模擬9組梨山平均降雨量樣本
sim_i = c()
sim = matrix(nrow=70,ncol=1)
#先執行1次並存入sim
sim_i[winter] = round(runif(23,5,552),1)
sim_i[spraut] = round(runif(21,37,1027.5),1)
sim_i[summer] = round(runif(26,3,676),1)
sim = as.matrix(sim_i)
#跑剩餘的8次並存入sim
for(i in 1:8){
  sim_i[winter] = round(runif(23,5,552),1)
  sim_i[spraut] = round(runif(21,37,1027.5),1)
  sim_i[summer] = round(runif(26,3,676),1)
  sim = cbind(sim,as.matrix(sim_i))
}
#將9組模擬降雨量的直方圖配適，觀察可能的機率分配
install.packages("MASS")
library(MASS)
for(j in 1:9){
  hist(sim[,j],main = "precp's distribution",xlab = 'precp')
}
#模擬gamma的shape和rate：分別取2及0.05(1/20)
est_para = c()
for(j in 1:9){
  est_para = c(est_para,fitdistr(sim[,j],"gamma")$estimate)
}
est_para

ks.pgamma = c()
for(j in 1:9){
  ks.pgamma = c(ks.pgamma,ks.test(sim[,j],"pgamma",2,1/20)$p.value)
}
#檢查normal
ks.pnorm = c()
for(j in 1:9){
  ks.pnorm = c(ks.pnorm,ks.test(sim[,j],"pnorm",)$p.value)
  
}
#檢查uniform
ks.punif = c()
for(j in 1:9){
  ks.punif = c(ks.punif,ks.test(sim[,j],"punif",0,600)$p.value)
}
data.frame(ks.punif)
#box plot
for(j in 1:9){
  boxplot(sim[,j])
}
###

###模擬9組梨山平均照光量樣本
sim_i = c()
sim = matrix(nrow=70,ncol=1)
#先執行1次並存入sim
sim_i[winter] = round(runif(23,100.6,225.1),1)
sim_i[spraut] = round(runif(21,143.8,256.1),1)
sim_i[summer] = round(runif(26,112,222.1),1)
sim = as.matrix(sim_i)
#跑剩餘的8次並存入sim
for(i in 1:8){
  sim_i[winter] = round(runif(23,100.6,225.1),1)
  sim_i[spraut] = round(runif(21,143.8,256.1),1)
  sim_i[summer] = round(runif(26,112,222.1),1)
  sim = cbind(sim,as.matrix(sim_i))
}
#將9組模擬照光量的直方圖配適，觀察可能的機率分配
for(j in 1:9){
  hist(sim[,j],main = "sunshine's distribution",xlab = 'sunshine')
}
#檢查是否服從常態
ks.pnorm = c()
for(j in 1:9){
  ks.pnorm = c(ks.pnorm,ks.test(sim[,j],"pnorm")$p.value)
}
#檢查uniform
ks.punif = c()
for(j in 1:9){
  ks.punif = c(ks.punif,ks.test(sim[,j],"punif",100,250)$p.value)
}
data.frame(ks.punif)
####測100次
sim_i = c()
sim = matrix(nrow=70,ncol=1)
#先執行1次並存入sim
sim_i[winter] = round(runif(23,100.6,225.1),1)
sim_i[spraut] = round(runif(21,143.8,256.1),1)
sim_i[summer] = round(runif(26,112,222.1),1)
sim = as.matrix(sim_i)
#跑剩餘的99次並存入sim
for(i in 1:99){
  sim_i[winter] = round(runif(23,100.6,225.1),1)
  sim_i[spraut] = round(runif(21,143.8,256.1),1)
  sim_i[summer] = round(runif(26,112,222.1),1)
  sim = cbind(sim,as.matrix(sim_i))
}
#檢查uniform
TRUE_count = 0
p100 = c()
ks.punif = c()
for(j in 1:100){
  ks.punif = c(ks.punif,ks.test(sim[,j],"punif",100,250)$p.value)
  p100[j] = ifelse(ks.punif[j] < 0.05 ,FALSE,TRUE)
  if(p100[j] == TRUE){
    TRUE_count = TRUE_count + 1
  }
}

####測1000次
sim_i = c()
sim = matrix(nrow=70,ncol=1)
#先執行1次並存入sim
sim_i[winter] = round(runif(23,100.6,225.1),1)
sim_i[spraut] = round(runif(21,143.8,256.1),1)
sim_i[summer] = round(runif(26,112,222.1),1)
sim = as.matrix(sim_i)
#跑剩餘的999次並存入sim
for(i in 1:999){
  sim_i[winter] = round(runif(23,100.6,225.1),1)
  sim_i[spraut] = round(runif(21,143.8,256.1),1)
  sim_i[summer] = round(runif(26,112,222.1),1)
  sim = cbind(sim,as.matrix(sim_i))
}
#檢查uniform
TRUE_count = 0
p1000 = c()
ks.punif = c()
for(j in 1:1000){
  ks.punif = c(ks.punif,ks.test(sim[,j],"punif",100,250)$p.value)
  p1000[j] = ifelse(ks.punif[j] < 0.05 ,FALSE,TRUE)
  if(p1000[j] == TRUE){
    TRUE_count = TRUE_count + 1
  }
}
####測10000次
sim_i = c()
sim = matrix(nrow=70,ncol=1)
#先執行1次並存入sim
sim_i[winter] = round(runif(23,100.6,225.1),1)
sim_i[spraut] = round(runif(21,143.8,256.1),1)
sim_i[summer] = round(runif(26,112,222.1),1)
sim = as.matrix(sim_i)
#跑剩餘的9999次並存入sim
for(i in 1:9999){
  sim_i[winter] = round(runif(23,100.6,225.1),1)
  sim_i[spraut] = round(runif(21,143.8,256.1),1)
  sim_i[summer] = round(runif(26,112,222.1),1)
  sim = cbind(sim,as.matrix(sim_i))
}
#檢查uniform
TRUE_count = 0
p10000 = c()
ks.punif = c()
for(j in 1:10000){
  ks.punif = c(ks.punif,ks.test(sim[,j],"punif",100,250)$p.value)
  p10000[j] = ifelse(ks.punif[j] < 0.05 ,FALSE,TRUE)
  if(p10000[j] == TRUE){
    TRUE_count = TRUE_count + 1
  }
}
####
###
rm(apple_data)
###實驗組
winter = c(6,7,14,15,16,17,23,24,30,31,32,33,40,41,42,43,49,50,60,61,62,69,70)
spraut = c(4,5,8,12,13,21,22,28,29,34,38,39,47,48,53,54,58,59,63,67,68)
summer = c(1,2,3,9,10,11,18,19,20,25,26,27,35,36,37,44,45,46,51,52,55,56,57,64,65,66)
##模擬溫度
temp_temperature = c()
temp_temperature[winter] = round(runif(23,7,13),1)
temp_temperature[spraut] = round(runif(21,11,16.3),1)
temp_temperature[summer] = round(runif(26,14.6,17.6),1)
temperature_sim = as.matrix(temp_temperature)
##模擬降雨量
temp_precp = c()
temp_precp[winter] = round(runif(23,5,552),1)
temp_precp[spraut] = round(runif(21,37,1027.5),1)
temp_precp[summer] = round(runif(26,3,676),1)
precp_sim = as.matrix(temp_precp)
##模擬光照時數
temp_sunshine = c()
temp_sunshine[winter] = round(runif(23,100.6,225.1),1)
temp_sunshine[spraut] = round(runif(21,143.8,256.1),1)
temp_sunshine[summer] = round(runif(26,112,222.1),1)
sunshine_sim = as.matrix(temp_sunshine)
##合併到實驗組
apple_experiment = cbind(apple_data,temperature_sim,precp_sim,sunshine_sim)
####
rm(apple_data)
#####
###匯出實驗組做testing data
install.packages("openxlsx")
library(openxlsx)
write.xlsx(exp_testing, file = "C:/Users/howger/Desktop/碩一下/統計模擬與計算/期末報告/data/exp_testing.xlsx", rowNames = FALSE)
###配適複迴歸分析且考慮交互作用
a = lm(price ~ temperature + precp + sunshine + temperature * precp , data = true_training)
summary(a)
b = lm(price ~ temperature + precp + sunshine + temperature * sunshine  , data = true_training)
summary(b)
c = lm(price ~ temperature + precp + sunshine + precp * sunshine  , data = true_training)
summary(c)
regression = lm(price ~ temperature + precp + sunshine  , data = true_training)
summary(regression)
#無任何交互作用
###模擬蘋果價格
price_sim = c()
for(k in 1:47){
  price_sim[k] = 97.936860 - 3.408602 * exp_testing$temperature_sim[k] + 0.001368 * exp_testing$precp_sim[k] + 0.025361 * exp_testing$sunshine_sim[k] 
}
exp_testing = cbind(exp_testing,price_sim)
#exp_testing = exp_testing[,-6]
###比較基本資料
#實驗組
cbind(mean(price_sim),median(price_sim),var(price_sim),range(price_sim))
#對照組
temp = true_testing$price
cbind(mean(temp),median(temp),var(temp),range(temp))
#兩組盒鬚圖
boxplot(exp_testing$price_sim,temp,names = c("實驗組","對照組"))
#畫兩組的走勢圖
library(ggplot2)


ggplot(ggplot, aes(x = month_date)) +
  geom_line(aes(y = price , color = "true",group = 1)) +
  geom_line(aes(y = price_sim , color = "simulate",group = 1))+
  labs(x = "日期", y = "模擬價格", title = "實驗組&對照組之走勢圖") +
  scale_color_manual(values = c("true" = "blue", "simulate" = "red")) +
  theme_minimal()

#檢定檢查兩組平均數、變異數是否有差異
t.test(true_testing$price,exp_testing$price_sim)
var.test(exp_testing$price_sim, true_testing$price, 
         alternative = "less")
###

###
##實驗組
#使用arima對模擬價格做預測
library(tseries)
library(forecast)
rm()
##對照組
#使用arima對模擬價格做預測
ts.data = ts(true_testing$price, frequency = 12)
#找出pdq
mod_wrong = auto.arima(ts.data,seasonal = TRUE,test = 'adf',ic = 'aic')
mod_wrong #未考慮到季節性
mod_true = auto.arima(ts.data,seasonal = TRUE,test = 'adf',ic = 'aic',D=1)
mod_true #d=1考慮到季節性
#or#
acf(ts.data,lag.max = 34) 
pacf(ts.data,lag.max = 34)
#acf圖 & pacf圖 搭配看ar(1),ar(2) or ma(1),ma(2) or arma(1,1)
adf.test(ts.data) #if p-value < 0.05 , then d = 1
# #
par(mfrow = c(1,2)) 

plot(forecast(mod_wrong))
plot(forecast(mod_true))
forecast_wrong = predict(mod_wrong , n.ahead=12)
forecast = predict(mod_true, n.ahead = 12)
###