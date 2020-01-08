rm(list=ls())


data <- read.csv("/home/archit/EcoProject/Econometrics---Final-Project/tmp.csv")
data2 <- read.csv("/home/archit/EcoProject/Econometrics---Final-Project/tmp2.csv")
#plot(data$actual,data$pred, main="Pred against Actual")
#abline(lm(data$pred ~ data$actual))

data[16,]
data2[16,]
c = ggplot() 
#ggplot(data = data, aes(y=actual, x=pred)) 
b <- c + 
  geom_point(data = data, aes(y=actual, x=pred),color='gray', size = 0) + geom_point(aes(y=data[16,]$actual, x=data[16,]$pred), colour="red",size = 2)+
  geom_smooth(data = data, aes(y=actual, x=pred),method=lm, color='#2C3E50') 

j = b + geom_point(data = data2, aes(y=actual, x=pred),color='gray', size = 0) + geom_point(aes(y=data2[16,]$actual, x=data2[16,]$pred), colour="blue",size = 2)+
  geom_smooth(data = data2, aes(y=actual, x=pred),method=lm, color='#2C3E50')
j
