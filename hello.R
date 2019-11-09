rm(list=ls())

direct_tax<-c(1.31,
              20.07,
              26.48,
              26.34,
              24.44,
              39.32,
              35.56,
              6.20,
              13.25,
              17.97,
              10.76,
              13.16,
              14.24,
              8.96,
              6.63)

gdp<-c(4.8,3.8,7.9,7.9,9.3,9.3,9.8,3.9,8.5,      10.3,      6.6,      5.5,      6.4,      7.4,8.2)

library(psychometric)

cor(direct_tax,gdp)
#0.5751463
CIr(cor(direct_tax,gdp),15,0.95)
#0.08914909 0.83994070

cor(direct_tax[1:11],gdp[1:11])
#0.6449207
CIr(cor(direct_tax[1:11],gdp[1:11]),11,0.95)
#0.07346861 0.89755643

cor(direct_tax[12:15],gdp[12:15])
# -0.9037216
CIr(cor(direct_tax[12:15],gdp[12:15]),n=4,0.95)
#-0.9979950  0.4364226



data1<-read.csv("/home/parasmehan123/Desktop/eco1_project/Data_Final2.csv")

count<-read.csv("/home/parasmehan123/Desktop/eco1_project/countries.csv")
countries<-count$x

cou<-data1$Country[data1$Country %in% countries]

linear1<-function(countries,data,year)
{

  # Linear regression without tax
  data2<-(data[data$Country %in% countries & data$T == year,])
  model=lm(GDP.growth..annual...~Domestic.credit.to.private.sector....of.GDP.+Exports.of.goods.and.services..annual...growth.+Imports.of.goods.and.services..annual...growth.+India_Dummy,data = data2)

  print(summary(model))
  print("Correlation b/w TAX and Y")
  print(cor(data2$Tax.revenue....of.GDP. ,data2$GDP.growth..annual...))
  print("Correlation b/w TAX and Other parms")
  print(cor(data2$Tax.revenue....of.GDP. ,data2$Exports.of.goods.and.services..annual...growth.))
  print(cor(data2$Tax.revenue....of.GDP. ,data2$Domestic.credit.to.private.sector....of.GDP.))
  print(cor(data2$Tax.revenue....of.GDP. ,data2$Imports.of.goods.and.services..annual...growth.))
  
}

linear2<-function(countries,data,year)
{
  # Linear regression with tax
  data2<-(data[data$Country %in% countries & data$T == year,])
  model=lm(GDP.growth..annual...~Domestic.credit.to.private.sector....of.GDP.+Exports.of.goods.and.services..annual...growth.+Imports.of.goods.and.services..annual...growth.+India_Dummy+Tax.revenue....of.GDP.,data = data2)
  x1<-as.matrix(data.frame(1,data2[data2$Country == "India",]$Domestic.credit.to.private.sector....of.GDP.,data2[data2$Country == "India",]$Exports.of.goods.and.services..annual...growth.,data2[data2$Country == "India",]$Imports.of.goods.and.services..annual...growth.,data2[data2$Country == "India",]$India_Dummy,data2[data2$Country == "India",]$Tax.revenue....of.GDP.))
  y1<-as.matrix(data.frame(model$coefficients))
  print(mean(x1 %*% y1))


  x2<-as.matrix(data.frame(1,data2[data2$Country != "India",]$Domestic.credit.to.private.sector....of.GDP.,data2[data2$Country == "India",]$Exports.of.goods.and.services..annual...growth.,data2[data2$Country == "India",]$Imports.of.goods.and.services..annual...growth.,data2[data2$Country == "India",]$India_Dummy,data2[data2$Country == "India",]$Tax.revenue....of.GDP.))
  y2<-as.matrix(data.frame(model$coefficients))
  print(mean(x2 %*% y2))

  print(summary(model))
}

linear3<-function(countries,data)
{
  data2<-(data[data$Country %in% countries,])
  model=lm(GDP.growth..annual...~Domestic.credit.to.private.sector....of.GDP.+Exports.of.goods.and.services..annual...growth.+Imports.of.goods.and.services..annual...growth.+Tax.revenue....of.GDP.+India_Dummy*T+India_Dummy+T+Domestic.credit.to.private.sector....of.GDP.*T+Exports.of.goods.and.services..annual...growth.*T+Imports.of.goods.and.services..annual...growth.*T+Tax.revenue....of.GDP.*T,data = data2)
  print(summary(model))
}

linear1(countries,data = data1,0)
linear1(countries,data = data1,1)

linear2(countries,data = data1,0)
linear2(countries,data = data1,1)

linear3(countries,data = data1)


data2<-(data1[data1$Country %in% countries & data1$T == 1,])
model=lm(GDP.growth..annual...~Domestic.credit.to.private.sector....of.GDP.+Exports.of.goods.and.services..annual...growth.+Imports.of.goods.and.services..annual...growth.+India_Dummy+Tax.revenue....of.GDP.,data = data2)

