data1<-read.csv("/home/archit/Data_Final2.csv")

count<-read.csv("/home/archit/countries.csv")
countries<-count$x

cou<-data1$Country[data1$Country %in% countries]
data <- data1

year <- 0


linear5<-function(Variable)
{
  print(mean(Variable))
  print(sd(Variable))
  print(min(Variable))
  print(max(Variable))
  #nrow(DomesticCredit)
}



DomesticCredit <- data$Domestic.credit.to.private.sector....of.GDP.[data$Country %in% countries]


GDP <- data$GDP.growth..annual...[data$Country %in% countries]
Export <- data$Exports.of.goods.and.services..annual...growth.[data$Country %in% countries]
Import <- data$Imports.of.goods.and.services..annual...growth.[data$Country %in% countries]
Tax <- data$Tax.revenue....of.GDP.[data$Country %in% countries]




linear5(DomesticCredit)
"
  [1] 82.54212
[1] 51.79284
[1] 0
[1] 206.6707
  "
linear5(Export)
"
  [1] 4.946827
[1] 8.897204
[1] -30.01776
[1] 85.61331
  "
linear5(GDP)
"
  [1] 3.314506
[1] 4.198047
[1] -33.10084
[1] 54.15777
  "
linear5(Import)
"
  [1] 5.457271
[1] 10.74485
[1] -50.05955
[1] 84.74949
  "
linear5(Tax)
"
  [1] 14.5922
[1] 8.905012
[1] 0
[1] 36.50029
  "

