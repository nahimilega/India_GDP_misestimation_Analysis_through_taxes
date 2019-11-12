#Read the Data
india <- read.csv("/home/archit/EcoProject/Econometrics---Final-Project/data/mises.csv")
summary(india)
complete.cases(india)
x <- na.omit(india)
c1 <- cor(x$credit_GR, x$GDP_GR)
c2 <- cor(x$Imports_LCU_GR, x$GDP_GR)
c3 <- cor(x$Imports_GDP_GR, x$GDP_GR)
c4 <- cor(x$Exports_GDP_GR, x$GDP_GR)
c5 <- cor(x$Exports_LCU_GR, x$GDP_GR)
c6 <- cor(x$Electricity_GR, x$GDP_GR)
c7 <- cor(x$Commercial_GR, x$GDP_GR)
c8 <- cor(x$Two.Wheeler_GR, x$GDP_GR)
c9 <- cor(x$Foreign_GR, x$GDP_GR)
c10 <- cor(x$IIP_GR,x$GDP_GR)
c11 <- cor(x$Air_GR,x$GDP_GR)
c12 <- cor(x$Railways_GR,x$GDP_GR)
y1 <- ifelse(x$Year<=2011,1,0)
y1
y2 <- ifelse(x$Year>2011,1,0)
y2

c1 <- cor(x$credit_GR, x$GDP_GR)
c2 <- cor(x$Imports_LCU_GR, x$GDP_GR)
c3 <- cor(x$Imports_GDP_GR, x$GDP_GR)
c4 <- cor(x$Exports_GDP_GR, x$GDP_GR)
c5 <- cor(x$Exports_LCU_GR, x$GDP_GR)
c6 <- cor(x$Electricity_GR, x$GDP_GR)
c7 <- cor(x$Commercial_GR, x$GDP_GR)
c8 <- cor(x$Two.Wheeler_GR, x$GDP_GR)
c9 <- cor(x$Foreign_GR, x$GDP_GR)
c10 <- cor(x$IIP_GR,x$GDP_GR)
c11 <- cor(x$Air_GR,x$GDP_GR)
c12 <- cor(x$Railways_GR,x$GDP_GR)


w1 <- subset(x, y1 == 1)
w2 <- subset(x, y2 == 1)
#correlations 2001-11
c1 <- cor(w1$credit_GR, w1$GDP_GR)
c2 <- cor(w1$Imports_LCU_GR, w1$GDP_GR)
c3 <- cor(w1$Imports_GDP_GR, w1$GDP_GR)
c4 <- cor(w1$Exports_GDP_GR, w1$GDP_GR)
c5 <- cor(w1$Exports_LCU_GR, w1$GDP_GR)
c6 <- cor(w1$Electricity_GR, w1$GDP_GR)
c7 <- cor(w1$Commercial_GR, w1$GDP_GR)
c8 <- cor(w1$Two.Wheeler_GR, w1$GDP_GR)
c9 <- cor(w1$Foreign_GR, w1$GDP_GR)
c10 <- cor(w1$IIP_GR,w1$GDP_GR)
c11 <- cor(w1$Air_GR,w1$GDP_GR)
c12 <- cor(w1$Railways_GR,w1$GDP_GR)
#correlations 2012-18
c13 <- cor(w2$credit_GR, w2$GDP_GR)
c14 <- cor(w2$Imports_LCU_GR, w2$GDP_GR)
c15 <- cor(w2$Imports_GDP_GR, w2$GDP_GR)
c16 <- cor(w2$Exports_GDP_GR, w2$GDP_GR)
c17 <- cor(w2$Exports_LCU_GR, w2$GDP_GR)
c18 <- cor(w2$Electricity_GR, w2$GDP_GR)
c19 <- cor(w2$Commercial_GR, w2$GDP_GR)
c20 <- cor(w2c23 <- cor(w2$Air_GR,w2$GDP_GR)
           $Two.Wheeler_GR, w2$GDP_GR)
c21 <- cor(w2$Foreign_GR, w2$GDP_GR)
c22 <- cor(w2$IIP_GR,w2$GDP_GR)
c24 <- cor(w2$Railways_GR,w2$GDP_GR)

#Figure 1
fig1 <- read.csv("/home/archit/EcoProject/Econometrics---Final-Project/data/fig1.csv")
ggplot(data = fig1,aes(x = fig1$Correlation.Between.2001.11, y = fig1$Correlation.between.2012.18))+
  geom_point(size = 2.0) + theme(axis.text.x= element_text(size = 10,angle = 45))
#Horizontal and Vertical Line
ggplot(data = fig1,aes(x = fig1$Correlation.Between.2001.11, y = fig1$Correlation.between.2012.18))+
  geom_point(size = 2.0) + theme(axis.text.x= element_text(size = 10,angle = 45)) +
  geom_hline(aes(yintercept=0), color="blue", linetype="dashed") + geom_vline(aes(xintercept=0), color="blue", linetype="dashed")
#put in the line x = y
ggplot(data = fig1,aes(x = fig1$Correlation.Between.2001.11, y = fig1$Correlation.between.2012.18))+
  geom_point(size = 2.0) + theme(axis.text.x= element_text(size = 10,angle = 45)) +
  geom_hline(aes(yintercept=0), color="green") + geom_vline(aes(xintercept=0), color="blue", linetype="dashed") + geom_abline(slope = 1,color="blue",) + coord_cartesian(xlim = c(-1,1), ylim = c(-1,1))
#labels to points
ggplot(data = fig1,aes(x = fig1$Correlation.Between.2001.11, y = fig1$Correlation.between.2012.18))+
  geom_point(size = 2.0, color="red") + theme(axis.text.x= element_text(size = 10,angle = 45)) +
  geom_hline(aes(yintercept=0), color="green") + geom_vline(aes(xintercept=0), color="purple", linetype="dashed") + geom_abline(slope = 1,color="blue",) + coord_cartesian(xlim = c(-1,1), ylim = c(-1,1)) + geom_text(aes(label=Variable),hjust=0, vjust=0)
