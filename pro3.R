#
library(MASS)
library(glmnet)
library(ncvreg)
library(mvtnorm)
library(ggplot2)

data<-read.csv('C:/Users/Acer/Documents/OSU/ST 599_S2015/Pro3/quantquote_daily_sp500_83986/quantquote_daily_sp500_83986/daily/table_a.csv',sep=',',head=F)

dat<-data[,-c(1,2)]
dat1<-dat[,-5]
attach(dat)
dat2<-data.frame(cbind(dat1,dat1^2,V3*V4,V3*V5,V3*V6,V4*V5,V4*V6,V5*V6,dat1^3,dat1^4,dat1^5,dat1^6,dat1^7,dat1^8,V7))
train<-dat2[1:2000,];test<-dat2[2001:3452,]
scaltest<-data.frame(scale(test, T,T))
scaltrain<-data.frame(scale(train, T,T))
Reg<-lm(V7 ~., data=scaltrain)
summary(Reg)

#cov(scaltrain[,1:4])
#quartz("Residual Plot")
plot(scaltrain$V7,Reg$residuals)

pred.lm = predict(Reg, scaltest)
pred.err.lm = mean((pred.lm - scaltest$V7)^2)
plot(scaltest$V7[1:200] ,pred.lm[1:200])


######Ridge
#Error in plot.new() : figure margins too large
#To avoid such errors you can first check par("mar") output
# get 5.1 4.1 4.1 2.1
#chge to par(mar=c(1,1,1,1))
par(mar=c(1,1,1,1))
RidgeReg = lm.ridge(V7~.,data=scaltrain, lambda = seq(0, 10, by=0.1))
summary(RidgeReg)

plot(RidgeReg$GCV)

lam = seq(0, 10, by = 0.1)[which.min(RidgeReg$GCV)]
coef = RidgeReg$coef[,which.min(RidgeReg$GCV)]
pred.ridge = matrix(unlist(scaltest[,1:38]), nrow = 1452, ncol = 38)%*%coef
pred.err.ridge = mean((pred.ridge - scaltest$V7)^2)
plot(scaltest$V7[1:200] ,pred.ridge[1:200])

############### LASSO ##############################################

LassoReg = cv.glmnet(matrix(unlist(scaltrain[,1:38]), nrow = 2000, ncol = 38), scaltrain$V7)
plot(LassoReg$cvm)
summary(LassoReg$glmnet.fit)
pred.lasso = predict(LassoReg$glmnet.fit, matrix(unlist(scaltest[,1:38]), nrow = 1452, ncol = 38))[,which(LassoReg$lambda == LassoReg$lambda.min)]
pred.err.lasso = mean((pred.lasso - scaltest$V7)^2)
plot(LassoReg$glmnet.fit, label = T,xlab='L1 norm')
qplot(scaltest$V7[1:200] ,pred.lasso[1:200])+xlab('Observed Volume')+
  ylab('Predicted Volume')+geom_abline(intercept = 0, slope = 1)

