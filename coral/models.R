setwd("D:/Study/MSSP/Spring 2017/676/Coral_Data_CSV_Files")
growth<-read.csv("Growth_Change.csv")
deltaheight<-growth$Height_16-growth$Height_15
deltaheight[deltaheight>0]<-1
deltaheight[deltaheight<=0]<-0
deltavolume<-growth$Volume_16-growth$Volume_15
deltavolume[deltavolume>0]<-1
deltavolume[deltavolume<=0]<-0
tips<-read.csv("Tip_Change.csv")
deltatip<-tips$Tip_2016-tips$Tip_2015
deltatip[deltatip>0]<-1
deltatip[deltatip<=0]<-0

plot(growth$L_C~growth$Length_15)
library(ggplot2)

ggplot(growth,aes(x=Height_15,y=H_C)) + geom_point() + geom_smooth(method ="lm") + ggtitle("2015 Height vs. Height difference")
ggplot(growth,aes(x=Volume_15,y=EV_C)) + geom_point() + geom_smooth(method ="lm")+ ggtitle("2015 Volume vs. Volume difference")
ggplot(tips[-which.max(tips$Tip_2015),],aes(x=Tip_2015,y=T_C)) + geom_point() + geom_smooth(method ="lm")+ ggtitle("2015 Tip vs. Tip difference")

##linear
#height 

library(car)
model_h <- lm(H_C~Height_15,data = growth[deltaheight==0,])
summary(model_h)
influenceIndexPlot(model_h, id.n=3,vars="hat",main = "hat-values plot of height") 
plot(model_h)
#height 
model_v <- lm(EV_C~log(Volume_15),data = growth[deltaheight==0,])
summary(model_v)
influenceIndexPlot(model_v, id.n=3,vars="hat",main = "hat-values plot of volume") 
plot(model_v)
#height 
model_t <- lm(T_C~log(Tip_2015+1),data = tips[deltaheight==0,])
summary(model_t)
influenceIndexPlot(model_t, id.n=3,vars="hat",main = "hat-values plot of tip") 

model_h1 <- lm(H_C~Height_15,data = growth[deltaheight==1,])

#height 
model_v1 <- lm(EV_C~log(Volume_15),data = growth[deltaheight==1,])

#height 
model_t1 <- lm(T_C~log(Tip_2015+1),data = tips[deltaheight==1,])

par(mfrow=c(2,3))
plot(model_h,which=4)
plot(model_v,which=4)
plot(model_t,which=4)
plot(model_h1,which=4)
plot(model_v1,which=4)
plot(model_t1,which=4)


#logestic regression
#without transformation
#height
model_h1<-glm(deltaheight~growth$Height_15,family = binomial)
summary(model_h1)
#significant

#volume
model_v1<-glm(deltavolume~growth$Volume_15,family = binomial)
summary(model_v1)

#tips
model_t1<-glm(deltatip~tips$Tip_2015,family = binomial)
summary(model_t1)

#with log transformation
model_h2<-glm(deltaheight~log(growth$Height_15),family = binomial)
summary(model_h2)

model_v2<-glm(deltavolume~log(growth$Volume_15),family = binomial)
summary(model_v2)
#significant

model_t2<-glm(deltatip~log(tips$Tip_2015),family = binomial)
summary(model_t2)

#multinomial regression
library(nnet)
deltaheight[deltaheight>0]<-1
deltaheight[deltaheight=0]<-0
deltaheight[deltaheight<0]<--1
regout = multinom(deltaheight ~ growth$Height_15)
summary(regout)

deltavolume[deltavolume>0]<-1
deltavolume[deltavolume=0]<-0
deltavolume[deltavolume<0]<--1
regout = multinom(deltavolume ~ log(growth$Volume_15))
summary(regout)

deltatip[deltatip>0]<-1
deltatip[deltatip=0]<-0
deltatip[deltatip<0]<--1
regout = multinom(deltatip ~ tips$Tip_2015)
summary(regout)

#log transformation
model_h3<-multinom(deltaheight~growth$Height_15)
summary(model_h3)

model_v3<-glm(deltavolume~log(growth$Volume_15),family = binomial)
summary(model_v3)

model_t3<-glm(deltatip~tips$Tip_2015,family = binomial)
summary(model_t3)

**************************************
the model for height with original value has significant coefficients
the model for volume with log value has significant coefficients
only volume could be log transformed
