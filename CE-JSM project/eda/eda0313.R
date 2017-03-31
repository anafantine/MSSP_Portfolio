setwd("D:/Study/MSSP/Spring 2017/676/CE/eda")
load("eda0313.rdata")

expd151 <- read_csv("D:/Study/MSSP/Spring 2017/676/CE/diary15/expd151.csv")
expd152 <- read_csv("D:/Study/MSSP/Spring 2017/676/CE/diary15/expd152.csv")
expd153 <- read_csv("D:/Study/MSSP/Spring 2017/676/CE/diary15/expd153.csv")
expd154 <- read_csv("D:/Study/MSSP/Spring 2017/676/CE/diary15/expd154.csv")


# check id

sum(expd151$NEWID %in% expd152$NEWID)
sum(expd151$NEWID %in% expd153$NEWID)
sum(expd151$NEWID %in% expd154$NEWID)
#### all zero


#subset what we care
expd151$UCC <- as.integer(expd151$UCC)

expd151_sub <- expd151[expd151$UCC>=540000 &
                         expd151$UCC < 580902,]

expd151_sub <- rbind(expd151_sub,
                     expd151[expd151$UCC>=600110 &
                               expd151$UCC <= 600903,])
expd151_sub <- rbind(expd151_sub,
                     expd151[expd151$UCC==620111 |
                               expd151$UCC==620121|
                               expd151$UCC==620221,])
expd151_sub <- rbind(expd151_sub,
                     expd151[expd151$UCC>=630110 &
                               expd151$UCC <= 630900,])
#class
expd151_sub$class <- ifelse(expd151_sub$UCC>=540000 &
                              expd151_sub$UCC <=570903,
                            "medicalcost",NA)
expd151_sub$class <- ifelse(expd151_sub$UCC>=580000 &
                              expd151_sub$UCC <=580901,
                            "medins",expd151_sub$class)
expd151_sub$class <- ifelse(expd151_sub$UCC>=600110 &
                              expd151_sub$UCC <=600903,
                            "sports",expd151_sub$class)
expd151_sub$class <- ifelse(expd151_sub$UCC==620111 |
                              expd151_sub$UCC==620121|
                              expd151_sub$UCC==620221,
                            "sports",expd151_sub$class)
expd151_sub$class <- ifelse(expd151_sub$UCC>=630110 &
                              expd151_sub$UCC <=630903,
                            "ciga",expd151_sub$class)
library(dplyr)
library(plyr)
expd151_class <- ddply(expd151_sub,
                       c("NEWID","class"),
                       summarise,
                       total=sum(COST))

library(reshape2)
expd151_inv <- dcast(expd151_class,NEWID ~ class)
expd151_inv[is.na(expd151_inv)] <- 0

expd151_class<- expd151_class[expd151_class$NEWID!="03140372",]

library(ggplot2)

a<- ggplot(expd151_inv,aes(x=medicalcost)) + geom_histogram(bins = 100)+ggtitle("Hist for total medical cost")
b <- ggplot(expd151_inv,aes(x=medins)) + geom_histogram(bins = 100)+ggtitle("Hist for total medical insurance")
c<- ggplot(expd151_inv,aes(x=sports)) + geom_histogram(bins = 100)+ggtitle("Hist for total sports expediture")
d<- ggplot(expd151_inv,aes(x=ciga)) + geom_histogram(bins = 100) + ggtitle("Hist for total cigarettes expediture")

e<- ggplot(expd151_inv[expd151_inv$medicalcost!=0,],aes(x=medicalcost)) + geom_histogram(bins = 100)+ggtitle("Hist for total medical cost (non-zero)")
f<- ggplot(expd151_inv[expd151_inv$medins!=0,],aes(x=medins)) + geom_histogram(bins = 100)+ggtitle("Hist for total medical insurance (non-zero)")
g<- ggplot(expd151_inv[expd151_inv$sports!=0,],aes(x=sports)) + geom_histogram(bins = 100)+ggtitle("Hist for total sports expediture (non-zero)")
h<- ggplot(expd151_inv[expd151_inv$ciga!=0,],aes(x=ciga)) + geom_histogram(bins = 100) + ggtitle("Hist for total cigarettes expediture (non-zero)")

gridExtra::grid.arrange(a,c,d,e,g,h,ncol=3,nrow=2)

ggplot(expd151_class,aes(x=NEWID,y=total,fill=factor(class))) + geom_bar(stat = "identity") + 
  facet_grid(class~.)

expd151_reg <- lm(medicalcost~ciga+sports,data = expd151_inv)
summary(expd151_reg)
par(mfrow=c(2,2))
plot(expd151_reg)

ggplot(expd151_inv,aes(x=log(medicalcost+1))) + geom_histogram()+ggtitle("Hist for log medical cost")
ggplot(expd151_inv,aes(x=log(medins+1))) + geom_histogram()+ggtitle("Hist for log medical insurance")
ggplot(expd151_inv,aes(x=log(sports+1))) + geom_histogram()+ggtitle("Hist for log sports expediture")
ggplot(expd151_inv,aes(x=log(ciga+1))) + geom_histogram() + ggtitle("Hist for cigarettes expediture")


expd151_logreg <- lm(log(medicalcost+1)~log(ciga+1)+log(sports+1),data = expd151_inv)
summary(expd151_logreg)
par(mfrow=c(2,2))
plot(expd151_logreg)

plot(expd151_inv)

plot(scale(expd151_inv$ciga),scale(expd151_inv$medicalcost))
