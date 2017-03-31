setwd("E:/Study/MSSP/Spring 2017/676/CE")

expd <- dir("./expd")
fmld <- dir("./fmld")


temp <- read.csv("./fmld/fmld151.csv")
temp <- temp[,c("NEWID","FAM_SIZE","STATE")]
fmld_data <- temp

for(i in fmld) {
  temp <- read.csv(paste("./fmld/",i,sep = ""))
  temp <- temp[,c("NEWID","FAM_SIZE","STATE")]
  fmld_data <- rbind(fmld_data,temp)
}

fmld_data <- unique(fmld_data)
#saveRDS(fmld_data,"fmld_data.rds")

temp <- read.csv("./expd/expd151.csv")
temp <- temp[,c("NEWID","UCC","COST","GIFT")]
expd_data <- temp

for(i in expd) {
  temp <- read.csv(paste("./expd/",i,sep = ""))
  temp <- temp[,c("NEWID","UCC","COST","GIFT")]
  expd_data <- rbind(expd_data,temp)
}

expd_data <- unique(expd_data)
expd_data <- expd_data[expd_data$GIFT==2,]
#saveRDS(expd_data,"expd_data.rds")

#save.image("rawdata.rdata")
load("rawdata.rdata")
expd_data <- expd_data[expd_data$UCC %in% c(90110:100510,110110:120410,
                                                 10110:20210,30110:80110,
                                                 20310:20820,150110:170110,
                                                 180310,190311:190316,
                                                 170210:180110,200111:200536,
                                                 630110:630900,190111,
                                                 190211,190321,190911,190921,
                                                 600110:600903,620111,620121,620221,
                                                 540000:570903,580000:580901),]

expd_data$category <- ifelse(expd_data$UCC %in% c(540000:570903),"medicost",NA)
expd_data$category <- ifelse(expd_data$UCC %in% c(580000:580901),"medinsur",expd_data$category)
expd_data$category <- ifelse(expd_data$UCC %in% c(90110:100510),"diary",expd_data$category)
expd_data$category <- ifelse(expd_data$UCC %in% c(110110:120410),"fruitveg",expd_data$category)
expd_data$category <- ifelse(expd_data$UCC %in% c(10110:20210),"grains",expd_data$category)
expd_data$category <- ifelse(expd_data$UCC %in% c(30110:80110),"meat",expd_data$category)
expd_data$category <- ifelse(expd_data$UCC %in% c(20310:20820,150110:170110,
                                                  180310,190311:190316),"confections",expd_data$category)
expd_data$category <- ifelse(expd_data$UCC %in% c(170210:180110),"water",expd_data$category)
expd_data$category <- ifelse(expd_data$UCC %in% c(200111:200536),"alcohol",expd_data$category)
expd_data$category <- ifelse(expd_data$UCC %in% c(630110:630900),"smoke",expd_data$category)
expd_data$category <- ifelse(expd_data$UCC %in% c(190111,190211,190321,
                                                  190911,190921),"fastfood",expd_data$category)

expd_data$category <- ifelse(expd_data$UCC %in% c(600110:600903,620111,
                                                  620121,620221),"sports",expd_data$category)
#saveRDS(expd_data,"expd_categ.rds")





















