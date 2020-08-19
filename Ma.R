library(plyr)
library(party)
library(rpart)
library(tree)
library(ipred)
library(rpart)
library(randomForest)
setwd("~/Desktop/MaData")
Long <- read.csv("~/Desktop/MaData/PTM_Anthro_Long.csv")
Demo <- read.csv("~/Desktop/MaData/PTM_Baseline.csv")
Lab <- read.csv("~/Desktop/MaData/PTM_Lab.csv")

###===================================Data Cleaning==========================================
#==========Long Dataset - Dataset does not contain NA 
diff <- function(a) {
  return(a[length(a)] - a[1])
}
long.diff <- aggregate(Long[ ,c("sid", "agem", "wtkg", "htcm", "HAZ", "WAZ", "WHZ")], 
                  by = list(Long$sid), FUN = diff)
long.diff<-long.diff[,-2]
long.diff<-rename(long.diff, c("Group.1"="sid"))

#==========Lab Dataset
lab.diff <- aggregate(Lab[ ,c("SID" ,"MPO", "Reg1b", "sCD14", "CRP")], 
                       by = list(Lab$SID), FUN = diff)
lab.diff<-lab.diff[,-2]
lab.diff<-rename(lab.diff, c("Group.1"="sid"))
#=deal with NA
#count the number of missing values - only 9 NA - remove 
sapply(lab.diff, function(x)sum(is.na(x)))
#remove columns with NA
#lab.diff<-lab.diff[-which(is.na(lab.diff$sCD14)),]
#lab.diff<-lab.diff[-which(is.na(lab.diff$CRP)),]

#==========Combine the Dataset
all<-merge(Demo, lab.diff,by='sid')
all<-merge(all, long.diff,by='sid')
#==========Split the datset into treatment and control
treat <- subset(all, trt==1)
control <- subset(all, trt==0)
#==========subset for lab and long 
lab.diff.trt<-treat[,c(1,20:23)]
long.diff.trt<-treat[,c(1,24:29)]

#============================Analysis on MPO ================================
#==========add significant and insignificant to dataset 
NotB.mpo<-subset(lab.diff.trt, MPO>=0)
NotB.mpo$Benefit<-c(0)
B.mpo<-subset(lab.diff.trt, MPO<0)
B.mpo$Benefit<-c(1)
MPO<-rbind(NotB.mpo,B.mpo)
#comebind the resule to Demo Dataset
MPO.all<-merge(Demo,MPO,by='sid')
MPO.all<-MPO.all[,-c(20:23)]
#change some variables to factor 
cols <- c("sex", "family_tp", "ownhh","hh_class", "mom_edu","dad_edu","septic_toilet","treated_water","toilet_share","open_drain","Benefit")
MPO.all[,cols] <- data.frame(apply(MPO.all[cols], 2, as.factor))
#**********Decision Tree*****************
set.seed(1)
MPO.use<-MPO.all[,-c(1,2)]
tree.mpo = tree(as.factor(MPO.use$Benefit)~., data = MPO.use)
plot(tree.mpo)
text(tree.mpo)
#*********Random Forest****************
# random forests
rf.mpo = randomForest(MPO.use[,-c(18)], as.factor(MPO.use$Benefit), ntree = 1000, mtry = 6, nodesize = 5, importance = TRUE)
varImpPlot(rf.mpo,type=2)


#============================Analysis on Reg1b ================================
#==========add significant and insignificant to dataset 
NotB.reg<-subset(lab.diff.trt, Reg1b>=0)
NotB.reg$Benefit<-c(0)
B.reg<-subset(lab.diff.trt, Reg1b<0)
B.reg$Benefit<-c(1)
Reg<-rbind(NotB.reg,B.reg)
#comebind the resule to Demo Dataset
reg.all<-merge(Demo,Reg,by='sid')
reg.all<-reg.all[,-c(20:23)]
#change some variables to factor 
cols <- c("sex", "family_tp", "ownhh","hh_class", "mom_edu","dad_edu","septic_toilet","treated_water","toilet_share","open_drain","Benefit")
reg.all[,cols] <- data.frame(apply(reg.all[cols], 2, as.factor))
#**********Decision Tree*****************
set.seed(1)
reg.use<-reg.all[,-c(1,2)]
tree.reg = tree(as.factor(reg.use$Benefit)~., data = reg.use)
plot(tree.reg)
text(tree.reg)
#*********Random Forest****************
# random forests
rf.reg = randomForest(reg.use[,-c(18)], as.factor(reg.use$Benefit), ntree = 1000, mtry = 6, nodesize = 5, importance = TRUE)
varImpPlot(rf.reg,type=2)



#============================Analysis on sCD14 ================================
#==========add significant and insignificant to dataset 
NotB.sCD<-subset(lab.diff.trt, sCD14>=0)
NotB.sCD$Benefit<-c(0)
B.sCD<-subset(lab.diff.trt, sCD14<0)
B.sCD$Benefit<-c(1)
sCD<-rbind(NotB.sCD,B.sCD)
#comebind the resule to Demo Dataset
sCD.all<-merge(Demo,sCD,by='sid')
sCD.all<-sCD.all[,-c(20:23)]
#change some variables to factor 
cols <- c("sex", "family_tp", "ownhh","hh_class", "mom_edu","dad_edu","septic_toilet","treated_water","toilet_share","open_drain","Benefit")
sCD.all[,cols] <- data.frame(apply(sCD.all[cols], 2, as.factor))
#**********Decision Tree*****************
set.seed(1)
sCD.use<-sCD.all[,-c(1,2)]
tree.sCD = tree(as.factor(sCD.use$Benefit)~., data = sCD.use)
plot(tree.sCD)
text(tree.sCD)
#*********Random Forest****************
# random forests
rf.sCD = randomForest(sCD.use[,-c(18)], as.factor(sCD.use$Benefit), ntree = 1000, mtry = 6, nodesize = 5, importance = TRUE)
varImpPlot(rf.sCD,type=2)



#============================Analysis on CRP ================================
#==========add significant and insignificant to dataset 
NotB.CRP<-subset(lab.diff.trt, CRP>=0)
NotB.CRP$Benefit<-c(0)
B.CRP<-subset(lab.diff.trt, CRP<0)
B.CRP$Benefit<-c(1)
CRP<-rbind(NotB.CRP,B.CRP)
#comebind the resule to Demo Dataset
CRP.all<-merge(Demo,CRP,by='sid')
CRP.all<-CRP.all[,-c(20:23)]
#change some variables to factor 
cols <- c("sex", "family_tp", "ownhh","hh_class", "mom_edu","dad_edu","septic_toilet","treated_water","toilet_share","open_drain","Benefit")
CRP.all[,cols] <- data.frame(apply(CRP.all[cols], 2, as.factor))
#**********Decision Tree*****************
set.seed(1)
CRP.use<-CRP.all[,-c(1,2)]
tree.CRP = tree(as.factor(CRP.use$Benefit)~., data = CRP.use)
plot(tree.CRP)
text(tree.CRP)
#*********Random Forest****************
# random forests
rf.CRP = randomForest(CRP.use[,-c(18)], as.factor(CRP.use$Benefit), ntree = 1000, mtry = 6, nodesize = 5, importance = TRUE)
varImpPlot(rf.CRP,type=2)
