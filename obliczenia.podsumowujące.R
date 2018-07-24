source("wykresy.R")
#.libPaths(c(.libPaths(), 
 #           "/home/users/lechu/R/x86_64-pc-linux-gnu-library/3.3.3", 
  #          "/opt/exp_soft/local/generic/R/3.3.3-gcc482/lib64/R/library"))


#install.packages(c("caret", "e1071", "doSNOW", "randomForest", "mlbench", "gdata", "ggplot2", "mgcv", "nlme", "dplyr", "tidyr", "RColorBrewer", "raster", "gbm", "kernlab", "icesTAF", "parallel"), lib = "~/SDM/kod/pakiety/", repos="http://cran.us.r-project.org", dependencies = TRUE)
#install.packages(c("caret", "e1071", "doSNOW", "randomForest", "mlbench", "gdata", "ggplot2", "mgcv", "nlme", "dplyr", "tidyr", "RColorBrewer", "raster", "gbm", "kernlab", "icesTAF", "parallel"))

library(caret)
library(e1071)
library(doSNOW)
library(randomForest)
library(mlbench)
library(gdata) 
library(ggplot2)
library(mgcv)
library(nlme)
library(dplyr)       
library(tidyr) 
library(RColorBrewer)
library(raster) 
library(gbm)  
library(kernlab) 
library(icesTAF)
library(parallel)

spec.list <- list.files("species.data/")
spec.list <- strsplit(spec.list, ".", fixed = TRUE)
spec.list <- unlist(lapply(spec.list, function(x) x[[1]]))

#spec.list <- data.frame(gatunek = c("A", "AA"))

load("data.RData")

rm(measures.df)
rm(measures.list)


for(spec in spec.list)
{
  measures.list<-c()
  for(method in methods)
  {
    measures.list<-c(wykresy(spec, method), measures.list)
  }
    
  if(exists("measures.df")){
    measures.df <- rbind(measures.df, measures.list)
  }else{
    measures.df <- t(data.frame(measures.list))
    #colnames(measures.df)<-c("glm.rmse.train","glm.r2.train","rf.rmse.train","rf.r2.train","svm.rmse.train","svm.r2.train", "gbm.rmse.train","gbm.r2.train")
    colnames(measures.df)<-c("gam.rmse.train","gam.r2.train","glm.rmse.train","glm.r2.train","rf.rmse.train","rf.r2.train", "svm.rmse.train","svm.r2.train","gbm.rmse.train","gbm.r2.train")
  }
  
  rownames(measures.df)[nrow(measures.df)] <- spec
}

write.csv(measures.df, file = "miary.oceny.csv", row.names = TRUE)

measures.df <-read.csv("miary.oceny.csv")
rownames(measures.df)<-c(as.character(measures.df[,1]))
measures.df <- measures.df[,-1]
summary.df<-apply(measures.df, 2, summary)

write.csv(summary.df, file = "summary.df.csv", row.names = TRUE)

jpeg("r2_boxplot.png")
boxplot(measures.df[,c(4,2,8,6,10)], names = c("GLM", "GAM", "SVM", "RF", "BRT"), las=2)
dev.off()

jpeg("rmse_boxplot.png")
boxplot(measures.df[,c(3,1,7,5,9)], names = c("GLM", "GAM", "SVM", "RF", "BRT"), las=2)
dev.off()

r2.stacked.measures.df <- stack(measures.df[,c(4,2,8,6,10)])
rmse.stacked.measures.df <- stack(measures.df[,c(3,1,7,5,9)])

anova.r2 <- aov(values ~ ind, data = r2.stacked.measures.df)
summary(anova.r2)
anova.rmse <- aov(values ~ ind, data = rmse.stacked.measures.df)
summary(anova.rmse)



