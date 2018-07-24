#.libPaths(c(.libPaths(), 
#            "/home/users/lechu/R/x86_64-pc-linux-gnu-library/3.3.3", 
#            "/opt/exp_soft/local/generic/R/3.3.3-gcc482/lib64/R/library"))


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

source("create.model.R")
mkdir("species.results")

# Obliczanie ilosci dostepnych rdzeni i paralelizacja obliczen
cores.n <- detectCores() - 1
cl <- makeCluster(cores.n, type = "SOCK")


cv.n <- 10         # Liczba podzialow w cv
imp.var.n <- 20   # Liczba zmiennych uzywanych do budowy finalnego modelu

# Lista metod 
methods <- c("gam" ,"glm", "rf","svmPoly", "gbm")

# Pobranie danych potrzebnych do predykcji
load("data.RData")

# Tworzenie listy gatunkow
spec.list <- list.files("species.data/")
spec.list <- strsplit(spec.list, ".", fixed = TRUE)
spec.list <- unlist(lapply(spec.list, function(x) x[[1]]))

# Zmienne nieuzywane do budowy modelu
location.vars <-c("id.kw", "total.visits", "dist.1", "dist.2", "dist.3", "dist.4", "dens", "d.min", "d.max", "d.sd", "suma.13", "present", "dem", "slope", "northness", "eastness")



# Petla wywolywana dla kazdego gatunku
for(spec in spec.list)
{
  # Tworzenie folderow
  mkdir(paste0("species.results/", spec))
  mkdir(paste0("species.results/", spec, "/wykresy"))
  
  # Ladowanie danych i wybranie odpowiednich kolumn
  load(paste0("species.data/", spec, ".RData"))
  data<-data[, -which(names(data) %in% location.vars)]
  
  
  # Usuniecie wierszy zawierajacych NA
  data <- data[complete.cases(data), ]
  
  
  # Centralizacja danych (bez kolumny 1 - ldens)
  mean.from.data <- apply(data[, 2:ncol(data)], 2, mean)
  mean.from.data <- as.data.frame(t(mean.from.data))
  
  write.csv(mean.from.data, paste0("species.results/", spec ,"/mean.from.data.csv"), row.names = FALSE)
  
  data[2:ncol(data)] <- scale(data[2:ncol(data)], scale = FALSE)
  data <- as.data.frame(data)
  
  
  
  #######  MODELE #######
  
  for(method in methods)
  {
    if(method == "rf")
      model<- create.model(method, data, cv.n, imp.var.n, importance = TRUE)
    else
      model<- create.model(method, data, cv.n, imp.var.n) 
    saveRDS(model, paste0("species.results/", spec, "/", method, ".model.rds"))
  }
}


stopCluster(cl)
