
################ TWORZENIE MODELU #################

create.model<-function(method, data, cv.n, imp.var.n,...){
  
  # Deklaracja warunkow budowania modelu
  control <- trainControl(method = "cv",  
                          number = cv.n,
                          verboseIter = TRUE
  )
  
  # Budowanie pierwszego modelu
  model <- train(ldens ~ ., 
                   data = data,
                   method = method,
                   trControl = control,
                   ...)
    
  
  
  
  # Wybor zmiennych do budowy drugiego modelu
  zmienne <- varImp(model)$importance
  zmienne<-cbind(row.names(zmienne), zmienne)
  zmienne<-zmienne[order(zmienne[2], decreasing = TRUE),]
  colnames(zmienne) <-(c("Feature", "Importance"))
  rownames(zmienne) <- NULL
  zmienne <- zmienne$Feature
  zmienne <- as.vector.factor(zmienne[1:imp.var.n])
  nowy <- data[,c("ldens", zmienne)]
  
  
  # Budowa drugiego modelu
  model <- train(ldens ~ ., 
                 data = nowy,
                 method = method,
                 trControl = control,
                 ...)
    
  
  
  model
}