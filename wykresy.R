######################  Wykresy  ############################
wykresy <- function(spec, method){

  # 1 zapis RMSE i R2
  model<-readRDS(paste0("species.results/", spec,"/",method,".model.rds"))  # czytanie modelu z pliku
  measures.list <- c(as.numeric(getTrainPerf(model)[1]), as.numeric(getTrainPerf(model)[2]))
  
  
  
  
  
  # 2 tworzenie wykresu ze zmiennymi znaczacymi
  theme_set(theme_bw())
  
  zmienne <- varImp(model)$importance
  zmienne<-cbind(row.names(zmienne), zmienne)
  zmienne<-zmienne[order(zmienne[2], decreasing = TRUE),]
  colnames(zmienne) <-(c("Feature", "Importance"))
  rownames(zmienne)<-c()
  

  zmienne.wykres <- zmienne %>%
    arrange(desc(Importance)) %>%
    arrange(Importance) %>%
    mutate(Feature = factor(Feature, levels = .$Feature))
  
  ggplot(zmienne.wykres, aes(Importance, Feature)) +
    geom_segment(aes(x = 0, y = Feature, xend = Importance, yend = Feature), color = "grey50") +
    geom_point(color="orange", size=4)
  ggsave(paste0("species.results/", spec,"/wykresy/",method,".varImp.png"), width = 5, height = 6)
  
  
  
  # 3 tworzenie mapki z przewidywaniami
  
  mean.from.data <- read.csv(paste0("species.results/", spec,"/mean.from.data.csv"))
  
  data2<-env.all
  for (name in colnames(mean.from.data)) {
    if(name %in% colnames(env.all)){
      data2[name]<-env.all[name]-as.numeric(mean.from.data[name])
    }
  }
  
  
  data2 <-data2[,c(as.character(zmienne$Feature))]

  pred <- predict(model, data2)
  for(index in 1:length(pred))
  {
    if (pred[index]<0)
      pred[index]=0
  }
  pred <- expm1(pred)
  
  jpeg(paste0("species.results/", spec,"/wykresy/",method,"_map.png"))
  
  
  n <- 7
  pal <- rev(brewer.pal(n, "RdYlBu"))
  colors <- colorRampPalette(pal, bias = 1, alpha = TRUE)
  map <- data.frame(id = env.all$id, x = env.all$x, y = env.all$y, pred = pred)
  summary(map)
  
  id <- raster("1992_1km.rst")
  
  pre <- subs(id, map, by = "id", which = 4)
  plot(pre, col = colors(256))
  hist(pre)
  
  #pres <- stretch(pre, minv = 0, maxv = cellStats(pre, "max"), maxq = 0.999)
  pres <- stretch(pre, minv = 0, maxv = 300, maxq = 0.999)
  
  summary(pres); hist(pres)
  plot(pres, col = colors(256),axes=FALSE)
  #plot( pres, col=rev( rainbow( 99, start=0,end=1 ) ), breaks=seq(min(minValue( pres )),max(maxValue(pres)),length.out=100) )
  Axis(side=1, labels=FALSE)
  Axis(side=2, labels=FALSE)
  dev.off()
  
  
  
  measures.list
}