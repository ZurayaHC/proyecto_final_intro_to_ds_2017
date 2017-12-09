EDA_univariado<-function(data){
  nombres=colnames(data)
  for (i in 1:length(data)){
    for (j in (i+1):length(data)){
      info<-ggplot(data, aes(data[[i]],data[[j]]))
      #Si la variable es numérica
      if (is.numeric(data[[i]])){
        print(hist(data[[i]]))
        print(boxplot(data[[i]]))
        print(stripchart(data[[i]], method = "jitter"))
        print(plot(density(data[[i]])))
        print(rug(data[[i]]))
        print(qqnorm(data[[i]]))
        print(qqnorm(log(data[[i]]), main = "Log-Normal Q-Q Plot"))
      }
      #Si las variable son categórica
      if (is.character(data[[i]])){
        print(info+geom_count()+labs(x = nombres[i]))
      }
    }
  }
}

EDA_bivariado <-function(data){
  nombres=colnames(data)
  for (i in 1:length(data)){
    for (j in (i+1):length(data)){
      info<-ggplot(data, aes(data[[i]],data[[j]]))
      #Si las dos son numéricas
      if (is.numeric(data[[i]])&is.numeric(data[[j]])){
        #print(info+geom_jitter()+labs(x = nombres[i], y=nombres[j]))
        print(info+geom_point()+labs(x = nombres[i], y=nombres[j]))
        #print(info+geom_quantile()+labs(x = nombres[i], y=nombres[j]))
        #print(info+geom_rug(sides='bl')+labs(x = nombres[i], y=nombres[j]))
        #print(info+geom_smooth(method=lm)+labs(x = nombres[i], y=nombres[j]))
        #print(info+geom_bin2d()+labs(x = nombres[i], y=nombres[j]))
        #print(info+geom_density2d()+labs(x = nombres[i], y=nombres[j]))
      }
      #Si las dos son categóricas
      if (is.character(data[[i]])&is.character(data[[j]])){
        print(info+geom_count()+labs(x = nombres[i], y=nombres[j]))
      }
      #Si la primeraes numérica y la segunda categórica
      if (is.character(data[[i]])&is.numeric(data[[j]])){
        info<-ggplot(data, aes(data[[j]],data[[i]]))
        print(info+geom_bar(stat="identity")+labs(x = nombres[j], y=nombres[i]))
        #print(info+geom_boxplot()+labs(x = nombres[j], y=nombres[i]))
        #print(info+geom_dotplot()+labs(x = nombres[j], y=nombres[i]))
        #print(info+geom_violin(scale="area")+labs(x = nombres[j], y=nombres[i]))
      }
      #Si la primera es categórica y la segunda numérica
      if (is.numeric(data[[i]])&is.character(data[[j]])){
        print(info+geom_bar(stat="identity")+labs(x = nombres[i], y=nombres[j]))
        #print(info+geom_boxplot()+labs(x = nombres[i], y=nombres[j]))
        #print(info+geom_dotplot()+labs(x = nombres[i], y=nombres[j]))
        #print(info+geom_violin(scale="area")+labs(x = nombres[i], y=nombres[j]))
      }
    }
  }
}