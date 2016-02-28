# Copyright (c) 2013-2016 YA <ya.androidapp@gmail.com> All rights reserved.
setwd("C:/Users/Public/Documents/R")
ENASINK <- 1 #sink()
FF <- 0 #from File
install.packages("TeachingDemos", dependencies = TRUE)
install.packages("fields", dependencies = TRUE)
install.packages("e1071", dependencies = TRUE)
install.packages("randomForest", dependencies = TRUE)
library(TeachingDemos)
library(fields)
#library(kernlab)
library(e1071)
library(randomForest)

cbinddiflen <- function(x1,x2){
  nrowmin <<- min( nrow(x1),nrow(x2) )
  return( cbind( x1[1:nrowmin] , x2[1:nrowmin] ) )
}

touch <- function(path){
  if(!file.exists(path)){
    file.create(path)
  }
}

dirCreate <- function(path){
  if(!file.exists(path)){
    dir.create(path)
  }
}

importFcsv <- function(stitle,n,x){
  dirCreate("result")
  return(read.table(paste("result/",stitle,"-",n,"_",x,".csv",sep=""),sep=",",header=F))
}

export2csv <- function(stitle,n,x){
  dirCreate("result")
  write.table(get(x),paste("result/",stitle,"-",n,"_",x,".csv",sep=""),sep=",",quote=F,col.names=F,row.names=F,append=F)
}

export2png4heat <- function(stitle,n,x,rev,main,xlab,ylab){
  x2 <- get(x)

  if(rev == 0){
    colors <- c("#FF0000FF",(heat.colors(1000))[401:1000])
  }else if(rev == 1){
    colors <- rev(c("#FF0000FF",(heat.colors(1000))[401:1000]))
  }else if(rev == 2){
    colors <- rev(c("#FF0000FF",heat.colors(30)))
  }
  cat("colors:",colors,"\n")

  dirCreate("result")

  cat("nrow(x2):",nrow(x2),"ncol(x2):",ncol(x2),"\n")

  png(paste("result/",stitle,"-",n,"_",x,"h.png",sep=""),width=720)
  y <- t(x2[nrow(x2):1,ncol(x2):1])[ncol(x2):1,]
  layout(t(1:1))
  image.plot(y,col = colors,axes=TRUE,main = main,xlab = xlab,ylab = ylab,cex.axis=2)
  dev.off()
}

export2png4line <- function(stitle,n,x,main,xlab,ylab){
  y <- get(x)

  dirCreate("result")

  png(paste("result/",stitle,"-",n,"_",x,"l.png",sep=""),width=720)
  plot(y,axes=TRUE,type = "l",main = main,xlab = xlab,ylab = ylab,cex.axis=2)
  dev.off()
}

export2png4linef <- function(stitle,n,x,main,xlab,ylab){
  y <- as.vector(get(x))
  y <- y / sum(y)

  dirCreate("result")

  png(paste("result/",stitle,"-",n,"_",x,"l.png",sep=""),width=720)
  plot(as.numeric(y),axes=TRUE,type = "l",main = main,xlab = xlab,ylab = ylab,cex.axis=2)
  dev.off()
}

nextX <- function(dire,x1,wide,istest){
  rnrm <- rnorm(1, mean=1, sd=0.19)
  derr <- ifelse(istest==1,rnrm,1)

  if((dire == 2) ||(dire == 1) ||(dire == 8)){
    d <- (  1 * derr * ifelse(wide>1,2,1) )
  }else if((dire == 3) ||(dire == 0) ||(dire == 7)){
    d <- 0
  }else if((dire == 4) ||(dire == 5) ||(dire == 6)){
    d <- ( -1 * derr * ifelse(wide>1,2,1) )
  }
  return( x1 + d )
}

nextY <- function(dire,y1,wide,istest){
  rnrm <- rnorm(1, mean=1, sd=0.08)
  derr <- ifelse(istest==1,rnrm,1)

  if((dire == 2) ||(dire == 3) ||(dire == 4)){
    d <- (  1 * derr * ifelse(wide>1,2,1) )
  }else if((dire == 1) ||(dire == 0) ||(dire == 5)){
    d <- 0
  }else if((dire == 8) ||(dire == 7) ||(dire == 6)){
    d <- ( -1 * derr * ifelse(wide>1,2,1) )
  }
  return( y1 + d )
}

sprintf20 <- function(x){
  if((is.null(x)) ||(is.nan(x)) ||(length(x) == 0)){
    x <- "00.0"
  }else if(is.character(x)){
  }else{
    x <- sprintf("%02d",x)
  }
  return(x)
}

sprintf21 <- function(x){
  if((is.null(x)) ||(is.nan(x)) ||(length(x) == 0)){
    x <- "00.0"
  }else if(is.character(x)){
  }else{
    x <- sprintf("%04.1f",x)
  }
  return(x)
}

loadData <- function(consolidated,filename,df,dfv,wide,istest){
  csvData <- read.csv(filename,header =T)
  if(consolidated > 1){
    y <- matrix(0,nrow=((90 **(consolidated - 1)) * nrow(csvData)),ncol=(consolidated * ncol(csvData) +2))
    colnames(y) <- c("NAME1","y1","x1","n1",
     "BSSID1.1","BSSID2.1","BSSID3.1",
     "dx","dy","NAME2","y2","x2","n2",
     "BSSID1.2","BSSID2.2","BSSID3.2")

    i <- 1
    j <- 1
    n2 <- 1
    d2 <- 0
    addpos <- 1 #NAの分を詰めたときのインデックス

    for(j in 1:nrow(csvData)){
      for(n2 in 1:10){
        if((df>0)&&(9>dfv)&&(dfv>-1)){
          start<-dfv
          end<-dfv
        }else{
          start<-0
          end<-8
        }
        for(d2 in start:end){
          #現在の観測に対して直前の観測は最大9方向*10観測=90通り

          y1 <- as.numeric(csvData[j,"y1"])
          x1 <- as.numeric(csvData[j,"x1"])
          n1 <- as.numeric(csvData[j,"n1"])
          y2 <- nextY(d2,y1,wide,istest)
          x2 <- nextX(d2,x1,wide,istest)
          dx <- as.numeric(x2 - x1)
          dy <- as.numeric(y2 - y1)

          y1 <- sprintf21(y1)
          x1 <- sprintf21(x1)
          y2 <- sprintf21(y2)
          x2 <- sprintf21(x2)

          name1 <- paste("X",x1,"Y",y1,"N",sprintf20(n1),"Z",sep = "")
          name2 <- paste("X",x2,"Y",y2,"N",sprintf20(n2),"Z",sep = "")

          cat("i =",i," addpos =",addpos," j =",j," n2 =",n2," d2 =",d2," = ",name1,"<-",name2,"\n")

          if((length(csvData[name2 == csvData$NAME1,"NAME1"]) > 0) && (((istest == 2) && (x1 %% 20 != 1) && (y1 %% 20 != 1)) || (istest != 2))){ # %% 20 != 1の部分は適当
            #cat(" *** length(csvDataSearched$NAME1) > 0 ***")
            y[addpos,"NAME1"] <- name1
            y[addpos,"y1"] <- as.numeric(y1)
            y[addpos,"x1"] <- as.numeric(x1)
            y[addpos,"n1"] <- as.integer(csvData[j,"n1"])
            y[addpos,"BSSID1.1"] <- csvData[j,"BSSID1.1"]
            y[addpos,"BSSID2.1"] <- csvData[j,"BSSID2.1"]
            y[addpos,"BSSID3.1"] <- csvData[j,"BSSID3.1"]

            y[addpos,"dx"] <- as.numeric(dx)
            y[addpos,"dy"] <- as.numeric(dy)
            y[addpos,"NAME2"] <- name2
            y[addpos,"y2"] <- as.numeric(y2)
            y[addpos,"x2"] <- as.numeric(x2)
            y[addpos,"n2"] <- as.integer(n2)

            csvDataSearched <- csvData[name2 == csvData$NAME1,]
            y[addpos,"BSSID1.2"] <- csvDataSearched[,"BSSID1.1"]
            y[addpos,"BSSID2.2"] <- csvDataSearched[,"BSSID2.1"]
            y[addpos,"BSSID3.2"] <- csvDataSearched[,"BSSID3.1"]

            addpos <- addpos + 1
          }
          #cat("\n")

          i <- i + 1
        }
      }
    }
    z <- y[1:addpos-1,]

    if(wide > 0){
      z <- z[(as.numeric(z[,"y1"])-0.5) %% 2 == 1,]
      z <- z[(as.numeric(z[,"y2"])-0.5) %% 2 == 1,]
      if(wide > 1){
        z <- z[as.numeric(z[,"x1"]) %% 2 == 1,]
        z <- z[as.numeric(z[,"x2"]) %% 2 == 1,]
      }
    }

    return(z)
  }else{
    if(wide > 0){
      y <- csvData[which((as.numeric(csvData[,"y1"]) - 0.5) %% 2 == 1),]
      if(wide > 1){
        y <- y[which(as.numeric(y[,"x1"]) %% 2 == 1),]
      }
      return(y)
    }

    return(csvData)
  }
}

divideData <- function(csvData,mode,consolidated){
  if(consolidated > 1){
    if(mode == 0 || mode == 2){
      y1 <- csvData[which(as.numeric(csvData[,"n1"]) %% 2 == 0),]
      y <- y1[which(as.numeric(y1[,"n2"]) %% 2 == 0),]
    }else if(mode == 1 || mode == 3){
      y1 <- csvData[which(as.numeric(csvData[,"n1"]) %% 2 != 0),]
      y <- y1[which(as.numeric(y1[,"n2"]) %% 2 != 0),]
    }
    if(mode == 0 || mode == 1){
      index <- setdiff(colnames(y),c("NAME1","y1","x1","n1","dx","dy","NAME2","y2","x2","n2"))
      return(y[,index])
    }else if(mode == 2 || mode == 3){
      return(y)
    }
  }else{
    if(mode == 0 || mode == 2){
      y <- csvData[as.numeric(csvData$n1) %% 2 == 0,]
    }else if(mode == 1 || mode == 3){
      y <- csvData[as.numeric(csvData$n1) %% 2 != 0,]
    }
    if(mode == 0 || mode == 1){
      index <- setdiff(colnames(y),c("NAME1","y1","x1","n1"))
      return(y[index])
    }else if(mode == 2 || mode == 3){
      return(y)
    }
  }
}

errorDis <- function(x1,x2,k){
  if(k<1){
    y <- matrix(0,nrow=nrow(x1),ncol=1)
    for(i in 1:nrow(x1)){
      y[i,1] <- sqrt((as.numeric(x1[i,1])-as.numeric(x2[i,"y1"]))^2 +(as.numeric(x1[i,2])-as.numeric(x2[i,"x1"]))^2)
    }
  }else{
    y <- matrix(0,nrow=nrow(x1),ncol=k)
    for(i in 1:nrow(x1)){
      for(j in 1:k){
        #cat("i =",i," j =",j,"\n")
        t <- unlist(strsplit(as.character(x1[i,j]),"[XYNZ]"))
        y[i,j] <- sqrt((as.numeric(t[2])-as.numeric(x2[i,"x1"]))^2 +(as.numeric(t[3])-as.numeric(x2[i,"y1"]))^2)
      }
    }
    colnames(y) <- 1:k
  }
  return(y)
}

errorTotalizationHeat <- function(stitle,n,x,k){
  if(length(x)>1){
    xmin <<- min(x)
    xmax <<- ifelse((max(x) == xmin),1,max(x))
    xbreaks <<- ifelse(((xmax - xmin)==0),1,(xmax - xmin)) / 100
    cat("xbreaks:",xbreaks," xmax:",xmax," xmin:",xmin,"\n")

    h <<- hist(x[,1],breaks=seq(xmin,xmax,xbreaks))
    c <- length(h$counts) # 階級の数
    y <- matrix(0,nrow=c,ncol=k)

    class_names <- rep("-",c)
    for(i in 1:c){
      class_names[i] <- paste(h$breaks[i],"-",h$breaks[i+1],sep = "")
    }
    rownames(y) <- class_names
    colnames(y) <- 1:k

    dirCreate("result")
    dirCreate(paste("result/",stitle,"-",n,sep=""))

    for(j in 1:k){
      if(k<2){
        png(paste("result/",stitle,"-",n,"_",j,".png",sep=""),width=720)
      }else{
        png(paste("result/",stitle,"-",n,"/",n,"_",j,".png",sep=""),width=720)
      }
      h <- hist(x[,j],breaks=seq(xmin,xmax,xbreaks),label = TRUE,main = paste("Histogram of K=",j),xlab = "Distance[m]")
      dev.off()

      y[,j] <- h$counts
    }
  }else{
    y <- matrix(1,nrow=1,ncol=1)
  }
  return(y)
}

errorTotalizationLineAver <- function(x){
  y <- apply(x, 2, mean)
  return(y)
}

errorTotalizationLineFreq <- function(x){
  y <- x[1,]
  return(y)
}

setML <- function(consolidated,stitle,n,x1,x2,isY){
  nrowmin <<- min(nrow(x1),nrow(x2))
  gammaRange = 10^(-5:5)
  costRange = 10^(-2:2)
  if(consolidated>1){
    d.test <<- as.data.frame(x1[1:nrowmin,c(-1,-4,-13,-14,-15,-16)])
    d.train <<- as.data.frame(x2[1:nrowmin,c(-1,-4,-13,-14,-15,-16)])
    v <<- d.train
    if(isY==0){
      tryCatch(
        {
          t <- tune.svm(v$x1 ~ v$dx + v$dy + v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1 + v$BSSID1.2 + v$BSSID2.2 + v$BSSID3.2, data = d.train, gamma=gammaRange, cost=costRange, tunecontrol = tune.control(sampling="cross", cross=8))
          cat("gamma:",t$best.parameters$gamma, " cost:",t$best.parameters$cost,"\n")
          v.result <<- svm(v$x1 ~ v$dx + v$dy + v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1 + v$BSSID1.2 + v$BSSID2.2 + v$BSSID3.2, data=d.train, gamma=t$best.parameters$gamma, cost=t$best.parameters$cost)
        }, 
        error = function(e) {
          cat("E!\n")
          v.result <<- svm(v$x1 ~ v$dx + v$dy + v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1 + v$BSSID1.2 + v$BSSID2.2 + v$BSSID3.2, data=d.train, gamma=100, cost=10)
        },
        warning = function(e) {
          cat("W!\n")
          v.result <<- svm(v$x1 ~ v$dx + v$dy + v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1 + v$BSSID1.2 + v$BSSID2.2 + v$BSSID3.2, data=d.train, gamma=100, cost=10)
        },
        silent = TRUE
      )
    }else{
      tryCatch(
        {
          t <- tune.svm(v$y1 ~ v$dx + v$dy + v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1 + v$BSSID1.2 + v$BSSID2.2 + v$BSSID3.2, data = d.train, gamma=gammaRange, cost=costRange, tunecontrol = tune.control(sampling="cross", cross=8))
          cat("gamma:",t$best.parameters$gamma, " cost:",t$best.parameters$cost,"\n")
          v.result <<- svm(v$y1 ~ v$dx + v$dy + v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1 + v$BSSID1.2 + v$BSSID2.2 + v$BSSID3.2, data=d.train, gamma=t$best.parameters$gamma, cost=t$best.parameters$cost)
        }, 
        error = function(e) {
          cat("E!\n")
          v.result <<- svm(v$y1 ~ v$dx + v$dy + v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1 + v$BSSID1.2 + v$BSSID2.2 + v$BSSID3.2, data=d.train, gamma=100, cost=10)
        },
        warning = function(e) {
          cat("W!\n")
          v.result <<- svm(v$y1 ~ v$dx + v$dy + v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1 + v$BSSID1.2 + v$BSSID2.2 + v$BSSID3.2, data=d.train, gamma=100, cost=10)
        },
        silent = TRUE
      )
    }
  }else{
    d.test <<- x1[1:nrowmin,c(-1,-4)]
    d.train <<- x2[1:nrowmin,c(-1,-4)]
    v <<- d.train
    if(isY==0){
      tryCatch(
        {
          t <- tune.svm(v$x1 ~ v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1, data = d.train, gamma=gammaRange, cost=costRange, tunecontrol = tune.control(sampling="cross", cross=8))
          cat("gamma:",t$best.parameters$gamma, " cost:",t$best.parameters$cost,"\n")
          v.result <<- svm(v$x1 ~ v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1, data=d.train, gamma=t$best.parameters$gamma, cost=t$best.parameters$cost)
        }, 
        error = function(e) {
          cat("E!\n")
          v.result <<- svm(v$x1 ~ v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1, data=d.train, gamma=0.1, cost=1)
        },
        warning = function(e) {
          cat("W!\n")
          v.result <<- svm(v$x1 ~ v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1, data=d.train, gamma=0.1, cost=1)
        },
        silent = TRUE
      )
    }else{
      tryCatch(
        {
          t <- tune.svm(v$y1 ~ v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1, data = d.train, gamma=gammaRange, cost=costRange, tunecontrol = tune.control(sampling="cross", cross=8))
          cat("gamma:",t$best.parameters$gamma, " cost:",t$best.parameters$cost,"\n")
          v.result <<- svm(v$y1 ~ v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1, data=d.train, gamma=t$best.parameters$gamma, cost=t$best.parameters$cost)
        }, 
        error = function(e) {
          cat("E!\n")
          v.result <<- svm(v$y1 ~ v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1, data=d.train, gamma=0.1, cost=1)
        },
        warning = function(e) {
          cat("W!\n")
          v.result <<- svm(v$y1 ~ v$BSSID1.1 + v$BSSID2.1 + v$BSSID3.1, data=d.train, gamma=0.1, cost=1)
        },
        silent = TRUE
      )
    }
  }
  print(v.result)
  summary(v.result)
  v.prediction <<- predict(v.result, d.test[,c(-1,-2)])
  v.prediction <<- as.matrix(v.prediction)
  print(head(v.prediction))
  nrowmax <- max(nrow(v.prediction), nrow(d.test))
  png(paste("result/",stitle,"-",n,"_",ifelse(isY==0,"x","y"),".png",sep=""),width=720)
  if(isY==0){
    plot(1:nrow(v.prediction), v.prediction, type="b", col="red",pch=3 ,xlim=c(0,nrowmax) ,ylim=c(0,5),cex.axis=2)
    par(new=T)
    plot(1:nrow(d.test), d.test[,2], type="p", col="blue",pch=4 ,xlim=c(0,nrowmax) ,ylim=c(0,5),cex.axis=2)
  }else{
    plot(1:nrow(v.prediction), v.prediction, type="b", col="red",pch=3 ,xlim=c(0,nrowmax) ,ylim=c(0,11),cex.axis=2)
    par(new=T)
    plot(1:nrow(d.test), d.test[,1], type="p", col="blue",pch=4 ,xlim=c(0,nrowmax) ,ylim=c(0,11),cex.axis=2)
  }
  par(new=F)
  legend("topleft", c(ifelse(isY==0,"x","y"), "真の値：青", "予測値：赤"))
  dev.off()
  cat("train",nrow(d.train)," test:",nrow(d.test)," pred:",nrow(v.prediction),"\n")
  return(v.prediction);
}

main <- function(CONSOLIDATED,DFILTER,DFILTERVALUE,K,SIMIMIN,SIMIMAX,WIDE,ISTEST){
  stitle <- paste("SVM_c",CONSOLIDATED,"d",DFILTER,"v",DFILTERVALUE,"k",K,"n",SIMIMIN,"x",SIMIMAX,"w",WIDE,"i",ISTEST,"t",0,sep="")
  nowdate <- format(Sys.time(), "%Y%b%d%H%M")
  dirCreate("result")
  touch(paste("result/",stitle,"_",nowdate,".txt",sep=""))
  if(ENASINK > 0)sink(paste("result/",stitle,"_",nowdate,".txt",sep=""),append=TRUE)
  cat(nowdate,"\t","main(",CONSOLIDATED,",",DFILTER,",",DFILTERVALUE,",",K,",",SIMIMIN,",",SIMIMAX,",",WIDE,",",ISTEST,")\n\n")

  if(FF == 0){
    data0  <<- loadData(CONSOLIDATED,"_Bs_2015-0604.2.csv",DFILTER,DFILTERVALUE,WIDE,0)
    export2csv(stitle,100,"data0")
    datae1 <<- divideData(data0,0,CONSOLIDATED)
    export2csv(stitle,101,"datae1")
    datao1 <<- divideData(data0,1,CONSOLIDATED)
    export2csv(stitle,102,"datao1")
    datae2 <<- divideData(data0,2,CONSOLIDATED)
    export2csv(stitle,103,"datae2")
    datao2 <<- divideData(data0,3,CONSOLIDATED)
    export2csv(stitle,104,"datao2")

    data0t  <<- loadData(CONSOLIDATED,"_Bs_2015-0604.2.csv",DFILTER,DFILTERVALUE,WIDE,ISTEST)
    export2csv(stitle,105,"data0t")
    datae1t <<- divideData(data0t,0,CONSOLIDATED)
    export2csv(stitle,106,"datae1t")
    datao1t <<- divideData(data0t,1,CONSOLIDATED)
    export2csv(stitle,107,"datao1t")
    datae2t <<- divideData(data0t,2,CONSOLIDATED)
    export2csv(stitle,108,"datae2t")
    datao2t <<- divideData(data0t,3,CONSOLIDATED)
    export2csv(stitle,109,"datao2t")
  }else{
    data0  <<- importFcsv(stitle,100,"data0")
    datae1 <<- importFcsv(stitle,101,"datae1")
    datao1 <<- importFcsv(stitle,102,"datao1")
    datae2 <<- importFcsv(stitle,103,"datae2")
    datao2 <<- importFcsv(stitle,104,"datao2")
    colnames(data0) <- c()
    colnames(datae1) <- c()
    colnames(datao1) <- c()
    colnames(datae2) <- c()
    colnames(datao2) <- c()
  }
  alarm()
  if(FF == 0){
    knncx <<- setML(CONSOLIDATED,stitle,201,datao2t,datao2,0)
    export2csv(stitle,201,"knncx")
    knncy <<- setML(CONSOLIDATED,stitle,202,datao2t,datao2,1)
    export2csv(stitle,202,"knncy")
    knnc <<- cbinddiflen(knncy,knncx)
    colnames(knnc) <- c("knncy", "knncx")
    export2csv(stitle,211,"knnc")
    knnex <<- setML(CONSOLIDATED,stitle,203,datao2t,datae2,0)
    export2csv(stitle,203,"knnex")
    knney <<- setML(CONSOLIDATED,stitle,204,datao2t,datae2,1)
    export2csv(stitle,204,"knney")
    knne <<- cbinddiflen(knney,knnex)
    colnames(knne) <- c("knney", "knnex")
    export2csv(stitle,212,"knne")
    knnox <<- setML(CONSOLIDATED,stitle,205,datae2t,datao2,0)
    export2csv(stitle,205,"knnox")
    knnoy <<- setML(CONSOLIDATED,stitle,206,datae2t,datao2,1)
    export2csv(stitle,206,"knnoy")
    knno <<- cbinddiflen(knnoy,knnox)
    colnames(knno) <- c("knnoy", "knnox")
    export2csv(stitle,213,"knno")
  }else{
    knnc <<- importFcsv(stitle,211,"knnc")
    knne <<- importFcsv(stitle,212,"knne")
    knno <<- importFcsv(stitle,213,"knno")
    colnames(knnc) <- c()
    colnames(knne) <- c()
    colnames(knno) <- c()
  }

  alarm()

  if(FF == 0){
    errorDisc <<- errorDis(knnc,datao2,-1)
    export2csv(stitle,501,"errorDisc")
    errorDise <<- errorDis(knne,datao2,-1)
    export2csv(stitle,502,"errorDise")
    errorDiso <<- errorDis(knno,datae2,-1)
    export2csv(stitle,503,"errorDiso")
  }else{
    errorDisc <<- importFcsv(stitle,501,"errorDisc")
    errorDise <<- importFcsv(stitle,502,"errorDise")
    errorDiso <<- importFcsv(stitle,503,"errorDiso")
    colnames(errorDisc) <- c()
    colnames(errorDise) <- c()
    colnames(errorDiso) <- c()
  }
  alarm()
  if(FF == 0){
    errorTotalc <<- try( errorTotalizationHeat(stitle,601,errorDisc,1) , silent = FALSE)
    export2csv(stitle,601,"errorTotalc")
    #export2png4heat(stitle,601,"errorTotalc",2,"Error Distance Frequency","","Error Distance[m]")
    errorTotale <<- try( errorTotalizationHeat(stitle,602,errorDise,1) , silent = FALSE)
    export2csv(stitle,602,"errorTotale")
    #export2png4heat(stitle,602,"errorTotale",2,"Error Distance Frequency","","Error Distance[m]")
    errorTotalo <<- try( errorTotalizationHeat(stitle,603,errorDiso,1) , silent = FALSE)
    export2csv(stitle,603,"errorTotalo")
    #export2png4heat(stitle,603,"errorTotalo",2,"Error Distance Frequency","","Error Distance[m]")
  }else{
    errorTotalc <<- importFcsv(stitle,601,"errorTotalc")
    errorTotale <<- importFcsv(stitle,602,"errorTotale")
    errorTotalo <<- importFcsv(stitle,603,"errorTotalo")
    colnames(errorTotalc) <- c()
    colnames(errorTotale) <- c()
    colnames(errorTotalo) <- c()
  }
  alarm()
  if(FF == 0){
    errorTotalLineAverc <<- errorTotalizationLineAver(errorDisc)
    export2csv(stitle,701,"errorTotalLineAverc")
    errorTotalLineAvere <<- errorTotalizationLineAver(errorDise)
    export2csv(stitle,702,"errorTotalLineAvere")
    errorTotalLineAvero <<- errorTotalizationLineAver(errorDiso)
    export2csv(stitle,703,"errorTotalLineAvero")
  }else{
    errorTotalLineAverc <<- importFcsv(stitle,701,"errorTotalLineAverc")
    errorTotalLineAvere <<- importFcsv(stitle,702,"errorTotalLineAvere")
    errorTotalLineAvero <<- importFcsv(stitle,703,"errorTotalLineAvero")
    colnames(errorTotalLineAverc) <- c()
    colnames(errorTotalLineAvere) <- c()
    colnames(errorTotalLineAvero) <- c()
  }
  alarm()
  if(FF == 0){
    errorTotalLineFreqc <<- errorTotalizationLineFreq(errorTotalc)
    export2csv(stitle,801,"errorTotalLineFreqc")
    errorTotalLineFreqe <<- errorTotalizationLineFreq(errorTotale)
    export2csv(stitle,802,"errorTotalLineFreqe")
    errorTotalLineFreqo <<- errorTotalizationLineFreq(errorTotalo)
    export2csv(stitle,803,"errorTotalLineFreqo")
  }else{
    errorTotalLinec <<- importFcsv(stitle,801,"errorTotalLinec")
    errorTotalLinee <<- importFcsv(stitle,802,"errorTotalLinee")
    errorTotalLineo <<- importFcsv(stitle,803,"errorTotalLineo")
    colnames(errorTotalLinec) <- c()
    colnames(errorTotalLinee) <- c()
    colnames(errorTotalLineo) <- c()
  }
  alarm()
  cat("--------------------------------------------------\n\n\n\n")
  if(ENASINK > 0)sink()
}

#####

CONSOLIDATED <- 1
DFILTER <- 0
DFILTERVALUE <- 0
K <- 10
SIMIMIN <- 1
SIMIMAX <- 3
WIDE <- 0
ISTEST <- 2

# for(CONSOLIDATED in 1:2){
#   for(WIDE in 0:2){
#     for(ISTEST in 0:2){ #1:通常; 2:DR誤差; 3:一部欠損
#       main(CONSOLIDATED,0,0,0,0,0,WIDE,ISTEST)
#     }
#   }
# }

main(2,0,0,0,0,0,1,1)
main(2,0,0,0,0,0,1,2)
main(2,0,0,0,0,0,1,3)
main(2,0,0,0,0,0,2,2)

alarm()
alarm()
alarm()
