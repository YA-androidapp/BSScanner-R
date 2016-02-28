# Copyright (c) 2013-2016 YA <ya.androidapp@gmail.com> All rights reserved.
setwd("C:/Users/Public/Documents/R")
ENASINK <- 1 #sink()
FF <- 0 #from File
install.packages("TeachingDemos", dependencies = TRUE)
install.packages("fields", dependencies = TRUE)
install.packages("kernlab", dependencies = TRUE)
install.packages("randomForest", dependencies = TRUE)
library(TeachingDemos)
library(fields)
library(kernlab)
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

nextX <- function(dire,x1,wide){
  if((dire == 2) ||(dire == 1) ||(dire == 8)){
    return( x1 + ifelse(wide>1,2,1) )
  }else if((dire == 3) ||(dire == 0) ||(dire == 7)){
    return( x1 )
  }else if((dire == 4) ||(dire == 5) ||(dire == 6)){
    return( x1 - ifelse(wide>1,2,1) )
  }
  return(-1)
}

nextY <- function(dire,y1,wide){
  if((dire == 2) ||(dire == 3) ||(dire == 4)){
    return( y1 + ifelse(wide>0,2,1) )
  }else if((dire == 1) ||(dire == 0) ||(dire == 5)){
    return(y1)
  }else if((dire == 8) ||(dire == 7) ||(dire == 6)){
    return( y1 - ifelse(wide>0,2,1) )
  }
  return(-1)
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
          y2 <- nextY(d2,y1,wide)
          x2 <- nextX(d2,x1,wide)

          y1 <- sprintf21(y1)
          x1 <- sprintf21(x1)
          y2 <- sprintf21(y2)
          x2 <- sprintf21(x2)

          name1 <- paste("X",x1,"Y",y1,"N",sprintf20(n1),"Z",sep = "")
          name2 <- paste("X",x2,"Y",y2,"N",sprintf20(n2),"Z",sep = "")

          cat("i =",i," addpos =",addpos," j =",j," n2 =",n2," d2 =",d2," = ",name1,"<-",name2,"\n")

          if(length(csvData[name2 == csvData$NAME1,"NAME1"]) > 0){
            cat(" *** length(csvDataSearched$NAME1) > 0 ***")
            y[addpos,"NAME1"] <- name1
            y[addpos,"y1"] <- as.numeric(y1)
            y[addpos,"x1"] <- as.numeric(x1)
            y[addpos,"n1"] <- as.integer(csvData[j,"n1"])
            y[addpos,"BSSID1.1"] <- csvData[j,"BSSID1.1"]
            y[addpos,"BSSID2.1"] <- csvData[j,"BSSID2.1"]
            y[addpos,"BSSID3.1"] <- csvData[j,"BSSID3.1"]

            y[addpos,"d2"] <- as.integer(d2)
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
    return(z)
  }else{
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
      index <- setdiff(colnames(y),c("NAME1","y1","x1","n1","d2","NAME2","y2","x2","n2"))
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
      png(paste("result/",stitle,"-",n,"/",n,"_",j,".png",sep=""),width=720)
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

cosineSimilarity <- function(t1,t2){
  return(1 -(sum(t1 * t2) /(sqrt(sum(t1*t1)) * sqrt(sum(t2*t2)))))
}

euclideanDistance <- function(t1,t2){
  return(sqrt(sum((t1-t2)^2)))
}

pearsonCoefficient <- function(t1,t2){
  v <- t1 - mean(t1)
  w <- t2 - mean(t2)
  return(1 -((sum(v*w)) /(sqrt(sum(v*v)) * sqrt(sum(w*w)))))
}

vSimilarity <- function(similarityIndex,x1,x2,xx1,xx2,dfltr){
  y <- matrix(0,nrow=nrow(x1),ncol=nrow(x2))
  for(i in 1:nrow(x1)){
    for(j in 1:nrow(x2)){
      #cat(paste("x1[i,]:",x1[i,]," x2[i,]:",x2[j,]))
      t1 <- as.matrix(as.numeric(x1[i,]))
      t2 <- as.matrix(as.numeric(x2[j,]))
      tt1 <- xx1[i,"d2"]
      tt2 <- xx2[j,"d2"]

      if(((dfltr > 0) &&(tt1 == tt2)) ||(dfltr == 0)){
        if(similarityIndex == 1){
          y[i,j] <- cosineSimilarity(t1,t2)
        }else if(similarityIndex == 2){
          y[i,j] <- euclideanDistance(t1,t2)
        }else if(similarityIndex == 3){
          y[i,j] <- pearsonCoefficient(t1,t2)
        }
      }else{
        y[i,j] <- 10000
      }
      #cat("vSimilarity: y[",i,",",j,"]:",y[i,j],"\n")
    }
  }
  return(y)
}

sortCossim <- function(x1,x2){
  y <- matrix(10000,nrow=nrow(x1),ncol=ncol(x1))
  for(i in 1:nrow(x1)){
    t1 <- x1[i,]
    t2 <- order(t1)
    for(j in 1:ncol(x1)){
      y[i,j] <- as.character(x2[t2[j],"NAME1"])
    }
  }
  
  return(y)
}

statmode <- function(x){
  r <- rev(sort(table(x)))
  n <- names(r)
  r <- as.character(r)
  m <- grep(r,pattern=r[1])
  lengthM <- length(m)
  if(lengthM  == 1){
    return(names(which.max(table(x))))
  }else{
    p <- numeric(lengthM)
    for(i in 1:lengthM){
      p[i] <-(1:length(x))[x == n[m[i]]][1]
    }
    o <- order(p)
    num <- which(abs(o) == min(abs(o)))
    return(x[p[num]])
  }
}

vKnn <- function(x,k){
  y <- matrix(0,nrow=nrow(x),ncol=k)
  for(i in 1:nrow(x)){
    t <- x[i,]
    for(j in 1:k){
      y[i,j] <- statmode(t[1:j])
    }
  }
  
  return(y)
}

vKnnDFiltering <- function(x1,x2,x3,k){
 y <- matrix(0,nrow=nrow(x2),ncol=k)
 addpos <- 1
 for(i in 1:nrow(x2)){
  if(x2[i,"d2"] == x3){
   y[addpos,] <- x1[i,]
   addpos <- addpos + 1
  }
 }
 z <- y[1:addpos-1,]

 return(z)
}

main <- function(CONSOLIDATED,DFILTER,DFILTERVALUE,K,SIMIMIN,SIMIMAX,WIDE){
  stitle <- paste("PM_c",CONSOLIDATED,"d",DFILTER,"v",DFILTERVALUE,"k",K,"n",SIMIMIN,"x",SIMIMAX,"w",WIDE,"t",0,sep="")
  nowdate <- format(Sys.time(), "%Y%b%d%H%M")
  dirCreate("result")
  touch(paste("result/",stitle,"_",nowdate,".txt",sep=""))
  if(ENASINK > 0)sink(paste("result/",stitle,"_",nowdate,".txt",sep=""),append=TRUE)
  cat(nowdate,"\t","main(",CONSOLIDATED,",",DFILTER,",",DFILTERVALUE,",",K,",",SIMIMIN,",",SIMIMAX,",",WIDE,")\n\n")

  if(FF == 0){
    data0  <<- loadData(CONSOLIDATED,"306_.csv",DFILTER,DFILTERVALUE,WIDE)
    export2csv(stitle,100,"data0")
    datae1 <<- divideData(data0,0,CONSOLIDATED)
    export2csv(stitle,101,"datae1")
    datao1 <<- divideData(data0,1,CONSOLIDATED)
    export2csv(stitle,102,"datao1")
    datae2 <<- divideData(data0,2,CONSOLIDATED)
    export2csv(stitle,103,"datae2")
    datao2 <<- divideData(data0,3,CONSOLIDATED)
    export2csv(stitle,104,"datao2")
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

  for(SIMITYPE in SIMIMIN:SIMIMAX){
    stitle <- paste("c",CONSOLIDATED,"d",DFILTER,"v",DFILTERVALUE,"k",K,"n",SIMIMIN,"x",SIMIMAX,"w",WIDE,"t",SIMITYPE,sep="")

    if(FF == 0){
      vecsimc <<- vSimilarity(SIMITYPE,datao1,datao1,datao2,datao2,DFILTER)
      export2csv(stitle,201,"vecsimc")
      export2png4heat(stitle,201,"vecsimc",0,"Distance (Similarity)","Test Data(o)","Training Data(o)")
      vecsime <<- vSimilarity(SIMITYPE,datao1,datae1,datao2,datae2,DFILTER)
      export2csv(stitle,202,"vecsime")
      export2png4heat(stitle,202,"vecsime",0,"Distance (Similarity)","Test Data(o)","Training Data(e)")
      vecsimo <<- vSimilarity(SIMITYPE,datae1,datao1,datae2,datao2,DFILTER)
      export2csv(stitle,203,"vecsimo")
      export2png4heat(stitle,203,"vecsimo",0,"Distance (Similarity)","Test Data(e)","Training Data(o)")
    }else{
      vecsimc <<- importFcsv(stitle,201,"vecsimc")
      vecsime <<- importFcsv(stitle,202,"vecsime")
      vecsimo <<- importFcsv(stitle,203,"vecsimo")
      colnames(vecsimc) <- c()
      colnames(vecsime) <- c()
      colnames(vecsimo) <- c()
    }
    alarm()
    if(FF == 0){
      rankc <<- sortCossim(vecsimc,datao2)
      export2csv(stitle,301,"rankc")
      ranke <<- sortCossim(vecsime,datae2)
      export2csv(stitle,302,"ranke")
      ranko <<- sortCossim(vecsimo,datao2)
      export2csv(stitle,303,"ranko")
    }else{
      rankc <<- importFcsv(stitle,301,"rankc")
      ranke <<- importFcsv(stitle,302,"ranke")
      ranko <<- importFcsv(stitle,303,"ranko")
      colnames(rankc) <- c()
      colnames(ranke) <- c()
      colnames(ranko) <- c()
    }
    alarm()
    if(FF == 0){
      vecknnc <<- vKnn(rankc,K)
      export2csv(stitle,401,"vecknnc")
      vecknne <<- vKnn(ranke,K)
      export2csv(stitle,402,"vecknne")
      vecknno <<- vKnn(ranko,K)
      export2csv(stitle,403,"vecknno")
    }else{
      vecknnc <<- importFcsv(stitle,401,"vecknnc")
      vecknne <<- importFcsv(stitle,402,"vecknne")
      vecknno <<- importFcsv(stitle,403,"vecknno")
      colnames(vecknnc) <- c()
      colnames(vecknne) <- c()
      colnames(vecknno) <- c()
    }
    alarm()
    if(DFILTER > 0){
      if(FF == 0){
        vecknncD2 <<- vKnnDFiltering(vecknnc,datae2,DFILTERVALUE,K)
        export2csv(stitle,401,"vecknncD2")
        vecknneD2 <<- vKnnDFiltering(vecknne,datao2,DFILTERVALUE,K)
        export2csv(stitle,402,"vecknneD2")
        vecknnoD2 <<- vKnnDFiltering(vecknno,datae2,DFILTERVALUE,K)
        export2csv(stitle,403,"vecknnoD2")
      }
      alarm()
    }
    if(FF == 0){
      errorDisc <<- errorDis(vecknnc,datao2,K)
      export2csv(stitle,501,"errorDisc")
      export2png4heat(stitle,501,"errorDisc",0,"Error Distance[m]","K","Test Data(e)")
      errorDise <<- errorDis(vecknne,datao2,K)
      export2csv(stitle,502,"errorDise")
      export2png4heat(stitle,502,"errorDise",0,"Error Distance[m]","K","Test Data(o)")
      errorDiso <<- errorDis(vecknno,datae2,K)
      export2csv(stitle,503,"errorDiso")
      export2png4heat(stitle,503,"errorDiso",0,"Error Distance[m]","K","Test Data(e)")
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
      errorTotalc <<- errorTotalizationHeat(stitle,601,errorDisc,K)
      export2csv(stitle,601,"errorTotalc")
      export2png4heat(stitle,601,"errorTotalc",2,"Error Distance Frequency","K","Error Distance[m]")
      errorTotale <<- errorTotalizationHeat(stitle,602,errorDise,K)
      export2csv(stitle,602,"errorTotale")
      export2png4heat(stitle,602,"errorTotale",2,"Error Distance Frequency","K","Error Distance[m]")
      errorTotalo <<- errorTotalizationHeat(stitle,603,errorDiso,K)
      export2csv(stitle,603,"errorTotalo")
      export2png4heat(stitle,603,"errorTotalo",2,"Error Distance Frequency","K","Error Distance[m]")
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
      export2png4line(stitle,701,"errorTotalLineAverc","Error Distance Average","K","Error Distance[m]")
      errorTotalLineAvere <<- errorTotalizationLineAver(errorDise)
      export2csv(stitle,702,"errorTotalLineAvere")
      export2png4line(stitle,702,"errorTotalLineAvere","Error Distance Average","K","Error Distance[m]")
      errorTotalLineAvero <<- errorTotalizationLineAver(errorDiso)
      export2csv(stitle,703,"errorTotalLineAvero")
      export2png4line(stitle,703,"errorTotalLineAvero","Error Distance Average","K","Error Distance[m]")
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
      export2png4linef(stitle,801,"errorTotalLineFreqc","Error Distance Frequency (Match)","K","Frequency")
      errorTotalLineFreqe <<- errorTotalizationLineFreq(errorTotale)
      export2csv(stitle,802,"errorTotalLineFreqe")
      export2png4linef(stitle,802,"errorTotalLineFreqe","Error Distance Frequency (Match)","K","Frequency")
      errorTotalLineFreqo <<- errorTotalizationLineFreq(errorTotalo)
      export2csv(stitle,803,"errorTotalLineFreqo")
      export2png4linef(stitle,803,"errorTotalLineFreqo","Error Distance Frequency (Match)","K","Frequency")
    }else{
      errorTotalLinec <<- importFcsv(stitle,801,"errorTotalLinec")
      errorTotalLinee <<- importFcsv(stitle,802,"errorTotalLinee")
      errorTotalLineo <<- importFcsv(stitle,803,"errorTotalLineo")
      colnames(errorTotalLinec) <- c()
      colnames(errorTotalLinee) <- c()
      colnames(errorTotalLineo) <- c()
    }
    alarm()
  }
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

#main(1,0,0,10,1,3,0)

main(2,1,1,30,1,1,2)
main(2,1,3,30,1,1,2)

#main(2,0,0,30,1,1,0)

alarm()
alarm()
alarm()
