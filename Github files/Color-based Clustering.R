rm(list=ls())
library(jpeg)
library("colorspace")

fileName = "pure maltose.jpg"
wd = paste0("/Users/tian/Downloads/Github files") # Change it to your directory
setwd(wd)

  img = readJPEG(fileName)
  img = img[1:1870,,]  # Remove legend bar
  dd = dim(img)
  colorColumns = array(img, c(dd[1]*dd[2],3))
  x <- RGB(c(img[,,1]),c(img[,,2]),c(img[,,3]))
  z <- as(x, "HSV")
  hsvIimage = coords(z)
  
  jpeg(paste0(substr(fileName,1,gregexpr(".jpg",fileName)[[1]][1]-1),"_res.jpeg"),width = 800*4, height = 800*4, res=100*4)
  
  xlocation = rep(1:dd[2],each = dd[1]); ylocation = rev(rep(1:dd[1],times = dd[2]))
  
  # Histogram
  h_interval =  seq(0,360,length=10);s_interval = seq(0,1,by=0.01);v_interval = seq(0,1,by=0.01)
  #hh = hist(hsvIimage[,1],breaks=h_interval,main="Histogram of H",xlab="",ylab="Frequency",plot=FALSE)
  
  tt.n = cut(hsvIimage[,1],hh$breaks,include.lowest = T)
  counttable = table(tt.n)
  # Make sure the No. of H value between 200-280 < 50%
  if(sum(counttable[c(6,7)])/sum(counttable) < 0.5) {
    # Blue group
    tt.lab = names(counttable[c(6,7)])
    blue_h_ID =  which(tt.n == tt.lab[1] | tt.n == tt.lab[2])
    blue_center = colMeans(colorColumns[blue_h_ID,])
  } else { # if the No. of H value between 200-280 is larger than 50%, take a smaller interval (250-260)
    tt.n = cut(hsvIimage[,1],seq(0,360,by=10),include.lowest = T)
    counttable = table(tt.n)
    tt.lab = names(counttable[c(26)])
    blue_h_ID =  which(tt.n == tt.lab)
    blue_center = colMeans(colorColumns[blue_h_ID,])
  }
 
  # The rest groups
  # Remove blue
  restImage = hsvIimage[-blue_h_ID,]
  restImageRGB = colorColumns[-blue_h_ID,]
  tt.rest = cut(restImage[,1],seq(0,360,length=10),include.lowest = T)
  counttable.rest = table(tt.rest)
  rest.lab = names(counttable.rest[counttable.rest/sum(counttable.rest) > 0.01])
  
  rest_center=NULL
  for(gg in 1:length(rest.lab)){
    rest_h_ID =  which(tt.rest == rest.lab[gg])
    rest_center = rbind(rest_center,colMeans(restImageRGB[rest_h_ID,]))
  }
  centers = rbind(blue_center,rest_center)
  
  # Start making plots
  np = nrow(centers)
  if(np <= 6){
    layout(matrix(c(1,1,3,3,5,8,1,1,3,3,5,8,1,1,3,3,6,9,2,2,4,4,6,9,2,2,4,4,7,10,2,2,4,4,7,10),6,6))
  } else if(np <= 8){
    layout(matrix(c(1,1,3,3,5,9,1,1,3,3,6,10,2,2,4,4,7,11,2,2,4,4,8,12),6,4))
  } else if(np <= 12){
    layout(matrix(c(1,1,3,3,5,8,1,1,3,3,5,8,1,1,3,3,6,9,2,2,4,4,6,9,2,2,4,4,7,10,2,2,4,4,7,10,11,12,13,14,15,16),6,7))
  }

  # Raw image
  par(mar=c(0,0,3,0))
  plot(xlocation,ylocation , col = rgb(colorColumns), asp = 1, pch =".",axes=F,xlab='',ylab='',main="Raw Image")
  
  # Histogram
  par(mar=c(3, 4, 3, 2))
  hist(hsvIimage[,1],breaks=h_interval,main="Histogram of H",xlab="",ylab="Frequency")
  
  # K-means 
  kc<-kmeans(colorColumns,centers) 
  tt = table(kc$cluster)
  
  # Pick up the blue cluster (LAB color space, B<0)
  bb <- RGB(kc$centers[,1],kc$centers[,2],kc$centers[,3])
  lab_centers <- coords(as(bb, "LAB"))
  blueCluster = which(lab_centers[,"B"] < c(-15))
  brightCluster = which(lab_centers[,"L"] == max(lab_centers[,"L"] ))
  
  
  # Pick up the bright cluster (sum of RGB is large)
  
  ## If there is overlap between blue and bright clusters, remove the one in blue
  inter = intersect(blueCluster,brightCluster)
  if(length(inter) > 0){ blueCluster = blueCluster[blueCluster!=inter] }
  ## End remove clusters
  
  par(mar=c(0,0,3,0))
  clustercolors = kc$centers[kc$cluster,]
  plot(xlocation, ylocation, col = rgb(clustercolors), asp = 1, pch =".",axes=F,xlab='',ylab='',main="Segmented Image")
  keepid = list()
  for(cc in 1:nrow(centers)){
    keepid[[cc]] = which(kc$cluster == cc)
  }
  ## Barplot for color clusters
  par(mar=c(3,4,3,2))
  bb=barplot(as.numeric(tt),col = rgb(kc$centers),names.arg = names(tt),main="Proportion of each cluster")
  #mtext(side=1,at=c(1:length(tt)),paste(round(tt/sum(tt),2)*100,"%"))
  text(x = bb, y = as.numeric(tt)-min(tt)/3,paste(round(tt/sum(tt),2)*100,"%"), adj=0.5)
  
  ## Segmented Images
  par(mar=c(0,0,0,0))
  for(ic in 1:nrow(centers)){
    plot(xlocation[keepid[[ic]]], ylocation[keepid[[ic]]], col = rgb(colorColumns)[keepid[[ic]]], asp = 1, pch =".",axes=F,xlab='',ylab='')
    mtext(paste("Cluster",ic),side=2,line = -2)
  }
  
  dev.off()
  



