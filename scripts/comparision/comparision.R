#author: Xun Xiao (x.xiao@massey.ac.nz)
#similarity analysis between systems based on a 
# distance metric and generate a heatmap with correlation and hierarchal clustering

# see the associated paper “Does class size matter? 
#An in-depth assessment of the effect of class size in software defect prediction” (Section 4.4) 
# for more details of this analysis



library(dplyr)
library(readr)
library(MASS)
library(gplots)

#setwd(".")
#getwd()

dir <- "/cloud/project/data"
setwd(dir)

dir1<-paste0(getwd(), "/DAMB")
dir2<-paste0(getwd(), "/JURE")
#setwd(dir1)

setwd(dir1)
list_file1 <- list.files(pattern = "*.csv") %>% 
  lapply(read.csv, stringsAsFactors=F) 

setwd(dir2)
list_file2 <- list.files(pattern = "*.csv") %>% 
  lapply(read.csv, stringsAsFactors=F) 

fulldata2<-cbind(group=rep(2,nrow(list_file2[[1]])),index=rep(6,nrow(list_file2[[1]])),list_file2[[1]][,-c(1:3,6,12:13,15:23)]) 

for (i in 2:length(list_file2)){
  #colnames(list_file2[[i]])<-colnames(list_file2[[1]])
  fulldata2<- rbind(fulldata2,cbind(group=rep(2,nrow(list_file2[[i]])),index=rep(i+5,nrow(list_file2[[i]])),list_file2[[i]][,-c(1:3,6,12:13,15:23)]))
}
colnames(fulldata2)

fulldata1<-cbind(group=rep(1,nrow(list_file1[[1]])),index=rep(1,nrow(list_file1[[1]])),list_file1[[1]][,-c(1,10:22)]) 
colnames(fulldata1)
for (i in 1:length(list_file1)){
  #colnames(list_file1[[i]])<-colnames(list_file1[[1]])
  fulldata1<- rbind(fulldata1,cbind(group=rep(2,nrow(list_file1[[i]])),index=rep(i,nrow(list_file1[[i]])),list_file1[[i]][,-c(1,10:22)]))
}

fulldata1<-fulldata1[,colnames(fulldata2)]
fulldata<-rbind(fulldata1,fulldata2)



X<-as.matrix(fulldata)
corlist<-list()
for(i in 1:max(X[,2])){
  systemmatrix<-as.data.frame(X[X[,2]==i,-c(1,2,12)])
  if (i==9) systemmatrix <- systemmatrix[-c(336:339),]
  if (i==11) systemmatrix <- systemmatrix[-c(200:205),]
  systemmatrixlog<-systemmatrix %>% mutate(loc=log(loc+1)) %>% mutate(bug=log(bug+1))
  corlist[[i]]<-cor(systemmatrix)
}

pairwisedistance<-matrix(0,23,23)
for(i in 1:23){
  for(j in 1:23){
    sqdiffmatrix<-abs(corlist[[i]]-corlist[[j]])
    diag(sqdiffmatrix)<-NA
    pairwisedistance[i,j]<-mean(sqdiffmatrix,na.rm=TRUE)
  }
}
diag(pairwisedistance)<-0

pairwisedistance
heatmap(pairwisedistance)
setwd(dir1)
list.files(pattern = "*.csv") 

setwd(dir2)
list.files(pattern = "*.csv") 

labelsys<-c('JDT','Equinox','Lucene','Mylyn','PDE',
            'Ant','ARC','Ivy','JEdit','Log4J', 'PDFTranslator','POI','Prop',
            'Redaktor','Serapion','Synapse','SystemDataMangt', 'TermoProjekt', 'Tomcat',
            'Velocity','WorkFlow',
            'Xalan','Xerces' )

heatmap(pairwisedistance)
heatmap(pairwisedistance,labRow = labelsys,
        labCol = labelsys, col=paste("Grey",1:99,sep=""),
        cexRow = 0.9, cexCol = 0.9, margins=c(12,8))
