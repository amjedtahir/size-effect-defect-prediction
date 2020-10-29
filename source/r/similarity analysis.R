#author: Xun Xiao (x.xiao@massey.ac.nz)

dir<-'~/size-effect-in-bug-prediction/datasets' #set the correct directory

setwd(dir)
dir1<-paste0(getwd(), "/DAMB")
dir2<-paste0(getwd(), "/JURE")
dir3<-paste0(getwd(), "/KC1")
library(dplyr)
library(readr)
library(MASS)
setwd(dir1)
 list_file1 <- list.files(pattern = "*.csv") %>%
   lapply(read.csv, stringsAsFactors=F)

setwd(dir2)
list_file2 <- list.files(pattern = "*.csv") %>%
  lapply(read.csv, stringsAsFactors=F)

fulldata2<-cbind(group=rep(2,nrow(list_file2[[1]])),index=rep(6,nrow(list_file2[[1]])),list_file2[[1]][,-c(1:3,12:13,15:23)])

for (i in 2:length(list_file2)){
  colnames(list_file2[[i]])<-colnames(list_file2[[1]])
  fulldata2<- rbind(fulldata2,cbind(group=rep(2,nrow(list_file2[[i]])),index=rep(i+5,nrow(list_file2[[i]])),list_file2[[i]][,-c(1:3,12:13,15:23)]))
}
colnames(fulldata2)

colnames(list_file1[[1]])[24:25]<-colnames(fulldata2)[12:13]
fulldata1<-cbind(group=rep(1,nrow(list_file1[[1]])),index=rep(1,nrow(list_file1[[1]])),list_file1[[1]][,-c(1,11:23)])

for (i in 1:length(list_file1)){
  colnames(list_file1[[i]])<-colnames(list_file1[[1]])
  fulldata1<- rbind(fulldata1,cbind(group=rep(2,nrow(list_file1[[i]])),index=rep(i,nrow(list_file1[[i]])),list_file1[[i]][,-c(1,11:23)]))
}
fulldata1<-fulldata1[,colnames(fulldata2)]
fulldata<-rbind(fulldata1,fulldata2)

K<-23

N<-nrow(fulldata)
fit.error.binary<-numeric(K)
pred.error.binary<-numeric(K)
fit.error.count<-numeric(K)
pred.error.count<-numeric(K)
average.buggy<-numeric(K)
average.bug<-numeric(K)
pred.naive.binary<-numeric(K)
fit.simple.binary<-numeric(K)
pred.simple.binary<-numeric(K)
p<-mean(fulldata$buggy)
threshold<-0.5
naivaprob<-p
p.value<-matrix(0,nrow=K,ncol=17)
i=13
for(i in 1:K){
  testdata<-fulldata[fulldata$index==i,]
  traindata<-fulldata[fulldata$index!=i,]
  n<-nrow(testdata)
  naivaprob<-mean(traindata$buggy)
  threshold<-mean(traindata$buggy)
  #threshold<-0.5
  average.buggy[i]<-mean(testdata$buggy)
  average.bug[i]<-mean(testdata$bug)
  model.binary <- glm(buggy ~ (.-index-group-bug-loc)*loc, data = traindata, family = binomial)
  simple.binary <- glm(buggy ~ loc, data = traindata, family = binomial)
  fit.error.binary[i]<-mean((1*(fitted.values(model.binary)>threshold)-traindata$buggy)^2)
  fit.simple.binary[i]<-mean((1*(fitted.values(simple.binary)>threshold)-traindata$buggy)^2)
  pred.error.binary[i]<-mean((1*(predict(model.binary,newdata=testdata, type = "response")>threshold)-testdata$buggy)^2)
  pred.simple.binary[i]<-mean((1*(predict(simple.binary,newdata=testdata, type = "response")>threshold)-testdata$buggy)^2)
  pred.naive.binary[i]<-mean((rbinom(n,1,naivaprob)-testdata$buggy)^2)
  model.binary2 <- glm(buggy ~ (.-index-group-bug-loc)*loc, data = testdata, family = binomial)
  if (i!=13&&i!=18&&i!=19&&i!=21&&i!=22&&i!=23) {
    p.value[i,]<-coef(summary(model.binary2, complete = TRUE))[, 4][-1]

    print(i)

   }
  #model.count <- glm( bug   ~ (.-index-group-buggy-loc)*loc, data = traindata, family = poisson)
  #fit.error.count[i]<-mean((fitted.values(model.count)-traindata$buggy)^2)
  #pred.error.count[i]<-mean((predict(model.count,newdata=testdata, type = "response")-testdata$bug)^2)
}
colnames(p.value)<-names(coef(summary(model.binary))[, 4][-1])
p.value.s<-p.value<0.10
which.max(average.buggy)
plotdata<-cbind(average.buggy,fit.error.binary,pred.error.binary,fit.simple.binary,pred.simple.binary,pred.naive.binary)
plotdata<-plotdata[order(plotdata[,1]),]


p.value.s<-p.value.s[-c(13,18,19,21,22,23),]
SimMat<-matrix(0,K-6,K-6)

for(i in 1:(K-6)){
  for(j in 1:(K-6)){
    SimMat[i,j]<-sum(abs(p.value.s[i,]-p.value.s[j,]))
  }
}
table(SimMat)
library(gplots)
breaks = 0:max(SimMat)+0.5
gradient1 = colorpanel( max(SimMat), low="white", high="red" )

hm.colors = gradient1

labelsys<-c('JDT','MYLYN','PDE','EQUINOX','LUCENE',
            'ANT','IVY','JEDIT','LOG4J','POI','PROP',
            'SYNAPSE','TOMCAT','VELOCITY',
            'XALAN','XERCES','ARC','PDFTRANSLATOR',
            'REDAKTOR','SERAPION','SYSTEMDATAMANGT',
            'TERMOPROJEKT','WORKFLOW')

pdf(file = "similarity_matrix.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 7)

heatmap.2(x =SimMat,breaks=breaks,col=hm.colors,Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote =SimMat, notecol = "black", notecex = 1,
          trace = "none", key = FALSE,  colsep=c(0:(N+1)),
          rowsep=c(0:(N+1)),sepcolor='black',lwid=c(0.1,5), lhei=c(0.1,5),margins = c(6, 6),
          labRow = labelsys[-c(13,18,19,21,22,23)],
          labCol = labelsys[-c(13,18,19,21,22,23)])
dev.off()
fulldata[fulldata$index==16,]
fulldata[fulldata$index==15,]
SimMat/17

mean(pred.naive.binary)/17

pdf(file = "prediction_errors.pdf",   # The directory you want to save the file in
    width = 7, #  width of the plot in inches
    height = 6) #  height of the plot in inches

matplot(sort(average.buggy),plotdata[,2:5],type='o',
        ylab='Prediction Errors', main=' ',xlab='Mean Defect-Proneness of Systems',
        col=c(1,1,2,2),pch=c(1,2,3,4),lty=c(1,2,1,2),cex=0.5,xlim=c(0,1),ylim=c(0,1),lwd=1)
legend('topleft',
       legend=c('Training Error - Universal Model',
                'Testing Error - Universal Model','Training Error - Simple Model','Testing Error - Simple Model'),
       col=c(1,1,2,2),pch=c(1,2,3,4),lty=c(1,2,1,2),cex=0.5,lwd=1)

dev.off()
