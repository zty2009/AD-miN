setwd('C:/Users/zty20/Documents/R/AD_mirna/data')
x=read.table('gene_mirna_few.txt')
truenames=as.matrix(which(x[,633]==1))
library(e1071)
library(pROC)
train=x[x[,633]==1,1:632]
label=x[,633]

pp=NULL
result=NULL
pl=NULL
for (i in truenames){

svm.model<-svm(train,y=NULL,
               type='one-classification',
               nu=0.313,
              scale=TRUE,
               kernel="polynomial")			   
svm.predtest<-predict(svm.model,x[,1:632])
a=which(svm.predtest==TRUE)

predict=matrix(0,nrow(x),1)
predict[a,]=1
pl=c(pl,predict)

	
	
}
la=rep(label,length(truenames))
roc1<-roc(la,pl)
	auc1=roc1$auc
plot(roc1)
svm.model<-svm(train,y=NULL,
               type='one-classification',
               nu=0.313,
              scale=TRUE,
               kernel="polynomial")			   
svm.predtest<-predict(svm.model,x[,1:632])
a=which(svm.predtest==TRUE)
