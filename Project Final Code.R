##########################################################################
# W4201 Final PRoject: Prediction of Online News Popularity
##########################################################################

### Read data
OnlineNewsPopularity <- read.csv("OnlineNewsPopularity.csv")

### Divide all variables into 10 sorts
links=c('num_hrefs','num_self_hrefs','self_reference_min_shares','self_reference_max_shares',
        'self_reference_avg_sharess')
kw1 = c('kw_min_min','kw_max_min','kw_avg_min','kw_min_max','kw_max_max','kw_avg_max',
     'kw_min_avg','kw_max_avg','kw_avg_avg')
kw2 = c('num_keywords')
kw3 = c('data_channel_is_lifestyle','data_channel_is_entertainment','data_channel_is_bus',
        'data_channel_is_socmed','data_channel_is_tech','data_channel_is_world') #categorical
other = c('LDA_00','LDA_01','LDA_02','LDA_03','LDA_04','url')
Language=c('global_subjectivity','global_sentiment_polarity', 
           'global_rate_positive_words','global_rate_negative_words',
           'rate_positive_words','rate_negative_words',
           'avg_positive_polarity','min_positive_polarity',
           'max_positive_polarity','avg_negative_polarity',
           'min_negative_polarity','max_negative_polarity',
           'title_subjectivity', 'title_sentiment_polarity',
           'abs_title_subjectivity', 'abs_title_sentiment_polarity')
words=c('n_tokens_title','n_tokens_content','n_unique_tokens','n_non_stop_words',
        'n_non_stop_unique_tokens','average_token_length')
media=c('num_imgs','num_videos')
time1=c('weekday_is_monday','weekday_is_tuesday','weekday_is_wednesday','weekday_is_thursday',
        'weekday_is_friday','weekday_is_saturday') #categorical
time2=c('weekday_is_sunday',"is_weekend")

###################### EDA #######################
### Detect whether exist missing values in origin data
anyNA(OnlineNewsPopularity) ### no missing

### Plot 
attach(OnlineNewsPopularity)
summary(shares)
par(mfrow=c(1,3))
plot(shares,xlab="")
boxplot(shares)
hist(shares,prob=T)
lines(density(shares),col="blue")


############## Classification ###################

news=OnlineNewsPopularity[,-c(1,2)] 
cato=c(12:17,30:37)      ### catogarical variables 
y=ifelse(news$shares>1400,1,0) ### set sample median (1400) as treshold,1-popular,0-not popular 
y=as.factor(y) 
news_n=scale(news[,-c(59,cato)],center=T,scale=T)  ###center and scale data
news1=data.frame(y,news[,cato],news_n)

### Feature selection (5 Rounds)
library(Boruta)
set.seed(1)
Boruta.Ozone=Boruta(y~.,data=news1, doTrace=2, ntree=50,maxRuns=15)  
### Choose variables "confirmed" by model
features=names(news1)[(which(Boruta.Ozone$finalDecision=="Confirmed"))]  
news_1=news1[,c("y",features)]

set.seed(1)
Boruta.Ozone2=Boruta(y~.,data=news_1, doTrace=2, ntree=50,maxRuns=15)
features2=names(news1)[(which(Boruta.Ozone2$finalDecision=="Confirmed"))]
news_2=news1[,c("y",features2)]

set.seed(1)
Boruta.Ozone3=Boruta(y~.,data=news_2, doTrace=2, ntree=50,maxRuns=15)
features3=names(news1)[(which(Boruta.Ozone3$finalDecision=="Confirmed"))]
news_3=news1[,c("y",features3)]

set.seed(1)
Boruta.Ozone4=Boruta(y~.,data=news_3, doTrace=2, ntree=50,maxRuns=15)
features4=names(news1)[(which(Boruta.Ozone4$finalDecision=="Confirmed"))]
news_4=news1[,c("y",features4)]

set.seed(1)
Boruta.Ozone5=Boruta(y~.,data=news_4, doTrace=2, ntree=50,maxRuns=15)
features5=names(news1)[(which(Boruta.Ozone5$finalDecision=="Confirmed"))]
news_5=news1[,c("y",features5)]

### Variables(predictors) we decided to use finally
final_features=c("y",kw3,"is_weekend","n_tokens_title", "n_non_stop_unique_tokens",
                 "num_hrefs","num_self_hrefs","num_imgs","num_videos",
                 "average_token_length","num_keywords","kw_avg_min",
                 "kw_avg_max","kw_avg_avg","self_reference_avg_sharess",
                 "LDA_00","LDA_01","LDA_02")

### The processed data we have, including 39644 observations, 22 predictors and 1 response
news1=news1[,final_features]

### Divide data into train set and test set (train 90% /test 10%)
n=dim(news1)
set.seed(1)
train_index=sample(n[1],floor(n[1]*0.90),replace=F)
test_index=seq(1:n[1])[-train_index]
news.train=news1[train_index,]
news.test=news1[test_index,]

### Fuction of 5-folds cross validation
train.fold=list()
test.fold=list()
l=floor(nrow(news.train)/5)
for(i in 1:5){
    test.fold[[i]]=news.train[((i-1)*l+1):(i*l),]
    train.fold[[i]]=news.train[-(((i-1)*l+1):(i*l)),]
}

### Function of accuracy
accuracy=function(table){
  a.rate=vector()
  a.rate[1]=(table[1,1]+table[2,2])/sum(table)
  a.rate[2]=table[2,2]/(table[2,2]+table[1,2])
  a.rate[3]=table[1,1]/(table[1,1]+table[2,1])
  names(a.rate)=c("Accuracy","Tp rate","Tn rate")
  return(a.rate)
}

### Method1: logistic classification
mod0=glm(y~.,data = news.train, family=binomial)
t0=table(ifelse(predict(mod0,news.test[,-1],type="response")>0.5,1,0),news.test[,1])
accuracy(t0)

### Method2: LDA classification
library(MASS)
### Use moment to estimate the mean and variance
mod1_1=lda(y~.,data=news.train,prior=c(0.5,0.5))
t1_1=table(predict(mod1_1,news.test[,-1])$class,news.test[,1])
accuracy(t1_1)

### Illustrate
Means=mod1_1$means
Means_0=Means[1,]
Means_1=Means[2,]

### Plot
plot(Means_0,col="blue",pch=16,type="b",lty=3,
     main="Comparing the Sample Means of Two Classes",xlab="Features",ylab="Sample Means after Scaling",
     xlim=c(1,23))
points(Means_1,col="red",pch=17,type="b",lty=1)
abline(v=22,lty=3)
abline(v=18,lty=3)
abline(v=15,lty=3)
abline(v=12,lty=3)
abline(v=10,lty=3)
abline(v=6,lty=3)
legend("topright",legend=c("not popular","popular"),col=c("blue","red"),lty=c(3,1))
text(x=6,y=0.13,label=colnames(Means)[6],cex=0.8,font=2)
text(x=10,y=-0.1,label=colnames(Means)[10],cex=0.8,font=2)
text(x=12,y=0.09,label=colnames(Means)[12],cex=0.8,font=2)
text(x=15,y=-0.09,label=colnames(Means)[15],cex=0.8,font=2)
text(x=18,y=0.18,label=colnames(Means)[18],cex=0.8,font=2)
text(x=22,y=0.18,label=colnames(Means)[22],cex=0.8,font=2)
colnames(Means)[c(6,10,12,15,18,22)]

### Use robust estimates based on t distribution
mod1_2=lda(y~.,data=news.train,prior=c(0.5,0.5),method="t")
t1_2=table(predict(mod1_2,news.test[,-1])$class,news.test[,1])
accuracy(t1_2)

### Method3: KNN classificaion
library(FNN)
### Cross validation to choose best K from(1,5,10,50,100,200,300,400,500,600)
cv.knn=vector()
j=1
for(k in c(1,5,10,50,100,200,300,400,500,600)){
  mod=list()
  temp=vector()
  for(i in 1:5){
    mod[[i]]=knn(train.fold[[i]][,-1],test.fold[[i]][,-1],train.fold[[1]][,1],prob=T,k)
    t=table(mod[[i]],test.fold[[i]][,1])
    temp[i]=(t[1,2]+t[2,1])/sum(t)
  }
  cv.knn[j]=mean(temp)
  j=j+1
}
cv.knn ### return cv result(error rate), choose the smallest one

### Plot the result of cross validation
color=colorRampPalette(c("red","royalblue"))
plot(c(1:10),cv.knn,main="Cross-Validation on kNN",xlab="k",ylab="Validation Error",
     pch=20,type="b",col=color(10),cex=2,xaxt="n",cex.lab=1.2)
axis(side=1,at=c(1:10),label=c("1","5","10","50","100","200","300","400","500","600"))

### Use best K to do classification
mod2=knn(news.train[,-1],test=news.test[,-1],news.train[,1],prob=T,k=300)
t2=table(mod2,news.test[,1])
accuracy(t2)

### Method4: SVM classification
library(e1071)
Mod3=list()
t3=list()
ac.svm=vector()
j=1
### Process Svm with different parameters(Cost=(0.1,1,10,100))
for(i in c(0.1,1,10,100)){
set.seed(1)
Mod3[[j]]=svm(y~.,data=news.train,type="C-classification",kernel="radial",cost=i,torlerance=0.1)
t3[[j]]=table(predict(Mod3[[j]],news.test[,-1],type="class"),news.test[,1])
ac.svm[j]=accuracy(t3[[j]])
j=j+1
}
ac.svm ### Choose the model have the highest accuracy(cost=10)
mod3=Mod[[3]]

### Method5: Adaboost classification
library(adabag)
Mod4=list()
t4=list()
ac.ada=vector()
j=1
### Process Adaboost with different iterations(10,20,50,100,200,300)
for(i in c(10,20,50,100,200,300)){
  set.seed(1)
  Mod4[[j]]=boosting(y~.,data=news.train,boos=T,mfinal=i)
  t4[[j]]=table(predict(Mod4[[j]],news.test[,-1])$class,news.test[,1])
  ac.ada[j]=accuracy(t4[[j]])
  j=j+1
}
ac.ada ### Choose the model have the highest accuracy (ntree=100)
mod4=Mod[[4]]


### Plot first four weighted tree classifiers
par(mfrow=c(2,2))
tree=mod4$trees[[1]]
plot(tree,main="1st Important Classifier",margin=0.2)
text(tree,font=2)

tree2=mod4$trees[[2]]
plot(tree2,main="2nd Important Classifier",margin=0.1)
text(tree2,font=2)

tree3=mod4$trees[[3]]
plot(tree3,main="3rd Important Classifer",margin=0.1)
text(tree3,font=2)

tree4=mod4$trees[[4]]
plot(tree4,main="4th Important Classifier",margin=0.2)
text(tree4,font=2)

### Importance of adaboost
par(mfrow=c(1,1))
plot(mod4$importance,main="Relative Importance of Each Variable",xlab="Features",
     ylab="Importance",pch=20,cex=2,col=color(22),xaxt="n")
segments(c(1:22),mod4$importance,y1=-5, col="black")

text(x=3,y=11,label=colnames(Means)[3],cex=0.8,font=2)
text(x=8,y=15.5,label=colnames(Means)[8],cex=0.8,font=2)
text(x=9,y=27,label=colnames(Means)[9],cex=0.8,font=2)
text(x=14,y=10.5,label=colnames(Means)[14],cex=0.8,font=2)
text(x=22,y=13.5,label=colnames(Means)[22],cex=0.8,font=2)

### Method6: Random Forest classification
library(randomForest)
cv.rf=vector()
j=1
for(k in c(10,20,50,100,200,300)){
  mod=list()
  temp=vector()
  for(i in 1:5){
    mod[[i]]=randomForest(y~.,data=train.fold[[i]],importance=T,ntrees=k)
    t=table(predict(mod[[i]],test.fold[[i]][,-1],type="class"),test.fold[[i]][,1])
    temp[i]=(t[1,2]+t[2,1])/sum(t)
  }
  cv.rf[j]=mean(temp)
  j=j+1
}
cv.rf ### return cv result(error rate), choose the smallest one

### Plot
plot(c(1:6),cv.rf,main="Cross-Validation on RF",xlab="n.trees",ylab="Validation Error",
     pch=20,type="b",col=color(6),cex=2,xaxt="n",cex.lab=1.2)
axis(side=1,at=c(1:6),label=c("10","20","50","100","200","300"))

### Choose best nTree to do Classification
set.seed(1)
mod5=randomForest(y~.,data=news.train,importance=T,ntree=200)
t5=table(predict(mod5,news.test[,-1],type="class"),news.test[,1])
accuracy(t5)

##importance of adaboost
varImpPlot(mod5,sort=T,main="Relative Importance of Each Variable",pch=20)

### ROC curve and AUC value 
library(ROCR)
label.test=news.test[,1]

### Plot ROC curve of all models
prob0=predict(mod0,news.test[,-1],type="response")
pred0=prediction(prob0,label.test)
perf0=performance(pred0,measure = "tpr",x.measure="fpr")
plot(perf0,lty=2,lwd=2,xlim=c(0.035,0.965),ylim=c(0.035,0.965),main="ROC plot for Logistic ")
abline(coef=c(0,1),lwd=1)


prob1=predict(mod1_1,news.test[,-1],type="prob")$posterior[,2]
pred1=prediction(prob1,label.test)
perf1=performance(pred1,measure = "tpr",x.measure="fpr")
plot(perf1,lty=2,lwd=2,xlim=c(0.035,0.965),ylim=c(0.035,0.965),main="ROC plot for LDA")
abline(coef=c(0,1),lwd=1)

prob2=attr(mod2,"prob")
prob2=2*ifelse(mod2==0, 1-prob2, prob2)-1 
pred2=prediction(prob2, label.test)
perf2=performance(pred2, measure="tpr",x.measure="fpr") 
plot(perf2,lty=2,lwd=2,xlim=c(0.035,0.965),ylim=c(0.035,0.965),main="ROC plot for KNN") # ROC
abline(coef=c(0,1),lwd=1)

mod3_ba=svm(y~.,data=news.train,type="C-classification",kernel="radial",cost=10,torlerance=0.1,probability=T)
prob3=attr(predict(mod3_ba,news.test[,-1], probability=T),"probabilities")[,2]
pred3=prediction(prob3, label.test)
perf3=performance(pred3, measure = "tpr", x.measure = "fpr") 
plot(perf3,lty=2,lwd=2,xlim=c(0.035,0.965),ylim=c(0.035,0.965),main="ROC plot for SVM") # ROC
abline(coef=c(0,1),lwd=1)

prob4=predict(mod4,news.test[,-1],type="prob")$prob[,2]
pred4=prediction(prob6, label.test)
perf4=performance(pred6, measure = "tpr", x.measure = "fpr") 
plot(perf4,lty=2,lwd=2,xlim=c(0.035,0.965),ylim=c(0.035,0.965),main="ROC plot for ADAboost") # ROC
abline(coef=c(0,1),lwd=1)

prob5=predict(mod5,news.test[,-1],type="prob")[,2]
pred5=prediction(prob5,label.test)
perf5=performance(pred5,measure = "tpr",x.measure="fpr") 
plot(perf5,lty=2,lwd=2,xlim=c(0.035,0.965),ylim=c(0.035,0.965),main="ROC plot for RadomForest") # ROC
abline(coef=c(0,1),lwd=1)

###Calculate AUC
AUC=vector()
AUC[1]=performance(pred0,"auc")@y.values
AUC[2]=performance(pred1,"auc")@y.values
AUC[3]=performance(pred2,"auc")@y.values
AUC[4]=performance(pred3,"auc")@y.values
AUC[5]=performance(pred4,"auc")@y.values
AUC[6]performance(pred25,"auc")@y.values
AUC

###Compare ROC curve of best model and worst
prob5=predict(mod5,news.test[,-1],type="prob")[,2]
pred5=prediction(prob5,label.test)
perf5=performance(pred5,measure = "tpr",x.measure="fpr") 
plot(perf5,lty=1,lwd=2,xlim=c(0.035,0.965),ylim=c(0.035,0.965),main="ROC plot for Best and Worst Model") # ROC
abline(coef=c(0,1),lwd=1)
par(new=T)

prob1=predict(mod1_1,news.test[,-1],type="prob")$posterior[,2]
pred1=prediction(prob1,label.test)
perf1=performance(pred1,measure = "tpr",x.measure="fpr")
plot(perf1,lty=2,lwd=2,xlim=c(0.035,0.965),ylim=c(0.035,0.965))
