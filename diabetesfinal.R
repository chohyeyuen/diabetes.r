rm(list = ls())
data=read.csv(file.choose(),header=T)
attach(data)
names(data)
summary(data)

###############logistic regression


#changing characters to factors
data$Gender = as.factor(data$Gender)
data$Polyuria = as.factor(data$Polyuria)
data$Polydipsia= as.factor(data$Polydipsia)
data$sudden.weight.loss= as.factor(data$sudden.weight.loss)
data$weakness = as.factor(data$weakness)
data$Polyphagia= as.factor(data$Polyphagia)
data$Genital.thrush= as.factor(data$Genital.thrush)
data$visual.blurring= as.factor(data$visual.blurring)
data$Itching= as.factor(data$Itching)
data$Irritability= as.factor(data$Irritability)
data$delayed.healing= as.factor(data$delayed.healing)
data$partial.paresis= as.factor(data$partial.paresis)
data$muscle.stiffness= as.factor(data$muscle.stiffness)
data$Alopecia= as.factor(data$Alopecia)
data$Obesity= as.factor(data$Obesity)
data$class= as.factor(data$class)

summary(data)

glm.fit1=glm(class~Age+Gender+Polyuria+Polydipsia+sudden.weight.loss+weakness+Polyphagia+Genital.thrush+visual.blurring+Itching+Irritability+delayed.healing+partial.paresis+muscle.stiffness+Alopecia+Obesity,family="binomial",data=data)
summary(glm.fit1)
coef(glm.fit1)
exp(coef(glm.fit1)) 

set.seed(1)
train=sample(nrow(data),nrow(data)*0.8)
data.test=data[-train, ]
test.truevalue=class[-train]
summary(train)

glm.fit2=glm(class~Age+Gender+Polyuria+Polydipsia+sudden.weight.loss+weakness+Polyphagia+Genital.thrush+visual.blurring+Itching+Irritability+delayed.healing+partial.paresis+muscle.stiffness+Alopecia+Obesity,data=data,subset=train,family =binomial)
summary(glm.fit2)
exp(coef(glm.fit2))

glm.probs2=predict(glm.fit2,data.test, type="response")
glm.pred2=rep("Negative",104)
glm.pred2[glm.probs2>.5]="Positive"

table(glm.pred2,test.truevalue)
mean(glm.pred2==test.truevalue)
mean(glm.pred2!=test.truevalue)

k=5
folds=sample(1:k,nrow(data),replace=TRUE)

accuracy=rep(0,k)
for(i in 1:k)
{
  glm.fit3=glm(class~Age+Gender+Polyuria+Polydipsia+sudden.weight.loss+weakness+Polyphagia+Genital.thrush+visual.blurring+Itching+Irritability+delayed.healing+partial.paresis+muscle.stiffness+Alopecia+Obesity,family="binomial",data=data[folds!=i,])
  data.test=data[folds==i, ]
  glm.probs3 =predict(glm.fit3,data.test, type="response")
  glm.pred3=rep("Negative",nrow(data[folds==i,]))
  glm.pred3[glm.probs3>.5]="Positive"
  
  test.truevalue=class[folds==i]
  accuracy[i]=mean(glm.pred3==test.truevalue)
}

mean(accuracy)




#####tree
library(tree)
tree.model=tree(class~.,data,subset=train)
cv.model=cv.tree(tree.model,K=5,FUN=prune.misclass)
cv.model

#3 or 4 terminal nodes results in the lowest cv error
prune.model=prune.tree(tree.model,best=8)
plot(prune.model)
text(prune.model,pretty=0)


prunetree.pred=predict(prune.model,data.test,type="class") 
table(prunetree.pred,test.truevalue)

library(randomForest)
set.seed(1)
rf.data=randomForest(class~.,data=data, subset=train, mtry=4,importance=TRUE)
yhat.rf=predict(rf.data,newdata=data[-train,])


new.rf=predict(rf.data,newdata=(26+0+1+1+0+0+1+0+1+0+1+0+0+0+0+0))

set.seed(1)
train=sample(nrow(data),nrow(data)*0.8)
data.test=data[-train, ]
test.truevalue=class[-train]

table(yhat.rf,test.truevalue)

importance(rf.data)
varImpPlot(rf.data)



#diabetes<- 2.74660+ my.age*-0.05117+my.gender*-4.35118+my.polyuria*4.43954+my.polydipsia*5.07044+my.sudden_weight_loss*0.19033+my.weakness*0.81707+my.polyphagia*1.19377+my.genital_thrush*1.86365+my.visual_blurring*0.91587+my.itching*-2.80293+my.irritability*2.34073+my.delayed_healing*-0.39163+my.partial_paresis*1.15930+my.muscle_stiffness*-0.72876+my.alopecia*0.15036+my.obesity*-0.28904
