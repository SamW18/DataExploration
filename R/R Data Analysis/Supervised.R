library(ggplot2)
library(MASS)

tmp= MASS::Boston
View(tmp)

working_data= aggregate(tmp$medv, by = list(tmp$chas), FUN=mean) 
working_data 
ggplot(working_data, aes(x=as.factor(Charles), y=Median_Value)) + geom_bar(stat="identity") +geom_text(aes(label=paste("$",floor(Median_Value))))

split= .75 
smp_size= floor(split *nrow(Boston))
set.seed(123)  
train_ind= sample(seq_len(nrow(Boston)), size=smp_size) 

train = Boston[train_ind,]
test= Boston[-train_ind, ] 

attach(train) 
sam.lm.fit= lm(medv~chas)
test$sam_test= data.frame(predict(sam.lm.fit,data.frame(chas~test$chas))) 
plot(test$medv, test$sam_test) 



### Classification Problem  
data(BreastCancer, package= "mlbench")

#Only keep complete rows
bc= BreastCancer[complete.cases(BreastCancer),]  

#Remove ID column 
bc= bc[,-1]

#finding structure of the dataframe 
str(bc)

#convert to numeric 
for (i in 1:9) 
{
  bc[,i]= as.numeric(as.character(bc[,i]))
}

#Change y values to 1's and 0's 
bc$Class= ifelse(bc$Class== "malignant", 1,0)
bc$Class= factor(bc$Class, levels=c(0,1))

library(caret)
#Define function- "not contained in"
'%ni%' = Negate('%in%')
options(scipen=999)
set.seed(100)
trainDataIndex=createDataPartition(bc$Class, p=.7, list= F)
trainDataIndex
trainData= bc[trainDataIndex,]
testData= bc[-trainDataIndex,]  

table(trainData$Class)
 
#Down Sample 
set.seed(100)
down_train= downSample(x=trainData[,colnames(trainData)[1:9]], y= trainData$Class)

View(down_train)

#Build Logistic Model 
logitmod = glm(Class~ Cl.thickness+  Cell.size + Cell.shape, family= "binomial", data= down_train) 

#Predict Function 
pred= predict(logitmod, newdata= testData, type= "response") 
pred
y_pred_num = ifelse(pred > .5, 1,0) 
y_pred= factor(y_pred_num, levels=c(0,1))
 
y_act= testData$Class
mean(y_pred==y_act)
confusionMatrix(y_pred,y_act)
