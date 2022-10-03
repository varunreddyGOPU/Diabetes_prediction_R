# Read the dataset present in the 'data' folder



diabetes=read.csv("D:/saturn/S51_Diabetes/diabetes_data.csv",header=F,sep=",")
# Set the appropriate column names
colnames(diabetes)=c("pregnant","glucose","bp","triceps","insulin","bmi","pedigree","age","outcome")
names(diabetes)
## to check missing values
is.na(diabetes)
colSums(is.na(diabetes))
str(diabetes)
dim(diabetes)
summary(diabetes)

###splitting the diabetes into training and testing
##Building the model

##install.packages('caTools')
library(caTools)

set.seed(1000)
split=sample.split(diabetes$outcome,SplitRatio = 0.5)
split
table(split)

training=subset(diabetes,split==TRUE)
training
test=subset(diabetes,split==FALSE)
nrow(training)
nrow(test)

##building logistic regression model with training diabetesset
###logistic regression model is knowm as generalised linear model
names(diabetes)

log_reg=glm(outcome~.,data = training,family = 'binomial')
log_reg

###null devience==bydefault system will through error without checking independent variabla
##residual deviance==with independent variable
#nulldevience>=residual deviance
###AIC==r^2==akike information criteria
summary(log_reg)

##predict the model with test diabetes set
pred=predict(log_reg,newdiabetes=test)##,type=responce in case if you use output to be binomial
pred
##which is a probablity value so build athreshold value
pred=ifelse(pred>0.5,1,0)


pred

cbind=cbind(test$outcome,pred)
cbind
###check accuracy
cm=table(test$outcome,pred)
cm

accuracy=(209+29)/(209+41+105+29)
accuracy

spec=29/(29+41)
spec

sen=209/(209+105)
sen

##threshold=60%

pred=ifelse(pred>0.6,1,0)
pred
cm=table(test$outcome,pred)
cm

###else if need to remove non significant variable ,but still in logistic it doesnot necessary
#log_reg=glm(Outcome~.-SkinThickness-Insulin-Age,diabetes=training)
#log_reg
#summary(log_reg)
#pred=predict(log_reg,newdiabetes=test)
#pred=ifelse(pred>=0.6,1,0)
#pred
#table(test$Outcome,pred)


#accuracy=(119+24)/(119+43+6+24)
#accuracy
names(diabetes)
#pred=write.csv(pred, "fhjvdgf.csv")
##pred

##calculating confusion matrix
##install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)



confusionMatrix(cm)

x=table(diabetes$age)  
x
mean(diabetes$glucose)
  
names(diabetes)

