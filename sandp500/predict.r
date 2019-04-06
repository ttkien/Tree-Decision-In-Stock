
# Loading the required libraries
library(quantmod) ; library(TTR);

data = read.csv("~/Desktop/DWH/sandp500/all_stocks_5yr.csv")
data = data[data$Name == "AAL",]
colnames(data) = c("Date","Open","High","Low","Close","Volume", "Name", "Change")

closeprice = data$Close

# Computing RSI
rsi = round(RSI(closeprice, n = 14, maType="WMA"),1);
rsi = c(NA,head(rsi,-1))

# Computing SMA and LMA
ShMA = 20; LMA = 50;

sma = round(SMA(closeprice, ShMA),1)
sma = c(NA,head(sma,-1))

lma = round(SMA(closeprice, LMA),1)
lma = c(NA,head(lma,-1))

# Computing ADX 
#data22 = ADX(data[,c("High","Low","Close")])
#data22 = as.data.frame(data22)
#adx = round(data22$ADX,1)
#adx = c(NA,head(adx,-1))

# Create the label 
class = character(nrow(data))
class = data$Change

data2 = data.frame(data,class,rsi,sma,lma)
write.csv(data2,file="decision tree charting_table.csv")

data = data.frame(class,rsi,sma,lma)
data = na.omit(data)

write.csv(data,file="decision tree charting.csv")


library("quantmod")
library("rpart")
library("rpart.plot")

df = read.csv("decision tree charting.csv")
df = df[,-1]
colnames(df) = c("Class","RSI","SMA","LMA")

trainingSet<-df[1:500,]
testSet<-tail(df, 5)

DecisionTree<-rpart(Class~RSI+SMA+LMA,data=trainingSet, cp=0.01)

prp(DecisionTree,type=2,extra=8)
table(predict(DecisionTree,testSet,type="class"),testSet[,1],dnn=list('predicted','actual'))
predict(DecisionTree,testSet,type="class")

