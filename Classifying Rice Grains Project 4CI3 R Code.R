
### Read in CSV###
Rice <- read.csv("Rice_Osmancik_Cammeo_Dataset.csv")

###Explore Data###
head(Rice)
str(Rice)

### Change CLASS to a Categorical Variable
Rice$Class <-as.factor(Rice$Class)
str(Rice)

### No. of observations 
nrow(Rice)
ncol(Rice)

### Summary
summary(Rice)

#### Standard Deviations

sd(Rice$Area)
sd(Rice$MinorAxis)
sd(Rice$MajorAxis)
sd(Rice$Perimeter)
sd(Rice$Extent)
sd(Rice$Eccentricity)
sd(Rice$ConvexArea)

library(dplyr)
library(ggplot2)
library(GGally)

### Pairs Plot
ggpairs(Rice, aes(colour=Class, alpha=0.4)) + theme(
  legend.position="Default", plot.title = element_text(size=5,hjust = 0.5),
  text = element_text(size = 5))

### Correlation Matrix

Cor_Matrix <- cor(Rice[,-8])

#### GPCM Analysis

library(mixture)
library(e1071)


### Scale the data
x<-scale(Rice[,-8])
x_Matrix<- as.matrix(x)
head(x_Matrix)


##Run gpcm

#### Try Pure Clustering, Try G between 1 and 5, R does not pick the correct number of clusters

Rice_gpcm_Clust <-gpcm(x, G=1:5, atol=1e-2)
Rice_gpcm_Clust
summary(Rice_gpcm_Clust)

### Specify 2 clusters, default start K means

Rice_gpcm<-gpcm(x_Matrix, G=2, atol=1e-2)
Rice_gpcm

##Positive Scale, Bigger BIC is better.
summary(Rice_gpcm)

### Pairs Plot
pairs(x,col=Rice_gpcm$map)

### ARI and % of correct Classification
tab<-as.matrix(table(Rice[,8],Rice_gpcm$map))
classAgreement(tab)
1-classAgreement(tab)$diag
classAgreement(tab)$crand

###Random Forrest
library(e1071)
library(nnet)
library(class)
library(randomForest)
##scale data
Rice[,-8]<-scale(Rice[,-8])

##Set seed to create training set 80% training, 20% hold-out
set.seed(8000)
train<-sample(1:3810,3048)


##Set seed tune model: perform Cross Validation
set.seed(8000)
Rice_rf = tune.randomForest(Class~., data = Rice[train,], 
                            mtry = 1:7,ntree=100*1:5,
                            tunecontrol = tune.control
                            (sampling = "cross",cross=5))
summary(Rice_rf)
plot(Rice_rf)

## Train with Mtry = 4 and ntrees = 100
set.seed(8000)
rf.Rice<-randomForest(Class~.,data=Rice,
                      subset=train,mtry=4,
                      ntree=100,
                      importance=TRUE,type="class")
rf.Rice

### Test Model on hold-out Set
set.seed(8000)
Rice.pred=predict(rf.Rice,Rice[-train,],type="class")
Rice.pred
tab_rf <- table(Rice[-train,8],Rice.pred)
tab_rf
classAgreement(tab_rf)
1-classAgreement(tab_rf)$diag
classAgreement(tab_rf)$crand


