Prediction Assignment
================
Rodolpho Talaisys Bernabel
July 12, 2016

Data Wrangling
--------------

``` r
rm(list=ls())

library(plyr)
library(caret)

setwd("C:/Users/Rodolpho/Downloads")

set.seed(1)

training <- read.csv("pml-training.csv", header=T)
testing <- read.csv("pml-testing.csv",header=T)
testing <- subset(testing, select = - X)
testing <- subset(testing, select = - user_name)
testing <- subset(testing, select = - problem_id)
testing <- testing[ , ! apply( testing , 2 , function(x) all(is.na(x)) ) ]
classe <- training$classe
training <- cbind(classe, training[,names(testing)])
```

Partitioning the data
---------------------

``` r
inTrain <- createDataPartition(y=training$classe,p=.7,list=F)
trainSet <- training[inTrain,]
validationSet <- training[-inTrain,]
dim(trainSet)
```

    ## [1] 13737    58

``` r
dim(validationSet)
```

    ## [1] 5885   58

Fitting the Linear Discriminant Analysis Model
----------------------------------------------

Note that the explanatory variables included in the model were selected in the data munging.

``` r
modlda <- train(classe~.,data=trainSet,method="lda")
```

Checking the Accuracy
---------------------

``` r
modlda
```

    ## Linear Discriminant Analysis 
    ## 
    ## 13737 samples
    ##    57 predictor
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 13737, 13737, 13737, 13737, 13737, 13737, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.8535766  0.8148474
    ## 
    ## 

Validating the Model
--------------------

``` r
plda <- predict(modlda,validationSet)
table(plda,validationSet$classe)
```

    ##     
    ## plda    A    B    C    D    E
    ##    A 1525  140    3    0    0
    ##    B  123  828   76    0    0
    ##    C   26  166  914  103    2
    ##    D    0    5   33  807   91
    ##    E    0    0    0   54  989

``` r
validationSet$predright <- plda==validationSet$classe
summary(validationSet$predright)
```

    ##    Mode   FALSE    TRUE    NA's 
    ## logical     822    5063       0

Error Rate in the Validation Set
--------------------------------

``` r
1 - (5000/5885)
```

    ## [1] 0.1503823

The error rate was consistent with the accuracy found. Therefore, I expect an accuracy of little less than 0.85 in the test set; and an error rate around 0.15.

Predicting the Test Cases
-------------------------

When checked with the quiz, the model yielded a 0.9 accuracy rate.
