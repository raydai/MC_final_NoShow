############################################# packages #############################################
# devtools::install_github('topepo/caret/pkg/caret')
#install.packages("caret")
# install.packages("mlr")
library("tidyverse")       # data manipulation
library("foreach")         # pararelle computation
library("doParallel")      # pararelle computation
library("mlr")             # create Dummy feature
library("caret")
########################## My referference population ##############################
s.data<-data.frame("Chart"=c(rep("VisitType",5),
                             rep("TimeToAppmt",3),
                             rep("AppmtMonth",12),
                             rep("AppmtWeekday",6),
                             rep("AppmtTime",2),
                             rep("CountPriorNoShow",6),
                             rep("Age",4),
                             rep("CountyToClinic",3),
                             rep("Race",5),
                             rep("Gender",2),
                             rep("NumHousehold",3),
                             rep("DistToClinic",4),
                             rep("PrimaryInsurance",4),
                             rep("MainInsurHolder",5),
                             rep("TotalInsurCarriers",2)),
                   "Variable"=c("NewPatient","ReturnVisit","Newborn","NewPatient_wellChildExam","ReturnVisit_WellChildExam",
                                "day_1","weeks_1to2","MoreThan2Weeks",
                                "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",
                                "Mon","Tue","Wed","Thr","Fri","Sat",
                                "Morning","Afternoon",
                                "NoHist_firstVisit","None","Once","Twice","ThreeAndFour","MoreThan4",
                                "Month18","Year5","Year10","MoreThan10",
                                "Inside","Adjacent","Other",
                                "Asian","AfrAmerican","Hispanic","Caucasian","Other",
                                "Male","Female",
                                "lessOr3","FourOrFive","MoreThan5",
                                "less5Miles","FiveTo10Miles","TenTo20Miles","MoreThan20Miles",
                                "BlueCross","Medicaid_Medicare","HMO","Other",
                                "Parents","Spouse","Grandparent","Patient","Other",
                                "One","MoreThan1"),
                   "n1"=c(1129,56426,1558,946,24869,
                          39159,22628,23141,
                          7502,7458,8015,7068,7027,6451,6341,7089,6987,7485,6851,6654,
                          19396,14980,16196,14847,15209,4300,
                          42237,42691,
                          7114,42090,18106,8703,6405,2510,
                          23197,20255,19453,22023,
                          70087,14477,364,
                          2830,4480,831,68193,8594,
                          44436,40492,
                          33014,33709,18205,
                          38957,35016,8153,2802,
                          46003,4001,20670,14254,
                          67514,305,306,16562,241,
                          79001,5927),
                   "n2"=c(267,12413,380,302,6509,
                          7255,5764,6852,
                          1682,1682,1904,1595,1607,1597,1420,1747,1622,1684,1688,1643,
                          4797,3716,3478,3753,3076,1051,
                          9547,10324,
                          874,7255,4757,2843,2612,1530,
                          5089,4182,4761,5839,
                          16272,3547,52,
                          595,1246,240,16160,1630,
                          10197,9674,
                          7901,7898,4072,
                          9839,7921,1729,382,
                          12524,1222,3857,2268,
                          14261,69,43,5480,18,
                          18329,1542),stringsAsFactors = FALSE)
s.data<-s.data%>%transform("Total"=n1+n2)%>%transform(prop=Total/ave(Total, Chart,FUN=sum))   # caculate the proportion under each type

# Generate the no-show varible based on the chacteristics from the simulated datasets.
coef.table<-data.frame("Predictor"=c("intercept","VisitType.ReturnVisit", "VisitType.ReturnVisit_WellChildExam", 
                                     "VisitType.NewPatient", "VisitType.NewPatient_wellChildExam", 
                                     "TimeToAppmt.day_1","TimeToAppmt.MoreThan2Weeks", 
                                     "AppmtMonth.Jan", "AppmtMonth.Feb", "AppmtMonth.Mar", "AppmtMonth.May", "AppmtMonth.Jun", 
                                     "AppmtMonth.Jul", "AppmtMonth.Aug", "AppmtMonth.Sep", "AppmtMonth.Oct", 
                                     "AppmtMonth.Nov", "AppmtMonth.Dec",
                                     "AppmtWeekday.Mon","AppmtWeekday.Tue", "AppmtWeekday.Wed","AppmtWeekday.Thr","AppmtWeekday.Sat", 
                                     "Afternoon",
                                     "CountPriorNoShow.NoHist_firstVisit","CountPriorNoShow.None","CountPriorNoShow.Once","CountPriorNoShow.Twice","CountPriorNoShow.MoreThan4",  
                                     "Age.Month18", "Age.Year5", "Age.Year10", 
                                     "CountyToClinic.Inside", "CountyToClinic.Other",
                                     "Race.Asian","Race.Hispanic","Race.Caucasian", "Race.Other",  
                                     "Female", 
                                     "NumHousehold.lessOr3", "NumHousehold.FourOrFive", 
                                     "DistToClinic.less5Miles","DistToClinic.FiveTo10Miles", "DistToClinic.MoreThan20Miles", 
                                     "PrimaryInsurance.Medicaid_Medicare","PrimaryInsurance.HMO", "PrimaryInsurance.Other", 
                                     "MainInsurHolder.Parents", "MainInsurHolder.Spouse", "MainInsurHolder.Patient", "MainInsurHolder.Other", 
                                     "One"),
                       "beta"=c(1.77879, #intercept
                                -0.98053,-0.45971,-1.17604,-0.98053,  #VisitType
                                1.38889,-0.41509, # Time to appoint
                                0.11690,0.13061,-0.30591,0.06925,0.06681,0.21308,0.24301,0.06525,0.16892,0.31265,0.01766, # Appoint Month
                                0.10703,0.09383,0.05991,0.11382,0.24379,   # # Appoint Weekdat
                                -0.14898, # Appoint time
                                0.74934,0.56375,0.21910,0.14005,-0.33250, # count of now show
                                0.24324,0.19334,0.18773, # Age
                                0.03018,-0.00358, # CountyToClinic
                                0.38749,0.45747,0.45925,0.43606, # Races
                                0, # gender
                                0.05776,0.10051, # NumHousehold
                                0.2179,0.11418,-0.08125, #DistToClinic
                                0.16248,-0.03024,-0.05279, # PrimaryInsurance
                                -0.38714,-0.25427,-0.77952,-0.27669, # MaimImsreHolder
                                0.16962
                       ),stringsAsFactors = FALSE)

############################################# Simulated Dataset #############################################
  # sample size
DataGenerator <- function(s.data,n,threshold,sd) {
  CatDataGenerator <- function(mydata,i,n) {
    #s.data: data frame #i: select the column #n: the number of sample size you want
    x<-unique(s.data$Chart)[i]
    data.n<-mydata%>%filter(Chart==x)%>%select(2)%>%pull%>%sample(.,size = n,replace=TRUE,prob=c(s.data%>%filter(Chart==x)%>%select("prop")%>%pull))
    return(data.n)}
  
  # generate data
  cl<-makeCluster(4)         # use four cores to generate data
  registerDoParallel(cl)    
  Alldata<-foreach(j=1:length(unique(s.data$Chart)), .packages="dplyr", .combine = cbind) %dopar% CatDataGenerator(mydata = s.data,i =j,n = n)  
  stopCluster(cl)
  colnames(Alldata)<-unique(s.data$Chart)
  ReferenceGreoup<-c("Newborn","weeks_1to2","Apr","Fri","Morning","ThreeAndFour","MoreThan10","Adjacent","AfrAmerican","Male","MoreThan5","TenTo20Miles","BlueCross","Grandparent","MoreThan1")
  # Here we need to change the reference groups in order to use the cefficients form the article
  # The reference group we listed below are beased on the article
  # Self-defined function to change the reference group for each data
  factorList<- function(mydata,i) {
    x<-mydata
    testd<-unique(x[,i])%>%as.character
    return(c(ReferenceGreoup[i],testd[-which(testd==ReferenceGreoup[i])]))
  }
  
  # Convert the data in to dummy code
  x<-as.data.frame(Alldata)
  for (i in 1:length(ReferenceGreoup)){
    x[,i]<-factor(x[,i],levels =factorList(x,i=i))  
  }
  dumm2<-createDummyFeatures(x,method = "reference")
  dumm2<-cbind("intercept"=1,dumm2)
  # Combine the coef and covariables matrix
  testdata<-cbind(rownames(t(dumm2)),t(dumm2))
  testdata<-left_join(x =as.data.frame(testdata,stringsAsFactors = FALSE),y=coef.table,by=c("V1"="Predictor"))
  rownames(testdata)<-testdata$V1
  covariable<-testdata[,-c(grep(pattern = "V1",x = names(testdata)),grep(pattern = "beta",x = names(testdata)))]
  covariable[] <-lapply(covariable, function(x) as.numeric(as.character(x)))    # convert the data to numeric
  # Caculate the odds X*beta
  odds=exp(t(covariable)%*%as.matrix(testdata$beta)+rnorm(n=n,mean = 0,sd=sd)) 
  prob.show<-odds/(1+odds)
  # Combine with the simulated dataset
  Alldata<-cbind(Alldata,prob.show,prob.show>threshold)
  colnames(Alldata)[c(16,17)]<-c("PorbShow","Show")    # rename the variables
  Alldata<-as.data.frame(Alldata)
  # change the reference group that are same as our setting 
  for (i in 1:length(ReferenceGreoup)){
    Alldata[,i]<-factor(Alldata[,i],levels =factorList(Alldata,i=i))  
  }
  return(as.data.frame(Alldata))
}
# write.csv(x = Test1,file = "test1.csv",row.names = FALSE)
########################################## Regression analysis  ##########################################
set.seed(22232)
Test1<-DataGenerator(s.data =s.data,n = 5000,threshold = 0.7,sd=0.5)
trainIndex<-createDataPartition(Test1$Show,p=0.8,times = 1,list = FALSE)
DataTrain<-Test1[trainIndex,-16]
DataTest<-Test1[-trainIndex,-16]
model1 <- glm(Show ~.,family=binomial(link="logit"),data=DataTrain)
# summary(model1)
true.show<-factor(x = DataTrain$Show,levels = c("TRUE","FALSE"))
predict.show<-(predict(model1,type = "response")>0.8)%>%factor(.,levels=c("TRUE","FALSE"))
performance.show<-confusionMatrix(table(predict.show,true.show))
# postResample(pred = predict.show, obs = true.show)
# http://www.statisticshowto.com/sensitivity-vs-specificity-statistics/
# performance.show$overall  # This can get the performance overview

# Use testing data
test.true.show<-factor(x = DataTest$Show,levels = c("TRUE","FALSE"))
fitted.results <-(predict(model1,newdata=DataTest,type='response')>0.8)%>%factor(.,levels=c("TRUE","FALSE"))
fitted.performance.show<-confusionMatrix(table(fitted.results,test.true.show))
#fitted.performance.show$overall

########################################## Other Method  ##########################################
#install.packages("corrplot")
#library("corrplot")
#devtools::install_github("rstudio/keras")
# Load in the keras package
library(keras)
# install_keras()
#Install TensorFlow
#install_tensorflow()

DataTest.dp<-as.data.frame(lapply(DataTest[,1:ncol(DataTest)-1], FUN = unclass))%>%as.matrix
DataTrain.dp<-as.data.frame(lapply(DataTrain[,1:ncol(DataTrain)-1], FUN = unclass))%>%as.matrix 
dimnames(DataTest.dp)<-NULL
dimnames(DataTrain.dp)<-NULL

#label 
Target.Test.dp<-to_categorical(DataTest[,ncol(DataTest)])%>%as.matrix
Target.Train.dp<-to_categorical(DataTrain[,ncol(DataTrain)])%>%as.matrix

# the softmax activation function is used in the output layer.
# the output values are in the range of 0 and 1 and may be used as predicted probabilities:
# Initialize a sequential model
# model <- keras_model_sequential()
# 
# # Add layers to the model
# model %>% 
#   layer_dense(units = 8, activation = 'relu', input_shape = c(15)) %>%
#   layer_dense(units = 3, activation = 'softmax')
# 
# 
# # Compile the model
# model %>% compile(
#   loss = 'binary_crossentropy',
#   optimizer = 'adam',
#   metrics = 'accuracy'
# )
# 
# # Store the fitting history in `history` 
# history <- model %>% fit(
#   DataTrain.dp, 
#   Target.Train.dp, 
#   epochs = 200,
#   batch_size = 5, 
#   validation_split = 0.2,
#   verbose = 1
# )
# 
###   Prediction ###
# Predict the classes for the test data
# 1: TRUE, 2: FALSE
classes <- model %>% predict_classes(DataTest.dp, batch_size = 128)

# Confusion matrix
predicted.show.dp<-ifelse(classes==1,"FALSE","TRUE")%>%factor(.,levels=c("TRUE","FALSE"))
test.true.show.dp<-DataTest[,ncol(DataTest)]%>%factor(.,levels=c("TRUE","FALSE"))
performance.dp<-confusionMatrix(table(predicted.show.dp,test.true.show.dp))
# ############## Evaluating Model ####################
# # Evaluate on test data and labels
# score <- model %>% evaluate(DataTest.dp, Target.Test.dp, batch_size = 128)
# 
# fitted.performance.show
# performance.dp
# Save model
# save_model_hdf5(model, "DNN.h5")
model <- load_model_hdf5("DNN.h5")
# save_model_weights_hdf5(object = model,filepath = "DNN_weights.h5")
model %>% load_model_weights_hdf5("DNN_weights.h5")

########################  plug test ########################

#Test22<-DataGenerator(s.data =s.data,n = 6000,threshold = 0.74,sd=3)
M.TestData <- function(mydata,threshold) {
  Test2=mydata
  # threshold=0.74
  DataTest2<-Test2[,-16]
  ## logistic regression
  test.true.show2<-factor(x = DataTest2$Show,levels = c("TRUE","FALSE"))
  fitted.results2 <-(predict(model1,newdata=DataTest2,type='response')>threshold)%>%factor(.,levels=c("TRUE","FALSE"))
  fitted.performance.show2<-confusionMatrix(table(fitted.results2,test.true.show2))
  # Deep learning
  # Predict the classes for the test data
  # 1: TRUE, 2: FALSE
  DataTest2.dp<-as.data.frame(lapply(DataTest2[,1:ncol(DataTest2)-1], FUN = unclass))%>%as.matrix 
  dimnames(DataTest2.dp)<-NULL
  classes <- model %>% predict_classes(DataTest2.dp, batch_size = 128)
  # Confusion matrix
  predicted.show.dp<-ifelse(classes==1,"FALSE","TRUE")%>%factor(.,levels=c("TRUE","FALSE"))
  test.true.show.dp<-Test2[,ncol(Test2)]%>%factor(.,levels=c("TRUE","FALSE"))
  performance2.dp<-confusionMatrix(table(predicted.show.dp,test.true.show.dp))
  
  Result.perf <- function(obj.p,lb) {
    # lb: label
    x<-as.vector(obj.p$table)
    names(x)<-c("TP","FN","FP","TN")
    y<-c(x,obj.p$byClass,obj.p$overall)
    names(y)<-paste(lb,names(y),sep = ".")
    return(y)
  }
  
  x1<-Result.perf(obj.p = fitted.performance.show2,lb = "lg")
  x2<-Result.perf(obj.p = performance2.dp,lb="nn")
  x<-c(x1,x2)
  return(x)
  # print("logistic")
  # print(fitted.performance.show2)
  # print("DeepLearning")
  # print(performance2.dp)
}

# rep10<-replicate(n=20,expr = M.TestData(mydata = DataGenerator(s.data = s.data,n=2000,threshold = 0.8,sd=2),threshold = 0.8))
# test1<-rep10%>%rowMeans%>%round(.,4)%>%matrix(.,ncol=2)
# rownames(test1)<-rownames(rep10)[1:22]
# colnames(test1)<-c("logistic","NN")
# test1
now1 <- Sys.time()
result.all<-c()
for (i in 1:1000){
  result.all.temp<-M.TestData(mydata = DataGenerator(s.data = s.data,n=2000,threshold = 0.6,sd=0.1),threshold = 0.6)
  result.all<-rbind(result.all,result.all.temp)
}
write.csv(x = result.all,file = "R2000_T06SD01.csv",row.names = TRUE)

result.all<-c()
for (i in 1:1000){
  result.all.temp<-M.TestData(mydata = DataGenerator(s.data = s.data,n=2000,threshold = 0.6,sd=0.5),threshold = 0.6)
  result.all<-rbind(result.all,result.all.temp)
}
write.csv(x = result.all,file = "R2000_T06SD05.csv",row.names = TRUE)

result.all<-c()
for (i in 1:1000){
  result.all.temp<-M.TestData(mydata = DataGenerator(s.data = s.data,n=2000,threshold = 0.6,sd=1),threshold = 0.6)
  result.all<-rbind(result.all,result.all.temp)
}
write.csv(x = result.all,file = "R2000_T06SD10.csv",row.names = TRUE)

result.all<-c()
for (i in 1:1000){
  result.all.temp<-M.TestData(mydata = DataGenerator(s.data = s.data,n=2000,threshold = 0.6,sd=1.5),threshold = 0.6)
  result.all<-rbind(result.all,result.all.temp)
}
write.csv(x = result.all,file = "R2000_T06SD15.csv",row.names = TRUE)
result.all<-c()
for (i in 1:1000){
  result.all.temp<-M.TestData(mydata = DataGenerator(s.data = s.data,n=2000,threshold = 0.6,sd=2),threshold = 0.6)
  result.all<-rbind(result.all,result.all.temp)
}
write.csv(x = result.all,file = "R2000_T06SD20.csv",row.names = TRUE)
###########################################################################################
now2 <- Sys.time()


# Cannotwork 
# frep10<-foreach(i=1:10,.packages = c("keras","caret","mlr","doParallel","dplyr"),.combine = cbind) %dopar% M.TestData(mydata = DataGenerator(s.data = s.data,n=2000,threshold = 0.8,sd=2),threshold = 0.8)

######################################################

