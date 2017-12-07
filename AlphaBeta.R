library("tidyverse")
library("foreach")          # parallel compute
library("qdap")             # text substitution in r 
library("scales")
# Import multiple files
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

# Remove the unuseful column
for (i in 1:length(temp)) assign(temp[i], get(temp[i])[,-1])

# Get the mean
temp.mean<-gsub(".csv","",temp)
temp.mean<-paste(temp.mean,".mean",sep = "")
for (i in 1:length(temp.mean)) assign(temp.mean[i], get(temp[i])%>%colMeans)

# Caculate Objective function
opt <- function(mydata) {
  my<-temp.mean[grep(mydata,temp.mean)]
  my.data<-foreach(i=1:length(my),.packages = c("tidyverse"),.combine = rbind) %do% get(my[i])
  rownames(my.data)<-my
  my.data<-as.data.frame(my.data)%>%transform("lg.objective.2"=(1-lg.Specificity)+2*(1-lg.Sensitivity),
                                                    "lg.objective.4"=(1-lg.Specificity)+4*(1-lg.Sensitivity),
                                                    "nn.objective.2"=(1-nn.Specificity)+2*(1-nn.Sensitivity),
                                                    "nn.objective.4"=(1-nn.Specificity)+4*(1-nn.Sensitivity)) 
  # add p and sd into my.data dataset
  Items<-rownames(my.data)%>%mgsub(pattern =list(paste(mydata,"_T",sep = ""),".mean") ,replacement = "",.) 
  my.data<-cbind(Items,my.data)%>%separate(Items,sep ="SD" ,into=c("threshold","sd")) 
  my.data<-my.data%>%transform(sd=ifelse(sd=="01",0.1,
                                         ifelse(sd=="05",0.5,
                                                ifelse(sd=="10",1,
                                                       ifelse(sd=="15",1.5,
                                                              ifelse(sd=="20",2,
                                                                     ifelse(sd=="30",3.0,sd)))))))
  return(my.data)
}

R3000.data<-opt(mydata = "R3000")
R4000.data<-opt(mydata = "R4000")
R5000.data<-opt(mydata = "R5000")
R6000.data<-opt(mydata = "R6000")
# Clean memory
rm(list= ls()[!(ls() %in% c("R3000.data","R4000.data","R5000.data","R6000.data","temp","temp.mean","opt"))])

# Convert to the long format dataframe
LongFormatConvet <- function(mydata) {
  # change the column names
  lg<-grep(pattern = "lg.*",x = names(mydata))
  mydata.lg<-mydata[,c(1,2,lg)]
  colnames(mydata.lg)<-gsub(pattern = "lg.",replacement = "",x = names(mydata.lg))
  mydata.lg$Method<-"LogisticRegression"
  mydata.nn<-mydata[,-lg]
  colnames(mydata.nn)<-gsub(pattern = "nn.",replacement = "",x = names(mydata.nn))
  mydata.nn$Method<-"DeepLearning"
  # combind two data frames and correct the data type
  mydata.all<-rbind(mydata.lg,mydata.nn)%>%transform(threshold=as.factor(threshold),Method=as.factor(Method))
  return(mydata.all)
}
R3000.data<-LongFormatConvet(mydata = R3000.data)
R4000.data<-LongFormatConvet(mydata = R4000.data)
R5000.data<-LongFormatConvet(mydata = R5000.data)
R6000.data<-LongFormatConvet(mydata = R6000.data)

# Precision sd
theme_set(theme_classic())
ggplot(R6000.data,aes(x=sd,y=Sensitivity,color=Method,shape=threshold,group=interaction(Method,threshold)))+geom_point(size=2)+geom_line()+
  scale_colour_discrete(breaks=c("DeepLearning","LogisticRegression"),labels=c("Deep Learnging", "Logistic Regression")) +
  scale_shape_discrete(name  ="Threshold")+ggtitle("Sensitivity vs Standard Deviation")+theme(plot.title = element_text(hjust = 0.5))
  
ggplot(R6000.data,aes(x=sd,y=Specificity,color=Method,shape=threshold,group=interaction(Method,threshold)))+geom_point(size=2)+geom_line()+
  scale_colour_discrete(breaks=c("DeepLearning","LogisticRegression"),labels=c("Deep Learnging", "Logistic Regression")) +
  scale_shape_discrete(name  ="Threshold")+ggtitle("Specificity vs Standard Deviation")+theme(plot.title = element_text(hjust = 0.5))

ggplot(R6000.data,aes(x=sd,y=Accuracy,color=Method,shape=threshold,group=interaction(Method,threshold)))+geom_point(size=2)+geom_line()+
  scale_colour_discrete(breaks=c("DeepLearning","LogisticRegression"),labels=c("Deep Learnging", "Logistic Regression")) +
  scale_shape_discrete(name  ="Threshold")+ggtitle("Accuracy vs Standard Deviation")+theme(plot.title = element_text(hjust = 0.5))
ggplot(R6000.data,aes(x=sd,y=objective.4,color=Method,shape=threshold,group=interaction(Method,threshold)))+geom_point(size=2)+geom_line()+
  scale_colour_discrete(breaks=c("DeepLearning","LogisticRegression"),labels=c("Deep Learnging", "Logistic Regression")) +
  scale_shape_discrete(name  ="Threshold")+ggtitle("Objective Score vs Standard Deviation")+theme(plot.title = element_text(hjust = 0.5))+ylab("Weighted sum of type I and type II error(Ratio 4)")+
  labs(subtitle="Note: Cost Ratio = 4")

ggplot(R6000.data,aes(x=sd,y=objective.2,color=Method,shape=threshold,group=interaction(Method,threshold)))+geom_point(size=2)+geom_line()+
  scale_colour_discrete(breaks=c("DeepLearning","LogisticRegression"),labels=c("Deep Learnging", "Logistic Regression")) +
  scale_shape_discrete(name  ="Threshold")+ggtitle("Objective Score vs Standard Deviation")+theme(plot.title = element_text(hjust = 0.5))+ylab("Weighted sum of type I and type II error(Ratio 2)")+
  labs(subtitle="Note: Cost Ratio = 2")


  
  

# #devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
# library(ggplot2)
# library(ggradar)
# suppressPackageStartupMessages(library(dplyr))
# library(scales)
# 
# mtcars %>%
#   add_rownames( var = "group" ) %>%
#   mutate_each(funs(rescale), -group) %>%
#   tail(4) %>% select(1:10) -> mtcars_radar
# 
# ggradar(mtcars_radar) 


# 
# tep.data<-R6000.data%>%filter(threshold=="07")
# ggplot(tep.data,aes(x=sd,y=Sensitivity,color=Method))+geom_point()+geom_line()


