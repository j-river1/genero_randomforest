
#Clean memory
rm(list=ls())

#Install packages 
list.of.packages <- c("caret", "nnet", "here")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Create folder data
dir.create(file.path(here(), "data"), showWarnings = FALSE)

#Load libraries
lapply(list.of.packages, require, character.only = TRUE)


#name file and load data
name_file <- "data_chiapas_19Abr.csv"
data <- read.csv(paste0(here(),"/data/", name_file))

#Variable for make the prediction
variable <- "genero"

#Partition
set.seed(123)
inTrain  <- createDataPartition(y=data[,which(colnames(data)== variable)], p=0.7, list=F)
training <- data[inTrain,]
testing  <- data[-inTrain,]


#Identify which variables are factors. If there are factors, these must have two levels.
l <- sapply(data , function(x) is.factor(x))

#If it is NODROP so has more levels and DROP other two levels
ifelse(n <- sapply(l, function(x) length(levels(x))) == 1, "DROP", "NODROP")


ctrl <- expand.grid(size = c(2,4,6) ,decay = c(0.1,0.5,0.8) )
model <- train(genero~.,data=training,method = "nnet", tuneGrid = ctrl, trControl = trainControl(method = "cv",number = 10), lineout =T) 

training[!(training$genero=="NULL"), ]










#Put the variable into first argument of train function.
model <- train(genero~.,data=training, method = "nnet", tuneGrid = ctrl, trControl = trainControl(method = "cv",
         number = 10), lineout =T) 
