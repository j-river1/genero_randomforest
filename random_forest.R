
#Clean memory
rm(list=ls())

#Install packages 
list.of.packages <- c("caret", "nnet", "here")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


#Load libraries
lapply(list.of.packages, require, character.only = TRUE)


#Create folder data
dir.create(file.path(here(), "data"), showWarnings = FALSE)




#name file and load data
name_file <- "data_chiapas_19Abr.csv"
data <- read.csv(paste0(here(),"/data/", name_file))

#Variable for make the prediction
variable <- "genero"


#data

data <- subset(data, genero != "NULL", select = -c(rodado, total_riegos, goteo, gravedad, aspercion, rio, pozo,
                                 escurrimientos_superficiales, afectaciones, ocurrencias, rotacion_cultivos,
                                 aguas_turbias_negras, presa_represa, insecticida, uso_biofertilizantes, 
                                 cont_qui_male_desp_siemb, uso_mejoradores, uso_biofertilizantes, fertilizacion_quimica_suelo,
                                 preparacion_suelo_herbicida, fungicida, humedad_al_sembrar, arreglo_de_la_siembra,
                                 fertilizacion_quimica_foliar, nivelacion))


data$genero <- as.factor(as.character(data$genero))
data$sabe_leer <- as.factor(as.character(data$sabe_leer))


data$p <- as.numeric(data$p)
data$n <- as.numeric(data$n)
data$k <- as.numeric(data$k)

data$densidad_de_siembra_en_kg_kg_ha <- as.numeric(data$densidad_de_siembra_en_kg_kg_ha)
data$densi_siem_num_semillas_ha <- as.numeric(data$densi_siem_num_semillas_ha)
data$costo_total_de_la_semilla_ha <- as.numeric(data$costo_total_de_la_semilla_ha)
data$costo_transporte_semilla <- as.numeric(data$costo_transporte_semilla)
data$dista_plant_o_matas_cm <- as.numeric(data$dista_plant_o_matas_cm)
data$distan_surc_cm <- as.numeric(data$distan_surc_cm)


data$deshierbe <- as.factor(data$deshierbe)
data$rastra_prebarbecho <- as.factor(data$rastra_prebarbecho)
data$escarda <- as.factor(data$escarda)
data$subsoleo_cinceleo_piqueo_escarificado <- as.factor(data$subsoleo_cinceleo_piqueo_escarificado)
data$barbecho <- as.factor(data$barbecho)
data$rastreo <- as.factor(data$rastreo)
data$trazos_urci <- as.factor(data$trazos_urci)
data$reformacion_camas_surcos <- as.factor(data$reformacion_camas_surcos)



















#Partition
set.seed(123)
inTrain  <- createDataPartition(y=data[,which(colnames(data)== variable)], p=0.7, list=F)
training <- data[inTrain,]
testing  <- data[-inTrain,]

#Stop
#Identify variables with few values so delete it

















#Identify which variables are factors. If there are factors, these must have two levels.
l <- sapply(training , function(x) is.factor(x))

#If it is NODROP so has more levels and DROP other two levels
ifelse(n <- sapply(l, function(x) length(levels(x))) == 1, "DROP", "NODROP")


ctrl <- expand.grid(size = c(2,4,6) ,decay = c(0.1,0.5,0.8) )
model <- train(genero~.,data=training,method = "nnet", trControl = trainControl(method = "cv",number = 10), lineout =T) 

training <- training[!(training$genero=="NULL"), ]

#Become from factor to numeric

training$densidad_de_siembra_en_kg_kg_ha <- as.numeric(training$densidad_de_siembra_en_kg_kg_ha)
training$densi_siem_num_semillas_ha <- as.numeric(training$densi_siem_num_semillas_ha)
training$costo_total_de_la_semilla_ha <- as.numeric(training$costo_total_de_la_semilla_ha)
training$costo_transporte_semilla <- as.numeric(training$costo_transporte_semilla)
training$dista_plant_o_matas_cm <- as.numeric(training$dista_plant_o_matas_cm)
training$distan_surc_cm <- as.numeric(training$distan_surc_cm)


training$deshierbe <- as.factor(training$deshierbe)
training$rastra_prebarbecho <- as.factor(training$rastra_prebarbecho)
training$escarda <- as.factor(training$escarda)
training$subsoleo_cinceleo_piqueo_escarificado <- as.factor(training$subsoleo_cinceleo_piqueo_escarificado)
training$barbecho <- as.factor(training$barbecho)
training$rastreo <- as.factor(training$rastreo)
training$nivelacion <- as.factor(training$nivelacion)
training$trazos_urci <- as.factor(training$trazos_urci)
training$reformacion_camas_surcos <- as.factor(training$reformacion_camas_surcos)


training$p <- as.numeric(training$p)
training$n <- as.numeric(training$n)
training$k <- as.numeric(training$k)

training$rotacion_cultivos<- NULL
training$ocurrencias<- NULL
training$afectaciones<- NULL
training$total_riegos<- NULL
training$rodado<- NULL
training$goteo<- NULL
training$gravedad<- NULL
training$aspercion<- NULL
training$rio<- NULL
training$escurrimientos_superficiales<- NULL
training$pozo<- NULL
training$aguas_turbias_negras<- NULL
training$presa_represa <- NULL
training$tipo_de_semilla <- NULL
training$humedad_al_sembrar <- NULL
training$arreglo_de_la_siembra <- NULL
training$tipo_de_siembra <- NULL
training$profun_sie_cm <- NULL
training$cultivo_maizfrijol_indiq_color_semilla <- NULL


ex <- subset(training, genero != "NULL", select=c("n", "p", "k", "reformacion_camas_surcos", "genero"))

levels(ex$genero) <- levels(droplevels(ex$genero))



#Put the variable into first argument of train function.
model <- train(genero~.,data=training, method = "nnet", tuneGrid = ctrl, trControl = trainControl(method = "cv",
         number = 10), lineout =T) 
