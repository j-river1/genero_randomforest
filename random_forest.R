
#Clean memory
rm(list=ls())

#Install packages 
list.of.packages <- c("caret", "nnet", "here", "rpart", "rpart.plot", "rattle")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


#Load libraries
lapply(list.of.packages, require, character.only = TRUE)


#Create folder data
dir.create(file.path(here(), "data"), showWarnings = FALSE)
dir.create(file.path(here(), "graphics"), showWarnings = FALSE)


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
                                 fertilizacion_quimica_foliar, nivelacion, num_semill_golpe, tipo_de_siembra, tipo_de_sembr_siembr,
                                 tipo_de_semilla, cultivo_maizfrijol_indiq_color_semilla,profun_sie_cm, apli_produ_qui_trat_sem,
                                 tipo_de_labranza, municipio, localidad, nombre_de_la_variedad_sembrada,nombre_de_la_parcela,
                                 id_de_tipo_de_bitacora_new, latitud_n, longitud_w))


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


#Names of varieaty les to 12
index_names <- which(as.vector(table(data$nombre_de_la_variedad_final) < 60))
names_variety <- names(table(data$nombre_de_la_variedad_final))[index_names]
index_change <- data$nombre_de_la_variedad_final %in% names_variety 
data$nombre_de_la_variedad_final <- as.vector(data$nombre_de_la_variedad_final)
data$nombre_de_la_variedad_final[index_change] <- "OTRA (ESPECIFIQUE)"
data$nombre_de_la_variedad_final <- as.factor(data$nombre_de_la_variedad_final )


#data
set.seed(123)
Masculino <- subset(data, genero == "Masculino")
Femenino <- subset(data, genero == "Femenino")
length_mas <- dim(Masculino)[1]
length_fem <- dim(Femenino)[1]
index <- sample(1:length_mas, length_fem)
sample_masculino <- Masculino[index,]
data <-  rbind(Femenino, sample_masculino)










#Partition
set.seed(123)
inTrain  <- createDataPartition(y=data[,which(colnames(data)== variable)], p=0.7, list=F)
training <- data[inTrain,]
testing  <- data[-inTrain,]
ctrl <- expand.grid(size = c(2,4,6) ,decay = c(0.1,0.5,0.8) )
model <- train(y=training$genero,x=training[,-4],method = "rf", trControl = trainControl(method = "cv",number = 10)) 
pred_val <- predict(model,testing)
postResample(pred_val,testing$genero)
plot(pred_val,testing$genero,col="red",pch=19)
abline(0,1,lty=2)
varImp <- varImp(model)
plot(varImp)



#Change the names of variety
names_variedad <- names(table(data$nombre_de_la_variedad_final))
data$nombre_de_la_variedad_final <- gsub("[[:space:]]", "", data$nombre_de_la_variedad_final) 
#Variety criollo
data$nombre_de_la_variedad_final[grep("CRIOLLO", data$nombre_de_la_variedad_final)] <- "VARIEDAD CRIOLLO"
data$nombre_de_la_variedad_final[grep("DEKALB", data$nombre_de_la_variedad_final)] <- "VARIEDAD DEKALB"
data$nombre_de_la_variedad_final[grep("NULL", data$nombre_de_la_variedad_final)] <- "OTRA(ESPECIFIQUE)"
data$nombre_de_la_variedad_final[grep("VS", data$nombre_de_la_variedad_final)] <- "VARIEDAD VS"
data$nombre_de_la_variedad_final[grep("^P.+W$",data$nombre_de_la_variedad_final)] <- "VARIEDAD_P_W"


#Boxplot for variables
ggplot(data, aes(x=genero, y=rendimiento_final, color=genero)) + geom_boxplot() +ggtitle (paste("Boxplot de rendimiento (kg/ ha)", "\n","por genero")) + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Kg/ha")
ggsave(paste0(here(),"/graphics/boxplot_rendimiento.png"))

ggplot(data, aes(x=genero, y=costo_total_de_la_semilla_ha, color=genero)) + geom_boxplot() +ggtitle (paste("Boxplot del costo total por semilla ", "\n","por genero")) + 
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave(paste0(here(),"/graphics/boxplot_costototal_semilla.png"))

ggplot(data, aes(x=genero, y=costo_transporte_semilla, color=genero)) + geom_boxplot() +ggtitle (paste("Boxplot del costo transporte ", "\n","por genero")) + 
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave(paste0(here(),"/graphics/boxplot_costo_transporte_semilla.png"))

ggplot(data, aes(x=genero, y=n, color=genero)) + geom_boxplot() +ggtitle (paste("Boxplot de la cantidad de nitrógeno", "\n","por genero")) + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("kg/ ha")
ggsave(paste0(here(),"/graphics/boxplot_nitrogeno.png"))

ggplot(data, aes(x=genero, y=superficie_ha, color=genero)) + geom_boxplot() +ggtitle (paste("Boxplot de la superficie sembrada", "\n","por genero")) + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("ha")
ggsave(paste0(here(),"/graphics/boxplot_superficie.png"))

ggplot(data, aes(x=genero, y=p, color=genero)) + geom_boxplot() +ggtitle (paste("Boxplot de la cantidad de potasio", "\n","por genero")) + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Kg/ha")
ggsave(paste0(here(),"/graphics/boxplot_potasio.png"))

ggplot(data, aes(x=genero, y=distan_surc_cm, color=genero)) + geom_boxplot() +ggtitle (paste("Boxplot de la distancia entre matas", "\n","por genero")) + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("cm")
ggsave(paste0(here(),"/graphics/boxplot_distanciaentrematas.png"))

ggplot(data, aes(x=genero, y=densidad_de_siembra_en_kg_kg_ha, color=genero)) + geom_boxplot() +ggtitle (paste("Boxplot de la densidad siembra", "\n","por genero")) + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("cm")
ggsave(paste0(here(),"/graphics/boxplot_densidadsiembra.png"))

ggplot(data, aes(x=genero, y=densi_siem_num_semillas_ha, color=genero)) + geom_boxplot() +ggtitle (paste("Boxplot de la densidad numero semillas", "\n","por genero")) + 
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave(paste0(here(),"/graphics/boxplot_numerosemillas.png"))




#Barplot for variables
#Femenino
femen_data <- subset(data, genero=="Femenino")
percentage_decimal_fem <- as.vector(table(femen_data$nombre_de_la_variedad_final))/sum(as.vector(table(femen_data$nombre_de_la_variedad_final)))
slices_Fem <-   round(percentage_decimal_fem*100, 1)
names_variety <- names(table(femen_data$nombre_de_la_variedad_final))
lbls <- paste(names_variety , slices_Fem)
lbls <- paste(lbls,"%", sep="")
png(paste0(here(),"/graphics/pie_chart_fem.png"), width = 510)
pie(slices_Fem,labels = lbls, col=rainbow(length(lbls)), main=paste("Distribución de las variedades cultivadas", "\n", "Genero Femenino"), cex=0.8)
dev.off()


#Masculino 

mascul_data <- subset(data, genero=="Masculino")
percentage_decimal_mas <- as.vector(table(mascul_data$nombre_de_la_variedad_final))/sum(as.vector(table(mascul_data$nombre_de_la_variedad_final)))
slices_Mas <-   round(percentage_decimal_mas*100, 1)
names_variety <- names(table(mascul_data$nombre_de_la_variedad_final))
lbls <- paste(names_variety , slices_Mas)
lbls <- paste(lbls,"%", sep="")
png(paste0(here(),"/graphics/pie_chart_mas.png"), width = 510)
pie(slices_Mas,labels = lbls, col=rainbow(length(lbls)), main=paste("Distribución de las variedades cultivadas", "\n", "Genero Masculino"), cex=0.8)
dev.off()


#Escolaridad
data_escolaridad_mas <- as.data.frame(table(mascul_data$escolaridad))
names(data_escolaridad_mas) <- c("Nivel_Estudios", "Total")
data_escolaridad_mas$Genero <- "Masculino"
data_escolaridad_mas$Total <- data_escolaridad_mas$Total/sum(data_escolaridad_mas$Total)


data_escolaridad_fem <- as.data.frame(table(femen_data$escolaridad))
names(data_escolaridad_fem) <- c("Nivel_Estudios", "Total")
data_escolaridad_fem$Genero <- "Femenino"
data_escolaridad_fem$Total <- data_escolaridad_fem$Total/sum(data_escolaridad_fem$Total)



complete_data <- rbind(data_escolaridad_mas, data_escolaridad_fem)
complete_data <- complete_data[-which(complete_data$Nivel_Estudios=="NULL"),]

levels(complete_data$Nivel_Estudios)[2] <- c("Tecnicos con primaria")
levels(complete_data$Nivel_Estudios)[3] <- c("Tecnicos con secundaria")
levels(complete_data$Nivel_Estudios)[4] <- c("Profesional")
levels(complete_data$Nivel_Estudios)[9] <- c("Bachillerato")

complete_data$Total <- complete_data$Total/sum(complete_data$Total)

p <- ggplot(complete_data, aes( Nivel_Estudios , Total))
q <- p +geom_bar(stat = "identity", aes(fill = Genero), , position='dodge') +theme(axis.text.x = element_text(angle = 90, hjust = 1))
q + ggtitle("Escolaridad por género \n Chiapas") + theme(plot.title = element_text(hjust = 0.5)) +scale_y_continuous(labels=scales::percent)
ggsave(paste0(here(),"/graphics/escolaridad_chiapas.png"))




#CART
cart <- rpart(genero ~., training)
prp(cart, uniform = TRUE)



