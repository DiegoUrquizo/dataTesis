#Carga de datos
data2020<-read.csv("C:/Users/WinUser/Documents/Big Data/Tesis/Data/Datos_abiertos_ ESI_2020/ESI_2020.csv")
data2021<-read.csv("C:/Users/WinUser/Documents/Big Data/Tesis/Data/Datos_abiertos_ ESI_2021/esi_2021.csv",sep=";")
data2022<-read.csv("C:/Users/WinUser/Documents/Big Data/Tesis/Data/Datos_abiertos_ ESI_2022/esi_2022.csv",sep=";")
bd22<-data2022[,-c(1)]
data2023<-read.csv("C:/Users/WinUser/Documents/Big Data/Tesis/Data/Datos_abiertos_ ESI_2023/esi2023.csv")
bd23<-data2023[,-c(1)]
#Limpieza de Datos
##Eliminar registros con datos faltantes
library(tidyverse)
bd20 <- data2020 %>% drop_na()
bd21 <- data2021 %>% drop_na()
bd22 <- bd22 %>% drop_na()
bd23 <- bd23 %>% drop_na()
## Unificaci髇 de la data y Eliminaci髇 de variables redundantes o con infirmaci髇 no neceasria
data_ESI<-rbind(bd20,bd21,bd22,bd23)
#Normalizaci髇 de la data
data_tes <- data_tes %>%
  mutate( pais_res= case_when(
    pais_res == "Turqu韆" ~ "Turquia",
    pais_res == "Uzbekist醤" ~ "Uzbekistan",
    pais_res == "Turkmenist醤" ~ "Turkmenistan",
    pais_res == "Tayikist醤" ~ "Tayikistan",
    pais_res == "Tanzania (Rep鷅lica Unida de)" ~ "Tanzania",
    pais_res == "Tanzania (Republica Unida de) " ~ "Tanzania",
    pais_res == "Tanzania (Rep鷅lica Unida de) " ~ "Tanzania",
    pais_res == "Taiw醤 (Rep鷅lica de China)" ~ "Taiwan (Republica de China)",
    TRUE ~ pais_res  # Dejar el resto de valores como est醤
  ))

table(data_tes$pais_prod)

data_tes <- data_tes %>%
  mutate( pais_res= case_when(
    pais_res == "Sud醤" ~ "Sudan",
    pais_res == "Sud醤 del Sur" ~ "Sudandel Sur",
    pais_res == "Sud醘rica" ~ "Sudafrica",
    pais_res == "T鷑ez" ~ "Tunez",
    pais_res == "Rep鷅lica Popular Democr醫ica de Corea" ~ "Republica Popular Democratica de Corea",
    pais_res == "Rep鷅lica Democr醫ica Popular Lao" ~ "Republica Democratica Popular Lao",
    pais_res == "Rep鷅lica Democr醫ica del Congo" ~ "Republica Democratica del Congo",
    pais_res == "Rep鷅lica de Moldovia" ~ "Republica de Moldovia",
    TRUE ~ pais_res  # Dejar el resto de valores como est醤
  ))
#Sucesivamente
#Variable num閞ica
data_ESI$edad<-as.numeric(data_ESI$edad)
data_ESI <- data_ESI %>% drop_na()

#Estadisticas
rbind(mean(data_ESI$edad),median(data_ESI$edad),var(data_ESI$edad),sd(data_ESI$edad),sd(data_ESI$edad)/mean(data_ESI$edad),min(data_ESI$edad),max(data_ESI$edad),skewness(data_ESI$edad),kurtosis(data_ESI$edad))
quantile(data_ESI$edad,c(.25,.5,.75))

#Moda
tf<-table(data_ESI$edad)
tf[max(tf)==tf]

#Histograma
ggplot(data_ESI, aes(x = edad)) + 
  geom_histogram(bins = 17, fill = "blue", color = "black") + 
  theme_minimal() + 
  labs(title = "Distribuci髇 de Edad", x = "Edad", y = "Frecuencia")


#variables cualitativas
#Tipo de Movimiento
frecuencia <- table(data_ESI$tip_movi)
frecuencia_rel <- as.data.frame(prop.table(frecuencia_tip_movi))
frecuencia_rel
colnames(frecuencia_rel) <- c("tip_movi", "Frecuencia_Relativa")

ggplot(frecuencia_rel, aes(x = tip_movi, y = Frecuencia_Relativa)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  geom_text(aes(label = scales::percent(Frecuencia_Relativa, accuracy = 0.1)), 
            vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Distribuci髇 por Tipo de Movimiento", 
       x = "Tipo de Movimiento", 
       y = "Frecuencia Relativa")
# Distribuci髇 de la variable Sexo

frecuencia <- table(data_ESI$sex_migr)
frecuencia_rel <- as.data.frame(prop.table(frecuencia))
frecuencia_rel
colnames(frecuencia_rel) <- c("sex_migr", "Frecuencia_Relativa")

frecuencia_rel<-frecuencia_rel[c(1,3,2),]

ggplot(frecuencia_rel, aes(x = sex_migr, y = Frecuencia_Relativa)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  geom_text(aes(label = scales::percent(Frecuencia_Relativa, accuracy = 0.1)), 
            vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Distribuci髇 por Sexo", 
       x = "Sexo", 
       y = "Frecuencia")
# Distribuci髇 de la variable Medio de Transporte

frecuencia <- table(data_ESI$via_tran)
frecuencia_rel <- as.data.frame(prop.table(frecuencia))
frecuencia_rel
colnames(frecuencia_rel) <- c("via_tran", "Frecuencia_Relativa")

#frecuencia_rel<-frecuencia_rel[c(1,3,2),]

ggplot(frecuencia_rel, aes(x = via_tran, y = Frecuencia_Relativa)) +
  geom_bar(stat = "identity", fill = "cyan", color = "black") +
  geom_text(aes(label = scales::percent(Frecuencia_Relativa, accuracy = 0.1)), 
            vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Distribuci髇 por Medio de Transporte", 
       x = "Medio de Transporte", 
       y = "Frecuencia")

# Distribuci髇 de la variable Motivo de Viaje

frecuencia <- table(data_ESI$mot_viam)
frecuencia_rel <- as.data.frame(prop.table(frecuencia))
frecuencia_rel
colnames(frecuencia_rel) <- c("mot_viam", "Frecuencia_Relativa")

ggplot(frecuencia_rel, aes(x = mot_viam, y = Frecuencia_Relativa)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = scales::percent(Frecuencia_Relativa, accuracy = 0.1)), 
            vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Distribuci髇 por Motivo de Viaje", 
       x = "Motivo de Viaje", 
       y = "Frecuencia")

ggplot(datos, aes(x = mot_viam)) + 
  geom_bar(fill = "cyan", color = "black") + 
  theme_minimal() + 
  labs(title = "Distribuci髇 por Motivo de Viaje", x = "Motivo de Viaje", y = "Frecuencia")


#Analisis bivariado
# Relaci髇 entre Edad y Tipo de Movimiento
ggplot(data_ESI, aes(x = tip_movi, y = edad)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribuci髇 de Edad por Tipo de Movimiento", x = "Tipo de Movimiento", y = "Edad")


# CSexo y tipo de movimeinto
tabla_tip <- table(data_ESI$tip_movi, data_ESI$sex_migr)
tabla_rel <- as.data.frame(prop.table(tabla_tip, margin = 1))
colnames(tabla_rel) <- c("tip_movi", "sex_migr", "Frecuencia_Relativa")

ggplot(tabla_rel, aes(x = tip_movi, y = Frecuencia_Relativa, fill = sex_migr)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = scales::percent(Frecuencia_Relativa, accuracy = 0.1)), 
            position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Tipo de Movimiento por Sexo", x = "Tipo de Movimiento", y = "Frecuencia Relativa")

#Tipo de movimeinto por tipo de nacionalidad 

tabla_tip <- table(data_ESI$tip_movi, data_ESI$tip_naci)
tabla_rel <- as.data.frame(prop.table(tabla_tip, margin = 1))
colnames(tabla_rel) <- c("tip_movi", "tip_naci", "Frecuencia_Relativa")

ggplot(tabla_rel, aes(x = tip_movi, y = Frecuencia_Relativa, fill = tip_naci)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = scales::percent(Frecuencia_Relativa, accuracy = 0.1)), 
            position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Tipo de Movimiento por Nacionalidad", x = "Tipo de Movimiento", y = "Frecuencia Relativa")

#Tipo de movimeinto por Contiente Nacionalidad 

tabla_tip <- table(data_ESI$tip_movi, data_ESI$cont_nac)
tabla_rel <- as.data.frame(prop.table(tabla_tip, margin = 1))
colnames(tabla_rel) <- c("tip_movi", "cont_nac", "Frecuencia_Relativa")

ggplot(tabla_rel, aes(x = tip_movi, y = Frecuencia_Relativa, fill = cont_nac)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = scales::percent(Frecuencia_Relativa, accuracy = 0.1)), 
            position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Tipo de Movimiento por Contiente de Nacionalidad", x = "Tipo de Movimiento", y = "Frecuencia Relativa")

#Motivo de viaje por Contiente Nacionalidad 

tabla_tip <- table(data_ESI$mot_viam, data_ESI$cont_nac)
tabla_rel <- as.data.frame(prop.table(tabla_tip, margin = 1))
colnames(tabla_rel) <- c("mot_viam", "cont_nac", "Frecuencia_Relativa")

ggplot(tabla_rel, aes(x = mot_viam, y = Frecuencia_Relativa, fill = cont_nac)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = scales::percent(Frecuencia_Relativa, accuracy = 0.1)), 
            position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Motivo de Viaje por Contiente de Nacionalidad", x = "Motivo de Viaje", y = "Frecuencia Relativa")

# Relaci髇 entre Motivo de Viaje y Edad
# Excluir casos at韕icos de la variable "edad"
sin_atipicos <- data_ESI %>%
  group_by(mot_viam) %>%
  filter(edad %in% boxplot.stats(edad)$stats)

# Verificar la exclusi髇
summary(sin_atipicos$edad)

# Graficar sin casos at韕icos y con m醩 marcas de edad en el eje y
ggplot(sin_atipicos, aes(x = mot_viam, y = edad)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_y_continuous(breaks = seq(0, max(sin_atipicos$edad, na.rm = TRUE), by = 5)) +
  theme_minimal() +
  labs(title = "Distribuci髇 de Edad por Motivo de Viaje", 
       x = "Motivo de Viaje", 
       y = "Edad")

#Separar la data en Entradas y Salidas
salidas <- data %>%
  filter(tip_movi == "Salida") %>%
  group_by(pais_prod) %>%
  summarise(count = n())

entradas <- data %>%
  filter(tip_movi == "Entrada") %>%
  group_by(pais_prod) %>%
  summarise(count = n())

world <- ne_countries(scale = "medium", returnclass = "sf")

# Contar el n鷐ero de movimientos de entrada por pa韘
entradas <- datos %>%
  filter(tip_movi == "Entrada") %>%
  group_by(pais_prod) %>%
  summarise(count = n())

# Contar el n鷐ero de movimientos de salida por pa韘
salidas <- datos %>%
  filter(tip_movi == "Salida") %>%
  group_by(pais_prod) %>%
  summarise(count = n())

# Unir los datos de movimientos con el shapefile
world_entradas <- world %>%
  left_join(entradas1, by = c("name" = "country"))

world_salidas <- world %>%
  left_join(data, by = c("name" = "country"))

# Dibujar el mapa de entradas
ggplot(data = world_entradas) +
  geom_sf(aes(fill = count)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(title = "Movimientos de Entrada en Ecuador por Pa韘",
       fill = "N鷐ero de Movimientos")

entradas$pais_prod[entradas$pais_prod == "Estados Unidos de America"] <- "United States of America"

#Dibujar mapa de entradas
ggplot(data = world_entradas) +
  geom_sf(aes(fill = count)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90",breaks = scales::pretty_breaks(n = 60) ) +
  theme_minimal() +
  labs(title = "Movimientos de Entrada en Ecuador por Pa韘",
       fill = "N鷐ero de Movimientos")

#Dibujar mapa de salidas
ggplot(data = world_salidas) +
  geom_sf(aes(fill = count)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90",breaks = scales::pretty_breaks(n = 60) ) +
  theme_minimal() +
  labs(title = "Movimientos de Salida de Ecuatorianos al Exterior",
       fill = "N鷐ero de Movimientos")

#Machine Learning
library(caret)
library(dplyr)
library(e1071)
library(nnet)
library(kernlab)

preProcess_missingdata <- preProcess(balanced_data, method = c("medianImpute"))
balanced_data <- predict(preProcess_missingdata, newdata = balanced_data)

# Estandarizar las variables num茅ricas
preProcess_standardize <- preProcess(balanced_data %>% select(edad), method = c("center", "scale"))
#balanced_data$edad <- predict(preProcess_standardize, newdata = balanced_data %>% select(edad))
names(balanced_data)
data_ESI<-balanced_data
rm(balanced_data)
names(data_ESI)


# Dividir los datos en conjunto de entrenamiento (80%) y prueba (20%)
set.seed(123)

data_ESI$edad<-as.numeric(data_ESI$edad)
trainIndex <- createDataPartition(data_ESI$tip_movi, p = .8, list = FALSE, times = 1)
data_train <- data_ESI[trainIndex,]
data_test  <- data_ESI[-trainIndex,]

#MODELOS PARA PREDECIR EL TIPO DE MOVIMIENTO
# Regresion log铆stica

model_logistic <- train(tip_movi ~  tip_naci + edad + sex_migr + mot_viam + pais_prod + via_tran + can_jefm + ocu_migr, data = data_train, method = "glm", family = "binomial")
predictions_logistic <- predict(model_logistic, newdata = data_test)
confusionMatrix(predictions_logistic, data_test$tip_movi)
summary(model_logistic)

#脕rbol de decisi贸n
model_tree <- train(tip_movi ~  tip_naci + edad + sex_migr + mot_viam + pais_prod + via_tran + can_jefm + ocu_migr, data = data_train, method = "rpart")
predictions_tree <- predict(model_tree, newdata = data_test)
confusionMatrix(predictions_tree, data_test$tip_movi)

#Modelo SVM
model_svm <- train(tip_movi ~  tip_naci + edad + sex_migr + mot_viam + pais_prod + via_tran + can_jefm + ocu_migr, data = data_train, method = "svmRadial")
predictions_svm <- predict(model_svm, newdata = data_test)
confusionMatrix(predictions_svm, data_test$tip_movi)

results <- resamples(list(logistica = model_logistic, DecisionTree = model_tree, SVM = model_svm))
summary(results)



#MODELOS PARA PREDECIR EL MOTIVO DE VIAJE
# Entrenar el modelo de regresi贸n log铆stica multinomial
model_multinom <- multinom(via_tran ~ edad + sex_migr + tip_movi + mot_viam + can_jefm + ocu_migr, data = data_train)
predictions_multinom <- predict(model_multinom, newdata = data_test)
confusionMatrix(predictions_multinom, data_test$via_tran)

# Entrenar el modelo de 谩rbol de decisi贸n
model_tree1 <- train(via_tran ~ edad + sex_migr + tip_movi + mot_viam + can_jefm + ocu_migr, data = data_train, method = "rpart")
predictions_tree1 <- predict(model_tree1, newdata = data_test)
confusionMatrix(predictions_tree1, data_test$via_tran)


# Entrenar el modelo de SVM
model_svm1 <- svm(via_tran ~ edad + sex_migr + tip_movi + mot_viam + can_jefm + ocu_migr, data = data_train)
predictions_svm1 <- predict(model_svm1, newdata = data_test)
confusionMatrix(predictions_svm1, data_test$via_tran)

results1 <- resamples(list(MultinomialLogistic = model_multinom, DecisionTree = model_tree1, SVM = model_svm1))
summary(results1)


names(data_ESI)
table(data_ESI$cont_prod)
data1<-data_ESI
data1$cont_prod<-droplevels(data1$cont_prod)
#data1$cont_prod<-factor(data1$cont_prod, levels = levels(data1$cont_prod)[levels(data1$cont_prod) != "Antartida"])
data1$cont_prod<-as.factor(data1$cont_prod)
summary(data1$cont_prod)

Entrada<-data1[data1$tip_movi=="Entrada",]
Salida<-data1[data1$tip_movi=="Salida",]

set.seed(567)


trainIndexE <- createDataPartition(Entrada$tip_movi, p = .8, list = FALSE, times = 1)
data_trainE <- Entrada[trainIndexE,]
data_testE  <- Entrada[-trainIndexE,]

trainIndexE <- createDataPartition(data_ESI$tip_movi, p = .8, list = FALSE, times = 1)
data_trainE <- data_ESI[trainIndex,]
data_testE  <- data_ESI[-trainIndex,]


model_multinom2 <- multinom(cont_prod ~ edad + sex_migr + via_tran + mot_viam + can_jefm + ocu_migr, data = Entrada)
predictions_multinom2 <- predict(model_multinom2, newdata = data_test)
confusionMatrix(predictions_multinom, data_test$cont_prod)

# Entrenar el modelo de 谩rbol de decisi贸n
model_tree1 <- train(via_tran ~ edad + sex_migr + tip_movi + mot_viam + can_jefm + ocu_migr, data = data_train, method = "rpart")
predictions_tree1 <- predict(model_tree1, newdata = data_test)
confusionMatrix(predictions_tree1, data_test$via_tran)


# Entrenar el modelo de SVM
model_svm1 <- svm(via_tran ~ edad + sex_migr + tip_movi + mot_viam + can_jefm + ocu_migr, data = data_train)
predictions_svm1 <- predict(model_svm1, newdata = data_test)
confusionMatrix(predictions_svm1, data_test$via_tran)

results1 <- resamples(list(MultinomialLogistic = model_multinom, DecisionTree = model_tree1, SVM = model_svm1))
summary(results1)
