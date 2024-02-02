#caricamento dataset
data = read.csv(file = "ai4i2020.csv", header = TRUE, sep = ",")
df <- data.frame(data)

#fase esplorativa:
#numero di prodotti per ogni tipologia
occurrence_of_each_type <- aggregate(x = df$UDI, by = list(df$Type), FUN = length)
names(occurrence_of_each_type) <- c("type", "occurrence") 
occurrence_of_each_type

#media e range per ogni colonna del dataframe
matrix <- data.matrix(df[,4:8])
col.mean <- apply(matrix, 2, mean)
col.range <- apply(matrix, 2, range)
col.mean
col.range

#numero di guasti per ogni tipologia di guasto + percentuale guasti totali
length(which(df$TWF>0))
length(which(df$HDF>0))
length(which(df$PWF>0))
length(which(df$OSF>0))
length(which(df$RNF>0))
tot_gusti <- length(which(df$Machine.failure>0))
perc_guasti <- (tot_gusti/nrow(df))*100
perc_guasti

#confronto medie (Air.Temp e Rot.Speed) differenziando per esempi con e senza guasto
mean(df[df$Machine.failure==0,]$Air.temperature..K.)
mean(df[df$Machine.failure==1,]$Air.temperature..K.)
mean(df[df$Machine.failure==0,]$Rotational.speed..rpm.)
mean(df[df$Machine.failure==1,]$Rotational.speed..rpm.)

#analisi delle cause di fallimento
library(dplyr)
#guasto per dissipazione di calore (HDF)
filter(df, (Process.temperature..K.-Air.temperature..K.)<8.6 & Rotational.speed..rpm.<1380)

#guasto causato da valori di potenza superiori a 9000 W o inferiori a 3500 W (PWF)
filter(df, (Rotational.speed..rpm.*Torque..Nm.*2*pi)/60>9000 | (Rotational.speed..rpm.*Torque..Nm.*2*pi)/60<3500)

#guasto per sovraccarico (OSF)
filter(df, ((Torque..Nm.*Tool.wear..min.)>11000 & Type=="L") | ((Torque..Nm.*Tool.wear..min.)>12000 & Type=="M") | ((Torque..Nm.*Tool.wear..min.)>13000 & Type=="H"))

#calcolo di correlazioni tra features del dataframe "df"
cor(df$Air.temperature..K., df$Process.temperature..K.)
cor(df$Process.temperature..K., df$Rotational.speed..rpm.)
cor(df$Rotational.speed..rpm., df$Air.temperature..K.)
cor(df$Rotational.speed..rpm., df$Torque..Nm.)
cor(df$Rotational.speed..rpm., df$Tool.wear..min.)
cor(df$Torque..Nm., df$Tool.wear..min.)
cor(df$Torque..Nm., df$Air.temperature..K.)


#scatter plot 
plot(df$Rotational.speed..rpm., df$Torque..Nm., cex=0.05, xlab="Rotational.speed[rpm]", ylab="Torque[Nm]")
plot(df$Air.temperature..K., df$Process.temperature..K., cex=0.3, xlab="Air.temperature[K]", ylab="Process.temperature[K]")

library(ggplot2)
ggplot(df, aes(x=UDI, y=Air.temperature..K.)) + geom_line(size=0.3) + labs(x="product processed [N°]", y="Air temperature [K]") + ggtitle("Air temperature trend")

ggplot(df, aes(x=UDI, y=factor(Machine.failure))) + geom_point(alpha=0.3, size=0.001) + labs(x="product processed [N°]", y="Machine Failure") + ggtitle("Machine failure trend")

noMF_df <- df %>% filter(df$Machine.failure == 0)
MF_df <- df %>% filter(df$Machine.failure == 1)
ggplot(df, aes(x=UDI, y=Air.temperature..K.)) + geom_line(data=noMF_df, size=0.2, aes(color="No failure")) + geom_point(data=MF_df, size=0.5, aes(color="Failure points")) + labs(x="Product processed [N°]", y ="Air temperature [K]") + ggtitle("Examination machine failures")

noPWF_df <- df %>% filter(df$PWF == 0)
PWF_df <- df %>% filter(df$PWF == 1)
ggplot(df, aes(Rotational.speed..rpm., Torque..Nm.)) + geom_point(data=noPWF_df, alpha=0.2, aes(color="No failure PWF")) + geom_point(data=PWF_df, alpha=0.8, aes(color="failure PWF")) + labs(x="Rotational speed [rpm]", y ="Torque [Nm]") + ggtitle("Examination PWF failures")
ggplot(df, aes(Tool.wear..min.,Torque..Nm.)) + geom_point(data=noPWF_df, alpha=0.3, aes(color="No failure PWF")) + geom_point(data=PWF_df, aes(color="failure PWF")) +labs(x="Tool wear [min]", y="Torque [Nm]") + ggtitle("Examination PWF failures 2")

noOSF_df <- df %>% filter(df$OSF == 0)
OSF_df <- df %>% filter(df$OSF == 1)
ggplot(df, aes(Tool.wear..min.,Torque..Nm.)) + geom_point(data=noOSF_df, alpha=0.3, aes(color="No failure OSF")) + geom_point(data=OSF_df, alpha=0.8, aes(color="failure OSF")) + labs(x="Tool wear [min]", y ="Torque [Nm]") + ggtitle("Examination OSF failures")

#bar plot (utilizzare library(dplyr))
f.type <- c("TWF","HDF","PWF","OSF","RNF") 
times <- c(46,115,95,98,19)
failure_modes <- data.frame(f.type, times)
failure_modes_sorted <- arrange(failure_modes,-times, f.type)
failure_modes_sorted$f.type <- factor(failure_modes_sorted$f.type, levels=failure_modes_sorted$f.type)
ggplot(data=failure_modes_sorted, aes(x=f.type, y=times)) + geom_bar(stat="identity", fill="sandybrown") + labs(x="Failure mode", y="Occurrence [N°]") + ggtitle("Occurrence for each failure mode")

#MACHINE LEARNING
#fase pre-processing dati
colSums(is.na(df))
DFOriginal = df
df[df == "L"] <- 0
df[df == "M"] <- 0.5
df[df == "H"] <- 1
df <- df[3:9]
for (i in 1:ncol(df)) df[,i] <- as.numeric(df[,i])

#normalizzazione dati
features <- df[1:6]
classes <- df[7]
min_col_value <- c(apply(features, 2, min))
max_col_value <- c(apply(features, 2, max))
for(i in c(1:ncol(features))) {                        
  for (j in c(1:nrow(features))) {                                                            
    features[j,i]=((features[j,i]-min_col_value[i])/(max_col_value[i]-min_col_value[i]))          
  }
}
head(features)


#decision tree
library(rpart)
library(rpart.plot)
best_tree <- NULL
best_F1 <- 0
best_featuresVector <- 0
V1 <- c(1,2,3,4,7)
V2 <- c(2,3,5,6,7)
V3 <- c(3,4,5,6,7)
V4 <- c(2,3,4,6,7)
V5 <- c(2,3,4,5,6,7)
V6 <- c(2,4,5,7)
V7 <- c(3,5,6,7)
List <- list(V1,V2,V3,V4,V5,V6,V7)
for(i in seq(1:7)){                                                        
  train <- df[1:4500, List[[i]]]
  validation <- df[4501:6400,List[[i]]]
  tree <- rpart(train$Machine.failure ~ ., data = train, method = "class")
  predictions <- predict(tree, validation, type = "class")
  conf_matrix <- table(predictions, validation$Machine.failure)                
  precision <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[2,1])
  recall <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[1,2])  
  F1 <- (2*precision*recall)/(precision + recall)                 
  if(F1 > best_F1){
    best_F1 <- F1
    best_tree <- tree
    best_featuresVector <- i
  }
}
best_featuresVector

train <- df[1:4500, List[[best_featuresVector]]]
validation <- df[4501:6400,List[[best_featuresVector]]]
tree <- rpart(train$Machine.failure ~ ., data = train, parms = list(split="information"), method = "class")
predictions <- predict(tree, validation, type = "class")
conf_matrix <- table(predictions, validation$Machine.failure)                
precision <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[2,1])
recall <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[1,2])  
F1 <- (2*precision*recall)/(precision + recall)                                              
if(F1 > best_F1){
  best_F1 = F1
  best_tree = tree
}
best_F1
rpart.plot(best_tree)
#applicazione sul test set
test <- df[6401:10000, List[[best_featuresVector]]]
predictions <- predict(best_tree, test, type = "class")
conf_matrix <- table(predictions, test$Machine.failure) 
accuracy <- (conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))                
precision <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[2,1])
recall <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[1,2])  
F1 <- (2*precision*recall)/(precision + recall) 
F1
accuracy


#KNN
library(class)
classes_train <- classes[1:4500,]
classes_validation <- classes[4501:6400,]
classes_test <- classes[6401:10000,]
best_F1 <- 0
best_featuresVector <- 0
best_K <- 0
V1 <- c(1,2,3,4)
V2 <- c(2,3,5,6)
V3 <- c(3,4,5,6)
V4 <- c(2,3,4,6)
V5 <- c(2,3,4,5,6)
V6 <- c(2,4,5)
V7 <- c(3,5,6)
V8 <- c(1,2,3,4,5,6)
List <- list(V1,V2,V3,V4,V5,V6,V7,V8)
K_vector <- c(1,3,5,7,9,11,13,15,17)
for(i in seq(1:8)){   
  for(j in seq(1:9)){                                                        
    features_train <- features[1:4500, List[[i]]]
    features_validation <- features[4501:6400,List[[i]]]
    pred_valid <- knn(features_train, features_validation, classes_train, k = K_vector[j])
    conf_matrix <- table(pred_valid, classes_validation)  
    precision <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[2,1])
    recall <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[1,2])  
    F1 <- (2*precision*recall)/(precision+recall)                 
    if(F1 > best_F1){
      best_F1 <- F1
      best_K <- K_vector[j]
      best_featuresVector <- i
    }
  }
}
best_F1 
best_K 
best_featuresVector 
#applicazione sul test set
features_train <- features[1:4500, List[[best_featuresVector]]]
features_test <- features[6401:10000, List[[best_featuresVector]]]
pred_valid <- knn(features_train, features_test, classes_train, k = best_K)
conf_matrix <- table(pred_valid, classes_test)  
precision <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[2,1])
recall <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[1,2])  
F1 <- (2*precision*recall)/(precision + recall)  
accuracy <- (conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))  
F1
accuracy


#regressione logistica
best_F1 <- 0
best_featuresVector <- 0
best_reg <- NULL
V1 <- c(1,2,3,4,7)
V2 <- c(2,3,5,6,7)
V3 <- c(3,4,5,6,7)
V4 <- c(2,3,4,6,7)
V5 <- c(2,3,4,5,6,7)
V6 <- c(2,4,5,7)
V7 <- c(3,5,6,7)
V8 <- c(1,2,3,4,5,6,7)
List <- list(V1,V2,V3,V4,V5,V6,V7,V8)
for(i in seq(1:8)){                                                        
  train <- df[1:4500, List[[i]]]
  validation <- df[4501:6400,List[[i]]]
  reg <- glm(train$Machine.failure  ~ ., family = "binomial", data=train)
  out <- predict(reg, validation, type = "response")
  out[out<0.5] <- 0
  out[out>=0.5] <- 1
  conf_matrix <- table(validation$Machine.failure, out)         
  if (ncol(conf_matrix) == 2){
    precision <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[2,1])
    recall <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[1,2])  
    F1 <- (2*precision*recall)/(precision + recall)                 
    if(F1 > best_F1){
      best_F1 <- F1
      best_featuresVector <- i
      best_reg <- reg
    }
  }
}
best_F1 
best_featuresVector 
best_reg 
#applicazione sul test set
test <- df[6401:10000, List[[best_featuresVector]]]
out <- predict(best_reg, test, type = "response")
out[out<0.5] <- 0
out[out>=0.5] <- 1
conf_matrix <- table(test$Machine.failure, out)         
accuracy <- (conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))                
precision <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[2,1])
recall <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[1,2])  
F1 <- (2*precision*recall)/(precision + recall) 
F1
accuracy


#kernel machines
library(e1071)
df$Machine.failure <- factor(df$Machine.failure, levels = c("0","1"))
best_F1 <- 0
best_cost <- 0
best_degree <- 0
best_kModel <- NULL
train <- df[1:4500,]
validation <- df[4501:6400,]

for(c in c(0.1, 1, 2, 3, 4, 5, 10, 50, 100, 200, 1000)){
  for(d in c( 3.5, 4, 5, 5.5, 6, 7, 8, 9, 10)){
    kernel_model <- svm(train$Machine.failure ~ ., data = train, method ="C-classification",
                    kernel = "polynomial", degree = d, cost =c, class.weights = c("0" = 0.3, "1" = 0.7))
    predictions <- predict(kernel_model, validation)
    conf_matrix <- table(predictions, validation$Machine.failure)
    precision <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[2,1])
    recall <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[1,2])  
    F1 <- (2*precision*recall)/(precision + recall) 
    if(F1 > best_F1){
      best_F1 <- F1
      best_cost <- c
      best_degree <- d
      best_kModel <- kernel_model
    } 
  }
}
best_F1
best_cost 
best_degree 
best_kModel 
#applicazione sul test set
test <- df[6401:10000,]
predictions <- predict(best_kModel, test)
conf_matrix <- table(predictions, test$Machine.failure)
accuracy <- (conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
precision <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[2,1])
recall <- (conf_matrix[2,2])/(conf_matrix[2,2]+conf_matrix[1,2])  
F1 <- (2*precision*recall)/(precision + recall)
F1
accuracy