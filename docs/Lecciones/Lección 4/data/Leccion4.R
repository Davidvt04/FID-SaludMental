# Ejercicio 1
# Apartado 1
setwd('C:/Users/david/OneDrive - UNIVERSIDAD DE SEVILLA/Máster Cloud, Datos y Gestión TI/FID/Lecciones/Lección 4/data')

train<-read.delim("data/train.csv", sep = "\t", head = TRUE) 
rownames(train) <- train$id 
train$id <- NULL 
head(train)

#Apartado 2
install.packages("rattle") 
library(rpart) 
library(rattle) 
library(RColorBrewer)

#Apartado 3
help("rpart")
# Fórmula - Conjunto de datos - método
#La fórmula: Variable a predecir ~ atributos sobre los que voy a predecir (. es todos)
tree <- rpart(Survived~., train, method = "class")
fancyRpartPlot(tree) 
#Indique cuál de las siguientes afirmaciones es correcta
# El árbol predecirá que las pasajeras de la clase 3 no sobrevivirán, aunque está cerca. VERDAD
# La clase mayoritaria del nodo raíz es positiva, lo que denota supervivencia. FALSO
# El atributo que sigue al género es hombre es una variable categórica. FALSO


#Apartado 4
test <- read.csv("data/test.csv", sep = "\t", head =TRUE) 
test$id <- NULL
head(test)

names(test)  

pred <- predict(tree, test, type = "class")
# Construye la matriz de confusion
conf <- table (test$Survived, pred)

#Calcula el accuracy
acc <- sum(diag(conf)) / sum(conf)
print(acc)


#Apartado 5
set.seed(1) 
tree2 <- rpart(Survived ~., train, method="class", 
              control=rpart.control(cp=0.00001)) 
fancyRpartPlot(tree2)

pred2 <- predict(tree2, test, type = "class")
# Construye la matriz de confusion
conf2 <- table (test$Survived, pred2)

#Calcula el accuracy
acc2 <- sum(diag(conf2)) / sum(conf2)
print(acc2)


#Pode el árbol con la función prune y vuelva a dibujarlo.
tree2_podado <- prune(tree2, cp = 0.01)
fancyRpartPlot(tree2_podado)

#Apartado 6
tree$parms
# ¿Los árboles construidos qué heurística aplican? Gini

# Modificar la heurística y generar el árbol tree_i
tree_i <- rpart(Survived ~ ., train, method = "class",
                parms = list(split = "information"))

# Dibujar el árbol
fancyRpartPlot(tree_i)

#Ejercicio 2

all_probs <- predict(tree, test, type="prob")
summary(all_probs) 
all_probs[,2] 
probs <- all_probs[,2]

#Apartado 1
install.packages("ROCR")
library(ROCR)

pred <- prediction(probs, test$Survived )

performance <- performance(pred, "tpr", "fpr")
plot(performance)

#Apartado 2
perf_auc <- performance(pred, "auc") 
print(perf_auc@y.values[[1]]) 

#Apartado 3
all_probs_i <- predict(tree_i, test, type="prob")
summary(all_probs_i) 
all_probs_i[,2] 
probs_i <- all_probs_i[,2]

pred_i <- prediction(probs_i, test$Survived )

performance_i <- performance(pred_i, "tpr", "fpr")
plot(performance_i)

perf_auc_i <- performance(pred_i, "auc") 
print(perf_auc_i)
print(perf_auc_i@y.values[[1]])

#Ejercicio 3
#Apartado 1
train_labels <- train$Survived 
test_labels <- test$Survived 
knn_train <- train 
knn_test <- test 
knn_train$Survived <- NULL 
knn_test$Survived <- NULL

#Apartado 2
library(scales)

knn_train$Age <- rescale(knn_train$Age)
knn_train$Pclass <- rescale(knn_train$Pclass)

knn_train$Sex[knn_train$Sex=="male"] <-1
knn_train$Sex[knn_train$Sex=="female"] <-0
knn_train$Sex <- as.numeric(knn_train$Sex)

knn_test$Age <- rescale(knn_test$Age)
knn_test$Pclass <- rescale(knn_test$Pclass)

knn_test$Sex[knn_test$Sex=="male"] <-1
knn_test$Sex[knn_test$Sex=="female"] <-0
knn_test$Sex <- as.numeric(knn_test$Sex)


#Apartado 3
library(class)
pred <- knn(knn_train, knn_test, cl=train_labels, k=5)

tab <- table(test_labels, pred)
tab
acc_knn <- sum(diag(tab)) /sum(tab)
acc_knn

#Apartado 4
set.seed(1) 
range <- 1:round(0.2 * nrow(knn_train)) 
accs <- rep(0, length(range))

for (k in range) {
  pred <- knn(
    train = knn_train,
    test = knn_test,
    cl = train_labels,
    k = k
  )
  conf <- table(test_labels, pred)
  
  accs[k] <- sum(diag(conf)) /sum(conf)
}

plot(range, accs, xlab = "k") 
which.max(accs) 
