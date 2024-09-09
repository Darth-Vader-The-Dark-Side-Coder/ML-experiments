library(ISLR)
data(Carseats)
summary(Carseats)
names(Carseats)
fix
data(Carseats) 

str(Carseats) 
install.packages("e1071") 
install.packages("caTools") 
install.packages("class") 

library(e1071)
library(caTools)
library(class)

data(Carseats)
head(Carseats) 

split <- sample.split(Carseats, SplitRatio = 0.9) 
train_cl <- subset(Carseats, split == "TRUE") 
test_cl <- subset(Carseats, split == "FALSE") 

train_scale <- scale(train_cl[, 1:4]) 
test_scale <- scale(test_cl[, 1:4]) 
train_scale

classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$Urban, 
                      k = 1) 
classifier_knn 

cm <- table(test_cl$Urban, classifier_knn) 
cm 

misClassError <- mean(classifier_knn != test_cl$Urban) 
print(paste('Accuracy =', 1-misClassError)) 

accuracies <- vector()

for(i in 1:20){
  print(paste("For k = ",i))
  classifier_knn <- knn(train = train_scale, 
                        test = test_scale, 
                        cl = train_cl$Urban, 
                        k = i) 
  misClassError <- mean(classifier_knn != test_cl$Urban) 
  print(paste('Accuracy =', 1-misClassError)) 
  accuracies[i] <- 1-misClassError
}
print(accuracies)
print(1:20)
plot(1:20,accuracies,ylab="Accuracy",xlab="K value", type='l')