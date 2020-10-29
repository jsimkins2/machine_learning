library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
# line of code
test <- iris[test_index,]
train <- iris[-test_index,]


test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)


cutoff <- seq(3.3,6.9, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length> x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff


y_hat <- ifelse(train$Petal.Length > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(train$Species))
y_hat <- factor(y_hat)
mean(y_hat == test)




cutoff <- seq(4.9,7.7, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Length> x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
