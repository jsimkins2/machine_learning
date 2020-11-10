# LDA and QDA are not meant to be used with many predictors  𝑝  because the number of parameters needed to be estimated becomes too large.
# Curse of dimensionality: For kernel methods such as kNN or local regression, when they have multiple predictors used,  the span/neighborhood/window made to include a given percentage of the data become large. With larger neighborhoods, our methods lose flexibility. The dimension here refers to the fact that when we have  𝑝  predictors, the distance between two observations is computed in  𝑝 -dimensional space.

##### Classification and Regression Trees (CART)
# A tree is basically a flow chart of yes or no questions. The general idea of the methods we are describing is to define an algorithm that uses data to create these trees with predictions at the ends, referred to as nodes.
# When the outcome is continuous, we call the decision tree method a regression tree.

# Load data
library(tidyverse)
library(dslabs)
data("olive")
olive %>% as_tibble()
table(olive$region)
olive <- select(olive, -area)

# Predict region using KNN
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)

# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())

# plot values for eicosenoic and linoleic
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)

# load data for regression tree
data("polls_2008")
qplot(day, margin, data = polls_2008)

library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)

# visualize the splits 
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# change parameters
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# use cross validation to choose cp
library(caret)
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
ggplot(train_rpart)

# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# prune the tree 
pruned_fit <- prune(fit, cp = 0.01)









##### Classification (Decision) Trees
# Classification trees, or decision trees, are used in prediction problems where the outcome is categorical. 
# Decision trees form predictions by calculating which class is the most common among the training set observations within the partition, rather than taking the average in each partition.
# 2 of the more popular metrics to choose the partitions are the Gini index and entropy.

# fit a classification tree and plot it
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)

# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]







##### random forests
# Random forests are a very popular machine learning approach that addresses the shortcomings of decision trees. The goal is to improve prediction performance and reduce instability by averaging multiple decision trees (a forest of trees constructed with randomness).
# The general idea of random forests is to generate many predictors, each using regression or classification trees, and then forming a final prediction based on the average prediction of all these trees. To assure that the individual trees are not the same, we use the bootstrap to induce randomness. 
# A disadvantage of random forests is that we lose interpretability.
# An approach that helps with interpretability is to examine variable importance. To define variable importance we count how often a predictor is used in the individual trees. The caret package includes the function varImp that extracts variable importance from any model in which the calculation is implemented. 

library(randomForest)
fit <- randomForest(margin~., data = polls_2008) 
plot(fit)

polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# use cross validation to choose parameter
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


# 5.1.5 Comprehension Check: Trees and Random Forests
#q1
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
# Q: Which code correctly uses rpart to fit a regression tree and saves the result to fit?
#fit1b <- train(y~x, data=dat, method='rpart')
fit1 <- rpart(y ~ ., data = dat)

#q2
plot(fit1)
text(fit1)

#q3
dat %>% 
  mutate(y_hat = predict(fit1)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  # MISSING CODE
  geom_step(aes(x, y_hat), col=2)
# END MISSING CODE


#q4
library(randomForest)
# MISSING CODE
fit4 <- randomForest(y ~ x, data = dat)
# END MISSING CODE
dat %>% 
  mutate(y_hat = predict(fit4)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

#q5
plot(fit4)


#q6
library(randomForest)
# MISSING CODE
fit5 <- randomForest(y~x, data = dat, nodesize = 50, maxnodes = 25)
# END MISSING CODE
dat %>% 
  mutate(y_hat = predict(fit5)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
# Q: What code should replace #BLANK in the provided code?
plot(fit5)



################################################
# Caret Package 5.2
################################################

# The caret package helps provides a uniform interface and standardized syntax for the many different machine learning packages in R. Note that caret does not automatically install the packages needed.

library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]



# The train() function automatically uses cross-validation to decide among a few default values of a tuning parameter.
# The getModelInfo() and modelLookup() functions can be used to learn more about a model and the parameters that can be optimized.
# We can use the tunegrid() parameter in the train() function to select a grid of values to be compared.
# The trControl parameter and trainControl() function can be used to change the way cross-validation is performed.
# Note that not all parameters in machine learning algorithms are tuned. We use the train() function to only optimize parameters that are tunable.


getModelInfo("knn")
modelLookup("knn")

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)

train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune
train_knn$finalModel
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])

install.packages("gam")
modelLookup("gamLoess")

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1



# 5.2.3 Comprehension Check: Caret Package
# q1

library(caret)
library(dslabs)
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")

fit1 <- with(tissue_gene_expression, 
             train(x, y, method = "rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit1)  


# q2
library(caret)
library(broom)
library(dslabs)
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")
fit2 <- with(tissue_gene_expression, 
             train(x, y, method = "rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                   control = rpart.control(minsplit = 0)
             ))
confusionMatrix(fit2)
ggplot(fit2) 

# q3
plot(fit2$finalModel, margin = 0.1)
text(fit2$finalModel, cex = 0.75)

# q4
library(randomForest)
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
fit4 <- with(tissue_gene_expression, 
             train(x, y, method = "rf", tuneGrid = data.frame(mtry = seq(50, 200, 25)),  nodesize =1))
fit4
# Q: What value of mtry maximizes accuracy?
# A: 100
ggplot(fit4)



# q5

imp = varImp(fit4)


# q6

tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms


tree_terms <- as.character(unique(fit2$finalModel$frame$var[!(fit2$finalModel$frame$var == "<leaf>")]))
tree_terms
data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)



########### titanic exercises ######################
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)
# 3 significant digits
options(digits = 3)
# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# 1
set.seed(42, sample.kind = 'Rounding') # if R version >= 3.6
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean[-test_index,]
test_set <- titanic_clean[test_index,]
nrow(train_set)
nrow(test_set)
mean(train_set$Survived == 1)

#2
set.seed(3, sample.kind = 'Rounding') # if R version >= 3.6
guess_ <- sample(c(0,1), nrow(test_set), replace = TRUE)
test_set %>% 
  filter(Survived == guess_) %>%
  summarize(n() / nrow(test_set))



# 3
train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1))

test_set %>%
  summarize( (sum(Sex == 'female' & Survived == 1) + sum(Sex == 'male' & Survived == 0)) / n())

# 4
survival_class <- titanic_clean %>%
  group_by(Pclass) %>%
  summarize(PredictingSurvival = ifelse(mean(Survived == 1) >=0.5, 1, 0))
survival_class


test_set %>%
  inner_join(survival_class, by='Pclass') %>%
  summarize(PredictingSurvival = mean(Survived == PredictingSurvival))


survival_class <- titanic_clean %>%
  group_by(Sex, Pclass) %>%
  summarize(PredictingSurvival = ifelse(mean(Survived == 1) > 0.5, 1, 0))
survival_class


test_set %>%
  inner_join(survival_class, by=c('Sex', 'Pclass')) %>%
  summarize(PredictingSurvival = mean(Survived == PredictingSurvival))


# 5
# Confusion Matrix: sex model
sex_model <- train_set %>%
  group_by(Sex) %>%
  summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
test_set1 <- test_set %>%
  inner_join(sex_model, by = 'Sex')
cm1 <- confusionMatrix(data = factor(test_set1$Survived_predict), reference = factor(test_set1$Survived))
cm1 %>%
  tidy() %>%
  filter(term == 'sensitivity') %>%
  .$estimate
cm1 %>%
  tidy() %>%
  filter(term == 'specificity') %>%
  .$estimate
cm1 %>%
  tidy() %>%
  filter(term == 'balanced_accuracy') %>%
  .$estimate
# Confusion Matrix: class model
class_model <- train_set %>%
  group_by(Pclass) %>%
  summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
test_set2 <- test_set %>%
  inner_join(class_model, by = 'Pclass')
cm2 <- confusionMatrix(data = factor(test_set2$Survived_predict), reference = factor(test_set2$Survived))
cm2 %>%
  tidy() %>%
  filter(term == 'sensitivity') %>%
  .$estimate
cm2 %>%
  tidy() %>%
  filter(term == 'specificity') %>%
  .$estimate
cm2 %>%
  tidy() %>%
  filter(term == 'balanced_accuracy') %>%
  .$estimate
# Confusion Matrix: sex and class model
sex_class_model <- train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
test_set3 <- test_set %>%
  inner_join(sex_class_model, by=c('Sex', 'Pclass'))
cm3 <- confusionMatrix(data = factor(test_set3$Survived_predict), reference = factor(test_set3$Survived))
cm3 %>%
  tidy() %>%
  filter(term == 'sensitivity') %>%
  .$estimate
cm3 %>%
  tidy() %>%
  filter(term == 'specificity') %>%
  .$estimate
cm3 %>%
  tidy() %>%
  filter(term == 'balanced_accuracy') %>%
  .$estimate


# 6

F_meas(data=factor(test_set1$Survived), reference = factor(test_set1$Survived_predict))
F_meas(data=factor(test_set2$Survived), reference = factor(test_set2$Survived_predict))
F_meas(data=factor(test_set3$Survived), reference = factor(test_set3$Survived_predict))


###############################################
# Titanic Exercises Part 2
###############################################

# 7
fit_lda <- train(Survived ~ Fare, data = train_set, method = 'lda')
Survived_hat <- predict(fit_lda, test_set)
mean(test_set$Survived == Survived_hat)


fit_qda <- train(Survived ~ Fare, data = train_set, method = 'qda')
Survived_hat <- predict(fit_qda, test_set)
mean(test_set$Survived == Survived_hat)

# 8
fit_logreg_a <- glm(Survived ~ Age, data = train_set, family = 'binomial')
survived_hat_a <- ifelse(predict(fit_logreg_a, test_set) >= 0, 1, 0)
mean(survived_hat_a == test_set$Survived)

fit_logreg_b <- glm(Survived ~ Sex + Pclass + Fare + Age, data = train_set, family = 'binomial')
survived_hat_b <- ifelse(predict(fit_logreg_b, test_set) >= 0, 1, 0)
mean(survived_hat_b == test_set$Survived)

str(train_set)
fit_logreg_c <- glm(Survived ~ ., data = train_set, family = 'binomial')
survived_hat_c <- ifelse(predict(fit_logreg_c, test_set) >= 0, 1, 0)
mean(survived_hat_c == test_set$Survived)


# 9
set.seed(6, sample.kind = "Rounding")
# Method below doesn't give same result as EdX (though it is correct)
# ks <- seq(3,51,2)
# res_knn9a <- sapply(ks, function(k) {
#     fit_knn9a <- knn3(Survived ~ ., data = train_set, k = k)
#     survived_hat <- predict(fit_knn9a, train_set, type = "class") %>% factor(levels = levels(train_set$Survived))
#     cm_test <- confusionMatrix(data = survived_hat, reference = train_set$Survived)
#     cm_test$overall["Accuracy"]
# })
# ks[which.max(res_knn9a)]
# Other method using train function
k <- seq(3,51,2)
fit_knn9a <- train(Survived ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k))
fit_knn9a$bestTune

ggplot(fit_knn9a)

survived_hat <- predict(fit_knn9a, test_set) %>% factor(levels = levels(test_set$Survived))
cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]

# 10
set.seed(8, sample.kind = "Rounding")
fit_knn10 <- train(Survived ~ ., 
                   data=train_set, 
                   method = "knn",
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   trControl = trainControl(method = "cv", number=10, p=0.9))
fit_knn10
survived_hat <- predict(fit_knn10, test_set)
cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]

# 11
set.seed(10, sample.kind = 'Rounding')
fit_rpart11 <- train(Survived ~ ., 
                     data=train_set, 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
plot(fit_rpart11)
survived_hat <- predict(fit_rpart11, test_set)
cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]

fit_rpart11$finalModel
plot(fit_rpart11$finalModel, margin=0.1)
text(fit_rpart11$finalModel, cex = 0.75)



# 12
set.seed(14, sample.kind = 'Rounding')
fit12_rf <- train(Survived ~., 
                  data = train_set,
                  method = "rf", 
                  tuneGrid = data.frame(mtry = seq(1, 7)), 
                  ntree = 100)
fit12_rf$bestTune
survived_hat <- predict(fit12_rf, test_set)
mean(survived_hat == test_set$Survived)
varImp(fit12_rf)

