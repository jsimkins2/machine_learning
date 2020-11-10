##### Distance
# Most clustering and machine learning techniques rely on being able to define distance between observations, using features or predictors.
# With high dimensional data, a quick way to compute all the distances at once is to use the function dist(), which computes the distance between each row and produces an object of class dist():

# knn
#K-nearest neighbors (kNN) estimates the conditional probabilities in a similar way to bin smoothing. However, kNN is easier to adapt to multiple dimensions.
#Using kNN, for any point  (𝑥1,𝑥2)  for which we want an estimate of  𝑝(𝑥1,𝑥2) , we look for the k nearest points to  (𝑥1,𝑥2)  and take an average 
#   of the 0s and 1s associated with these points. We refer to the set of points used to compute the average as the neighborhood. Larger values of k result in smoother estimates, while smaller values of k result in more flexible and more wiggly estimates. 


# overtraining and oversmoothing
# Over-training is the reason that we have higher accuracy in the train set compared to the test set. Over-training is at its worst when we set  𝑘=1 . With  𝑘=1 , the estimate for each  (𝑥1,𝑥2)  in the training set is obtained with just the  𝑦  corresponding to that point. 
# When we try a larger  𝑘 , the  𝑘  might be so large that it does not permit enough flexibility. We call this over-smoothing.
# Note that if we use the test set to pick this  𝑘 , we should not expect the accompanying accuracy estimate to extrapolate to the real world. This is because even here we broke a golden rule of machine learning: we selected the  𝑘  using the test set. Cross validation also provides an estimate that takes this into account.

# For  𝑘 -fold cross validation, we divide the dataset into a training set and a test set. We train our algorithm exclusively on the training set and use the test set only for evaluation purposes. 
# For each set of algorithm parameters being considered, we want an estimate of the MSE and then we will choose the parameters with the smallest MSE. In  𝑘 -fold cross validation, we randomly split the observations into  𝑘  non-overlapping sets, and repeat the calculation for MSE for each of these sets. Then, we compute the average MSE and obtain an estimate of our loss. Finally, we can select the optimal parameter that minimized the MSE.
# In terms of how to select  𝑘  for cross validation, larger values of  𝑘  are preferable but they will also take much more computational time. For this reason, the choices of  𝑘=5  and  𝑘=10  are common.

library(tidyverse)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]


fit <- train(x_subset, y, method = "glm")
fit$results



install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

pvals <- tt$p.value

ind <- which(pvals < 0.01)
length(ind)

ind <- which(pvals <= 0.01)
newx_subset <- x[,ind]
head(newx_subset)

fit <- train(newx_subset, y, method = "glm")
fit$results

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

tt <- colttests(x, y)
pvals <- tt$p.value

ind <- which(pvals <= 0.01)
newx_subset <- x[,ind]

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)



# When we don't have access to the entire population, we can use bootstrap to estimate the population median  𝑚  .
#  The bootstrap permits us to approximate a Monte Carlo simulation without access to the entire distribution. The general idea is relatively simple. We act as if the observed sample is the population. We then sample datasets (with replacement) of the same sample size as the original dataset. Then we compute the summary statistic, in this case the median, on this bootstrap sample.
# Note that we can use ideas similar to those used in the bootstrap in cross validation: instead of dividing the data into equal partitions, we simply bootstrap many times.

n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m

set.seed(1)
#use set.seed(1, sample.kind="Rounding") instead if using R 3.6 or later
N <- 250
X <- sample(income, N)
M<- median(X)
M

library(gridExtra)
B <- 10^5
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(M)
sd(M)

B <- 10^5
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()

quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

mean(M) + 1.96 * sd(M) * c(-1,1)

mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)


# comprehension check

library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

y <- rnorm(100, 0, 1)
sq <- quantile(y, 0.75)

set.seed(1)

B <- 10000
res <- replicate(B, {
  y_hat <- rnorm(100, 0, 1)
  t75q <- quantile(y_hat, 0.75)
})

mean(res)
sd(res)

# question 4
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)

# Create the indexes 
set.seed(1)
ind10 <- createResample(y, 10, list = FALSE)

# Create the dataframe 
df10 <- as.data.frame(as.table(ind10))

# Add a new column y in the dataframe that contains the actual values
df10 <- df10 %>% mutate(y = y[ind10])
head(df10)
# Calculate the quartile 
Q_stars <- df10 %>% group_by(df10$Var2) %>% summarize(Q_star = quantile(y, 0.75))

# Calculate the mean and sd of Q_stars$Q_star
mean(Q_stars$Q_star)

sd(Q_stars$Q_star)



# Question 5
set.seed(1)
y <- rnorm(100, 0, 1)

# Create the indexes 
set.seed(1)
ind10 <- createResample(y, 10000, list = FALSE)

# Create the dataframe 
df10 <- as.data.frame(as.table(ind10))

# Add a new column y in the dataframe that contains the actual values
df10 <- df10 %>% mutate(y = y[ind10])
head(df10)
# Calculate the quartile 
Q_stars <- df10 %>% group_by(df10$Var2) %>% summarize(Q_star = quantile(y, 0.75))

# Calculate the mean and sd of Q_stars$Q_star
mean(Q_stars$Q_star)

sd(Q_stars$Q_star)

# The bootstrap is particularly useful in situations when we do not have access to the distribution or it is unknown.

