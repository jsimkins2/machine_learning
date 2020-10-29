library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


inclass = dat[dat$type== "inclass",]
table(inclass)

online = dat[dat$type == "online",]
table(online)

levels(dat$sex) = c("Male", "Female")
test = dat
test$sex[test$type == "online"] = "Male"
test$sex[test$type == "inclass"] = "Female"
y_hat <- sample(c("Male", "Female"), length(test$sex), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test$sex), replace = TRUE) %>% factor(levels = levels(test$sex))

mean(y_hat == test$sex)
x = test$type
y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(test$sex))
mean(y_hat == test$sex)


y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)

table(predicted = y_hat, actual = dat$sex)

cm <- confusionMatrix(data = y_hat, reference = y)

