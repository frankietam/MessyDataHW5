## Messy Data and Machine Learning Homework 5
## Paul Sergent, Ruoyu Zhu, Frankie Tam

# Question 1

library(randomForest)
library(ROCR)

# import data
sqf_08_16.data <- read_csv('sqf_08_16.csv')

# restrict to CPW stops
sqf <- sqf_08_16.data %>% filter(suspected.crime=='cpw') 


sqf <- sqf %>%
  select(id, year, found.weapon, precinct, location.housing, 
         stopped.bc.desc, stopped.bc.violent, stopped.bc.other, stopped.bc.object, 
         stopped.bc.casing, stopped.bc.lookout, stopped.bc.drugs, stopped.bc.clothing, 
         stopped.bc.furtive, stopped.bc.bulge, 
         additional.report, additional.investigation, additional.proximity, additional.evasive,
         additional.associating, additional.direction, additional.highcrime, additional.time, 
         additional.sights, additional.other, 
         suspect.age, suspect.build, suspect.sex, suspect.height, suspect.weight,
         inside, radio.run, officer.uniform, observation.period, day, month, time.period)

# Convert variable types as necessary
sqf <- sqf %>% mutate(suspect.build = as.factor(suspect.build),
                      suspect.sex = as.factor(suspect.sex),
                      location.housing = as.factor(location.housing),
                      day = as.factor(day),
                      month = as.factor(month),
                      time.period = as.factor(time.period),
                      precinct = as.factor(precinct),
                      found.weapon = as.factor(found.weapon))

# A)

# spread precinct into binary indicator
# age, height, weight
precinct_spread <- sqf %>% count(id, precinct) %>% spread(precinct, n, fill=0)

# join precinct_spread
sqf_spread <- left_join(sqf, precinct_spread, by = "id")

# remove precinct column
sqf_spread <- sqf_spread %>% select(-precinct)

# convert precinct column names to legal names
names(sqf_spread) <- make.names(names(sqf_spread))

# restrict to 2013, 2014 
sqf.1314 <- sqf_spread %>% filter(year==2013|year==2014)

# shuffle the data
sqf.1314 <- sqf.1314 %>% slice(sample(1:n()))

# 50% for training set
split_size = floor(nrow(sqf.1314)/2)
train_half <- sqf.1314 %>% slice(1:split_size)

# 50% for test set
test_half <- sqf.1314 %>% slice(split_size+1:n())

# restrict to 2015
test_later <- sqf_spread %>% filter(year==2015)

# remove stop id, year
train_half <- train_half %>% select(-id, -year)
test_half <- test_half %>% select(-id, -year)
test_later <- test_later %>% select(-id, -year)

# B)

# remove rows with NA
# remove observations with missing values
sum(is.na(train_half))
train_half <- na.omit(train_half)

# Create a Random Forest model with default parameters
model <- randomForest(found.weapon ~ ., data = train_half, ntree = 200, importance = TRUE)
model

# C)

# test half set
test_half$predicted.probability <- predict(model, newdata = test_half, type='prob') 

# compute AUC using ROCR package
testhalf.pred <- prediction(test_half$predicted.probability[,2], test_half$found.weapon)
testhalf.perf <- performance(testhalf.pred, "auc")
cat('the auc score is ', 100*testhalf.perf@y.values[[1]], "\n") 

# test later set
test_later$predicted.probability <- predict(model, newdata = test_later, type='prob')

# compute AUC using ROCR package
testlater.pred <- prediction(test_later$predicted.probability[,2], test_later$found.weapon)
testlater.perf <- performance(testlater.pred, "auc")
cat('the auc score is ', 100*testlater.perf@y.values[[1]], "\n")

