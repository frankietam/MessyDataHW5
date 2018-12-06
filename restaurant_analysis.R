## Messy Data and Machine Learning Homework 5
## Paul Sergent, Ruoyu Zhu, Frankie Tam

# Question 2

source('library.R')

all_data <- read.csv('DOHMH_New_York_City_Restaurant_Inspection_Results.csv')

#Part A(a.)

#(i.)
#Remove columns
all_data$ZIPCODE <- NULL
all_data$BUILDING <- NULL
all_data$STREET <- NULL
all_data$PHONE <- NULL
all_data$DBA <- NULL
all_data$RECORD.DATE <- NULL
all_data$VIOLATION.DESCRIPTION <- NULL
all_data$GRADE.DATE <- NULL
  
#(ii.)
#Create date and year columns
inspection_date <-  as.Date(all_data$INSPECTION.DATE, "%m/%d/%Y")
inspection_year <-  year(inspection_date)
all_data <- mutate(all_data, 
                   inspection_year = inspection_year,
                   inspection_date = inspection_date)
all_data$INSPECTION.DATE <- NULL

#(iii.)
#Rename columns
all_data <- plyr::rename(all_data, c('CAMIS' = 'id',
                               'BORO' = 'borough',
                               'CUISINE.DESCRIPTION' = 'cuisine',
                               'ACTION' = 'action', 
                               'VIOLATION.CODE' = 'code',
                               'CRITICAL.FLAG' = 'critical',
                               'SCORE' = 'score', 
                               'GRADE' = 'grade', 
                               'INSPECTION.TYPE' = 'inspection_type'))

#Simplify ACTION column

all_data$action <- gsub(pattern = '[[:punct:]]', replacement = '', x = all_data$action) 

all_data$action <- gsub(pattern = 'Establishment reopened by DOHMH', replacement = 're-opened; ', x = all_data$action)
all_data$action <- gsub(pattern = 'Violations were cited in the following area', replacement = 'violations; ', x = all_data$action)
all_data$action <- gsub(pattern = 'Establishment Closed by DOHMH', replacement = 'closed;', x = all_data$action)
all_data$action <- gsub(pattern = 'Establishment reclosed by DOHMH', replacement = 're-closed; ', x = all_data$action)
all_data$action <- gsub(pattern = 'No violations were recorded at the time of this inspection', replacement = 'no violations; ', x = all_data$action)
all_data$action <- gsub(pattern = 'those requiring immediate action were addressed', replacement = 'immediate action taken; ', x = all_data$action)

all_data$action <- gsub(pattern = ' s', replacement = '', x = all_data$action)
all_data$action <- gsub(pattern = ' and', replacement = '', x = all_data$action)


all_data <- all_data %>% filter(action != '')


#Part A(b.)

#Remove observations with 'Missing' borough 
all_data <- all_data %>% filter(all_data$borough != "Missing")

#Remove observations for restaurants not inspected
all_data <- all_data %>%  filter(all_data$inspection_date != "1900-01-01")

#Remove observations without or with negative score 
all_data <- all_data %>%  filter(all_data$score != "NA")
#all_data <- all_data %>%  filter(!is.na(all_data$score))
all_data <- all_data %>%  filter(all_data$score >= 0)

#(i.) to (vi.)
#Remove observations with specific inspection types
remove_inspections <- c("Calorie Posting / Re-inspection", 
                         "Inter-Agency Task Force / Re-inspection", 
                         "Smoke-Free Air Act / Re-inspection", 
                         "Administrative Miscellaneous / Re-inspection", 
                         "Trans Fat / Re-inspection", 
                         "Inter-Agency Task Force / Initial Inspection")


remove_inspections <- all_data %>% filter(all_data$inspection_type %in% remove_inspections)

all_data <- anti_join(all_data, remove_inspections, by = "inspection_type")

#Simplify INSPECTION_TYPE column

all_data$inspection_type <- gsub(pattern = ' Inspection', replacement = '', x = all_data$inspection_type)
all_data$inspection_type <- gsub(pattern = '[[:punct:]]', replacement = '', x = all_data$inspection_type)


#Part A(c.)
#assign maximum score for each inspection, to all observations of said inspection
all_data <- all_data %>% 
  group_by(id, inspection_date, score) %>% 
  mutate(max_score = max(score))

all_data$score <- NULL

all_data <- all_data %>% rename(score = max_score)

#check for duplicate inspections with different scores
check <- all_data %>%  summarise(n_distinct(score))

stored_data <- all_data
stored_data_c <- all_data

#Part B
restaurant_data <- all_data %>% 
  filter(grepl('Initial', inspection_type)) %>% 
  filter(inspection_year == 2015  | 
           inspection_year == 2016  | 
           inspection_year == 2017)

#Part B(a.)
restaurant_data <- restaurant_data %>% mutate(outcome = as.numeric(score >= 28))
restaurant_data_cpoy <- restaurant_data #make a copy (cpoy is a typo! )

#Part B(b.)
# saving inspection date and score for Part b
#restaurant_data <- restaurant_data %>% select(-action, -code, -grade, -critical, -inspection_type)
restaurant_data <- restaurant_data %>% dplyr::select(borough, cuisine, outcome, inspection_year)

#Part C(a.)
restaurant_data <- mutate(restaurant_data, 
                          inspection_month = month(inspection_date),
                          inspection_day = weekdays(inspection_date),
                          initial = T)

restaurant_data <- restaurant_data %>% ungroup()
restaurant_data <- unique(restaurant_data)

restaurant_data_copy_2 <- restaurant_data #copy 2

#Part C(b.)
# left join 
merged <- merge(restaurant_data,all_data[c('id', 'score', 'action', 'inspection_date')],
                by = 'id', all.x = TRUE, allow.cartesian=TRUE)
merged <- merged %>% filter(inspection_date.y < inspection_date.x) %>% distinct
# Get summarized merged first, then left join with restaurant_data
merged <- merged %>% group_by(id, inspection_date.x) %>%   
  dplyr::summarise(num_previous_low_inspections= sum(score.y <14),
                   num_previous_med_inspections= sum(score.y >=14 & score.y <28),
                   num_previous_high_inspections= sum(score.y >=28),
                   num_previous_closings= sum(action %in% c('closed',"re-closed"))
  )


#merged <- merged %>% rename('inspection_date.x' = 'inspection_date')
merged <- merged %>% rename('inspection_date' = 'inspection_date.x')
restaurant_data<-restaurant_data %>% merge(merged,by = c('id','inspection_date'),all.x = TRUE)

restaurant_data$initial <- NULL
restaurant_data$n <- NULL
restaurant_data[is.na(restaurant_data)] <- 0

#Part C(c.)
cuisine_rank <- restaurant_data %>% group_by(cuisine) %>% count(cuisine)
cuisine_50 <- head(arrange(cuisine_rank,desc(n)), n = 50)
restaurant_data <- restaurant_data %>% merge(cuisine_50, by = 'cuisine')


restaurant_data <- restaurant_data %>% transform(inspection_month = as.factor(inspection_month),
                                                 inspection_day = as.factor(inspection_day),
                                                 outcome = as.factor(outcome),
                                                 num_previous_low_inspections = as.factor(num_previous_low_inspections),
                                                 num_previous_med_inspections = as.factor(num_previous_med_inspections),
                                                 num_previous_high_inspections = as.factor(num_previous_high_inspections),
                                                 num_previous_closings = as.factor(num_previous_closings))

restaurant_data <- restaurant_data %>% droplevels()
#Part D

#only use 2015 and 2016 data for training set
train.lr <- restaurant_data %>% filter(inspection_year==2015|inspection_year==2016)

# use 2017 data for testing set
test.lr <- restaurant_data %>% filter(inspection_year==2017)

# train model with training set
model.lr <- glm(outcome ~ cuisine + borough + inspection_month + inspection_day, data=train.lr, family = 'binomial')

# examine model
summary(model.lr)

# generate predictions for testing set
test.lr$predicted.probability <- predict(model.lr, newdata = test.lr, type='response') 


# compute AUC using ROCR package
test.pred.lr <- prediction(test.lr$predicted.probability, test.lr$outcome)
test.pref.lr <- performance(test.pred.lr, "auc")
cat('the auc score is ', 100*test.pref.lr@y.values[[1]], "\n") 


#Part E

#only use 2015 and 2016 data for training set
train.rf <- restaurant_data %>% filter(inspection_year==2015|inspection_year==2016)

# use 2017 data for testing set
test.rf <- restaurant_data %>% filter(inspection_year==2017)

# Create a Random Forest model with default parameters
model.rf <- randomForest(outcome ~ cuisine + borough + inspection_month + inspection_day + 
                           num_previous_low_inspections + num_previous_med_inspections + 
                           num_previous_high_inspections + num_previous_closings, 
                         data = train.rf, ntree = 1000, importance = TRUE)


model.rf


# testing set
test.rf$predicted.probability <- predict(model.rf, newdata = test.rf, type='prob') 

# compute AUC using ROCR package
testrf.pred <- prediction(test.rf$predicted.probability[,2], test.rf$outcome)
testrf.perf <- performance(testrf.pred, "auc")
cat('the auc score is ', 100*testrf.perf@y.values[[1]], "\n") 


#Part F

#Logistic regression
test_test <- test.lr %>% arrange(desc(predicted.probability))

plot.data.lr <- test.lr %>% arrange(desc(predicted.probability)) %>% 
  mutate(num_restaurant = row_number(), percent.precision = cumsum(as.numeric(outcome))/n()) %>% 
  select(num_restaurant, percent.precision)

#Randomforest
test_test_rf <- test.rf %>% arrange(desc(predicted.probability[,2]))

plot.data.rf <- test.rf %>% arrange(desc(predicted.probability)) %>% 
  mutate(numstops = row_number(), percent.recall = cumsum(found.contraband)/sum(found.contraband),
         stops = numstops/n()) %>% select(stops, percent.recall)

theme_set(theme_bw())
#p <- ggplot(data=plot.data, aes(x=num_restaurant, y=percent.precision)) 
p <- ggplot()
p <- p + geom_line(data=plot.data.lr, aes(x=num_restaurant, y=percent.precision))
p <- p + geom_line(data=plot.data.lr, aes(x=num_restaurant, y=percent.precision))
p <- p + scale_x_continuous('\nNumber of Restaurants', limits=c(100, 2000), breaks=c(100,200,500,1000,1500,2000), 
                            labels=c('100','200','500','1000','1500','2000'))
p <- p + scale_y_continuous("Percent of Initial Inspection is higher than 28", limits=c(0, 1), labels=scales::percent)
p