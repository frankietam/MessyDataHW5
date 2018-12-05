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
all_data <- rename(all_data, c('CAMIS' = 'id',
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


#all_data <- all_data %>% filter(action != '')


#Part A(b.)

#Remove observations with 'Missing' borough 
all_data <- all_data %>% filter(all_data$borough != "Missing")

#Remove observations for restaurants not inspected
all_data <- all_data %>%  filter(all_data$inspection_date != "1900-01-01")

#Remove observations without or with negative score 
#all_data <- all_data %>%  filter(all_data$score != "NA")
all_data <- all_data %>%  filter(!is.na(all_data$score))
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
  group_by(id, inspection_date) %>% 
  mutate(max_score = max(score))

all_data$score <- NULL

all_data <- all_data %>% rename(score = max_score)

#check for duplicate inspections with different scores
check <- all_data %>%  summarise(n_distinct(score))

stored_data <- all_data

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
#restrict to id, score, action, and inspection_date
all_data_restricted <- all_data %>% select(-borough, -cuisine, -inspection_year,
                                -code, -grade, -critical, -inspection_type)

require(data.table)
features_data <- merge(all_data_restricted, 
                       restaurant_data,
                       all.x = TRUE, 
                       allow.cartesian = TRUE)


#before 2017
his_before_2017 <- stored_data_c %>% filter(grepl('Initial', inspection_type)) %>% filter(inspection_year < 2017)

his_before_2017 <- his_before_2017 %>% dplyr::select(id, inspection_date, score, borough, cuisine, inspection_year, action)
his_before_2017 <- his_before_2017 %>% ungroup() %>% unique()

his_before_2017 <- his_before_2017 %>% mutate(num_previous_low_inspections = as.numeric(his_before_2017$score < 14),
                                              num_previous_med_inspections = as.numeric(his_before_2017$score >= 14 & his_before_2017$score < 28),
                                              num_previous_high_inspections = as.numeric(his_before_2017$score >= 28))
#'re-closed' and 'closed' in action column
his_before_2017 <- his_before_2017 %>% mutate(num_previous_closings = as.numeric(grepl('closed', his_before_2017$action)))
his_before_2017 <- his_before_2017 %>% group_by(id)  

#need to  get row sum for 4 new feature columns --Ruoyu Dec.4

# then use join function to join restaurant_data and his_before_2017 by id


#Part D

#only use 2015 and 2016 data for training set
train.lr <- restaurant_data %>% filter(year==2015|year==2016)

# use 2017 data for testing set
test.lr <- restaurant_data %>% filter(year==2017)

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
train.rf <- restaurant_data %>% filter(year==2015|year==2016)

# use 2017 data for testing set
test.rf <- restaurant_data %>% filter(year==2017)

# Create a Random Forest model with default parameters
model.rf <- randomForest(outcome ~ cuisine + borough + inspection_month + inspection_day + 
                           num_previous_low_inspections + num_previous_med_inspections + 
                           num_previous_high_inspections + num_previous_closings, 
                         data = train.rf, ntree = 1000, importance = TRUE)
model.rf


# testing set
test.rf$predicted.probability <- predict(model, newdata = test.rf, type='prob') 

# compute AUC using ROCR package
testrf.pred <- prediction(test.rf$predicted.probability[,2], test.rf$outcome)
testrf.perf <- performance(testrf.pred, "auc")
cat('the auc score is ', 100*testrf.perf@y.values[[1]], "\n") 


