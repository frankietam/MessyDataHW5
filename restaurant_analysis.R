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
all_data <- rename(all_data,
                   id = CAMIS,
                   borough = BORO,
                   cuisine = CUISINE.DESCRIPTION,
                   action = ACTION, 
                   code = VIOLATION.CODE,
                   critical = CRITICAL.FLAG,
                   score = SCORE, 
                   grade = GRADE, 
                   inspection_type = INSPECTION.TYPE)

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

#Part B(b.)
# saving inspection date and score for Part b
restaurant_data <- restaurant_data %>% select(-action, -code, -grade, -critical, -inspection_type)

#Part C(a.)
restaurant_data <- mutate(restaurant_data, 
                          inspection_month = month(inspection_date),
                          inspection_day = weekdays(inspection_date),
                          initial = T)

#Part C(b.)
#restrict to id, score, action, and inspection_date
all_data_restricted <- all_data %>% select(-borough, -cuisine, -inspection_year,
                                -code, -grade, -critical, -inspection_type)

require(data.table)
features_data <- merge(all_data_restricted, 
                       restaurant_data,
                       all.x = TRUE, 
                       allow.cartesian = TRUE)


#Part C(b.)(i.)

#Create a tibble of non-initial inspections for years before 2017
historical <- left_join(all_data, restaurant_data) %>% 
  filter(inspection_year <= 2017)

historical$initial[is.na(historical$initial)] <- F


historical <- mutate(historical,
                     num_previous_low_inspections = NA)


stored_non <- historical
historical <- stored_non



func <- function(i){
  counter <- historical %>% 
    group_by(id, inspection_date) %>% 
    count(inspection_date < inspection_date[i])
  
  historical$num_previous_low_inspections[i] <-  counter
}

historical <- ifelse(historical$initial[i %in% 1:nrow(historical)] == T,
                     yes = func(i),
                     no=NA)






