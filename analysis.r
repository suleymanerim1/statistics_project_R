# import libraries
library(tidyverse)

data_train = read.csv("train.csv")
data_test = read.csv("test.csv")

# merge train and test data
data = rbind(data_train, data_test)
attach(data)

# print dimension of data
print(dim(data))
summary(data)

# replace dots with underscores in column names
names(data) = gsub("\\.", "_", names(data))

# print column names
print(names(data))

# drop X and id column
#TODO: explain why
data = data %>% select(-X, -id)

# insert all categorical variables into a list
categorical_var = c(data$Cleanliness,
  data$Inflight_wifi_service,
  data$Departure_Arrival_time_convenient,
  data$Ease_of_Online_booking,
  data$Gate_location,
  data$Food_and_drink,
  data$Online_boarding,
  data$Seat_comfort,
  data$Inflight_entertainment,
  data$On_board_service,
  data$Leg_room_service,
  data$Baggage_handling,
  data$Checkin_service,
  data$Inflight_service,
  data$Cleanliness
)

# convert categorical variables to factors and then to numeric
categorical_var = as.factor(categorical_var)
categorical_var = as.numeric(categorical_var)

# convert gender to numeric and then to factor
data$Gender = as.numeric(as.factor(data$Gender))

# change type of customer to 0 and disloyal customer to 1
data$Customer_Type = as.numeric(factor(data$Customer_Type, levels = c("Loyal Customer", "disloyal Customer"))) - 1

# change type of tr avel to 0 and personal travel to 1
data$Type_of_Travel = as.numeric(factor(data$Type_of_Travel, levels = c("Personal Travel", "Business travel"))) - 1

# change class Business is 2, Eco Plus is 1 and Eco is 0
data$Class = as.numeric(factor(data$Class, levels = c("Business", "Eco Plus", "Eco"))) - 1

data$satisfaction = as.numeric(factor(data$satisfaction, levels = c("neutral or dissatisfied", "satisfied"))) - 1

# drop na values in Arrival Delay in Minutes
# TODO: explain why (now it's dropped to simplify the analysis)
data = data %>% drop_na(Arrival_Delay_in_Minutes)

# DATA BALANCE: quite balanced
prop.table(table(data$satisfaction))

# Train-test split
set.seed(123)
train_index = sample(1:nrow(data), 0.8*nrow(data))
# 80% of data is used for training
train = data[train_index,]
# 20% of data is used for testing
test = data[-train_index,]

# print dimension of train and test data
dim(train)
dim(test)

# save true values of test satisfaction column
test_true = test$satisfaction

# drop satisfaction column from test data
test = test %>% select(-satisfaction)

# print proportion of satisfied and dissatisfied customers in train and test data
prop.table(table(train$satisfaction))
prop.table(table(test_true))

# DATA ANALYSIS

# save satisfaction column of train data
train_satisfaction = train$satisfaction

# correlation matrix only for numeric variables
correlation_matrix = cor(train[, sapply(train, is.numeric)])

# plot correlation matrix
corrplot(correlation_matrix, method = "circle")


