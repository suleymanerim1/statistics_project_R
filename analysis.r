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
data$Cleanliness = as.factor(data$Cleanliness)
data$Inflight_wifi_service = as.factor(data$Inflight_wifi_service)
data$Departure_Arrival_time_convenient = as.factor(data$Departure_Arrival_time_convenient)
data$Ease_of_Online_booking = as.factor(data$Ease_of_Online_booking)
data$Gate_location = as.factor(data$Gate_location)
data$Food_and_drink = as.factor(data$Food_and_drink)
data$Online_boarding = as.factor(data$Online_boarding)
data$Seat_comfort = as.factor(data$Seat_comfort)
data$Inflight_entertainment = as.factor(data$Inflight_entertainment)
data$On_board_service = as.factor(data$On_board_service)
data$Leg_room_service = as.factor(data$Leg_room_service)
data$Baggage_handling = as.factor(data$Baggage_handling)
data$Checkin_service = as.factor(data$Checkin_service)
data$Inflight_service = as.factor(data$Inflight_service)
data$Cleanliness = as.factor(data$Cleanliness)


# change male to 0 and female to 1
data <- data %>% mutate(Gender = ifelse(Gender == "Male", 1, 0)) %>% as.factor()
summary(data$Gender)

data$Customer_Type = as.factor(data$Customer_Type)

# comment 
