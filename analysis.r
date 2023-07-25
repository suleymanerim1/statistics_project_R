# import libraries
library(tidyverse)
library(tinytex)
library(dplyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(correlation)
library(reshape)
library(reshape2)
library(tidyverse) # for data manipulation
library(ggplot2) # for plotting
library(gridExtra) # for grid.arrange
library(regclass) # for VIF package
library(MLmetrics) # to create confusion matrix
library(pROC) # for ROC Curve
library(e1071) # for Naive Bayes Classifier
library(class)
library(caret)
library(corrr)
library(ppcor)
library(glmnet) # for Lasso Regression


data_train = read.csv("train.csv")
data_test = read.csv("test.csv")

# merge train and test data
data = rbind(data_train, data_test)
attach(data)

# DATA PREPROCESSING

# replace dots with underscores in column names
names(data) = gsub("\\.", "_", names(data))

# drop X and id column
data = data %>% select(-X, -id)

# convert categorical features to factor
data$Gender = factor(data$Gender, levels = c("Male", "Female"))
data$Customer_Type = factor(data$Customer_Type, levels = c("Loyal Customer", "disloyal Customer"))
data$Type_of_Travel = factor(data$Type_of_Travel, levels = c("Personal Travel", "Business travel"))
data$Class = factor(data$Class, levels = c("Business", "Eco Plus", "Eco"))
data$satisfaction = factor(data$satisfaction, levels = c("neutral or dissatisfied", "satisfied"))

######################################################

#   HANDLING NA VALUES

# list features with na values
prop.table(colSums(is.na(data)))

# Arrival_Delay_in_Minutes has na values, proportion of na values
prop.table(table(is.na(data$Arrival_Delay_in_Minutes)))

# na values are only 0.03% of the data -> drop na values
data = data %>% drop_na(Arrival_Delay_in_Minutes)

######################################################
# Print summary for each variable grouped by satisfaction, including the name of the variable
for (col in names(data)) {
  print(col)
  print(by(data[[col]], data$satisfaction, summary))
}
######################################################

### OUTLIERS

# plot boxplot of numeric variables
plots = list()
for (col in names(data)[sapply(data, is.numeric)]) {
  plot = ggplot(data, aes(x = .data[[col]])) +
  geom_boxplot() +
  labs(title = col, x = col, y = "Count") 
  plots[[col]] = plot
}

grid.arrange(grobs = plots, ncol = 3)

# plot boxplot against satisfaction with colors
plots = list()
for (col in names(data)[sapply(data, is.numeric)]) {
  plot = ggplot(data, aes(x = satisfaction, y = .data[[col]], fill = satisfaction)) +
  geom_boxplot() +
  labs(title = col, x = "Satisfaction", y = col) 
  plots[[col]] = plot
}

# Create density plots for Age and Flight_Distance
plots = list()
for (col in c("Age", "Flight_Distance")) {
  plot = ggplot(data, aes(x = .data[[col]], fill = satisfaction)) +
  geom_density(alpha = 0.4) +
  labs(title = paste("Density Plot of", col), x = col, y = "Density") 
  plots[[col]] = plot
}

# Arrange the density plots in a grid
grid.arrange(grobs = plots)

######################################################

grid.arrange(grobs = plots, ncol = 2)

###################################################################

# select examples of departure delay greater than 500
examples=data[data$Departure_Delay_in_Minutes > 800,] 
# and print table of satisfaction by departure delay
table(examples$satisfaction)

# count the number of examples with departure delay = 0
sum(data$Departure_Delay_in_Minutes > 0)
sum(data$Departure_Delay_in_Minutes <= 0)

sum(data$Arrival_Delay_in_Minutes > 0)
sum(data$Arrival_Delay_in_Minutes <= 0)

summary(data)

######################################################
#   VISUALIZATION 

# plot pie chart for each variable
plots = list()
for (col in names(data)[sapply(data, is.factor)]) {
  plot = ggplot(data, aes(x = "", fill = .data[[col]])) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(title = paste("Pie Chart of", col))
  plots[[col]] = plot
}

grid.arrange(grobs = plots, ncol = 2)



######################################################à

#   VISUALIZATION 

# plot distribution of categorical variables
plots = list()
for (col in names(data)[sapply(data, is.factor)]) {
  plot = ggplot(data, aes(x = .data[[col]], fill = .data[[col]])) +
  geom_bar() +
  labs(title = paste("Histogram of", col), x = col, y = "Count")

  plots[[col]] = plot
}

grid.arrange(grobs = plots, ncol = 2)

##################

# plot distribution of numeric variables
plots = list()
for (col in names(data)[sapply(data, is.numeric)]) {
  plot = ggplot(data, aes(x = .data[[col]])) +
  geom_histogram() +
  labs(title = col, x = col, y = "Count") 
  plots[[col]] = plot
}

grid.arrange(grobs = plots, ncol = 3)

##################

# plots categorical variables vs satisfaction
plots = list()
for (col in names(data)[sapply(data, is.factor)]) {
  if (col == "satisfaction") {
    next
  }
  plot = ggplot(data, aes(x = satisfaction, fill = .data[[col]])) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = rainbow(length(unique(data[[col]]))), 
                    labels = unique(data[[col]]),
                    name = col) +
  labs(title = paste("Histogram of Satisfaction by", col), x = "Satisfaction", y = "Count")

  plots[[col]] = plot
  
}

grid.arrange(grobs = plots, ncol = 2)

##################

# plots numeric variables vs satisfaction
plots = list()
for (col in names(data)[sapply(data, is.numeric)]) {
  if (col == "satisfaction") {
    next
  }
  plot = ggplot(data, aes(x = satisfaction, y = .data[[col]])) +
  geom_boxplot() +
  labs(x = "Satisfaction", y = col)

  plots[[col]] = plot
  
}

grid.arrange(grobs = plots, ncol = 4)

#############################################

#   CONVERT CATEGORICAL TO NUMERIC

data$Gender = as.numeric(data$Gender) - 1
data$Customer_Type = as.numeric(data$Customer_Type) - 1
data$Type_of_Travel = as.numeric(data$Type_of_Travel) - 1
data$Class = as.numeric(data$Class) - 1
data$satisfaction = as.numeric(data$satisfaction) - 1

#############################################

#   DATA BALANCE

prop.table(table(data$satisfaction))

##############################################

#  TRAIN TEST SPLIT

set.seed(123)
train_index = sample(1:nrow(data), 0.8*nrow(data))
# 80% of data is used for training
train = data[train_index,]
# 20% of data is used for testing
test = data[-train_index,]

# merge train and test data
data = rbind(train, test)
#save on cvs
write.csv(data, file = "data.csv")

# save true values of test satisfaction column
test_true = test$satisfaction

# drop satisfaction column from test data
test = test %>% select(-satisfaction)

# print proportion of satisfied and dissatisfied customers in train and test data
prop.table(table(train$satisfaction))
prop.table(table(test_true))

##############################################

## CORRELATION MATRIX FOR NUMERICAL VARIABLES

ds_cor1 <- cor(subset(data,select = c(Age, Flight_Distance,Departure_Delay_in_Minutes,Arrival_Delay_in_Minutes)))
summary(ds_cor1)
options(repr.plot.width = 14, repr.plot.height = 8)
corrplot(ds_cor1, na.label = " ", method="color", tl.col = "black", tl.cex = 1)


## CORRELATION MATRIX FOR ORDINAL VARIABLES

ds_cor2 <- cor(subset(data, select = c(Inflight_wifi_service, Departure_Arrival_time_convenient, Ease_of_Online_booking, Gate_location, Food_and_drink, Online_boarding, Seat_comfort, Inflight_entertainment, On_board_service, Leg_room_service, Baggage_handling, Checkin_service, Inflight_service,Cleanliness)))
summary(ds_cor2)
options(repr.plot.width = 14, repr.plot.height = 8)
corrplot(ds_cor2, na.label=" ", tl.cex=1, tl.col="black", method="color")





##############################################

# PLOT ALL DISTRIBUTIONS (with numerical categories)

sat = data$satisfaction
features_names = names(data)

num_cols = 4
num_rows = ceiling(ncol(data)/num_cols)
par(mfrow = c(num_rows, num_cols))

# for each feature plot the density of satisfied and dissatisfied customers
for(col in features_names) {
  # calculate number of breaks
  num_breaks = length(unique(data[[col]]))
  num_breaks = min(num_breaks, 20) 
  hist(data[[col]], breaks = num_breaks,
    main = paste("Histogram of ", col), xlab = col, ylab = "Frequency",
    col = "lightblue"
  )
}

#plot the density of columns of data which names are in correlations. use barplots.

#TODO: make them visually right.

hist(train$Type_of_Travel, breaks = 2, col = "blue", xlab = "Type_of_Travel", main = "Type_of_Travel - Density Plot")

a = ggplot(train, aes(x = Type_of_Travel, fill = sat)) +
  geom_histogram(fill = 'Blue', alpha = 0.4, bins = 2) 


b = ggplot(train, aes(x = Class, fill = sat)) +
  geom_bar(fill = 'Blue', alpha = 0.4) +
  ggtitle("Class - Density Plot") +
  xlab('Class')

c = ggplot(train, aes(x = Online_boarding, fill = sat)) +
  geom_bar(fill = 'Blue', alpha = 0.4) +
  ggtitle("Online_boarding - Density Plot") +
  xlab('Online_boarding')

d = ggplot(train, aes(x = Seat_comfort, fill = sat)) +
  geom_bar(fill = 'Blue', alpha = 0.4) +
  ggtitle("Seat_comfort - Density Plot") +
  xlab('Seat_comfort')

e = ggplot(train, aes(x = Inflight_entertainment, fill = sat)) +
  geom_bar(fill = 'Blue', alpha = 0.4) +
  ggtitle("Inflight_entertainment - Density Plot") +
  xlab('Inflight_entertainment')

f = ggplot(train, aes(x = On_board_service, fill = sat)) +
  geom_bar(fill = 'Blue', alpha = 0.4) +
  ggtitle("On_board_service - Density Plot") +
  xlab('On_board_service')


g = ggplot(train, aes(x = Leg_room_service, fill = sat)) +
  geom_bar(fill = 'Blue', alpha = 0.4) +
  ggtitle("Leg_room_service - Density Plot") +
  xlab('Leg_room_service')

h = ggplot(train, aes(x = Cleanliness, fill = sat)) +
  geom_bar(fill = 'Blue', alpha = 0.4) +
  ggtitle("Cleanliness - Density Plot") +
  xlab('Cleanliness')


grid.arrange(a, b, c, d, e, f, g, h, ncol = )







numerical_features <- c("Age", "Flight_Distance", "Departure_Delay_in_Minutes", "Arrival_Delay_in_Minutes")
partial_correlations_num <- pcor(data[, numerical_features], method = "spearman")

# make the matrix
partial_corr_matrix_num <- partial_correlations_num$estimate

# Customize the plot size
options(repr.plot.width = 200, repr.plot.height = 200)

# Create the correlation plot
corrplot(partial_corr_matrix_num,
         method = "color",  # Choose "color" to visualize the correlations using colors
         type = "upper",    # Only plot the upper triangular part of the matrix (to avoid duplication)
         tl.col = "black",  # Label color
         tl.cex = 1,        # Label font size
)

#PARTIAL CORRELATION MATRIX FOR CATEGORICAL/ORDINAL
categorical_features = c('Inflight_wifi_service', 'Departure_Arrival_time_convenient', 'Ease_of_Online_booking', 'Gate_location', 'Food_and_drink', 'Online_boarding', 'Seat_comfort', 'Inflight_entertainment', 'On_board_service', 'Leg_room_service', 'Baggage_handling', 'Checkin_service', 'Inflight_service','Cleanliness')
partial_correlations_cat <- pcor(data[, categorical_features], method = "spearman")

# make the matrix
partial_corr_matrix_cat <- partial_correlations_cat$estimate

# Customize the plot size
options(repr.plot.width = 200, repr.plot.height = 200)

# Create the correlation plot
corrplot(partial_corr_matrix_cat,
         method = "color",  # Choose "color" to visualize the correlations using colors
         type = "upper",    # Only plot the upper triangular part of the matrix (to avoid duplication)
         tl.col = "black",  # Label color
         tl.cex = 1,        # Label font size
)


# make a dataframe with the partial correlation matrix of categorical
partial_correlations_cat_df <- as.data.frame(partial_corr_matrix_cat)

head(partial_correlations_cat_df)





######################################################
# ANOVA

# libraries
library(ggplot2)
library(car)
library(stats)
library(dplyr)
library(rstatix)
library(ggpubr)
# To apply anova we have to see if the ANOVA conditions are satisfied.
# 1. The samples are independent
# 2. The samples are from normally distributed populations
# 3. The population standard deviations of the groups are all equal

bartlett.test(data$satisfaction ~ data$Gate_location)
# do a boxplot comparisons between the groups

ggplot(data, aes(x = Gate_location, y = satisfaction, fill = Gate_location)) +
  geom_boxplot() +
  theme_minimal()

# count number of examples for each group of gate locations
table(data$Gate_location)

# we have that at gate loc 0 there is only one flight, useless for further considerations.
# drop it only for the barlett test, inside the function
bartlett.test(data$satisfaction[-which(data$Gate_location == 0)] ~ data$Gate_location[-which(data$Gate_location == 0)])


#see the entire row of gate location 0
data[data$Gate_location == 0,]


#LOGISTIC REGRESSION MODELS
# Fit the logistic regression model
features = colnames(partial_correlations)
model <- glm(formula = formula("satisfaction ~  -Gender -Customer_type -Age -Type_of_Travel"), data = train, family = binomial)


# View the model summary
summary(model)


