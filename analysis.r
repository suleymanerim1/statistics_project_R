# import libraries
library(tidyverse)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(correlation)

data_train = read.csv("train.csv")
data_test = read.csv("test.csv")

# merge train and test data
data = rbind(data_train, data_test)
attach(data)




# DATA PREPROCESSING
# replace dots with underscores in column names
names(data) = gsub("\\.", "_", names(data))

# drop X and id column
#TODO: explain why
data = data %>% select(-X, -id)

# convert gender to numeric and then to factor
data$Gender = as.numeric(as.factor(data$Gender)) -1
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











# DATA ANALYSIS

# save satisfaction column of train data
train_satisfaction = train$satisfaction

# correlation matrix only for numeric variables
correlation_matrix = cor(data[, sapply(data, is.numeric)])

# plot correlation matrix
corrplot(correlation_matrix, method = "circle")

# Plot a heatmap of the correlation matrix
ggplot(data = reshape2::melt(corr)) +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                    size = 10, hjust = 1)) +
  coord_fixed()

# Find the high correlation features
#NOTE: i decided to use 0.3 as threshold
satisfaction_corr <- corr['satisfaction',]
high_corr_features <- names(satisfaction_corr[abs(satisfaction_corr) > 0.3 | abs(satisfaction_corr) < -0.3])



# Compute the correlations between the high correlation features and satisfaction
correlations <- data.frame(
  feature = high_corr_features,
  correlation = sapply(high_corr_features, function(x) cor(data[,x], data$satisfaction))
)

correlations
#plot the correlations
ggplot(correlations, aes(x = reorder(feature, correlation), y = correlation)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.4) +
  ggtitle("Correlation between features and satisfaction") +
  xlab('Features') +
  ylab('Correlation')



#save on cvs
write.csv(correlations, file = "correlations.csv")

#satisfaction
sat = train$satisfaction


#plot the density of columns of data which names are in correlations. use barplots.

#TODO: make them visually right.

a = ggplot(train, aes(x = Type_of_Travel, fill = satisfaction)) +
  geom_bar(fill = 'Blue', alpha = 0.4) +
  ggtitle("Type of Travel - Density Plot") +
  xlab('Type of Travel')


b = ggplot(train, aes(x = Class, fill = satisfaction)) +
  geom_density(fill = 'Blue', alpha = 0.4) +
  ggtitle("Class - Density Plot") +
  xlab('Class')

c = ggplot(train, aes(x = Online_boarding, fill = satisfaction)) +
  geom_density(fill = 'Blue', alpha = 0.4) +
  ggtitle("Online_boarding - Density Plot") +
  xlab('Online_boarding')

d = ggplot(train, aes(x = Seat_comfort, fill = satisfaction)) +
  geom_density(fill = 'Blue', alpha = 0.4) +
  ggtitle("Seat_comfort - Density Plot") +
  xlab('Seat_comfort')

e = ggplot(train, aes(x = Inflight_entertainment, fill = satisfaction)) +
  geom_density(fill = 'Blue', alpha = 0.4) +
  ggtitle("Inflight_entertainment - Density Plot") +
  xlab('Inflight_entertainment')

f = ggplot(train, aes(x = On_board_service, fill = satisfaction)) +
  geom_density(fill = 'Blue', alpha = 0.4) +
  ggtitle("On_board_service - Density Plot") +
  xlab('On_board_service')


g = ggplot(train, aes(x = Leg_room_service, fill = satisfaction)) +
  geom_density(fill = 'Blue', alpha = 0.4) +
  ggtitle("Leg_room_service - Density Plot") +
  xlab('Leg_room_service')

h = ggplot(train, aes(x = Cleanliness, fill = satisfaction)) +
  geom_density(fill = 'Blue', alpha = 0.4) +
  ggtitle("Cleanliness - Density Plot") +
  xlab('Cleanliness')




grid.arrange(a, b, c, d, e, f, g, h, ncol = )



#CORRELATION MATRIX again but now we are interested in partial correlation
#So we look for all the correlations between variables
#We pick the highest, setting a treshold of our choice

#build a dataframe where for each variable we look the partial correlation with all the others
#we pick the highest and we save it in a dataframe
#we set a treshold of 0


#correlation(train, partial=TRUE, method='pearson')
#save the partial correlation matrix result in a dataframe and output a file for further analysis


#partial_corr <- correlation(train, partial=TRUE, method='pearson')
#write.csv(partial_corr, file = "partial_corr.csv")

partial_correlations = read.csv("partial_corr.csv", header = TRUE, sep = ",")

#make the first column the row names
rownames(partial_correlations) = partial_correlations[,1]

colnames(partial_correlations)
#drop the first  (X) column
partial_correlations = partial_correlations[,-1]

# Create a new matrix with rounded partial correlations
partial_correlations_rounded <- round(partial_correlations, digits = 3)


# Initialize empty data frame with 0 rows
# We need it to create a data frame with the results and
# so to show better the correlations.
df <- data.frame(variable1 = character(),
                 variable2 = character(),
                 value = numeric(),
                 stringsAsFactors = FALSE)

# Loop over rows and columns of matrix
for (i in 1:nrow(partial_correlations_rounded)) {
  for (j in 1:ncol(partial_correlations_rounded)) {
    print(partial_correlations_rounded[i,j])
    # Check if value meets criterion
    if ((partial_correlations_rounded[i,j] > 0.300 | partial_correlations_rounded[i,j] < -0.300)& i != j) {
      print('it is true')
      # Add row to data frame
      df <- rbind(df, data.frame(variable1 = rownames(partial_correlations_rounded)[i],
                                 variable2 = colnames(partial_correlations_rounded)[j],
                                 value = partial_correlations_rounded[i,j],
                                 stringsAsFactors = FALSE))
    }
  }
}


# Group the data frame by variable1 and extract top 3 values for each group
df_top3 <- df %>% group_by(variable1) %>% top_n(4, value) %>% ungroup()

#order by variable1
df_top3 <- df_top3[order(df_top3$variable1),]


#delete duplicates in the dataframe if variable1 is equal to variable2
df_top3 <- df_top3[!(df_top3$variable1 == df_top3$variable2),]

print(df_top3, n = nrow(df_top3))
#save on cvs
write.csv(df_top3, file = "df_top3.csv")



#TODO:EXPLAIN THE CORRELATIONS AND PLOTTING THE RESULTS 

#Relations Map for Features



#LOGISTIC REGRESSION MODELS
# Fit the logistic regression model
features = colnames(partial_correlations)
model <- glm(formula = formula("satisfaction ~  -Gender -Customer_type -Age -Type_of_Travel"), data = train, family = binomial)


# View the model summary
summary(model)
