# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.survived)

# A bit about R data types (e.g., factors)
str(data.combined)

data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)

# Hypothesis - Rich folks survived at a higer rate
train$pclass <- as.factor(train$pclass)

# Examine the first few names in the training data set
head(as.character(train$name))

# How many unique names are there across both train & test?
length(unique(as.character(data.combined$name)))


# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])

# Next, take a look at the records in the combined data set
data.combined[which(data.combined$name %in% dup.names),]


# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)

# Any correlation with other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$name, "Miss.")),]
misses[1:5,]


# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5,]

# Check out males to see if pattern continues
males <- data.combined[which(train$sex == "male"), ]
males[1:5,]


# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"name"]))
}
data.combined$title <- as.factor(titles)


# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$age)


# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)


# OK, appears female children may have different survival rate, 
# could be a candidate for feature engineering later
misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))


# Move on to the sibsp variable, summarize the variable
summary(data.combined$sibsp)


# Can we treat as a factor?
length(unique(data.combined$sibsp))


data.combined$sibsp <- as.factor(data.combined$sibsp)


# Let's try some feature engineering. What about creating a family size feature?
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)


# Based on the huge number of levels ticket really isn't a factor variable it is a string. 
# Convert it and display first 20
data.combined$ticket <- as.character(data.combined$ticket)
data.combined$ticket[1:20]


# There's no immediately apparent structure in the data, let's see if we can find some.
# We'll start with taking a look at just the first char for each
ticket.first.char <- ifelse(data.combined$ticket == "", " ", substr(data.combined$ticket, 1, 1))
unique(ticket.first.char)


# OK, we can make a factor for analysis purposes and visualize
data.combined$ticket.first.char <- as.factor(ticket.first.char)


# Analysis of the cabin variable
str(data.combined$cabin)


# Cabin really isn't a factor, make a string and the display first 100
data.combined$cabin <- as.character(data.combined$cabin)
data.combined$cabin[1:100]


# Replace empty cabins with a "U"
data.combined[which(data.combined$cabin == ""), "cabin"] <- "U"
data.combined$cabin[1:100]


# Take a look at just the first char as a factor
cabin.first.char <- as.factor(substr(data.combined$cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)


# Add to combined data set and plot 
data.combined$cabin.first.char <- cabin.first.char


#==============================================================================
#
# Exploratory Modeling of Random Forest
#
#==============================================================================


library(randomForest)

train_rf <- data.combined[1:891,]
test_rf <- data.combined[892:1309,]
rf_model <- randomForest(factor(survived) ~ pclass + sex + age + sibsp + parch + fare + embarked + title + family.size, data = train_rf,na.action=na.exclude)
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# The above plot shows us the order of importance of differet variables.

# Train a Random Forest with the default parameters using pclass & title
rf.train.1 <- data.combined[1:891, c("pclass", "title")]    # Create a new rf.train.1 dataframe and put the pclass and title values
rf.label <- as.factor(train$survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)
#  OOB estimate of  error rate: 20.76% means we are getting 80 % accuracy
#   Confusion matrix:
#     0   1 class.error
# 0 538  11  0.02003643    <- This means that 538 people perished which was true, and we predicted 11 people perished, when they did survive. Hence, we have 2.003% error
# 1 174 168  0.50877193    <- We predicted 174 survived wich was true, and we predicted that 168 people didnt survive, which was wrong. Hence, error rate is 50.877%




# Train a Random Forest using pclass, title, & sibsp
rf.train.2 <- data.combined[1:891, c("pclass", "title", "sibsp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)
#   OOB estimate of  error rate: 19.75%     <- We got about 1% increase in accuracy!!!
#   Confusion matrix:
#   0   1 class.error
#   0 487  62   0.1129326   <- Increased with 0.09 %
#   1 114 228   0.3333333   <- Decreased with 0.2 %


# Train a Random Forest using pclass, title, & parch
rf.train.3 <- data.combined[1:891, c("pclass", "title", "parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)
#   OOB estimate of  error rate: 19.98%     <- Its not as accurate as previous model
#   Confusion matrix:
#     0     1   class.  error
# 0   495   54      0.09836066
# 1   124   218     0.36257310



# Train a Random Forest using pclass, title, sibsp, parch
rf.train.4 <- data.combined[1:891, c("pclass", "title", "sibsp", "parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)
#   OOB estimate of  error rate: 18.52%     <- We improved the model again. Its 2% more accurae as compared to first model 
#   Confusion matrix:
#     0   1 class.error
# 0   489  60   0.1092896
# 1   105 237   0.3070175


# Train a Random Forest using pclass, title, & family.size
rf.train.5 <- data.combined[1:891, c("pclass", "title", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)
#OOB estimate of  error rate: 18.41%      <- Improved accuracy!!!
# Confusion matrix:
#   0     1     class.error
# 0 485   64    0.1165756
# 1 100   242   0.2923977




# Train a Random Forest using pclass, title, sibsp, & family.size
rf.train.6 <- data.combined[1:891, c("pclass", "title", "sibsp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)
#   OOB estimate of  error rate: 18.74%   <- Not as accurate as previous model
#   Confusion matrix:
#     0   1 class.error
#   0 486  63   0.1147541
#   1 104 238   0.3040936


# Train a Random Forest using pclass, title, parch, & family.size
rf.train.7 <- data.combined[1:891, c("pclass", "title", "parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)
#   OOB estimate of  error rate: 18.97%     <- Not as accurate !!
#   Confusion matrix:
#     0   1 class.error
#   0 486  63   0.1147541
#   1 106 236   0.3099415

# The conclusion of our exploratory model is title, pclass and family.size are the most improtant when it comes to predicting the accuracy. sibsp and parch is not as important
# rf.5 is the best model till now