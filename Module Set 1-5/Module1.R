# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
# You can type - ?data.frame to know more about the data.frame function
# We are using it because the test dataset does not contain "survived" variable rep 
# function is used to repeat values - repeat the value None 428 times (= nrow or no  
# of rows in test) and merge them inside the test variable
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

# Combine data sets using row bind function by appending the values of test.survived into 
# train dataset by rows to create final dataset
data.combined <- rbind(train, test.survived)

# A bit about R data types or its structre(e.g., factors)
str(data.combined)

# Factors are a variable or data-type that take a limited set of discrete values (numeric or character). For eg, drop down list in HTML or an enumeration in C
# Bellow, we are converting pclass and survived into factors, beacuse computers dont like strings in general and machine learing algorithms work faster when we 
#   are using integers, so we are convering character strings or numeric - character datatype into factor integers. Also, NA denotes absence of value
data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)


# Take a look at gross survival rates. Take the survived variable and tabulate it.1 Means survived. 2 Means did not survived. 3 means NA, or we dont have the value. (Reason why the data is skewed)
table(data.combined$survived)


# Distribution across classes. 1 means upper, 2 means middle and 3 means lower
table(data.combined$pclass)

# Load up ggplot2 package to use for visualizations
library(ggplot2)
#Function->  ggplot(data = NULL, mapping = aes(), ..., environment = parent.frame())

#--------------------------------------------------------------------------------------
#Question - Is it true that Rich folks survived at a higer rate ?
#
# Hypothesis Rich folks survived at a higer rate. -  Here, we are using hypothesis, checking our hypothesis that the guys who had Upper class (rich folks)
#   easily survived because upper class was closer to the deck and lower class was closer to the base of the ship.

# We are using train class because it contains the data of pclass. Also, we are convering pclass into factor
train$pclass <- as.factor(train$pclass)

ggplot(train, aes(x = pclass, fill = factor(survived))) +  # data that we are interested in plotting (dataframe), aes means aesthetic mappings.  We'r plotting p class on x axis, and using fill variable is used to colour code the graph based on survived variable. This is completely optional. Also, we are convering survived variable into factor variable on the fly. 
  geom_bar() + # Plot a bar graph using geom bar (which is used to plot continious data. geom_histogram is for continuous data (it will do binning)
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 

# Examining the bar graph proves that our hypothesis is correct. If you are in first class, your chances of survival are much more as compared to the 3rd class


# Examine the first few names in the training data set. The name variable is in the form of factors, thats why we are convering it into characters using as.character function
head(as.character(train$name)) # Data is in the format- Last Name, Tite_First Name


# How many unique names are there across both train & test?
length(unique(as.character(data.combined$name)))
# We get 1307 names, rather than 1309 => We have some duplicate names

# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])
# Hence, there are no duplicate names.

# Next, take a look at the records in the combined data set
data.combined[which(data.combined$name %in% dup.names),]


# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)


# Any correlation with other variables (e.g., sibsp - no of sibblings/spouses on board, parch- no of parents/children on board)? from data.combined dataset, detect if Miss variable is there in name, and output it to misses variable
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
# NOTE - Using the grep function here, but could have used the str_detect function as well.
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


# NOTE - The code below uses a for loop which is not a very R way of
#        doing things
titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"name"]))
}
data.combined$title <- as.factor(titles)


# Here we are plotting data purely on the basis of the titles
# Since we only have survived lables for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) + # fill means colour code it on the basis of value of survived
  stat_count(width = 0.5) +
  facet_wrap(~pclass) +  # Pivot it based on pclass - facet_wrap wraps a 1d sequence of panels into 2d. The bar graph subdivides itself on the basis of class, hence becoming from 1D to 2D
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")



# We are doing analysis on Sex vs survival
table(data.combined$sex)

#--------------------------------------------------------------------------------------
#Question - Is it true that women are more likely to survive as compared to men? ?
#
# Visualize the 3-way relationship of sex, pclass, and survival, compare to analysis of title
ggplot(data.combined[1:891,], aes(x = sex, fill = survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~pclass) +  
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")



# OK, age and sex seem pretty important as derived from analysis of title, let's take a closer look at the distibutions of age over entire data set
summary(data.combined$age)
summary(data.combined[1:891,"age"])
# We reach the conclusion that data is very scewed and there are a lot of missing values


# Just to be thorough, take a look at survival rates broken out by sex, pclass, and age
ggplot(data.combined[1:891,], aes(x = age, fill = survived)) +
  facet_wrap(~sex + pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")


# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$age)

# Plot for people with title as 'Master'
ggplot(boys[boys$survived != "None",], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Master' by Pclass") + 
  xlab("Age") +
  ylab("Total Count")

# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)

ggplot(misses[misses$survived != "None",], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") + 
  xlab("Age") +
  ylab("Total Count")


# OK, appears female children may have different survival rate, 
# could be a candidate for feature engineering later
misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5)) # Which is = to 4, hence we have very less female children.

# Move on to the sibsp variable, summarize the variable
summary(data.combined$sibsp)


# Can we treat as a factor?
length(unique(data.combined$sibsp))


data.combined$sibsp <- as.factor(data.combined$sibsp)


# We believe title is predictive. Visualize survival reates by sibsp, pclass, and title
ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Treat the parch vaiable as a factor and visualize
data.combined$parch <- as.factor(data.combined$parch)
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Let's try some feature engineering. What about creating a family size feature?
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)




#--------------------------------------------------------------------------------------------
# Question - What is the corelation between family size and survival rate ?
#
# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Take a look at the ticket variable
str(data.combined$ticket)


# Based on the huge number of levels ticket really isn't a factor variable it is a string. 
# Convert it and display first 20
data.combined$ticket <- as.character(data.combined$ticket)
data.combined$ticket[1:20]


# There's no immediately apparent structure in the data, let's see if we can find some.
# We'll start with taking a look at just the first char for each. If there is an empty string, replace it with " " and if not, return the result from substr call
ticket.first.char <- ifelse(data.combined$ticket == "", " ", substr(data.combined$ticket, 1, 1))  # substr(x, start, stop)
unique(ticket.first.char) # Find the unique values in ticket.first.char variable


# OK, we can make a factor for analysis purposes and visualize
data.combined$ticket.first.char <- as.factor(ticket.first.char)

# First, a high-level plot of the data based on the ticket
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Ticket seems like it might be predictive, drill down a bit
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")

# There is no relation between pclass, survival ratio and ticket starting letter
# Lastly, see if we get a pattern when using combination of pclass & title
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")

# Hence, there is no relation with ticket starting letter, hence we would not use it modelling



# Next up - the fares Titanic passengers paid
summary(data.combined$fare) # The distribution is scewed to the high end since mean > median
length(unique(data.combined$fare))


# Can't make fare a factor, treat as numeric & visualize with histogram. Plot fare vs the total count of tickets
ggplot(data.combined, aes(x = fare)) +
  stat_count(width = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)


# Let's check to see if fare has predictive power.We see that fare doesnt correspond to any specific result in the plot, hence we wont use ticket in our modelling 
ggplot(data.combined[1:891,], aes(x = fare, fill = survived)) +
  stat_count(width = 5) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")




# Analysis of the cabin variable. We see that there are 187 levels, and since Random Forest Algoritm cannot work with more than 32 factors or levels, we have to reduce the cabins.
str(data.combined$cabin)


# Cabin really isn't a factor, make a string and the display first 100
data.combined$cabin <- as.character(data.combined$cabin)
data.combined$cabin[1:100] # we use 100 because lost of cabin entries are empty


# Replace empty cabins with a "U"
data.combined[which(data.combined$cabin == ""), "cabin"] <- "U"
data.combined$cabin[1:100]


# Take a look at just the first char as a factor
cabin.first.char <- as.factor(substr(data.combined$cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)


# Add to combined data set and plot 
data.combined$cabin.first.char <- cabin.first.char

# High level plot
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

# Could have some predictive power, drill in
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# Does this feature improve upon pclass + title?
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

#---------------------------------------------------------------------------------------------------
# What about folks with multiple cabins? str_detect looks at all the cabins and finds if they are having a blank space. If yes, then return Y else N
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")


ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")


#--------------------------------------------------------------------------------------------------
# Does survivability depend on where you got onboard the Titanic?
str(data.combined$embarked)
levels(data.combined$embarked)
# c- Cherbourg, Q - QueenStown, S- Southampton


# Plot data for analysis. Looking at the graph, we find that embarked is useless and we wont use it in modelling
ggplot(data.combined[1:891,], aes(x = embarked, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = embarked, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

