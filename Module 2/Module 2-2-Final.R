# 1. Prerequisite
# import datasets
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Load required packages for machine learning
library(e1071)
library(rpart)

# Join together the training/test sets for feature engineering
test$Survived <- NA
combi <- rbind(train, test)

# Check data
str(combi)

# 2. Feature Engineering

# Engineer variable: Name
combi$Name <- as.character(combi$Name)

# Split name to: Title, Surname
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$Title <- sub(' ', '', combi$Title)

# Combine similar titles together
combi$Title[combi$Title %in% c('Mme', 'Mlle', 'Ms', 'Miss')] <- 'Miss'
combi$Title[combi$Title %in% c('Rev', 'Dr')] <- 'Rev'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

# Discretize family size
combi$FamilyID[combi$FamilySize == 1] <- 'Singleton'
combi$FamilyID[1 < combi$FamilySize & combi$FamilySize <= 3] <- 'Small'
combi$FamilyID[3 < combi$FamilySize & combi$FamilySize <= 5] <- 'Normal'
combi$FamilyID[combi$FamilySize > 5] <- 'Large'

# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# 3. Impute missing values

# Fill in Embarked blanks
# Find Embarked blanks
summary(combi$Embarked)
which(combi$Embarked == '')

# Get rid of our missing passenger IDs
Embark_Fare <- combi %>%
  filter(PassengerId != 62 & PassengerId != 830)

# We see that the median fare for a first class passenger departing from Charbourg (‘C’) coincides nicely with the $80 paid by our embarkment-deficient passengers. I think we can safely replace the NA values with ‘C’.
combi$Embarked[c(62,830)] = "C"
combi$Embarked <- factor(combi$Embarked)

# Fill in Fare NAs
# Find Fare NAs
summary(combi$Fare)
which(is.na(combi$Fare))

# It seems quite reasonable to replace the NA Fare value with median for their class and embarkment which is $8.05.
# Replace missing fare value with median fare for class/embarkment
combi$Fare[1044] <- median(combi[combi$Pclass == '3' & combi$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Fill in Age NAs
summary(combi$Age)
combi$Age2 <- combi$Age
Agefit <- rpart(Age2 ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data=combi[!is.na(combi$Age2),], method="anova")
combi$Age2[is.na(combi$Age2)] <- predict(Agefit, combi[is.na(combi$Age2),])

# Things look good, so we replace Age variable from the rpart model
combi$Age <- combi$Age2
combi$Age2 <- NULL

# Show new number of missing Age values
sum(is.na(full$Age))

# Create the column child, and indicate whether child or adult
combi$Child[combi$Age < 18] <- 'Child'
combi$Child[combi$Age >= 18] <- 'Adult'

# Show counts
table(combi$Child, combi$Survived)

# Create Mother variable
combi$Mother <- 'Not Mother'
combi$Mother[combi$Sex == 'female' & combi$Parch > 0 & combi$Age > 18 & combi$Title != 'Miss'] <- 'Mother'

# Show counts
table(combi$Mother, combi$Survived)

# Finish by factorizing our two new factor variables
combi$Child  <- factor(combi$Child)
combi$Mother <- factor(combi$Mother)



# Calculate Ticket Price (Fare per person)
ticket.count <- aggregate(combi$Ticket, by=list(combi$Ticket), function(x) sum( !is.na(x) ))
combi$Price<-apply(combi, 1, function(x) as.numeric(x["Fare"]) / ticket.count[which(ticket.count[, 1] == x["Ticket"]), 2])

pclass.price<-aggregate(combi$Price, by = list(combi$Pclass), FUN = function(x) median(x, na.rm = T))
combi[which(combi$Price==0), "Price"] <- apply(combi[which(combi$Price==0), ] , 1, function(x) pclass.price[pclass.price[, 1]==x["Pclass"], 2])

# Feature TicketCount
combi$TicketCount<-apply(combi, 1, function(x) ticket.count[which(ticket.count[, 1] == x["Ticket"]), 2])

# 4. Prediction
# Split back into test and train sets
set.seed(415)

train <- combi[1:891,]
test <- combi[892:1309,]
inTrain<-createDataPartition(train$Survived, p = 0.8)[[1]]


# Fitting a SVM model
svm <- svm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare 
           + Embarked + Title + FamilySize + FamilyID + Price + TicketCount
           + Child + Mother, data = train)
summary(svm)
confusionMatrix(train[-inTrain,"Survived"], predict(svm, train[-inTrain,], OOB = TRUE, type = "response"))$overall[1]

# Make prediction and write to submission file
test$Survived = 0

Prediction <- predict(svm, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "svm.csv", row.names = FALSE)
