#---------------LOAD DATA, DATA CLEANSING, EXPORT DATA FOR APP------------------
# Packages
library(readxl)
library(dplyr)
library(dummies)
library(caret)
library(FNN)

# Load file using readxl library and convert to data frame
df <- data.frame(read_excel("autism_raw.xlsx"))

# Duplicate data frame for analysis
df1 <- df

# Observe the structure of the dataset
str(df1)
summary(df1)

# Format dataset to my preferences
df1[df1 == "YES"] <- "Yes"
df1[df1 == "yes"] <- "Yes"
df1[df1 == "NO"] <- "No"
df1[df1 == "no"] <- "No"
df1[df1 == "?"] <- "None"
df1[df1 == "AmericanSamoa"] <- "American Samoa"
df1[df1 == "Viet Nam"] <- "Vietnam"
df1[df1 == "White-European"] <- "White"
df1[df1 == "South Asian"] <- "S. Asian"
df1[df1 == "Middle Eastern"] <- "Mid. Eastern"
df1[df1 == "others"] <- "Others"
names(df1) <- toupper(names(df1))  
df1["GENDER"][df1["GENDER"] == "m"] <- "M"
df1["GENDER"][df1["GENDER"] == "f"] <- "F"

# Clean NA values and drop unneeded columns
df1 <- na.omit(df1)
df1 <- subset(df1, select = -c(JAUNDICE,AGE_DESC, USED_APP_BEFORE))
summary(df1)

# Display data frame after cleanup
df1

# One of the ages listed was 383 per the summary max, so I removed this record
df1 <- df1[-c(53), ] 
summary(df1)

prep.data <- df1

# Export cleaned dataset for Shiny app
write.csv(prep.data, "autism_clean.csv", row.names = FALSE)

#---------------------TESTING PREDICTIVE MODELING USING KNN---------------------

# Bring in cleaned dataset
df2 <- read.csv("autism_clean.csv")

# Duplicate data frame for analysis
df3 <- df2

# Cleaned version of data frame for predictive modeling
clean.screen.df <- df3
str(clean.screen.df)
screenData.dummy <- dummy.data.frame(select(clean.screen.df, -c(13,15,17)))

str(screenData.dummy)

# convert numeric variables to integer
screenData.dummy$A1_SCORE <- as.integer(screenData.dummy$A1_SCORE)
screenData.dummy$A2_SCORE <- as.integer(screenData.dummy$A2_SCORE)
screenData.dummy$A3_SCORE <- as.integer(screenData.dummy$A3_SCORE)
screenData.dummy$A4_SCORE <- as.integer(screenData.dummy$A4_SCORE)
screenData.dummy$A5_SCORE <- as.integer(screenData.dummy$A5_SCORE)
screenData.dummy$A6_SCORE <- as.integer(screenData.dummy$A6_SCORE)
screenData.dummy$A7_SCORE <- as.integer(screenData.dummy$A7_SCORE)
screenData.dummy$A8_SCORE <- as.integer(screenData.dummy$A8_SCORE)
screenData.dummy$A9_SCORE <- as.integer(screenData.dummy$A9_SCORE)
screenData.dummy$A10_SCORE <- as.integer(screenData.dummy$A10_SCORE)
screenData.dummy$AGE <- as.integer(screenData.dummy$AGE)
screenData.dummy$RESULT <- as.integer(screenData.dummy$RESULT)
str(screenData.dummy)

# partition the dataset into 60% training and 40% validation sets 
set.seed(111)
train.index <- sample(1:421)
train.df <- screenData.dummy[train.index, ]
valid.df <- screenData.dummy[-train.index, ]

# use preProcess() from the caret package to normalize
norm.values <- preProcess(train.df, method=c("center", "scale"))
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values,valid.df)

# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# compute knn for different k on validation.
for(i in 1:14){
  knn.pred <- knn(train.norm.df, valid.norm.df,
                  cl = train.norm.df[, "A6_SCORE"], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, 
                                       factor(valid.norm.df[, "A6_SCORE"]))$overall[1]
}
accuracy.df
plot(accuracy.df)

# Show the confusion matrix for the validation data from using the best k.
knn.pred <- knn(train.norm.df, valid.norm.df,
                cl = train.norm.df[, "A6_SCORE"], k = 5)
KNN.CM <- confusionMatrix(knn.pred, factor(valid.norm.df[, "A6_SCORE"]))
KNN.CM

# After confirming that I will use this method, write new files for train/test sets
write.csv(train.df, "screen_train.csv", row.names = FALSE)
write.csv(valid.df, "screen_valid.csv", row.names = FALSE)
