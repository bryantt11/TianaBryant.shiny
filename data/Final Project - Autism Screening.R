#----------PART 1: LOAD DATA, DATA CLEANSING, EXPORT DATA FOR APP---------------
# Packages
library(readxl)
library(writexl)


# Load file using readxl library and convert to data frame
df <- data.frame(read_excel("/Users/tianabryant/Downloads/autism_screening_raw.xlsx"))

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

# Export cleaned dataset for Shiny app
write_xlsx(df1, "/Users/tianabryant/Desktop/School/BDAT 630/Autism_Screening/autism_screening_clean.xlsx")
