#----------PART 1: LOAD DATA, DATA CLEANSING, EXPORT DATA FOR APP---------------
# Packages
library(readxl)
library(shiny)
library(writexl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(readr)
library(shinythemes)
library(ggthemes)

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

#---------------------------PART 2: BUILD THE APP-------------------------------
# Load libraries needed


# Bring in cleaned dataset
screen_data <- read_excel("/Users/tianabryant/Desktop/School/BDAT 630/Autism_Screening/autism_screening_clean.xlsx")

# Build pages in app
reference_page <- tabPanel(
  title = "References",
  titlePanel("References"),
  "Allison, C., Auyeung, B., & Baron-Cohen, S. (2012, 1 5). Quantitative 
    Checklist for Autism in Toddlers (Q-CHAT-10). Retrieved from Embloom: 
    https://www.embloom.com/content/q-chat-10/",
  br(),
  br(),
  "Larxel. (2020, August 17). Autism Screening on Adults. Retrieved from 
    Kaggle: https://www.kaggle.com/andrewmvd/autism-screening-on-adults",
  br(),
  br(),
  "Thabtah, F. (2017). Autism Spectrum Disorder Screening: Machine Learning 
    Adaptation and DSM-5 Fulfillment. Proceedings of the 1st International 
    Conference on Medical and Health Informatics 2017, pp.1-6. Taichung City, 
    Taiwan, ACM.",
  br(),
  br(),
  "Thabtah, F. (2017). Machine Learning in Autistic Spectrum Disorder 
    Behavioural Research: A Review. To Appear in Informatics for Health and 
    Social Care Journal. December, 2017",
  br(),
  br(),
  "APA. (2022). Diagnostic and Statistical Manual of Mental Disorders (DSM–5). 
    Retrieved from American Psychiatric Association: 
    https://www.psychiatry.org/psychiatrists/practice/dsm"
)

about_page <- tabPanel(
  title = "Purpose",
  titlePanel("Purpose"),
  br(),
  "Welcome, and thank you for taking the time to view my project!",
  
  "My name is Tiana Bryant, and I prepared this dashboard as a final project 
    for the BDAT 630: Data Visualization course at Maryville University Online.
    Using RShiny, I have created an interactive dashboard that will share the
    results of my analysis on autism spectrum disorder (ASD) screening data.",
  br(),
  br(),
  "Screening early for ASD and providing appropriate diagnosis as soon as 
    possible will ideally allow for early intervention.  Early intervention is 
    critical for supporting those with ASD, as “high quality intervention can 
    improve learning, communication and social skills, as well as underlying 
    brain development” (Larxel, 2020).  It has been the experience for myself 
    and my oldest son, with ASD levels 1 and 2, that there can be a delay to 
    diagnosis. There is such a wide variety of symptoms that are still poorly 
    understood.  However, in the case of my youngest son (level 1), because my 
    husband and I were more knowledgeable, we were able to introduce early 
    intervention and his academic and social performance have been superior to 
    that of my oldest son and myself at the same age.",
  br(),
  br(),
  "Therefore, I wanted to explore a dataset that examines a screening tool 
    outside of the typical Diagnostic and Statistical Manual of Mental Disorders 
    (DSM), the main tool that offers the classification of mental disorders in 
    the United States (APA, 2022).  The Q-CHAT-10, or Quantitative Checklist for 
    Autism in Toddlers, dataset looks for “red flags” that can assist parents or 
    caregivers with recognizing ASD in children (Allison, Auyeung, & 
    Baron-Cohen, 2012).  For the dataset that I am using, this Q-CHAT-10 
    screening was given to adults instead to test the efficacy of screening.",
  br(),
  br(),
  "To better understand ASD and the contributing factors to diagnosis, it is 
    important to better understand the screening tools used.  This analysis will 
    help me to test the efficacy of a tool that I can share with others in the 
    future to help them identify and seek a potential ASD diagnosis, particularly 
    in underdiagnosed groups (e.g., women, minorities) more easily. This analysis 
    will look at the Q-CHAT-10 screening tests that were given to 701 adults and 
    take a deep dive into the data.",
  br(),
  br(),
  img(src='Neurodiversity.jpeg', align = 'center')
)

main_page <- tabPanel(
  # Application title
  title = "Screening Data - Analysis",
  titlePanel("Selection Criteria"),
  # Sidebar with a 2 inputs 
  sidebarLayout(
    sidebarPanel(
      # Input
      selectInput(inputId = "COUNTRY",
                  label = "Select Country:",
                  choices = c("Afghanistan" = "Afghanistan",
                              "American Samoa"="American Samoa",
                              "Angola" = "Angola",
                              "Argentina" = "Argentina",
                              "Armenia" = "Armenia",
                              "Aruba" = "Aruba",
                              "Australia" = "Australia",
                              "Austria" = "Austria",
                              "Azerbaijan" = "Azerbaijan",
                              "Bahamas" = "Bahamas",
                              "Bangladesh" = "Bangladesh",
                              "Belgium" = "Belgium",
                              "Bolivia" = "Bolivia",
                              "Brazil" = "Brazil",
                              "Burundi" = "Burundi",
                              "Canada" = "Canada",
                              "Chile" = "Chile",
                              "China" = "China",
                              "Costa Rica" = "Costa Rica",
                              "Cyprus" = "Cyprus",
                              "Czech Republic" = "Czech Republic",
                              "Ecuador" = "Ecuador",
                              "Egypt" = "Egypt",
                              "Ethiopia" = "Ethiopia",
                              "Finland" = "Finland",
                              "France" = "France",
                              "Germany" = "Germany",
                              "Hong Kong" = "Hong Kong",
                              "Iceland" = "Iceland",
                              "India" = "India",
                              "Indonesia" = "Indonesia",
                              "Iran" = "Iran",
                              "Iraq" = "Iraq",
                              "Ireland" = "Ireland",
                              "Italy" = "Italy",
                              "Japan" = "Japan",
                              "Jordan" = "Jordan",
                              "Kazakhstan" = "Kazakhstan",
                              "Lebanon" = "Lebanon",
                              "Malaysia" = "Malaysia",
                              "Mexico" = "Mexico",
                              "Nepal" = "Nepal",
                              "Netherlands" = "Netherlands",
                              "New Zealand" = "New Zealand",
                              "Nicaragua" = "Nicaragua",
                              "Niger" = "Niger",
                              "Oman" = "Oman",
                              "Pakistan" = "Pakistan",
                              "Philippines" = "Philippines",
                              "Portugal" = "Portugal",
                              "Romania" = "Romania",
                              "Russia" = "Russia",
                              "Saudi Arabia" = "Saudi Arabia",
                              "Serbia" = "Serbia",
                              "Sierra Leone" = "Sierra Leone",
                              "South Africa" = "South Africa",
                              "Spain" = "Spain",
                              "Sri Lanka" = "Sri Lanka",
                              "Sweden" = "Sweden",
                              "Tonga" = "Tonga",
                              "Turkey" = "Turkey",
                              "Ukraine" = "Ukraine",
                              "United Arab Emirates" = "United Arab Emirates",
                              "United Kingdom" = "United Kingdom",
                              "United States" = "United States",
                              "Uruguay" = "Uruguay",
                              "Vietnam" = "Vietnam")),
      radioButtons(inputId = "GENDER",
                   label = "Gender:",
                   choices = c(
                     "Female" = "F",
                     "Male" = "M")),
    ),
    mainPanel(
      
      h3("ASD Classification Barplot"),
      p("The outcome of the screening results are shown in this barplot.
                  This is a count of the participants who were either classified
                  by the screening as not likely or likely of having ASD."),
      plotOutput("bar1Plot"),
      
      h3("Ethnicity and Relation Barplot"),
      p("The comparison of ethnicity and relation shows the different
                  ethnicities and those relatives who are completing the 
                  screening. It is common for minorities to experience 
                  underdiagnosis, either due to cultural concerns or lack of 
                  proper resources. The goal of this visualizataion was to 
                  determine if this reality is also represented in the 
                  screening data. If so, it proves the screening tool useful 
                  in leading to an autism diagnosis."),
      plotOutput("bar2Plot"),
      
      h3("Frequency Polygon"),
      p("This visualization shows the distribution of the variables
                age and a participant with and without the classification of ASD 
                within the dataset."),
      plotOutput("frequencyPlot"),
      
      h3("Ethnicity and Age Box and Whisker Plot"),
      p("To see the variance of the ethnicity and age variables as it
                relates to those with family members with an ASD diagnosis (per
                the DSM-5 criteria), the box and whisker plot. This will assist
                with determining the skewness of the data, as well as identifying
                outliers."),
      plotOutput("boxPlot"),
      
      h3("Screening Score Correlation Plot"),
      p("A vital part of a successful screening is asking the right
                  questions.  The correlation plot shows the relationship of 
                  the questions to the other variables selected to display."),
      plotOutput("corrPlot"),
      
      h3("Conclusion"),
      p("A vital part of a successful screening is asking the right
                  questions.  The correlation plot shows the relationship of 
                  the questions to the other variables selected to display."),
    )
  )
)


ui <- navbarPage(theme = shinytheme("yeti"),
                 title = "Q-CHAT-10 Autism Screening Analysis", 
                 about_page,
                 main_page,
                 reference_page
)

# Coding the server
server <- function(input, output) {
  
  selections <- reactive({
    req(input$COUNTRY)
    req(input$GENDER)
    filter(screen_data, COUNTRY==input$COUNTRY) %>%
      filter(GENDER %in% input$GENDER)
  })
  output$bar1Plot <- renderPlot({
    ggplot(data = selections(), aes(x = CLASS, fill = CLASS)) +
      geom_bar() +
      labs(
        x = "Classification",
        y = "No. of Participants"
      ) +
      theme_clean() +
      theme(legend.position = "top")
  })
  
  output$bar2Plot <- renderPlot({
    ggplot(data = selections(), aes(x = ETHNICITY, fill = RELATION)) +
      geom_bar() +
      labs(
        x = "Ethnicity",
        y = "No. Relatives Who Completed the Screening"
      ) +
      theme_clean() +
      theme(legend.position = "top")
  })
  
  output$frequencyPlot <- renderPlot({
    ggplot(data = selections(), aes(x = AGE, col=CLASS)) + 
      geom_freqpoly(bins=10) +
      labs(
        x = "Age",
        y = "Class"
      ) +
      theme_clean() +
      theme(legend.position = "top")
  })
  
  output$boxPlot <- renderPlot({
    ggplot(selections(), aes(x = ETHNICITY, y = AGE, color = AUTISM)) +
      geom_boxplot() +
      labs(
        x = "Ethnicity",
        y = "Age of Participants"
      ) +
      theme_clean() +
      theme(legend.position = "top")
  })
  
  output$corrPlot <- renderPlot({
    corrplot(cor(selections()[,1:10]), 
             method = "shade", 
             type = "full",
             diag = TRUE,
             tl.col = "black",
             tl.srt = 35,
             mar=c(0,0,2,0),
             col = NULL
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

