#---------------------------PART 2: BUILD THE APP-------------------------------
# Load libraries needed
library(shiny)
library(tidyverse)
library(corrplot)
library(readr)
library(shinythemes)
library(ggthemes)

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
    https://www.psychiatry.org/psychiatrists/practice/dsm",
    br(),
    br(),
    "Ruta L, Chiarotti F, Arduino GM, Apicella F, Leonardi E, Maggio R, Carrozza 
    C, Chericoni N, Costanzo V, Turco N, Tartarisco G, Gagliano A, Allison C, 
    Baron Cohen S, Pioggia G and Muratori F (2019) Validation of the Quantitative 
    Checklist for Autism in Toddlers in an Italian Clinical Sample of Young 
    Children With Autism and Other Developmental Disorders. Front. Psychiatry 
    10:488. doi: 10.3389/fpsyt.2019.00488"
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
    possible will ideally allow for early intervention (Thabtah, 2017).  Early 
    intervention is critical for supporting those with ASD, as “high quality 
    intervention can improve learning, communication and social skills, as well 
    as underlying brain development” (Larxel, 2020).  It has been the experience 
    for myself and my oldest son, with ASD levels 1 and 2, that there can be a 
    delay to diagnosis. There is such a wide variety of symptoms that are still 
    poorly understood.  However, in the case of my youngest son (level 1), because 
    my husband and I were more knowledgeable, we were able to introduce early 
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
    titlePanel("Sample Criteria"),
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
                             "Male" = "M"))
            
    ),
            mainPanel(
                
                h3("Sample Overview"),
                p("This is a table that provides an overview of the data for the 
                  variables subject to this analysis, as selected in the sample 
                  criteria.  This includes age, ethnicity, whether they have a 
                  relative that has autism, the relative who completed the 
                  screening, and if the participant was classified as likely or 
                  not to have ASD. Additionally, the details of the count of 
                  entries, or total records included as defined in the sample 
                  criteria, is displayed."),
                DT::dataTableOutput("dtable"),
                
                h3("Screening Results"),
                p("This is a table that provides the responses of the screening
                  for reference. This information can be used to compare against a
                  correlation plot. Additionally, the details of the count of 
                  entries, or total records included as defined in the sample 
                  criteria, is displayed."),
                DT::dataTableOutput("qtable"),
                
                h3("Screening Score Correlation Plot - Sample Data"),
                p("A vital part of a successful screening is asking the right
                  questions.  The correlation plot shows the relationship of 
                  the questions to the other variables selected to display."),
                plotOutput("corrPlot"),
                
                h3("Participant Count - by Gender"),
                p("Simple barplot to show a comparative count of particpants
                  by gender for the total population."),
                plotOutput("bar1Plot"),
                
                h3("Participant Count - by Ethnicity"),
                p("Simple bar plot to show a comparative count of particpants
                  by ethnicity for the total population."),
                plotOutput("bar2Plot"),
                
                h3("ASD Classification Barplot"),
                p("The outcome of the screening results are shown in this barplot.
                  This is a count of the participants who were either classified
                  by the screening as not likely or likely of having ASD."),
                plotOutput("bar3Plot"),
                
                h3("Ethnicity and Relation Barplot"),
                p("The comparison of ethnicity and relation shows the different
                  ethnicities and those relatives who are completing the 
                  screening. It is common for minorities to experience 
                  underdiagnosis, either due to cultural concerns or lack of 
                  proper resources. The goal of this visualizataion was to 
                  determine if this reality is also represented in the 
                  screening data. If so, it proves the screening tool useful 
                  in leading to an autism diagnosis."),
                plotOutput("bar4Plot"),
                
                h3("Frequency Distribution - Age Histogram"),
                p("To visualize the distribution of the ages contained within
                  the dataset, a histogram plot was produced. The histogram can
                  also identify skewness within the dataset."),
                plotOutput("histogramPlot"),
                
                h3("Ethnicity and Age Box and Whisker Plot"),
                p("To see the variance of the ethnicity and age variables as it
                relates to those with family members with an ASD diagnosis (per
                the DSM-5 criteria), the box and whisker plot. This will also assist
                with determining the skewness of the data, as well as identifying
                outliers."),
                plotOutput("boxPlot"),
                
                h3("Conclusion"),
                p("This at a glance analysis provided mulitple visualizations 
                  that convey the screening results from the Q-CHAT-10. The 
                  analysis supports the accuracy that was found in a study 
                  conducted of the Q-CHAT that was administered to toddlers. The
                  short form Q-CHAT-10 was tested among autistic and non-autistic
                  participants and 'the screening cut-point of 3 demonstrated 
                  sensitivity and specificity estimates as high as 91% and 89%, 
                  respectively' (Ruta, L. et.al, 2019). While more studies need
                  to be conducted to determine if similar efficacy can be obtained
                  in using on adults, or as a formal diagnostic tool in toddlers, 
                  the screening tool is promising and should be considered by 
                  those who are showing signs indicative of ASD to pursue further
                  evaluation.")
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
    
output$dtable = DT::renderDataTable({
    (datatable(selections()[,c(11,13,14,17,18)],
                 rownames = TRUE,
                 class = 'cell-border stripe',
                 caption = 'Filter the table results to your preferences.',
                 filter = 'none'))
})

output$corrPlot <- renderPlot({
    corrplot(cor(selections()[,1:10]), 
             method = "color", 
             type = "full",
             diag = TRUE,
             tl.col = "black",
             tl.srt = 35,
             col = NULL
    )
})

output$qtable = DT::renderDataTable({
    (datatable(selections()[,c(1:10)],
               rownames = TRUE,
               class = 'cell-border stripe',
               caption = 'Filter the table results to your preferences.',
               filter = 'none'))
})
    
output$bar1Plot <- renderPlot({
    ggplot(data = screen_data, aes(x=CLASS, fill=CLASS)) +
        geom_bar() +
        labs(
            x = "Classification",
            y = "No. of Participants"
        ) +
        theme_test()
})

output$bar2Plot <- renderPlot({
    ggplot(data = screen_data, aes(x= ETHNICITY, fill=ETHNICITY)) +
        geom_bar() +
        labs(
            x = "Ethnicity",
            y = "No. of Participants"
        ) +
        coord_flip() +
        theme_test()
})

output$bar3Plot <- renderPlot({
    ggplot(data = selections(), aes(x = CLASS, fill = AUTISM)) +
        geom_bar() +
        labs(
            x = "Classification",
            y = "No. of Participants"
            ) +
        theme_test()
})


output$bar4Plot <- renderPlot({
    ggplot(data = selections(), aes(x = ETHNICITY, fill = RELATION)) +
        geom_bar() +
        labs(
            x = "Ethnicity",
            y = "No. Relatives Who Completed the Screening"
        ) +
        theme_test()
})

output$histogramPlot <- renderPlot({
    ggplot(selections(), aes(x=AGE, fill = CLASS)) + 
        geom_histogram(binwidth=5) +
        theme_test()
    
})

output$boxPlot <- renderPlot({
    ggplot(selections(), aes(x = ETHNICITY, y = AGE, color = AUTISM)) +
        geom_boxplot() +
        labs(
            x = "Ethnicity",
            y = "Age of Participants"
        ) +
        theme_test()
})

}

# Run the application 
shinyApp(ui = ui, server = server)
