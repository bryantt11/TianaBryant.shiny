#---------------------------PART 3: BUILD THE APP-------------------------------
# Load libraries needed
library(dplyr)
library(shiny)
library(ggplot2)
library(corrplot)
library(readr)
library(shinythemes)
library(ggthemes)
library(caret)
library(DT)
library(FNN)
library(ggthemes)

# Bring in cleaned dataset
screen_data <- read.csv("data/autism_clean.csv")

# Bring in training/validation data
screen.train <- read.csv("data/screen_train.csv")
screen.valid <- read.csv("data/screen_train.csv")

norm.values <- preProcess(screen.train, method=c("center", "scale"))
train.norm.df <- predict(norm.values, screen.train)
valid.norm.df <- predict(norm.values, screen.valid)

knn.pred <- knn(train.norm.df, valid.norm.df,
                cl = train.norm.df[, "A6_SCORE"], k = 5)

reference_page <- tabPanel(
    title = "References",
    titlePanel("References"),
    br(),
    "Allison, C., Auyeung, B., & Baron-Cohen, S. (2012, 1 5). Quantitative 
    Checklist for Autism in Toddlers (Q-CHAT-10). Retrieved from Embloom: 
    https://www.embloom.com/content/q-chat-10/",
    br(),
    br(),
    "APA. (2022). Diagnostic and Statistical Manual of Mental Disorders (DSM–5). 
    Retrieved from American Psychiatric Association: https://www.psychiatry.org/psychiatrists/practice/dsms",
    br(),
    br(),
    "Autism Society. (2022). What is Autism? Retrieved from Autism Society: 
    https://www.autism-society.org/what-is/",
    br(),
    br(),
    "Bryant, T. (2022, March 3). Exploring Q-CHAT-10 ASD Screening Data.",
    br(),
    br(),
    "Larxel. (2020, August 17). Autism Screening on Adults. Retrieved from Kaggle
    https://www.kaggle.com/andrewmvd/autism-screening-on-adults ",
    br(),
    br(),
    "MJ, M., KA, S., AV, B., & etal. (2018). Prevalence and Characteristics of 
    Autism Spectrum Disorder Among Children Aged 8 Years. Autism and Developmental 
    Disabilities Monitoring Network, 1–16.",
    br(),
    br(),
    "Raj, S., & Masood, S. (2020). Analysis and Detection of Autism Spectrum 
    Disorder Using Machine Learning Techniques. Procedia Computer Science, 
    Pages 994-1004.",
    br(),
    br(),
    "Thabtah, F. (2017). Autism Spectrum Disorder Screening: Machine Learning 
    Adaptation and DSM-5 Fulfillment. Proceedings of the 1st International 
    Conference on Medical and Health Informatics, 1-6."
)
userguide_page <- tabPanel(
    img(src='Neurodiversity.jpeg', height="20%", width="20%", align="center"),
    title = "User Information",
    titlePanel(""),

    h3("Introduction"),
    "Autism, also known as autism spectrum disorder (ASD), “is a complex, 
    lifelong developmental disability that typically appears during early 
    childhood and can impact a person’s social skills, communication, relationships, 
    and self-regulation” (Autism Society, 2022).  In 2018, it was found that ASD 
    impacts nearly 1 in 44 children, more than doubling the prevalence found in 
    2004 of 1 in 125 (MJ, KA, AV, & etal, 2018).  As there are barriers to diagnosis 
    for underprivileged communities, it is important to make widely available 
    proper screening tools to aid caregivers to identify any signs of autism, 
    so that early intervention can take place. This analysis will test the efficacy 
    of a dataset containing 704 records of screening results from the Q-CHAT-10, 
    as retrieved from Kaggle (Larxel, 2020).  This the Q-CHAT-10 is a short form 
    of the Q-CHAT, or Quantitative Checklist for Autism in Toddlers, screening 
    (Bryant, 2022).",
    br(),
    br(),
    h3("Purpose"),
    "ASD has impacted my family directly, so I take great interest in being an 
    advocate for neurodiversity.  My oldest son was diagnosed autistic first at 
    age 10, followed by myself at age 35.  Knowing that ASD is a highly genetic 
    condition, I had my youngest son tested at age 6 and he also is autistic, 
    along with ADHD. Late diagnoses for my oldest child and myself had a tremendous 
    negative impact in both the social and academic atmospheres.  As such, I have 
    found myself passionate about research and education for both the neurotypical 
    (no developmental differences) and neurodiverse people that I interact with.  
    Analyzing tools such as the Q-CHAT-10 and testing their effectiveness can 
    support ensuring that other people may not have to endure the same hardships 
    my family did through knowledge and information sharing.  However, it is 
    important to put useful tools in the hands of caregivers and those with ASD 
    to ensure that it is useful in identifying ASD traits.",
    br(),
    br(),
    h3("Dashboard"),
    "Prior to developing the dashboard, I conducted an exploratory analysis of 
    the data, cleaning and formatting it so that the data produced for the analysis 
    are useful.  There were several fields or records that would skew the results, 
    so I truncated or updated where applicable.  I also developed a predictive 
    model using K-Nearest Neighbors as part of the analysis. From there, three 
    export files were produced of the processed dataset, training dataset and 
    validation dataset.  These datasets were then used in the development of an 
    application in R Shiny to display the results.",
    br(),
    br(),
    h3("User Information"),
    "Screening early for ASD and providing appropriate diagnosis as soon as 
    possible will ideally allow for early intervention (Thabtah, 2017).  
    Early intervention is critical for supporting those with ASD, as “high 
    quality intervention can improve learning, communication and social skills, 
    as well as underlying brain development” (Larxel, 2020). This dashboard 
    examines a screening tool outside of the typical Diagnostic and Statistical 
    Manual of Mental Disorders (DSM) methodology used to classify mental disorders 
    in the United States (APA, 2022). The Q-CHAT-10 looks for “red flags” that 
    can identify ASD traits (Allison, Auyeung, & Baron-Cohen, 2012).",
    br(),
    br(),
    h3("Getting Started"),
    "The analysis is comprised of the following: User Information, ASD Screening 
    Dashboard, References, and About.  To access the Q-CHAT-10 Autism Screening 
    Analysis, select the ASD Screening Dashboard. As you review the data, keep 
    in mind that the outputs are mostly interactive and will populate with the 
    sample criteria selected. To change the criteria, follow these steps.",
    br(),
    br(),
    h4("Step 1:"), 
    "After accessing the ASD Screening Dashboard, retrieve the sample population 
    that you would like to view by clicking the drop-down arrow in the “Sample 
    Criteria” section, located under “Select Country:”.  Select the country and 
    “Gender:” criteria; the sample data will automatically populate.",
    h4("Step 2:"), 
    "Conduct your review of the output.  There are explanations provided of the 
    output for each visualization provided.  Once you’ve set your criteria, the 
    dashboard will not automatically reformat itself. If another sample 
    population needs to be reviewed, simply select new Sample Criteria.",
    br(),
    "Note:  Results will be displayed on the Interactive Visualizations tab.",
    br(),
    h4("Step 3:"), 
    "The Non-Interactive tab displays the predictive modeling analysis.  These
    features help to provide context to the analysis and support the findings.",
    br(),
    h4("Step 4:"), 
    "For a conclusion of the analysis, the Findings tab briefly describes the results
    from the predictive model for your information. Any sources associated with this
    research are displayed in the References tab.",
    br(),
    br(),
    h3("Data Dictionary"), 
    "Below is the collection of variable names, definitions and attributes contained
    within the dataset.",
    br(),
    img(src='Table 1.png', height="35%", width="35%", align = "center"),
    br(),
    img(src='Table 2.png', height="40%", width="40%", align = "center")
)

about_page <- tabPanel(
    title = "About",
    titlePanel("About this Project"),
    br(),
    "Welcome, and thank you for taking the time to view my project!",
    br(),
    br(),
    "My name is Tiana Bryant, and I prepared this dashboard as a final project 
    for the BDAT 630: Data Visualization course at Maryville University Online.
    Using RShiny, I have created an interactive dashboard that will share the
    results of my analysis on autism spectrum disorder (ASD) Q-CHAT-10 screening data.",
    br(),
    br(),
    "It has been the experience for myself and my oldest son, with ASD levels 1 
    and 2, that there can be a delay to diagnosis. There is such a wide variety 
    of symptoms that are still poorly understood.  However, in the case of my 
    youngest son (level 1), because my husband and I were more knowledgeable, we 
    were able to introduce early intervention and his academic and social 
    performance have been superior to that of my oldest son and myself at the 
    same age.  It is my goal to be an advocate and support other people on the
    spectrum, and their families, in ways that we did not have the support and 
    projects like these have helped me learn and serve others through knowledge
    sharing.",
    br(),
    br(),
    img(src='Selfie.png', height="15%", width="15%", align = "center"),
    br(),
    br()
)
                
main_page <- tabPanel(
    # Application title
    title = "ASD Screening Dashboard",
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
          tabsetPanel(type = "tab",
                      tabPanel("Interactive Visualizations",
                               h4("Sample Overview"),
                               
                               p("This is a table that provides an overview of the data for the 
                                variables subject to this analysis, as selected in the sample 
                                criteria.  This includes age, ethnicity, whether they have a 
                                relative that has autism, the relative who completed the 
                                screening, and if the participant was classified as likely or 
                                not to have ASD. Additionally, the details of the count of 
                                entries, or total records included as defined in the sample 
                                criteria, is displayed."),
                               DT::dataTableOutput("dtable"),
                               br(),
                               h4("Screening Results"),
                               p("This is a table that provides the responses of the screening
                                for reference. This information can be used to compare against a
                                correlation plot. The details of the count of 
                                entries, or total records included as defined in the sample 
                                criteria, is displayed."),
                               DT::dataTableOutput("qtable"),
                               br(),
                               h4("Participant Count - by Gender"),
                               p("Simple barplot to show a comparative count of particpants
                               by gender for the total population."),
                               plotOutput("bar1Plot"),
                               br(),
                               h4("Participant Count - by Ethnicity"),
                               p("Simple bar plot to show a comparative count of particpants
                               by ethnicity for the total population."),
                               plotOutput("bar2Plot"),
                               br(),
                               h4("ASD Classification Barplot"),
                               p("The outcome of the screening results are shown in this barplot.
                               This is a count of the participants who were either classified
                               by the screening as not likely or likely of having ASD."),
                               plotOutput("bar3Plot"),
                               br(),
                               h4("Ethnicity and Relation Barplot"),
                               p("The comparison of ethnicity and relation shows the different
                               ethnicities and those relatives who are completing the 
                               screening. It is common for minorities to experience 
                               underdiagnosis, either due to cultural concerns or lack of 
                               proper resources. The goal of this visualizataion was to 
                               determine if this reality is also represented in the 
                               screening data. If so, it proves the screening tool useful 
                               in leading to an autism diagnosis."),
                               plotOutput("bar4Plot"),
                               br(),
                               h4("Frequency Distribution - Age Histogram"),
                               p("To visualize the distribution of the ages contained within
                               the dataset, a histogram plot was produced. The histogram can
                               also identify skewness within the dataset."),
                               plotOutput("histogramPlot"),
                               br(),
                               h4("Ethnicity and Age Box and Whisker Plot"),
                               p("To see the variance of the ethnicity and age variables as it
                                relates to those with family members with an ASD diagnosis (per
                                the DSM-5 criteria), the box and whisker plot. This will also assist
                                with determining the skewness of the data, as well as identifying
                                outliers."),
                               plotOutput("boxPlot"),
                      ),
                      tabPanel("Non-Interactive Visualizations",
                               
                               h4("Summary Statistics"),
                               p("An exploratory analysis of the dataset that provides
                                 preliminary summary statistics."),
                                verbatimTextOutput("summary"),
                               br(),
                               h4("Screening Score Correlation Plot - Sample Data"),
                               p("A vital part of a successful screening is asking the right
                                  questions.  The correlation plot shows the relationship of 
                                  the questions to the other variables selected to display. Note 
                                  the high level of correlation between A6 and the other variables."),
                               plotOutput("corrPlot"),
                               br(),
                               h4("K-Nearest Neighbors"),
                               # Display KNN results
                               p("KNN predictive model of one of the questions with high correlation, 
                               A6 - “Does your child follow where you’re looking?”, were found to 
                               have as high as a 94.29% accuracy in predicting the classification 
                               of “Yes” for having ASD traits."),
                               verbatimTextOutput("confusionMatrix"),
                      ),
                      tabPanel("Findings",
                               h4("Findings"),
                               p("This at a glance analysis provided mulitple visualizations 
                               that convey the screening results from the Q-CHAT-10. The 
                               analysis supports the accuracy that was found in a study 
                               conducted of the Q-CHAT that was administered to toddlers. 
                               Using the same dataset, Raj and Masood (2020) report the 
                               findings were that 'CNN based prediction models work better on all 
                               these datasets with higher accuracy of 99.53%, 98.30%, 96.88% for 
                               Autistic Spectrum Disorder Screening in Data for Adult, Children, 
                               and Adolescents respectively.' With such a perceptive screening 
                               tool, especially one that is in short form such as the Q-CHAT-10, 
                               there will be more access to treatment by taking down barriers 
                               through a simple screening given by a loved one and better outcomes 
                               for those with ASD and their families who need the support.")

                )
            )
        )
    )
)


ui <- navbarPage(theme = shinytheme("yeti"),
                 title = "Q-CHAT-10 Autism Screening Analysis", 
                 userguide_page,
                 main_page,
                 reference_page,
                 about_page

)

# Coding the server

server <- function(input, output) {
    selections <- reactive({
        req(input$COUNTRY)
        req(input$GENDER)
        filter(screen_data, COUNTRY==input$COUNTRY) %>%
            filter(GENDER %in% input$GENDER)
    })
    
    output$confusionMatrix <- renderPrint({
      confusionMatrix(knn.pred, factor(valid.norm.df[, "A6_SCORE"])
            )
    })
    
    output$summary <- renderPrint({
      dataset <- selections()
      summary(dataset)
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
        theme_igray() 
   
    })
    
    output$bar2Plot <- renderPlot({
        ggplot(data = screen_data, aes(x= ETHNICITY, fill=ETHNICITY)) +
            geom_bar() +
            labs(
                x = "Ethnicity",
                y = "No. of Participants"
            ) +
            coord_flip() +
        theme_igray() 
      
    })
    
    output$bar3Plot <- renderPlot({
        ggplot(data = selections(), aes(x = CLASS, fill = AUTISM)) +
            geom_bar() +
            labs(
                x = "Classification",
                y = "No. of Participants"
            ) +
        theme_igray() 
      
    })
    
    
    output$bar4Plot <- renderPlot({
        ggplot(data = selections(), aes(x = ETHNICITY, fill = RELATION)) +
            geom_bar() +
            labs(
                x = "Ethnicity",
                y = "No. Relatives Who Completed the Screening"
            ) +
        theme_igray() 
    })
    
    output$histogramPlot <- renderPlot({
        ggplot(selections(), aes(x=AGE, fill = CLASS)) + 
          geom_histogram(binwidth=5) +
        theme_igray() 
        
    })
    
    output$boxPlot <- renderPlot({
        ggplot(selections(), aes(x = ETHNICITY, y = AGE, color = AUTISM)) +
            geom_boxplot() +
            labs(
                x = "Ethnicity",
                y = "Age of Participants"
            ) +
        theme_igray() 
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
