library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(readr)
library(dplyr)

## Deklarasi Dataset ----
dataset <- read.csv("heart.csv", header = TRUE)
names(dataset)[1] <- "age"
dataset <- dataset[c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","thal","target")]
description <- read_csv("description.csv")
interpretasibox <- read_csv("interpretasibox.csv")
interpretasiscatter <- read_csv("interpretasiscatter.csv")

## Define UI ----
ui <- dashboardPage(skin = "black",
                    #Ada 3 layout
                    dashboardHeader(title = "Dashboard Kelompok 10 | NGGAK A+++ NGGAK PULANG", titleWidth = 580),
                    
                    dashboardSidebar(
                      width = 280,
                      sidebarMenu(
                        menuItem("Data", tabName = "Data", icon = icon("dashboard")),
                        menuItem("Visualisasi", tabName = "Visualisasi", icon = icon("th")),
                        menuItem("Intepretasi", tabName = "Intepretasi", icon = icon("table")))),
                    
                    dashboardBody(
                      tags$head( 
                        tags$style(HTML(".main-sidebar { font-size: 18px; }")) #change the font size to 18
                      ),
                      tabItems(
                        tabItem(tabName = "Data",
                                tabBox(id="t1", width = 12, 
                                       tabPanel("About", icon = icon("address-card"), 
                                                p("Heart disease is broad term used for diseases and conditions affecting the heart and circulatory system. They are also referred as cardiovascular diseases. It is a major cause of disability all around the world. Since heart is amongst the most vital organs of the body, its diseases affect other organs and part of the body as well. There are several different types and forms of heart diseases. The most common ones cause narrowing or blockage of the coronary arteries, malfunctioning in the valves of the heart, enlargement in the size of heart and several others leading to heart failure and heart attack."),
                                                tags$br(),
                                                p("Key facts according to WHO (World Health Organaizations):"),
                                                tags$ul(
                                                  tags$li("Cardiovascular diseases (CVDs) are the leading cause of death globally."), 
                                                  tags$li("An estimated 17.9 million people died from CVDs in 2019, representing 32% of all global deaths. Of these deaths, 85% were due to heart attack and stroke."), 
                                                  tags$li("Most cardiovascular diseases can be prevented by addressing behavioural risk factors such as tobacco use, unhealthy diet and obesity, physical inactivity and harmful use of alcohol."),
                                                  tags$li("It is important to detect cardiovascular disease as early as possible so that management with counselling and medicines can begin.")),
                                                tags$br(),
                                                p("Context: This database contains 76 attributes, but all published experiments refer to using a subset of 14 of them. In particular, the Cleveland database is the only one that has been used by ML researchers to 
                                                this date. The goal field refers to the presence of heart disease in the patient. It is integer-valued from 0 (no presence) to 4.
                                            
                                                Acknowledgements
                                                Creators:"),
                                                tags$ol(
                                                  tags$li("Hungarian Institute of Cardiology."),
                                                  tags$li("Budapest: Andras Janosi, M.D."),
                                                  tags$li("University Hospital, Zurich, Switzerland: William Steinbrunn, M.D."),
                                                  tags$li("University Hospital, Basel, Switzerland: Matthias Pfisterer, M.D."),
                                                  tags$li("V.A. Medical Center, Long Beach and Cleveland Clinic Foundation: Robert Detrano, M.D., PhD.")),
                                                tags$br(),
                                                p("Donor: David W. Aha (aha '@' ics.uci.edu) (714) 856-8779"),  
                                                tags$br(),
                                                tags$a(href="https://www.kaggle.com/datasets/data855/heart-disease", "Data Resources")),
                                       tabPanel("Data", dataTableOutput("dataT"), icon = icon("table")), 
                                       tabPanel("Structure", verbatimTextOutput("structure"), icon=icon("uncharted")),
                                       tabPanel("Description", dataTableOutput("dataD"), icon = icon("folder")),
                                )), #tabItem Data
                        
                        tabItem(tabName = "Visualisasi",
                                tabBox(id="t1", width = 12,
                                       tabPanel("Descriptive Statistics", verbatimTextOutput("summary"), icon=icon("chart-pie")), 
                                       
                                       tabPanel("Histogram", icon = icon("chart-simple"),
                                                fluidRow(box(plotOutput("histogram"), width = 12)),
                                                fluidRow(box(selectInput("histo_theme", "Select Theme:",
                                                                         c("Theme 1"= "PuRd",
                                                                           "Theme 2"="Spectral",
                                                                           "Theme 3" = "BuGn",
                                                                           "Theme 4" = "Paired",
                                                                           "Theme 5" = "YlOrBr")))),
                                                fluidRow(box(radioButtons("variable", "Choice Variable:",
                                                                          c("Age" = "age",
                                                                            "Resting Blood Pressure (mmHg)" = "trestbps",
                                                                            "Cholesterol (mg/dl)" = "chol",
                                                                            "Maximum Heart Rate" = "thalach"))),
                                                         box(sliderInput(inputId = "bins",
                                                                         label = "Number of bins:",
                                                                         min = 1,
                                                                         max = 303,
                                                                         value = 50)))),
                                       
                                       tabPanel("Boxplot", icon = icon("database"),
                                                fluidRow(box(plotOutput("boxplot"), width = 12)),
                                                fluidRow(box(selectInput("x_boxplot", "Choice Factor Variable:",
                                                                         c("Sex" = "sex",
                                                                           "Fasting Blood Sugar" = "fbs",
                                                                           "Resting Electrocardiographic Measurement" = "restecg",
                                                                           "Blood Disorder" = "thal",
                                                                           "Heart Disease" = "target"))),
                                                         box(radioButtons("y_boxplot", "Choice Numeric Variable:",
                                                                          c("Age" = "age",
                                                                            "Resting Blood Pressure (mmHg)" = "trestbps",
                                                                            "Cholesterol (mg/dl)" = "chol",
                                                                            "Maximum Heart Rate" = "thalach"))))),
                                       
                                       tabPanel("Scatter Plot", icon = icon("chart-line"),
                                                fluidRow(box(plotOutput("scatter_plot"), width = 12)),
                                                fluidRow(box(selectInput("scatter_theme", "Select Theme:",
                                                                         c("Theme 1"= "PuRd",
                                                                           "Theme 2"="Spectral",
                                                                           "Theme 3" = "BuGn",
                                                                           "Theme 4" = "Paired",
                                                                           "Theme 5" = "YlOrBr")))),
                                                fluidRow(box(radioButtons("x_scatter", "Choice x Variable:",
                                                                          c("Age" = "age",
                                                                            "Resting Blood Pressure (mmHg)" = "trestbps",
                                                                            "Cholesterol (mg/dl)" = "chol",
                                                                            "Maximum Heart Rate" = "thalach"))),
                                                         box(radioButtons("y_scatter", "Choice y Variable:",
                                                                          c("Age" = "age",
                                                                            "Resting Blood Pressure (mmHg)" = "trestbps",
                                                                            "Cholesterol (mg/dl)" = "chol",
                                                                            "Maximum Heart Rate" = "thalach"))))
                                                ), #tabPanel
                                )), #tabItem Visualisasi
                        
                        tabItem(tabName = "Intepretasi", 
                                tabBox(id="t1", width = 12,
                                       tabPanel("Histogram", icon = icon("calculator"),
                                                tags$ul(
                                                tags$li(h3("Age"), 
                                                p("Histogram berfungsi untuk memvisualisasikan frekuensi variabel kuantitatif, 
                                                seperti usia. Secara visual, pada histogram tampak bahwa frekuensi tertinggi berada pada interval 
                                                usia 50-60 tahun, dengan jangkauan usia terendah 29 tahun dan usia tertinggi 77 tahun. Kelompok usia 
                                                dengan frekuensi paling rendah, yaitu kelompok usia dibawah 40 tahun dan diatas 70 tahun.")),
                                                tags$li(h3("Resting Blood"), 
                                                p("Resting blood pressure menunjukkan tekanan darah ketika seseorang tidak 
                                                melakukan apapun (mmHg). Frekuensi tertinggi terletak pada interval 120 - 140 mmHg. Selain itu, 
                                                secara visual tampak bahwa kurva mengalami kemencengen ke kanan (positive skewness). Artinya, 
                                                modus rendah daripada median, dan median lebih rendah daripada rata rata.")),
                                                tags$li(h3("Cholesterol"), 
                                                p("Pada histogram yang menunjukkan frekuensi kolesterol juga menunjukkan adanya 
                                                kemencengen ke kanan (positive skewness). Kolesterol terendah yang menjadi objek pengamatan adalah 126, 
                                                sedangkan yang tertinggi adalah 564. Secara visual, tampak bahwa frekuensi tertinggi ada pada rentang 200 
                                                dan 250.")),
                                                tags$li(h3("Maximum Heart Rate"), 
                                                p("Frekuensi detak jantung tertinggi ada pada rentang 160 - 170 per menit. 
                                                Secara visual, persebaran frekuensi berada di sebelah kanan, ada kemungkinan terjadi negative skewness. 
                                                Jika benar, maka dapat dikatakan bahwa mean lebih rendah dari median, dan median lebih rendah daripada modus.")))),
                                       
                                       tabPanel("Boxplot", icon = icon("comments"), dataTableOutput("box_inter")),
                                       
                                       tabPanel("Scatter Plot", icon = icon("paper-plane"), dataTableOutput("scatter_inter"),
                                                p("Nilai Korelasi Pearson:"),
                                                tags$ul(
                                                  tags$li("0,2 - 0,4 rendah atau lemah"), 
                                                  tags$li("0,4 - 0,6 cukup besar atau cukup kuat"), 
                                                  tags$li("0,6 - 0,8 besar atau kuat"),
                                                  tags$li("0,8 - 1 sangat besar atau sangat kuat")),
                                                tags$br(),
                                                ),
                                       ) #tabBox t1
                                ) #tabItem Interpretasi
                      ) #tabItems
                      
                    ) #dashboardBody
) #dashboardPage


## Define Server Function ----
server <- function(input, output) {
  output$dataT <- renderDataTable(dataset)
  
  output$dataD <- renderDataTable(description)
  
  output$structure <- renderPrint({
    dataset %>% 
      str()
  })
  
  output$summary <- renderPrint({
    dataset %>% 
      summary()
  })
  
  output$histogram <- renderPlot({
    var <- dataset[,input$variable]
    bins <- seq(min(var), max(var), length.out = input$bins + 1)
    hist(var, breaks = bins, 
         col =  brewer.pal(n = 12, name = input$histo_theme), border = "white",
         xlab = "Variable", ylab = "Frequency",
         main = "Histogram Frequency")
  })
  
  output$boxplot <- renderPlot({
    x_boxplot <- dataset[,input$x_boxplot]
    y_boxplot <- dataset[,input$y_boxplot]
    
    boxplot(y_boxplot~x_boxplot, main ="Boxplot",
            xlab="Factor", ylab="Measurement", col = brewer.pal(n = 4, name = "Pastel1"))
  })
  
  output$scatter_plot <- renderPlot({
    x_scatter <- dataset[,input$x_scatter]
    y_scatter <- dataset[,input$y_scatter]
    
    plot(x_scatter, y_scatter, 
         main = "Scatter Plot",
         col = brewer.pal(n = 9, name = input$scatter_theme), pch=20 , cex=2.5, xlab= "Your X Variable", 
         ylab = "Your Y Variable")
    
    reg <- lm(dataset[,input$y_scatter] ~ dataset[,input$x_scatter], data = dataset)
    abline(reg, col = "red")
    cor(dataset[,input$x_scatter], dataset[,input$y_scatter], method = "pearson")
  })
  
  output$box_inter <- renderDataTable(interpretasibox)
  
  output$scatter_inter <- renderDataTable(interpretasiscatter)
  
} #server

## Run the App ----
shinyApp(ui = ui, server = server)
