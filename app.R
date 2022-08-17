library(shiny)
library(covdata)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggrepel)
library(shinythemes)
library(plotly)
library(rsconnect)

# Filter and organize nytcovstate data
nytcovstate$month <- floor_date(nytcovstate$date, "month")
state_month_df <- nytcovstate %>%
  group_by(month, state) %>%
  summarise(cases = sum(cases), deaths = sum(deaths))

# nytcovus data
national_df <- nytcovus 

# Age group data
age_df <- cdc_deaths_by_age
age_df <- age_df %>%
  rename(AgeGroup = age_group,
         CovidDeath = covid_deaths,
         TotalDeath = total_deaths,
         PneumoniaDeath = pneumonia_deaths,
         PneumoniaAndCovidDeath = pneumonia_and_covid_deaths)

# Filter and organize covus_race data
race_df <- na.omit(covus_race) %>%
  group_by(group) %>%
  summarise(cases = sum(cases), death = sum(deaths))
race_df <- race_df %>%
  rename(Cases = cases, Deaths = death)


introduction <- tabPanel(
  "Home",
  h2("Introduction"),
  includeCSS("style.css"),
  p("This web is using a dataset package that contains multiple datasets relating 
    to COVID-19 including the Apple Mobility data and the New York Times COVID data."),
  h3("Background"),
  p("The pandemic has played an essential role in human civilization for a long time. 
    On December 31th, 2019, Wuhan Municipal Health Commission, 
    China, reported the first case of Coronavirus disease. 
    The first confirmed case of Covid-19 in the United States was reported on January 20th, 2020. 
    On January 30th, the World Health Organization declared 
    this disease as Public Health Emergency of International Concern; 
    on March 11th, WHO announced COVID-19 as a “pandemic”. 
    Now the disease has spread to every country on Earth."),
  tags$b("Hopefully, As we explore these datasets of Covid-19 cases/deaths records, 
    we can have better understanding of the situation."),
  h3("Dataset Sources: "),
  p("The whole dataset Package: ",
  a(href = "https://kjhealy.github.io/covdata/articles/covdata.html", "Covdata")),
  p("The datesets I used in this project: ",
  a(href = "https://kjhealy.github.io/covdata/reference/cdc_deaths_by_age.html", 
    "CDC Surveillance Network Death Counts by Age/"),
  a(href = "https://kjhealy.github.io/covdata/reference/nytcovstate.html", 
    "NYT COVID-19 data for the US states/"),
  a(href = "https://kjhealy.github.io/covdata/reference/covus_race.html", 
    "COVID-19 case and death counts for the USA by race and state/"),
  a(href = "https://kjhealy.github.io/covdata/reference/nytcovus.html", 
    "NYT COVID-19 data for the US")),
  h3("Author: "),
  p("Yilin Li")
)

plot1 <- tabPanel(
  "US State Cases&Deaths",
  h2("Choose a State to see its cases/deaths from 2020 to 2022 in US"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "choices",
                  label = "Cases or Deaths",
                  choices = c("cases", "deaths")),
      br(),
      selectInput(inputId = "State",
                  label = "Choose a State",
                  choices = unique(state_month_df$state), multiple = F)),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Line Plot", plotOutput("state_plot")),
                  tabPanel("Data", tableOutput("state_data"))
      )
    )
  )
)

plot2 <- tabPanel(
  "Bar Chart",
  h2("US Cases/Death by Date Range(from 2020-01-21 to 2021-10-18)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "options",
                  label = "Cases or Deaths",
                  choices = c("cases", "deaths")),
      br(),
      br(),
      dateRangeInput(inputId = "date",
                     strong("Date Range"),
                     start = "2020-01-21", end = "2021-10-18",
                     min = "2020-01-21", max ="2021-10-18")
    ),
    mainPanel(
      plotlyOutput("barPlot"),
    )
  )
)


plot3 <- tabPanel(
  "ScatterPlot",
  h2("Explore the deaths/cases by Different Race and Age Group"),
  p("Data displays in the Race Group plot has removed NA values for a better visualization"),
  p("The race data's time range is from 2020 to 2021, 
    The age data's time range is from Feburary to April in 2020."),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "reasons",
                  label = "Choose one death reason for Age groups",
                  choices = c("CovidDeath", 
                              "TotalDeath", 
                              "PneumoniaDeath", 
                              "PneumoniaAndCovidDeath"
                              )),
      br(),
      selectInput(inputId = "options2",
                  label = "Choose Cases/Deaths for Races",
                  choices = c("Cases", "Deaths")),
      
    ),
    mainPanel(
              fluidRow(
                column(8,plotOutput(outputId="graph1")),  
                column(12,plotOutput(outputId="graph2"))
              )
    )
  )
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("journal"),
  navbarPage("Covid Data Visualization Anaylsis",
             introduction,
             plot1,
             plot2,
             plot3)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # US state cases and deaths plot
  state_data <- reactive({
    state_month_df %>%
      filter(state == input$State)
  })
  output$state_plot <- renderPlot({
    ggplot(state_data(), aes(x = month, y = .data[[input$choices]]))+
      geom_line(color = "cornflowerblue") +
      geom_point(color = "chocolate1") +
      labs(x = "Time(Year-Month)",
           title = "Line Plot of Cases/Deaths in US by State") +
      ggrepel::geom_label_repel(aes(label = .data[[input$choices]])) +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 18, hjust = 0.5))
  })
  output$state_data <- renderTable({
    state_data()
  })
  
  
  # plot2
  output$barPlot <- renderPlotly({
    barChart <- national_df %>%
      filter(date >= input$date[1], date <= input$date[2]) %>%
        ggplot(aes(x = date, y = .data[[input$options]])) +
        geom_bar(stat = "identity", 
                 position = "stack", 
                 aes(fill = .data[[input$options]])) +
        scale_fill_gradient(low='pink', high='tomato3') +
        labs(x = "Date",
             title = "Bar Chart of US Cases/Deaths in a specific Date Range") +
        theme(axis.text = element_text(size = 16),
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 18, hjust = 0.5))
        ggplotly(barChart)
  })
  
  # plot3
  output$graph1 <- renderPlot({
    ggplot(age_df, aes(x = AgeGroup, y = .data[[input$reasons]], color=AgeGroup))+
      geom_point(size=3) +
      labs(x = "Age Group",
           title = "Scatter Plot of Different Deaths Reasons by Age Groups") +
      ggrepel::geom_label_repel(aes(label = .data[[input$reasons]])) +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 18, hjust = 0.5))
  })
  output$graph2 <- renderPlot({
    ggplot(race_df, aes(x = group, y = .data[[input$options2]], color=group))+
      geom_point(size=3)  +
      labs(x = "Race Group",
           title = "Scatter Plot of Cases/Deaths by Race",
           caption="Data displays in this Race Group plot has removed NA values for a better visualization") +
      ggrepel::geom_label_repel(aes(label = .data[[input$options2]])) +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 18, hjust = 0.5))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
