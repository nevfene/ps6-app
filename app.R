#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)

data2015 <- read_delim("2015StatResults.csv")
data2015 <- data2015[-1:-2,]

data2015 <- gather(data2015, key = "drug", value = "usage", 3:7)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Nev Fenelon, PS6"),
  headerPanel("Statistics for Drug Usage"),
  tabsetPanel(type = "tab",
              tabPanel("Introduction to the Data",
                       mainPanel(
                         h3("This app compares drug usage rates by different substances across mulitple demographics."),
                         p("There are 3 focus demographics (age, race, gender) with observations over 5 different substances 
                           (cocaine, heroin, opiates, amphetamines, and stimulates)"),
                         em("Data collected in 2015"),
                         h3("Here is a random sample from the data:"),
                         dataTableOutput("sample"),
                         p("I spent 10 hours on this problem  set"))),
              
              
              tabPanel("Plot",
                       sidebarLayout(
                         sidebarPanel(
                           p("You can observe and compare the usage rates for different substances
                betweeen different age groups"),
                           selectInput("Age_Range","Select age range:",
                                       choices = c("12-17 years","18-20 years","21-25 years","26-30 years","31-35 years",
                                                   "36-40 years","41-45 years","46-50 years","51-55 years","56-60 years",
                                                   "61-65 years","66 years and over")),
                           p("View values?"),
                           selectInput("values", "Select yes or no:",
                                       choices = c("Yes", "No"),
                                       selected = "No"),
                         ),
                         mainPanel(
                           textOutput("obs1"),
                           plotOutput("barGraph"),
                           p("(Note that the specified drugs do not make up all of the substances considered in the usage total.)"),
                         ))),
              
              tabPanel("Data Table",
                       sidebarLayout(
                         sidebarPanel(
                           p("Choose one substance to compare the 
                highest and lowest usage rate per demographic group."),
                           selectInput("drugs", "Choose substance(s) to view",
                                       choices = unique(data2015$drug))
                         ),
                         mainPanel(
                           textOutput("obs2"),
                           dataTableOutput("table")
                         )
                       )
              )
              
  )
)

server <- function(input, output) {
  
  output$sample <- renderDataTable({
    data2015 %>% sample_n(3)
  })
  
  #PAGE 2: PLOT BAR/DONUT GRAPH
  
  graphdata <- reactive ({
    data2015 %>% 
      filter(Demographic == "Age") %>%
      filter(`State: US` %in% input$Age_Range)
  })
  output$barGraph <- renderPlot({
    if(input$values == "Yes")({
      ggplot(graphdata(), aes(x = drug, y = (usage/100), fill = drug)) + geom_bar(stat = "identity") +
        ggtitle("Drug usage rates by substance in 2015 per age group") +
        geom_text((aes(label = scales::percent((usage/100), accuracy = 0.1)))) + 
        scale_y_continuous(labels = scales::percent) +
        labs(x = "Substances used",
             y = "Percent of usage by drug")
    })else({
      ggplot(graphdata(), aes(x = drug, y = (usage/100), fill = drug)) + geom_bar(stat = "identity") +
        ggtitle("Drug usage rates by substance in 2015 per age group") +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "Substances",
             y = "Percent of usage")
    })
  })
  #PAGE 3: DATA TABLE
  output$table <- renderDataTable({
    data2015 %>%
      filter(drug %in% input$drugs) %>%
      group_by(Demographic) %>%
      summarise(min(usage),
                max(usage))
  })
  output$obs1 <- renderPrint({
    data2015 %>%
      filter(Demographic == "Age") %>%
      filter(`State: US` %in% input$Age_Range) %>%
      summarise(max(usage)) %>%
      paste("The maximum usage rate is ",., "%")
  })
  output$obs2 <- renderPrint({
    data2015 %>%
      filter(drug %in% input$drugs) %>%
      summarise(max(usage)) %>%
      paste("The maximum usage rate is ",., "%")
  })
  output$obs3 <- renderPrint({
    data2015 %>%
      filter(drug %in% input$drugs) %>%
      paste(`State: US` == "Total (Number)")
  })
}


shinyApp(ui = ui, server = server)
