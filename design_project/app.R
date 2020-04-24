#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

read_rds("census_model_joined.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Deep Dive into Design in America"),
    
    # Adding tabs
    navbarPage("",
               
    # Sidebar with a slider input for number of bins 
    tabPanel("Who Are Our Designers?", 
             sidebarLayout(
                 sidebarPanel(
                     sliderInput("bins",
                                 "Number of bins:",
                                 min = 1,
                                 max = 50,
                                 value = 30)
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     plotOutput("tab1")
                 )
             )),
    tabPanel("By Location"
             ),
    tabPanel("By Salary"
             ),
    tabPanel("By Satisfaction"
             )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$tab1 <- renderPlot(
        
        census_model_joined %>%
            group_by(age) %>%
            count() %>%
            ggplot(aes(age, n)) +
            geom_bar(stat = "identity")
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
