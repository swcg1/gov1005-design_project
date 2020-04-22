#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Deep Dive into Design in America"),
    
    # Adding tabs
    navbarPage(":)",
               
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
                     textOutput("selected")
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

    output$selected <- renderText({
        
        paste("You have selected")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
