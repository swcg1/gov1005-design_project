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
    tabPanel("By Salary and Job Satisfaction",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("y_var",
                                 label = "Choose a metric to investigate",
                                 choices = list("Salary", "Job Satisfaction"),
                                 selected = "Salary"),
                     selectInput("x_var",
                                 label = "Choose a variable to investigate",
                                 choices = list("Age",
                                                "Gender",
                                                "Career Duration",
                                                "Organization Size",
                                                "Department Size"))
                 ),
                 mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("2017",
                                          plotOutput("salary_age_2017")),
                                 tabPanel("2019",
                                          plotOutput("salary_age_2019")))
                 )
             )
             ),
    tabPanel("Predictor"
             ),
    tabPanel("By Location"
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
    
    salary_satisfaction_react_2017 <- reactive({
        if(input$y_var == "Salary"){
            if(input$x_var == "Age"){
                summarized <- census_model_joined %>%
                    group_by(age_group) %>%
                    filter(year == 2017) %>%
                    summarize(mean_salary = mean(salary)) %>%
                    ggplot(aes(age_group, mean_salary)) +
                    geom_col(aes(fill = age_group))
            }
            else if(input$x_var == "Gender"){
                summarized <- census_model_joined %>%
                    group_by(gender) %>%
                    filter(year == 2017) %>%
                    summarize(mean_salary = mean(salary)) %>%
                    ggplot(aes(gender, mean_salary)) +
                    geom_col(aes(fill = gender))
            }
            else if(input$x_var == "Career Duration"){
                summarized <- census_model_joined %>%
                    group_by(career_duration) %>%
                    filter(year == 2017) %>%
                    summarize(mean_salary = mean(salary)) %>%
                    ggplot(aes(career_duration, mean_salary)) +
                    geom_col(aes(fill = career_duration))
            }
            else if(input$x_var == "Organization Size"){
                summarized <- census_model_joined %>%
                    group_by(org_size) %>%
                    filter(year == 2017,
                           !org_size == "") %>%
                    summarize(mean_salary = mean(salary)) %>%
                    ggplot(aes(org_size, mean_salary)) +
                    geom_col(aes(fill = org_size))
            }
            else if(input$x_var == "Department Size"){
                summarized <- census_model_joined %>%
                    group_by(department_size) %>%
                    filter(year == 2017,
                           !department_size == "") %>%
                    summarize(mean_salary = mean(salary)) %>%
                    ggplot(aes(department_size, mean_salary)) +
                    geom_col(aes(fill = department_size))
            }
        }
        
        else if(input$y_var == "Job Satisfaction"){
            if(input$x_var == "Age"){
                summarized <- census_model_joined %>%
                    group_by(age_group) %>%
                    filter(year == 2017,
                           !is.na(satisfaction_level)) %>%
                    summarize(mean_satisfaction = mean(satisfaction_level)) %>%
                    ggplot(aes(age_group, mean_satisfaction)) +
                    geom_col(aes(fill = age_group))
            }
            else if(input$x_var == "Gender"){
                summarized <- census_model_joined %>%
                    group_by(gender) %>%
                    filter(year == 2017,
                           !is.na(satisfaction_level)) %>%
                    summarize(mean_satisfaction = mean(satisfaction_level)) %>%
                    ggplot(aes(gender, mean_satisfaction)) +
                    geom_col(aes(fill = gender))
            }
            else if(input$x_var == "Career Duration"){
                summarized <- census_model_joined %>%
                    group_by(career_duration) %>%
                    filter(year == 2017,
                           !is.na(satisfaction_level)) %>%
                    summarize(mean_satisfaction = mean(satisfaction_level)) %>%
                    ggplot(aes(career_duration, mean_satisfaction)) +
                    geom_col(aes(fill = career_duration))
            }
            else if(input$x_var == "Organization Size"){
                summarized <- census_model_joined %>%
                    group_by(org_size) %>%
                    filter(year == 2017,
                           !org_size == "",
                           !is.na(satisfaction_level)) %>%
                    summarize(mean_satisfaction = mean(satisfaction_level)) %>%
                    ggplot(aes(org_size, mean_satisfaction)) +
                    geom_col(aes(fill = org_size))
            }
            else if(input$x_var == "Department Size"){
                summarized <- census_model_joined %>%
                    group_by(department_size) %>%
                    filter(year == 2017,
                           !department_size == "",
                           !is.na(satisfaction_level)) %>%
                    summarize(mean_satisfaction = mean(satisfaction_level)) %>%
                    ggplot(aes(department_size, mean_satisfaction)) +
                    geom_col(aes(fill = department_size))
            }
        }
    })
    
    salary_satisfaction_react_2019 <- reactive({
        if(input$y_var == "Salary"){
            if(input$x_var == "Age"){
                summarized <- census_model_joined %>%
                    group_by(age_group) %>%
                    filter(year == 2019) %>%
                    summarize(mean_salary = mean(salary)) %>%
                    ggplot(aes(age_group, mean_salary)) +
                    geom_col(aes(fill = age_group))
            }
            else if(input$x_var == "Gender"){
                summarized <- census_model_joined %>%
                    group_by(gender) %>%
                    filter(year == 2019) %>%
                    summarize(mean_salary = mean(salary)) %>%
                    ggplot(aes(gender, mean_salary)) +
                    geom_col(aes(fill = gender))
            }
            else if(input$x_var == "Career Duration"){
                summarized <- census_model_joined %>%
                    group_by(career_duration) %>%
                    filter(year == 2019) %>%
                    summarize(mean_salary = mean(salary)) %>%
                    ggplot(aes(career_duration, mean_salary)) +
                    geom_col(aes(fill = career_duration))
            }
            else if(input$x_var == "Organization Size"){
                summarized <- census_model_joined %>%
                    group_by(org_size) %>%
                    filter(year == 2019,
                           !org_size == "") %>%
                    summarize(mean_salary = mean(salary)) %>%
                    ggplot(aes(org_size, mean_salary)) +
                    geom_col(aes(fill = org_size))
            }
            else if(input$x_var == "Department Size"){
                summarized <- census_model_joined %>%
                    group_by(department_size) %>%
                    filter(year == 2019,
                           !department_size == "") %>%
                    summarize(mean_salary = mean(salary)) %>%
                    ggplot(aes(department_size, mean_salary)) +
                    geom_col(aes(fill = department_size))
            }
        }
        
        else if(input$y_var == "Job Satisfaction"){
            if(input$x_var == "Age"){
                summarized <- census_model_joined %>%
                    group_by(age_group) %>%
                    filter(year == 2019,
                           !is.na(satisfaction_level)) %>%
                    summarize(mean_satisfaction = mean(satisfaction_level)) %>%
                    ggplot(aes(age_group, mean_satisfaction)) +
                    geom_col(aes(fill = age_group))
            }
            else if(input$x_var == "Gender"){
                summarized <- census_model_joined %>%
                    group_by(gender) %>%
                    filter(year == 2019,
                           !is.na(satisfaction_level)) %>%
                    summarize(mean_satisfaction = mean(satisfaction_level)) %>%
                    ggplot(aes(gender, mean_satisfaction)) +
                    geom_col(aes(fill = gender))
            }
            else if(input$x_var == "Career Duration"){
                summarized <- census_model_joined %>%
                    group_by(career_duration) %>%
                    filter(year == 2019,
                           !is.na(satisfaction_level)) %>%
                    summarize(mean_satisfaction = mean(satisfaction_level)) %>%
                    ggplot(aes(career_duration, mean_satisfaction)) +
                    geom_col(aes(fill = career_duration))
            }
            else if(input$x_var == "Organization Size"){
                summarized <- census_model_joined %>%
                    group_by(org_size) %>%
                    filter(year == 2019,
                           !org_size == "",
                           !is.na(satisfaction_level)) %>%
                    summarize(mean_satisfaction = mean(satisfaction_level)) %>%
                    ggplot(aes(org_size, mean_satisfaction)) +
                    geom_col(aes(fill = org_size))
            }
            else if(input$x_var == "Department Size"){
                summarized <- census_model_joined %>%
                    group_by(department_size) %>%
                    filter(year == 2019,
                           !department_size == "",
                           !is.na(satisfaction_level)) %>%
                    summarize(mean_satisfaction = mean(satisfaction_level)) %>%
                    ggplot(aes(department_size, mean_satisfaction)) +
                    geom_col(aes(fill = department_size))
            }
        }
    })
    
    output$salary_age_2017 <- renderPlot({
        bar_palette <- c('#6981e6', '#6f8ce6', '#7597e7', '#7aa2e7', '#7eade7', '#82b9e7', '#86c4e7', '#89cfe7', '#8cdbe6')
        
        salary_age <- salary_satisfaction_react_2017() +
            scale_fill_manual(values = bar_palette, name = input$x_var) +
            theme_classic() +
            labs(x = input$x_var,
                 y = "Mean Salary",
                 title = paste("Salary by", input$x_var),
                 subtitle = "Of Designers in the US in 2017")
        
        salary_age
    })
    
    output$salary_age_2019 <- renderPlot({
        bar_palette <- c('#ff6998', '#ff7499', '#ff7f99', '#ff889a', '#ff929a', '#ff9b9a', '#ffa49b', '#ffac9b', '#ffb49b')
        
        salary_age <- salary_satisfaction_react_2019() +
            scale_fill_manual(values = bar_palette, name = input$x_var) +
            theme_classic() +
            labs(x = input$x_var,
                 y = "Mean Salary",
                 title = paste("Salary by", input$x_var),
                 subtitle = "Of Designers in the US in 2019")
        
        salary_age
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
