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

census_model_joined <- read_rds("./census_model_joined.rds")
model_salary <- read_rds("./model_salary.rds")

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
    tabPanel("Predictor",
             sidebarLayout(
                 sidebarPanel(
                     numericInput("age",
                                  label = "How old are you?",
                                  value = 21,
                                  step = 1),
                     selectInput("gender",
                                 label = "Select your gender",
                                 choices = list("Male", "Female", "Other"),
                                 selected = "Female"),
                     selectInput("career_duration",
                                 label = "How long have you been a designer?",
                                 choices = list("Less than 1 year", "1-4 years", "5-9 years", "10-14 years", "15-20 years", "20+ years"),
                                 selected = "Less than 1 year"),
                     selectInput("org_size",
                                 label = "How large is the organization you currently work at?",
                                 choices = list("1-10 employees", "11-50 employees", "51-100 employees", "101-250 employees", "251-500 employees", "501-1000 employees", "1000+ employees"),
                                 selected = "1-10 employees"),
                     selectInput("department_size",
                                 label = "How large is your organization's design department?",
                                 choices = list("Just Me", "2-4 people", "5-10 people", "11-20 people", "20+ people"),
                                 selected = "Just Me")
                 ),
                 mainPanel(
                     h2("Your predicted salary"),
                     plotOutput("salary_plot")
                 )
             )),
    tabPanel("By Location",
             sidebarLayout(
                 sidebarPanel(),
                 mainPanel(
                     plotOutput("tab1")
                 )
             )
             ),
    tabPanel("About",
             mainPanel(
                 h2("About the Topic"),
                 
                 p("As an emerging product and UI/UX designer coming from an unconventional design
        background, I have recently been interested in taking a deeper dive into better
        understanding what the design field quantitatively looks like. I am particularly
        interested in learning more about what the demographic of current designers are,
        how its have changed over the last two years, and how it correlates with the
        general spread of companies and industries in the US."),
                 
                 p("This project aims to hopefully elucidate design career trends for anyone
        interested in entering the field, already in the field, or curious about design."),
                 
                 h2("About the Data"),
                 
                 p("The Design Census data was collected by The American Institute of Graphic Arts
        (AIGA) most recently in 2017 and 2019. The Design Census was a survey that was
        circulated in the design community and open to public for 5 weeks. The raw csv
        data files were downloaded from the respective Design Census websites, which can
        be found", a("here for the 2019 data", href = "https://designcensus.org/"), "and",
                 a("here for the 2017 data.", href = "http://designcensus2017.aiga.org/")),
                 
                 
                 p("The data set on US companies was from the Open Data 500 Project, which is the
        first comprehensive study of US companies that use open government data
        conducted by the GovLab of New York University. The raw csv data files were
        downloaded from the Open Data 500 website, which can be found", 
                   a("here.", href ="https://www.opendata500.com/us/")),
                 
                 p("In order to make sense of the zip codes from the aforementioned data sets, a US
        zip codes data set from SimpleMaps was used to configure zip codes to states and
        cities. The zip codes dataset can be found", a("here.", href = "https://simplemaps.com/data/us-zips")),
                 
                 h2("About me"),
                 
                 p("I am a senior concentrating in Neuroscience with a secondary in Mind, Brain, and Behavior.
                 When I'm not studying science, I like to design things!"),
                 p("Check out my design", a("portfolio", href = "http://www.stephcheng.com"), "and the code for this project can be found on my",
                   a("Github!", href = "https://github.com/swcg1/gov1005-final_project"))
             ))
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
                 y = paste("Mean", input$y_var),
                 title = paste(input$y_var, "by", input$x_var),
                 subtitle = "Of Designers in the US in 2017")
        
        salary_age
    })
    
    output$salary_age_2019 <- renderPlot({
        bar_palette <- c('#ff6998', '#ff7499', '#ff7f99', '#ff889a', '#ff929a', '#ff9b9a', '#ffa49b', '#ffac9b', '#ffb49b')
        
        salary_age <- salary_satisfaction_react_2019() +
            scale_fill_manual(values = bar_palette, name = input$x_var) +
            theme_classic() +
            labs(x = input$x_var,
                 y = paste("Mean", input$y_var),
                 title = paste(input$y_var, "by", input$x_var),
                 subtitle = "Of Designers in the US in 2019")
        
        salary_age
    })
    
    predictor <- reactive({
        predict(model_salary,
                tibble(age = input$age,
                       gender = input$gender,
                       career_duration = input$career_duration,
                       org_size = input$org_size,
                       department_size = input$department_size),
                interval = "confidence")

    })
    
    output$salary_plot <- renderPlot({
        prediction <- predictor()
        
        ggplot() +
            scale_x_continuous(name = "x") +
            scale_y_continuous(name = "y") +
            geom_rect(aes(xmin = prediction[2],
                          xmax = prediction[3], 
                          ymin = 0, 
                          ymax = 0.5),
                      fill = "#8cc8db", alpha = 0.5) +
            geom_segment(aes(x = prediction[1], y = 0, xend = prediction[1], yend = 0.5), 
                         color = "#8cc8db", size = 2) +
            ylim(-0.5, 0.7) +
            theme_void() +
            annotate("text", 
                     x = prediction[2] + 500,
                     y = -0.1,
                     label = paste("Lower estimation: \n $", round(prediction[2]), sep = ""),
                     size = 6,
                     color = "#93a5ab",
                     fontface = 2) +
            annotate("text", 
                     x = prediction[3] - 500,
                     y = -0.1,
                     label = paste("Upper estimation: \n $", round(prediction[3]), sep = ""),
                     size = 6,
                     color = "#93a5ab",
                     fontface = 2) +
            annotate("text", 
                     x = prediction[1],
                     y = 0.65,
                     label = paste("Estimated salary: \n $", round(prediction[1]), sep = ""),
                     size = 7,
                     color = "#37758a",
                     fontface = 2)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
