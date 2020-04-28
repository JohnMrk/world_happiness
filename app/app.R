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
library(readxl)
library(janitor)
library(readr)
library(ggplot2)
source("app_setup.R")

# User interface ----


# Define UI for application that draws a histogram
ui <- navbarPage(
    "Worldwide Happiness",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "var",
                             "Choose variable to explore", 
                             choices = c("GDP Per Capita" , "Human Freedom Index" , "Gun Violence", "Wealth Inequality(Gini Index)" )
                         ),
                         br(),
                        selectInput(
                            "wrap",
                            "Facet Wrap by Region?",
                            choices = c("Yes", "No")
                        )),
                mainPanel(plotOutput("line_plot")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")))
  
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        data <- switch(input$var, 
                       "GDP Per Capita" = master$gdpPC,
                       "Human Freedom Index" = master$hf_score,
                       "Gun Violence" = master$log_td,
                       "Wealth Inequality(Gini Index)"  = master$gini_index)
        wrap <- switch(input$wrap, 
                           "Yes" = "region",
                           "No" = "country_name")
        # Draw the histogram with the specified number of bins
        
       ggplot(master, aes(x = data, y = happiness_score, color = region)) +geom_point() + facet_wrap(wrap)+
           labs(x = input$var, y = "Happiness Score from 0-10")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

