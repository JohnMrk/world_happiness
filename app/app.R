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
source("app/app_setup.R")

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
                       "GDP Per Capita" = master$x2018,
                       "Human Freedom Index" = master$hf_score,
                       "Gun Violence" = master$total_deaths,
                       "Wealth Inequality(Gini Index)"  = master$gini_index)
        
        # Draw the histogram with the specified number of bins
        
       ggplot(master, aes(x = data, y = happiness_score)) +geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
