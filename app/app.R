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
library(maps)
library(mapproj)
source("helpers.R")
counties <- readRDS("data/counties.rds")

# User interface ----
'ui <- fluidPage(
    titlePanel("censusVis"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Create demographic maps with 
        information from the 2010 US Census."),
            
            selectInput("var", 
                        label = "Choose a variable to display",
                        choices = c("Percent White", "Percent Black",
                                    "Percent Hispanic", "Percent Asian"),
                        selected = "Percent White"),
            
            sliderInput("range", 
                        label = "Range of interest:",
                        min = 0, max = 100, value = c(0, 100))
        ),
        
        mainPanel(plotOutput("map"))
    )
)'

'server <- function(input, output) {
    output$map <- renderPlot({
        data <- switch(input$var, 
                       "Percent White" = counties$white,
                       "Percent Black" = counties$black,
                       "Percent Hispanic" = counties$hispanic,
                       "Percent Asian" = counties$asian)
        
        color <- switch(input$var, 
                        "Percent White" = "darkgreen",
                        "Percent Black" = "black",
                        "Percent Hispanic" = "darkorange",
                        "Percent Asian" = "darkviolet")
        
        legend <- switch(input$var, 
                         "Percent White" = "% White",
                         "Percent Black" = "% Black",
                         "Percent Hispanic" = "% Hispanic",
                         "Percent Asian" = "% Asian")
        
        percent_map(data, color, legend, input$range[1], input$range[2])
    })
}'




# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Title",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
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
        
        ifelse(
            input$plot_type == "a",
            
            # If input$plot_type is "a", plot histogram of "waiting" column 
            # from the faithful dataframe
            
            x   <- faithful[, 2],
            
            # If input$plot_type is "b", plot histogram of "eruptions" column
            # from the faithful dataframe
            
            x   <- faithful[, 1]
        )
        
        # Draw the histogram with the specified number of bins
        
        hist(x, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
