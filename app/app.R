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
library(rsconnect)
source("app_setup.R")

# User interface ----


# Define UI for application that draws a histogram
ui <- navbarPage(
    "Worldwide Happiness",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Worldwide Happiness Model"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "var",
                             "Choose variable to explore", 
                             choices = c("GDP Per Capita(log10)" , "Human Freedom Index" , "Gun Violence(log10)", 
                                         "Wealth Inequality(Gini Index)", "Life Expectancy", "Fertility Rate" )
                         ),
                         br(),
                        selectInput(
                            "wrap",
                            "Facet Wrap by Region?",
                            choices = c("Yes", "No")
                        ),
                        p("Explore the various factors that affect(or don't affect) happiness in various countries around the world.")),
                mainPanel(plotOutput("line_plot")))
             )),
    tabPanel("Quantifying Happiness",
             titlePanel("Data on Happiness"),
             h3("How is Happiness Scored?"),
             p("Happiness is something that we think about a lot. Am I happy? Are you? In this project I hoped to answer questions that
               we may have about happiness around the world. To answer this question I had to figure out just how happy the world is. To 
               do this I collected data from the World Happiness Report that surveyed people around the world on how happy they were with
               their lives. The question posed to each country was: 'If you could rate your life on a 1-10 scale, 1 being the worst life you could
               possibly have, and 10 being the best life you could possibly have. What would you rate your life?' This question was asked to
               over 150 countries. Of those countries I searched for data that could explain why some countries are happier than others"),
             h3("What Correlates with Happiness?"),
             p("After exploring dozens of variables that could explain happiness in various countries, I came to many interesting and 
               sometimes surprising conclusions. I found GDP per capita to be the best explanatory variable for variations in happiness among countries.
               I also was unable to find a single statistic that positively correlated with happiness that didn't also correlate with GDP per capita. 
               This begs the question: does being richer cause all other positive variables to increase or do those characteristics make a country wealthier?"),
             h3("Does Money Buy Happiness?"),
             p("Yes, maybe.")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("This project started because I hoped to be able to answer a big question with my project. I figured there could be no greater question 
               than what makes humans around the world happy. As I looked through data about the countries og this world I hoped to find a silver bullet:
               something clear that would make happiness make more sense. I hoped to find, if I could, one thing that above all else, made humans happy.
               However, our world is not that simple. What made people more satisfied with their life often came in packages. Freedom came with prosperity.
               Health came with wealth. When our expanisve world is broken into its respective parts, variations in happiness start to decline. This would 
               suggest that the biggest reason some places are happier than others, even with the same material resources, may come from cultural differences. 
               These would require further examination that I cannot yet provide as an amateur data scientist.."),
             h3("About Me"),
             p("My name is John Mark Ozaeta and I study Economics. 
             You can reach me at jozaeta@college.harvard.edu.")))
  
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        data <- switch(input$var, 
                       "GDP Per Capita(log10)" = master$gdpPC,
                       "Human Freedom Index" = master$hf_score,
                       "Gun Violence(log10)" = master$log_td,
                       "Wealth Inequality(Gini Index)"  = master$gini_index,
                       "Life Expectancy" = master$life_expectancy,
                       "Fertility Rate" = master$fertility_rate)
        wrap <- switch(input$wrap, 
                           "Yes" = ggplot(master, aes(x = data, y = happiness_score, color = region)) 
                       + geom_point() + facet_wrap("region"),
                           "No" = ggplot(master, aes(x = data, y = happiness_score, color = region))
                       + geom_point())
        # Draw the histogram with the specified number of bins
        
       wrap +
           labs(x = input$var, y = "Happiness Score from 0-10", title = paste("Effect of", input$var, "on Happiness Around the World", sep = " ")) + theme_classic()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

