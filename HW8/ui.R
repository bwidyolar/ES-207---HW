#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Triangle Floodplain"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       helpText("Visualize different measurements on different dates."),
    
       selectInput("var.date",
                   label = "Choose a date to display", 
                   choices=c("Feb 16, 2005","Feb 17, 2005","Feb 18, 2005","Feb 23, 2005","Feb 28, 2005"),
                   selected="Feb 16, 2005"),
       selectInput("var.var",
                   label = "Choose a variable to display",
                   choices = c("Temperature", "Chlorophyl","Dissolved Oxygen","TDS","Turbidity"),
                   selected = "Temperature")
    ),
    
    mainPanel(plotOutput("map"))
  )
))
