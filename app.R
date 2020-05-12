### Shiny app for teaching ggplot2

library(shiny)
library(shinythemes)

library(tidyverse)
library(DT)


ui <- navbarPage("gglearn2", windowTitle = NULL, theme = "lumen",
                 # Tab 1: Dataframe and summary statistics
                 
                 tabPanel(title = "Introduction", icon = icon("stats", lib = "glyphicon"),
                          fluidPage(
                            fluidRow(
                              column(width = 6,
                                     wellPanel(
                                       "This is help text")
                                     ),
                              column(width = 3,
                                     wellPanel(
                                       "Number of rows: ")
                              ),
                              column(width = 3,
                                     wellPanel(
                                       "Number of columns :")
                              )
                            ),
                            fluidRow(
                              column(width = 3, offset = 6,
                                     wellPanel(
                                       "More stats: ")
                              ),
                              column(width = 3,
                                     wellPanel(
                                       "Even more: ")
                              ),
                            ),
                            hr(),
                            fluidRow(
                              datatable(iris)
                            ),
                            textAreaInput("test", "Code goes here", value = "CODEEE")
                            
                          )
                 ),
                 tabPanel(title = "1 Variable", icon = icon("chart-area"),
                          # Exploring 1 variables
                          fluidPage(
                            fluidRow(
                              column(width = 6,
                                     wellPanel(
                                       "This is help text")
                                     ),
                              column(width = 3,
                                     varSelectInput("x_1", "Variable", iris)
                                     ),
                              column(width = 3,
                                     varSelectInput("col_1", "Color", iris)
                                     )
                            )
                          )
                          
                 ),
                 tabPanel(title = "2 Variables", icon = icon("chart-line")
                 ),
                 tabPanel(title = "Making it pretty!", icon = icon("child")
                 ),
                 navbarMenu(title = "About",
                            tabPanel("The app"),
                            tabPanel("The authors"))
                 
                 
)



# textAreaInput

server <- function(input, output) { 
  
  }

shinyApp(ui, server)

