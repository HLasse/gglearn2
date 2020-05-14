### Shiny app for teaching ggplot2

library(shiny)
library(shinythemes)

library(tidyverse)
library(DT)
library(skimr)
library(listviewer)
library(knitr)
library(kableExtra)
#, options = list(columns = list(width = "100%"))



gglearn <- function(dataset){
  
  source("helpers.R")

  # Get named list of columns in dataset
  columns <- setNames(as.list(names(dataset)), names(dataset))

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
                                column(5,
                                       h3("Summary of factor variables"),
                                       DTOutput("skim_factor")
                                ),
                                column(5, offset = 1,
                                       h3("Summary of numeric variables"),
                                       DTOutput("skim_numeric")
                                )
                              ),
                              fluidRow(
                                column(12,
                                       h3("First 10 rows of your data"),
                                       DTOutput("dataframe") )
                                
                              ),
                              textAreaInput("code_1", "Code goes here", value = "CODEEE")
                              
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
                                       selectInput("graph_1", "Graph Type",
                                                   c("Density" = "dens",
                                                     "Histogram" = "hist",
                                                     "QQ Plot" = "qq"))
                                ),
                                column(width = 3,
                                       varSelectInput("x_1", "Variable", dataset)
                                )
                              ),
                              fluidRow(
                                column(width = 3, offset = 6,
                                       selectInput("trans_1", "Transformation",
                                                   c("None" = "none",
                                                     "Natural Logarithm" = "ln",
                                                     "Inverse" = "inverse",
                                                     "Others?" = "..."))
                                ),
                                column(width = 3,
                                       selectInput("overlay_norm", "Overlay Normal Dist.",
                                                   c("No" = "none",
                                                     "Yes" = "yes")))
                              ),
                              fluidRow(
                                column(width = 8,
                                       plotOutput("plot_1_var")
                                       ),
                                column(width = 4,
                                      
                                         verbatimTextOutput("code_1_var"),
                                        tags$head(tags$style("#code_1_var{color: #a2000d;
                                 }"
                                       )
                                       )
                                       
                                )
                              )
                            )

                   ),
                   tabPanel(title = "2 Variables", icon = icon("chart-line"),
                            fluidPage(
                              fluidRow(
                                column(6,
                                       wellPanel(
                                         "This is help text"
                                       )
                                ),
                                column(3,
                                       varSelectInput("x_2", "X", dataset)
                                ),
                                column(3,
                                       varSelectInput("y_2", "Y", dataset)
                                ),
                                br(),
                                column(3, offset = 6,
                                       selectInput("geom_2_1", "Plot Type",
                                                   c("Scatter" = "scatter",
                                                     "Line" = "line",
                                                     "Violin" = "violin",
                                                     "Boxplot" = "box",
                                                     "Bar" = "bar",
                                                     "Smooth" = "smooth")
                                       )
                                ),
                                column(3,
                                       selectInput("geom_2_2", "Plot Type 2",
                                                   c("None" = "NULL",
                                                     "Scatter" = "scatter",
                                                     "Line" = "line",
                                                     "Violin" = "violin",
                                                     "Boxplot" = "box",
                                                     "Bar" = "bar",
                                                     "Smooth" = "smooth")
                                       )

                                )
                              ),
                              fluidRow(
                                column(width = 8,
                                       plotOutput("plot_2_var")
                                ),
                                column(width = 4,
                                       verbatimTextOutput("code_2_var"),
                                       tags$head(tags$style("#code_2_var{color: #a2000d;
                                 }"
                                       )
                                       )
                                )
                              )
                            )
                   ),
                   tabPanel(title = "Grouping", icon = icon("group"),
                            fluidPage(
                              fluidRow(
                                column(6,
                                       wellPanel(
                                         "This is help text"
                                       )
                                ),
                                column(3,
                                       selectInput("group_plot", "Choose plot",
                                                   c("1 variable" = "1_var_plot",
                                                     "2 varaibles" = "2_var_plot"))
                                ),
                                column(3,
                                       selectInput("group_color", "Color", c("None" = "NULL", columns))
                                ),
                                br(),
                                column(3, offset = 6,
                                       selectInput("group_fill", "Fill", c("None" = "NULL", columns))
                                ),
                                column(3,
                                       selectInput("group_facet", "Facet", c("None" = "NULL", columns))

                                )
                              ),
                              fluidRow(
                                column(width = 8,
                                       plotOutput("plot_3_var")
                                ),
                                column(width = 4,
                                       verbatimTextOutput("code_3_var"),
                                       tags$head(tags$style("#code_3_var{color: #a2000d;
                                 }"
                                       )
                                       )
                                )
                              )
                            )
                   ),
                   tabPanel(title = "Making it pretty!", icon = icon("child"),
                            fluidPage(
                              fluidRow(
                                column(6,
                                       wellPanel(
                                         "This is help text"
                                       )
                                ),
                                column(3,
                                       textInput("p_col", "Geom color", "steelblue")
                                ),
                                column(3,
                                       sliderInput("p_alpha", "Geom opacity", 0, 1, 0.5, 0.2)
                                ),
                                br(),
                                column(3, offset = 6,
                                       textInput("p_shape", "Geom shape", "Your Title")
                                ),
                                column(3,
                                       selectInput("p_legend", "Legend position", 
                                                   c("Top" = "top",
                                                     "Bottom" = "bottom",
                                                     "Right" = "right",
                                                     "Remove" = "none")
                                                   )
                                ),
                                br(),
                                column(3, offset = 6,
                                       textInput("p_x_lab", "X label", "X-variable")
                                ),
                                column(3,
                                       textInput("p_y_lab", "Y label", "Y-variable")
                                       
                                ),
                                br(),
                                column(3, offset = 6,
                                       textInput("p_title", "Title", "Your Title")
                                ),
                                column(3,
                                       selectInput("p_theme", "Theme", 
                                                   c("Default (gray)" = "gray",
                                                     "light" = "light",
                                                     "bw" = "bw",
                                                     "minimal" = "minimal",
                                                     "classic" = "classic",
                                                     "void" = "void",
                                                     "dark" = "dark")
                                                   )
                                )
                              ),
                              fluidRow(
                                column(width = 8,
                                       plotOutput("plot_4_var")
                                ),
                                column(width = 4,
                                       verbatimTextOutput("code_4_var"),
                                       tags$head(tags$style("#code_4_var{color: #a2000d;
                                 }"
                                       )
                                       )
                                )
                              )
                            )
                   ),
                   navbarMenu(title = "About", icon = icon("info"),
                              tabPanel("The app"),
                              tabPanel("The authors"))
                   
                   
  )
  
  
  
  # textAreaInput
  
  server <- function(input, output) { 
    source("ggplot_string.R")
    
    ######################### Setting reactive values
    vars <- c("x_1", "graph_1", "trans_1", "overlay_norm",
              "x_2", "y_2", "geom_2_1", "geom_2_2",
              "group_plot", "group_color", "group_fill", "group_facet",
              "p_col", "p_alpha", "p_shape", "p_legend",
              "p_x_lab", "p_y_lab", "p_title", "p_theme")
    values <- reactiveValues()
    for (var in vars){
      values[[var]] <- NULL
    }
    
    obs_up <- function(variable){
      observeEvent(input[[variable]], {
        values[[variable]] <- input[[variable]]
      })
    }
    lapply(vars, obs_up)
  
    ######################### OUTPUTS FOR TAB 1 - SUMMARY STATS
    output$skim_factor <- renderDT({
      dataset %>%
        select(where(is.factor)) %>%
        skim() %>%
        as_tibble() %>%
        select(skim_variable, n_missing, factor.n_unique, factor.top_counts) %>% 
        rename_at(vars(starts_with("factor")),
                  funs(str_replace(., "factor.", ""))) %>% 
        rename(variable = skim_variable) %>% 
        rename_all(funs(str_replace(., "_", " "))) %>% 
        rename_with(capitalize)
    }, 
    options = dt_options(), 
    rownames = F
    )
    
    output$skim_numeric <- renderDT({
      dataset %>%
        select(where(is.numeric)) %>%
        skim() %>%
        as_tibble() %>% 
        select(skim_variable, n_missing, numeric.mean, numeric.sd, numeric.hist) %>% 
        rename_at(vars(starts_with("numeric")),
                  funs(str_replace(., "numeric.", ""))) %>% 
        mutate_if(is.numeric, funs(round(., 2))) %>% 
        rename(Variable = skim_variable,
               Histogram = hist) %>% 
        rename_all(funs(str_replace(., "_", " "))) %>%
        rename_with(capitalize)
    }, options = dt_options(), rownames = F
    )
    
    output$dataframe <- renderDT({
      head(dataset, 10)
    },
    options = dt_options("t)")
    )
    
    ################################## OUTPUT FOR TAB 2: 1 VARIABLE
    
    observe({
      values$str_plot_1 <- {
        init_layer <- create_init(x = values$x_1)
        geoms <- create_geom(as.character(values$graph_1))
        final_str <- combine_string(init_layer = init_layer, geoms = geoms)}
    })

    output$code_1_var <- renderText({
      values$str_plot_1
    })
    
    output$plot_1_var <- renderPlot({
      eval(parse(text = values$str_plot_1))
    })
    ########################## OUTPUT TAB 2: 2 VARIABLES
    observe({
      values$str_plot_2 <- {
        init_layer <- create_init(x = values$x_2, y = values$y_2)
        geoms <- create_geom(c(as.character(values$geom_2_1), as.character(values$geom_2_2)))
        final_str <- combine_string(init_layer = init_layer, geoms = geoms)}
    })
    
    
    output$code_2_var <- renderText({
      values$str_plot_2
    })
    
    output$plot_2_var <- renderPlot({
      eval(parse(text = values$str_plot_2))
    })
    
    ######################33 OUTPUT TAB 3: GROUPINGS
    observe({
      values$str_plot_3 <- {
        if(values$group_plot == "1_var_plot"){
          init_layer <- create_init(x = values$x_1, color = values$group_color, fill = values$group_fill)
          geoms <- create_geom(as.character(values$graph_1))
        }
        if(values$group_plot == "2_var_plot"){
          init_layer <- create_init(x = values$x_2, y = values$y_2, color = values$group_color, fill = values$group_fill)
          geoms <- create_geom(c(as.character(values$geom_2_1), as.character(values$geom_2_2)))
        }
        facet <- create_facet(as.character(values$group_facet))
        final_str <- combine_string(init_layer = init_layer, geoms = geoms, facet = facet)}
    })
    output$code_3_var <- renderText({
      values$str_plot_3
    })
    
    output$plot_3_var <- renderPlot({
      eval(parse(text = values$str_plot_3))
    })
    
    output$code_4_var <- renderUI({
      code("This is more code")
    })
    
    output$plot_4_var <- renderPlot({
      ggplot(dataset, aes(Petal.Length, Petal.Width, color = Species)) + 
        geom_point() + 
        labs(title = "A title") +
        theme(legend.position = "top")
    })
    
    
  }
  
  runApp(shinyApp(ui, server))
}


gglearn(dataset = iris)

launch_app <- function(datas, ...) {
 # file_path <- system.file("myapp.R", package = "mypackage")
  if (!nzchar("app.R")) stop("Shiny app not found")
  ui <- server <- NULL # avoid NOTE about undefined globals
  source("app.R", local = TRUE)
  server_env <- environment(server)

  # Here you add any variables that your server can find
  server_env$dataset <- datas

  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app, ...)
}

launch_app(datas = dataset)

# 
# launch_app(datas = iris)
# 
# launch_2 <- function(datas) {
#   ui <- server <- NULL
#   source("app.R", local = TRUE)
#   dataset = datas
#   shinyApp(ui, server)
# }
# launch_2(iris)
