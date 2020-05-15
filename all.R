### Shiny app for teaching ggplot2

library(shiny)
library(shinythemes)

library(tidyverse)
library(DT)
library(skimr)
library(listviewer)
library(knitr)
library(kableExtra)

##### STANDARDIZE OG SCALE

#### FORKLAR GATHER

#### i facet behold kun factor/fjern numeric
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
                                       tabsetPanel(type='pills', id = "tabset_1",
                                                   tabPanel(title = strong("Code"), 
                                                            value = "code",
                                                            
                                                            verbatimTextOutput("code_1_var"),
                                                            tags$head(tags$style("#code_1_var{color: #a2000d; height: 300px;}"
                                                            )
                                                            ),
                                                            actionButton("insert_code_1", "Insert code in script")
                                                   ),
                                                   tabPanel(title = strong("Playground"),
                                                            value = "playground",
                                                            textAreaInput("playground_1", label = NULL, height = "300px"),
                                                            actionButton("update_plot_1", "Update plot"))
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
                                       tabsetPanel(type='pills', id = "tabset_2",
                                                   tabPanel(title = strong("Code"), 
                                                            value = "code",
                                                            
                                                            verbatimTextOutput("code_2_var"),
                                                            tags$head(tags$style("#code_2_var{color: #a2000d; height: 300px;}"
                                                            )
                                                            )
                                                   ),
                                                   tabPanel(title = strong("Playground"),
                                                            value = "playground",
                                                            textAreaInput("playground_2", label = NULL, height = "300px"),
                                                            actionButton("update_plot_2", "Update plot"))
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
                                                     "2 variables" = "2_var_plot"))
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
                                       tabsetPanel(type='pills', id = "tabset_3",
                                                   tabPanel(title = strong("Code"), 
                                                            value = "code",
                                                            
                                                            verbatimTextOutput("code_3_var"),
                                                            tags$head(tags$style("#code_3_var{color: #a2000d; height: 300px;}"
                                                            )
                                                            )
                                                   ),
                                                   tabPanel(title = strong("Playground"),
                                                            value = "playground",
                                                            textAreaInput("playground_3", label = NULL, height = "300px"),
                                                            actionButton("update_plot_3", "Update plot"))
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
                                       selectizeInput("p_col", "Geom color", c("NULL", colors()), multiple = F)
                                ),
                                column(3,
                                       sliderInput("p_alpha", "Geom opacity", 0, 1, 1, 0.2)
                                ),
                                br(),
                                column(3, offset = 6,
                                       selectInput("p_shape", "Geom shape", shape_opts())
                                ),
                                column(3,
                                       selectInput("p_legend", "Legend position", 
                                                   c("Right" = "right",
                                                     "Top" = "top",
                                                     "Bottom" = "bottom",
                                                     "Remove" = "none")
                                       )
                                )
                              )
                            ),
                            fluidRow(
                              column(width = 8,
                                     plotOutput("plot_4_var")
                              ),
                              column(width = 4,
                                     tabsetPanel(type='pills', id = "tabset_4",
                                                 tabPanel(title = strong("Code"), 
                                                          value = "code",
                                                          
                                                          verbatimTextOutput("code_4_var"),
                                                          tags$head(tags$style("#code_4_var{color: #a2000d; height: 300px;}"
                                                          )
                                                          )
                                                 ),
                                                 tabPanel(title = strong("Playground"),
                                                          value = "playground",
                                                          textAreaInput("playground_4", label = NULL, height = "300px"),
                                                          actionButton("update_plot_4", "Update plot"))
                                     )
                              )
                            )
                   ),
                   navbarMenu(title = "About", icon = icon("info"),
                              tabPanel("The app"),
                              tabPanel("The authors"))
                   
                   
  )
  
  
  
  # textAreaInput
  
  server <- function(input, output, session) { 
    source("ggplot_string.R")
    
    ######################### Setting reactive values
    vars <- c("x_1", "graph_1", "trans_1", "overlay_norm",
              "x_2", "y_2", "geom_2_1", "geom_2_2",
              "group_plot", "group_color", "group_fill", "group_facet",
              "p_col", "p_alpha", "p_shape", "p_legend",
              "p_x_lab", "p_y_lab", "p_title", "p_theme")
    
    playground_vals <- c("show_playground_1", "show_playground_2", "show_playground_3", "show_playground_4")
    
    values <- reactiveValues()
    
    for(play in playground_vals){
      values[[play]] <- FALSE
    }
    
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
    
    
    # Update code based on inputs
    observe({
      values$str_plot_1 <- {
        init_layer <- create_init(x = values$x_1)
        geoms <- create_geom(as.character(values$graph_1))
        final_str <- combine_string(init_layer = init_layer, geoms = geoms)}
    })
    
    # Check which tab is selected
    observeEvent(input$tabset_1, {
      updateTextAreaInput(session, "playground_1", value = values$str_plot_1)
      values$show_playground_1 <- FALSE
    })
    
    # Update plot if button is pressed
    observeEvent(input$update_plot_1, {
      values$show_playground_1 <- TRUE
      values$playground_1 <- input$playground_1
    })
    
    output$code_1_var <- renderText({
      values$str_plot_1
    })
    
    output$plot_1_var <- renderPlot({
      req(input$x_1)
      if(isTRUE(values$show_playground_1)){
        return(eval(parse(text = values$playground_1)))
      } else {
        return(eval(parse(text = values$str_plot_1)))
      }
    })
    
    ## Insert code
    insert_code <- function(button, code){
      observeEvent(input[[button]], {
        context <- rstudioapi::getSourceEditorContext()
        rstudioapi::insertText(text = paste0("\n", values[[code]], "\n"), id = context$id)
      })
    }
    insert_code("insert_code_1", "str_plot_1")

    
    
    
    
    ########################## OUTPUT TAB 2: 2 VARIABLES
    observe({
      values$str_plot_2 <- {
        init_layer <- create_init(x = values$x_2, y = values$y_2)
        geoms <- create_geom(c(as.character(values$geom_2_1), as.character(values$geom_2_2)))
        final_str <- combine_string(init_layer = init_layer, geoms = geoms)}
    })
    
    observeEvent(input$tabset_2, {
      updateTextAreaInput(session, "playground_2", value = values$str_plot_2)
      values$show_playground_2 <- FALSE
    })
    
    observeEvent(input$update_plot_2, {
      values$show_playground_2 <- TRUE
      values$playground_2 <- input$playground_2
    })
    
    output$code_2_var <- renderText({
      values$str_plot_2
    })
    
    
    output$plot_2_var <- renderPlot({
      req(input$x_2)
      if(isTRUE(values$show_playground_2)){
        return(eval(parse(text = values$playground_2)))
      } else {
        return(eval(parse(text = values$str_plot_2)))
      }
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
    
    observeEvent(input$tabset_3, {
      updateTextAreaInput(session, "playground_3", value = values$str_plot_3)
      values$show_playground_3 <- FALSE
    })
    
    observeEvent(input$update_plot_3, {
      values$show_playground_3 <- TRUE
      values$playground_3 <- input$playground_3
    })
    
    output$plot_3_var <- renderPlot({
      req(input$group_plot)
      if(isTRUE(values$show_playground_3)){
        return(eval(parse(text = values$playground_3)))
      } else {
        return(eval(parse(text = values$str_plot_3)))
      }
    })
    ############################# OUTPUT TAB 4:  MAKING IT PRETTY
    observe({
      values$str_plot_4 <- {
        if(values$group_plot == "1_var_plot"){
          init_layer <- create_init(x = values$x_1, color = values$group_color, fill = values$group_fill)
          geoms <- create_geom(as.character(values$graph_1), color = values$p_col, shape = values$p_shape, alpha = values$p_alpha)
        }
        if(values$group_plot == "2_var_plot"){
          init_layer <- create_init(x = values$x_2, y = values$y_2, color = values$group_color, fill = values$group_fill)
          geoms <- create_geom(c(as.character(values$geom_2_1), as.character(values$geom_2_2)), color = c(values$p_col, "NULL"), shape = c(values$p_shape, "NULL"), alpha = c(values$p_alpha, "NULL"))
        }
        labs <- create_labs(title = values$p_title, x = values$p_x_lab, y = values$p_y_lab)
        plot_theme <- create_std_theme(values$p_theme)
        legend <- create_custom_theme(values$p_legend)
        
        facet <- create_facet(as.character(values$group_facet))
        final_str <- combine_string(init_layer = init_layer, geoms = geoms, facet = facet, labs = labs, theme_std = plot_theme, theme_custom = legend)}
    })
    
    output$code_4_var <- renderText({
      values$str_plot_4
    })
    
    observeEvent(input$tabset_4, {
      updateTextAreaInput(session, "playground_4", value = values$str_plot_4)
      values$show_playground_4 <- FALSE
    })
    
    observeEvent(input$update_plot_4, {
      values$show_playground_4 <- TRUE
      values$playground_4 <- input$playground_4
    })
    
    output$plot_4_var <- renderPlot({
      req(input$p_col)
      if(isTRUE(values$show_playground_4)){
        return(eval(parse(text = values$playground_4)))
      } else {
        return(eval(parse(text = values$str_plot_4)))
      }
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


observeEvent(input$insert_code, {
  context <- rstudioapi::getSourceEditorContext()
  code <- ggplot_rv$code
  code <- stri_replace_all(str = code, replacement = "+\n", fixed = "+")
  if (!is.null(output_filter$code$expr)) {
    code_dplyr <- deparse(output_filter$code$dplyr, width.cutoff = 80L)
    code_dplyr <- paste(code_dplyr, collapse = "\n")
    nm_dat <- data_name()
    code_dplyr <- stri_replace_all(str = code_dplyr, replacement = "%>%\n", fixed = "%>%")
    code <- stri_replace_all(str = code, replacement = " ggplot()", fixed = sprintf("ggplot(%s)", nm_dat))
    code <- paste(code_dplyr, code, sep = " %>%\n")
    if (input$insert_code == 1) {
      code <- paste("library(dplyr)\nlibrary(ggplot2)", code, sep = "\n\n")
    }
  } else {
    if (input$insert_code == 1) {
      code <- paste("library(ggplot2)", code, sep = "\n\n")
    }
  }
  rstudioapi::insertText(text = paste0("\n", code, "\n"), id = context$id)
})


