### Shiny app for teaching ggplot2

library(shiny)
library(shinythemes)

library(tidyverse)
library(DT)
library(skimr)
library(listviewer)
library(knitr)
library(kableExtra)
library(shinyAce)
##### STANDARDIZE OG SCALE

#### FORKLAR GATHER

## check shinyWidgets::panel, dropdown, spectrumInput
## shinyFeedback
## shinyAce (tjek https://github.com/cardiomoon/ggplotAssist/blob/master/R/textFunction.R hvis issues)

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
                                       aceEditor("code_1_ace", "", wordWrap = T, theme = tolower(rstudioapi::getThemeInfo()$editor), mode = "r"),
                                       actionButton("insert_code_1", "Insert code in script")
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
                                       aceEditor("code_2_ace", "", wordWrap = T, theme = tolower(rstudioapi::getThemeInfo()$editor), mode = "r"),
                                       actionButton("insert_code_2", "Insert code in script")
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
                                       aceEditor("code_3_ace", "", wordWrap = T, theme = tolower(rstudioapi::getThemeInfo()$editor), mode = "r"),
                                       actionButton("insert_code_3", "Insert code in script")
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
                                     aceEditor("code_4_ace", "", wordWrap = T, theme = tolower(rstudioapi::getThemeInfo()$editor), mode = "r"),
                                     actionButton("insert_code_4", "Insert code in script")
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
    
    # Update code block based on selectInput
    observe({
      updateAceEditor(session, "code_1_ace", values$str_plot_1)
    })
    
    # Update plot code after input
    observeEvent(input$code_1_ace, {
      values$code_1_ace <- input$code_1_ace
    })
    
    output$plot_1_var <- renderPlot({
      return(eval(parse(text = values$code_1_ace)))
    })
    
    ## Insert code
    insert_code <- function(button, code){
      observeEvent(input[[button]], {
        context <- rstudioapi::getSourceEditorContext()
        rstudioapi::insertText(text = paste0("\n", values[[code]], "\n"), id = context$id)
      })
    }
    insert_code("insert_code_1", "code_1_ace")

    
    
    
    
    ########################## OUTPUT TAB 2: 2 VARIABLES
    observe({
      values$str_plot_2 <- {
        init_layer <- create_init(x = values$x_2, y = values$y_2)
        geoms <- create_geom(c(as.character(values$geom_2_1), as.character(values$geom_2_2)))
        final_str <- combine_string(init_layer = init_layer, geoms = geoms)}
    })
    
    # Update code block based on selectInput
    observe({
      updateAceEditor(session, "code_2_ace", values$str_plot_2)
    })
    
    # Update plot code after input
    observeEvent(input$code_2_ace, {
      values$code_2_ace <- input$code_2_ace
    })
    
    output$plot_2_var <- renderPlot({
      return(eval(parse(text = values$code_2_ace)))
    })
    insert_code("insert_code_2", "code_2_ace")
    
    
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
    
    # Update code block based on selectInput
    observe({
      updateAceEditor(session, "code_3_ace", values$str_plot_3)
    })
    
    # Update plot code after input
    observeEvent(input$code_3_ace, {
      values$code_3_ace <- input$code_3_ace
    })
    
    output$plot_3_var <- renderPlot({
      return(eval(parse(text = values$code_3_ace)))
    })
    
    insert_code("insert_code_3", "code_3_ace")
    
    
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
    
    # Update code block based on selectInput
    observe({
      updateAceEditor(session, "code_4_ace", values$str_plot_4)
    })
    
    # Update plot code after input
    observeEvent(input$code_4_ace, {
      values$code_4_ace <- input$code_4_ace
    })
    
    output$plot_4_var <- renderPlot({
      return(eval(parse(text = values$code_4_ace)))
    })
    
    insert_code("insert_code_4", "code_4_ace")
    
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


