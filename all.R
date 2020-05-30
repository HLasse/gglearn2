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
library(shinyWidgets)
library(scales)
library(shinyFeedback)
library(magick)
library(ggimage)

## choose plot toggle knap

#### i facet behold kun factor/fjern numeric
gglearn <- function(dataset){
  
  source("helpers.R")
  
  # Get named list of columns in dataset
  columns <- setNames(as.list(names(dataset)), names(dataset))
  
  ui <- navbarPage("gglearn2", windowTitle = NULL, theme = "lumen",
                   
                   # Tab 1: Intro and flowchart
                   tabPanel(title = "Introduction", icon = icon("stats", lib = "glyphicon"),
                            # Initialize shinyfeedback
                            useShinyFeedback(),
                            fluidPage(
                              fluidRow(
                                column(width = 6,
                                       panel(
                                         p("gglearn is a tool for learning to code using ggplot2."),
                                         p("The app is structured into 3 categories to help you learn the fundamentals of statistical plotting
                                           as well as basic ways of making your plots look pretty."),
                                         p("All tabs have an interactive code block which shows you the code used to produce the currently shown plot."),
                                         p("Experiment with modifying the code and watch the plots change in response!")
                                       )
                                ),
                                column(width = 6, 
                                       aceEditor("code_intro_ace", "", wordWrap = T, theme = tolower(rstudioapi::getThemeInfo()$editor), 
                                                 mode = "r", height = "120px", value = 'print("Welcome to gglearn2!")'),
                                       verbatimTextOutput("intro_output")
                                ),
                                hr(),
                                column(width = 12,
                                       imageOutput("flow_chart", height = "auto")
                                )
                                
                              )
                            )
                   ),
                   ############################################################
                   ###############      1 VARIABLE
                   ##############
                   #############################################################
                   tabPanel(title = "1 Variable", icon = icon("chart-area"),
                            # Exploring 1 variables
                            fluidPage(
                              fluidRow(
                                column(width = 6,
                                       panel(
                                         p("1 variable plots are useful for investigating distributions."),
                                         p("Density plots give you a sense of the general distribution by smoothing out the data"),
                                         p("Histograms provides counts and a less-smooth representation of the distribution"),
                                         p("QQ-plots easily lets you investigate whether your data follows the normal/Gaussian distribution. 
                                           A straight line indicates perfect normality and a curved line indicates skewed data.")
                                       )
                                ),
                                column(width = 6,
                                       tabsetPanel(type = "tabs", id = "tab_1_var",
                                                   tabPanel("Layout", value = "l",
                                                            column(width = 6,
                                                                   selectInput("graph_1", "Graph Type",
                                                                               c("Density" = "dens",
                                                                                 "Histogram" = "hist",
                                                                                 "QQ Plot" = "qq"))
                                                            ),
                                                            column(width = 6,
                                                                   varSelectInput("x_1", "Variable", dataset)
                                                            ),
                                                            
                                                            column(width = 6,
                                                                   selectInput("trans_1", "Transformation",
                                                                               c("None" = "NULL",
                                                                                 "Logarithm" = "log",
                                                                                 "Scale" = "scale"))
                                                            ),
                                                            column(6,
                                                                   switchInput(inputId = "coord_flip_1", label = strong("Flip axes"), value = FALSE, onLabel = "", offLabel = "")
                                                            ),
                                                            icon = icon("chart-area")
                                                   ),
                                                   tabPanel("Styling", value = "s",
                                                            column(6,
                                                                   textInput("p_1_title", "Title", "Your Title")
                                                            ),
                                                            column(6,
                                                                   textInput("p_1_x_lab", "X label", "X")
                                                            ),
                                                            column(6,
                                                                   selectInput("p_1_theme", "Theme", 
                                                                               c("Default (gray)" = "gray",
                                                                                 "light" = "light",
                                                                                 "bw" = "bw",
                                                                                 "minimal" = "minimal",
                                                                                 "classic" = "classic",
                                                                                 "void" = "void",
                                                                                 "dark" = "dark")
                                                                   )
                                                            ), 
                                                            column(6,
                                                                   spectrumInput(
                                                                     inputId = "p_1_fill",
                                                                     label = "Fill color",
                                                                     choices = list(
                                                                       list("steelblue", "forestgreen", "#C93312", "#DC863B", "#E1AF00" , "slateblue4"),
                                                                       as.list(brewer_pal(palette = "Blues")(6)),
                                                                       as.list(brewer_pal(palette = "Greens")(6)),
                                                                       as.list(brewer_pal(palette = "Spectral")(6)),
                                                                       as.list(brewer_pal(palette = "Dark2")(6))
                                                                     ),
                                                                     options = list(`toggle-palette-more-text` = "Show palette",
                                                                                    `toggle-palette-less-text` = "Hide palette")
                                                                   )
                                                            ),
                                                            icon = icon("palette"))
                                       )
                                )
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
                   ############################################################
                   ###############      2 VARIABLES
                   ##############
                   #############################################################
                   tabPanel(title = "2 Variables", icon = icon("chart-line"),
                            fluidPage(
                              fluidRow(
                                column(6,
                                       wellPanel(
                                         "This is help text"
                                       )
                                ),
                                column(6, 
                                       tabsetPanel(type = "tabs", id = "tab_2_var",
                                                   tabPanel("Layout", value = "l",
                                                            column(6,
                                                                   varSelectInput("x_2", "X", dataset)
                                                            ),
                                                            column(6,
                                                                   varSelectInput("y_2", "Y", dataset)
                                                            ),
                                                            br(),
                                                            column(6, 
                                                                   selectInput("geom_2_1", "Plot Type",
                                                                               c("Scatter" = "scatter",
                                                                                 "Line" = "line",
                                                                                 "Violin" = "violin",
                                                                                 "Boxplot" = "box",
                                                                                 "Bar" = "bar",
                                                                                 "Smooth" = "smooth",
                                                                                 "Smooth (linear)" = "lm")
                                                                   )
                                                            ),
                                                            column(6,
                                                                   selectInput("geom_2_2", "Plot Type 2",
                                                                               c("None" = "NULL",
                                                                                 "Scatter" = "scatter",
                                                                                 "Line" = "line",
                                                                                 "Violin" = "violin",
                                                                                 "Boxplot" = "box",
                                                                                 "Bar" = "bar",
                                                                                 "Smooth" = "smooth",
                                                                                 "Smooth (linear)" = "lm")
                                                                   )
                                                                   
                                                            ),
                                                            icon = icon("chart-line")
                                                   ),
                                                   tabPanel("Styling", value = "s",
                                                            column(6,
                                                                   textInput("p_2_x_lab", "X label", "X")
                                                            ),
                                                            column(6,
                                                                   textInput("p_2_y_lab", "Y label", "Y")
                                                            ),
                                                            column(6,
                                                                   selectInput("p_2_shape", "Geom 1 shape", shape_opts())
                                                            ),
                                                            column(6,
                                                                   spectrumInput(
                                                                     inputId = "p_2_color",
                                                                     label = "Geom 2 color",
                                                                     choices = list(
                                                                       list("steelblue", "forestgreen", "#C93312", "#DC863B", "#E1AF00" , "slateblue4"),
                                                                       as.list(brewer_pal(palette = "Blues")(6)),
                                                                       as.list(brewer_pal(palette = "Greens")(6)),
                                                                       as.list(brewer_pal(palette = "Spectral")(6)),
                                                                       as.list(brewer_pal(palette = "Dark2")(6))
                                                                     ),
                                                                     options = list(`toggle-palette-more-text` = "Show palette",
                                                                                    `toggle-palette-less-text` = "Hide palette")
                                                                   )
                                                            ),
                                                            
                                                            icon = icon("palette")
                                                   ) 
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
                   ############################################################
                   ###############      GROUPING
                   ##############
                   #############################################################
                   tabPanel(title = "Grouping", icon = icon("group"),
                            fluidPage(
                              fluidRow(
                                column(6,
                                       wellPanel(
                                         "This is help text"
                                       )
                                ),
                                column(6,
                                       tabsetPanel(type = "tabs", id = "tab_group",
                                                   tabPanel("Layout", value = "l",
                                                            column(6,
                                                                   
                                                                   tags$style(HTML(".chart_list { font-size: 30px; color: SteelBlue;}")),
                                                                   radioButtons("group_plot", label = "Choose plot",
                                                                                choiceNames = list(icon("chart-area", class = "chart_list"), icon("chart-line", class = "chart_list")),
                                                                                choiceValues = list("1_var_plot", "2_var_plot"), inline = T)
                                                            ),
                                                            column(6,
                                                                   selectInput("group_color", "Color", c("None" = "NULL", columns))
                                                            ),
                                                            # br(),
                                                            column(6,
                                                                   selectInput("group_fill", "Fill", c("None" = "NULL", columns))
                                                            ),
                                                            column(6,
                                                                   selectInput("group_facet", "Facet", c("None" = "NULL", columns))
                                                                   
                                                            ), icon = icon("group")
                                                   ),
                                                   tabPanel("Styling", value = "s",
                                                            # PALETTE, OPACITY, COORD FLIP, LEGEND POSITION
                                                            column(6,
                                                                   selectizeInput("group_color_palette", "Color Palette", choices = palette_opts()
                                                                   )
                                                            ),
                                                            column(6,
                                                                   selectizeInput("group_fill_palette", "Fill Palette", choices = palette_opts()
                                                                   )
                                                            ),
                                                            br(), 
                                                            column(6,
                                                                   selectInput("group_legend", "Legend position", 
                                                                               c("Right" = "right",
                                                                                 "Top" = "top",
                                                                                 "Bottom" = "bottom",
                                                                                 "Remove" = "none")
                                                                   )
                                                            ),
                                                            column(6,
                                                                   sliderInput("group_alpha", "Geom 1 opacity", 0, 1, 1, 0.2)
                                                            ),
                                                            
                                                            icon = icon("palette")
                                                   )
                                       )
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
                   ############################################################
                   ###############      EXERCISES
                   ##############
                   #############################################################                   
                   tabPanel(title = "Exercises", icon = icon("book")
                   ),
                   navbarMenu(title = "About", icon = icon("info"),
                              tabPanel("The app"),
                              tabPanel("The authors"))
                   
                   
  )
  
  
  
  # textAreaInput
  
  server <- function(input, output, session) { 
    source("ggplot_string.R")
    
    ######################### Setting reactive values
    vars <- c("x_1", "graph_1", "trans_1", "coord_flip_1",
              "p_1_x_lab", "p_1_title", "p_1_fill", "p_1_theme",
              # 2 vars
              "x_2", "y_2", "geom_2_1", "geom_2_2",
              "p_2_x_lab", "p_2_y_lab", "p_2_shape", "p_2_color",
              # grouping
              "group_plot", "group_color", "group_fill", "group_facet",
              "group_alpha", "group_fill_palette", "group_color_palette", "group_legend")
    
    
    show_styling_vars <- c("tab_1_var", "tab_2_var", "tab_group")
    
    values <- reactiveValues()
    
    for(var in show_styling_vars){
      values[[var]] <- FALSE
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
    
    # Show flowchart
    # output$flow_chart <- renderPlot({
    #   filename <- normalizePath(file.path('./images', 'example2.png'))
    #   ggbackground(ggplot(), filename )# + theme(aspect.ratio = 1), filename)
    # }, height=function() { session$clientData$output_flow_chart_width * 0.7 })
    # 
    
    output$flow_chart <- renderImage({
      filename <- normalizePath(file.path('./images', 'example2.png'))
      list(src = filename,
           height = "70%",
           width = "100%")
    }, deleteFile = FALSE)
    
    # Update vals
    observeEvent(input$code_intro_ace, {
      values$code_intro_ace <- input$code_intro_ace
    })
    
    # Render output (if text)
    output$intro_output <- renderText({
      return(eval(parse(text = values$code_intro_ace)))
    })
    
    
    
    ################################## OUTPUT FOR TAB 2: 1 VARIABLE
    
    # Set warning if variable not numeric and if histogram with continuous variable
    observe( {
      if(!is.numeric(dataset[[values$x_1]]) & values$graph_1 == "hist"){
        showFeedbackWarning("x_1", "You need to add \'stat=\"count\"\' to geom_histogram to make it work with factors")
      }
      else if(!is.numeric(dataset[[values$x_1]]) & values$graph_1 %in% c("dens", "qq")){
        showFeedbackWarning("x_1", "You should use continuous variables for this type of plot")
      }
      else{
        hideFeedback("x_1")
      }
    })
    
    # Small helper (move?)
    show_styling <- function(tab){
      if(input[[tab]] == "s"){
        values[[tab]] <- TRUE
      }
    }
    
    # Add styling if tab is clicked
    observeEvent(input$tab_1_var, {show_styling("tab_1_var")})
    
    # Update code based on inputs
    observe({
      if (isTRUE(values$tab_1_var)) {
        values$str_plot_1 <- {
          init_layer <- create_init(x = values$x_1)
          init_layer <- add_transform(init_layer, transform = values$trans_1)
          geoms <- create_geom(as.character(values$graph_1), fill = as.character(values$p_1_fill))
          labs <- create_labs(title = values$p_1_title, x = values$p_1_x_lab)
          theme <- create_std_theme(values$p_1_theme)
          if(isTRUE(values$coord_flip_1)){
            geoms <- c(geoms, create_geom("coord_flip"))
          }
          final_str <- combine_string(init_layer = init_layer, geoms = geoms, labs = labs, theme_std = theme)}
      }
      else {
        values$str_plot_1 <- {
          init_layer <- create_init(x = values$x_1)
          init_layer <- add_transform(init_layer, transform = values$trans_1)
          geoms <- create_geom(as.character(values$graph_1))
          if(isTRUE(values$coord_flip_1)){
            geoms <- c(geoms, create_geom("coord_flip"))
          }
          final_str <- combine_string(init_layer = init_layer, geoms = geoms)
        }
      }
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
    
    observeEvent(input$tab_2_var, {show_styling("tab_2_var")})
    
    ## Warning if violin or boxplot with non-factor x-variable
    observe( {
      feedbackWarning("geom_2_1", input$geom_2_1 %in% c("violin", "box") & is.numeric(dataset[[input$x_2]]), 
                      "The X variable should be a factor for this type of plot")
    })
    
    observe( {
      feedbackWarning("geom_2_2", input$geom_2_2 %in% c("violin", "box") & is.numeric(dataset[[input$x_2]]), 
                      "The X variable should be a factor for this type of plot")
    })
    
    
    observe({
      if (isTRUE(values$tab_2_var)){
        values$str_plot_2 <- {
          init_layer <- create_init(x = values$x_2, y = values$y_2)
          geoms <- create_geom(c(as.character(values$geom_2_1), as.character(values$geom_2_2)),
                               shape = c(values$p_2_shape, "NULL"),
                               color = c("NULL", values$p_2_color)
          )
          labs <- create_labs(x = values$p_2_x_lab, y = values$p_2_y_lab)
          final_str <- combine_string(init_layer = init_layer, geoms = geoms, labs = labs)}
      }
      else {
        values$str_plot_2 <- {
          init_layer <- create_init(x = values$x_2, y = values$y_2)
          geoms <- create_geom(c(as.character(values$geom_2_1), as.character(values$geom_2_2)))
          final_str <- combine_string(init_layer = init_layer, geoms = geoms)
        }
      }
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
    
    observeEvent(input$tab_group, {show_styling("tab_group")})
    
    # Show warning if facetting with numeric variable
    observe( {
      feedbackWarning("group_facet", is.numeric(dataset[[input$group_facet]]), 
                      "You usually want to facet by factor, not numeric, columns")
    })
    
    
    # update plot
    observe({
      values$str_plot_3 <- {
        if(values$group_plot == "1_var_plot"){
          init_layer <- create_init(x = values$x_1, color = values$group_color, fill = values$group_fill)
          geoms <- if_else(!isTRUE(values$tab_group),
                           create_geom(as.character(values$graph_1)),
                           create_geom(as.character(values$graph_1) , alpha = values$group_alpha)
          )
          if(isTRUE(values$coord_flip_1)){
            geoms <- c(geoms, create_geom("coord_flip"))
          }
          
        }
        if(values$group_plot == "2_var_plot"){
          init_layer <- create_init(x = values$x_2, y = values$y_2, color = values$group_color, fill = values$group_fill)
          geoms <- if_else(!isTRUE(values$tab_group),
                           create_geom(c(as.character(values$geom_2_1), as.character(values$geom_2_2))),
                           create_geom(c(as.character(values$geom_2_1), as.character(values$geom_2_2)), alpha = c(values$group_alpha, "NULL"))
          )
        }
        
        facet <- create_facet(as.character(values$group_facet))
        legend_pos <- create_custom_theme(values$group_legend)
        
        final_str <- if_else(!isTRUE(values$tab_group),
                             combine_string(init_layer = init_layer, geoms = geoms, facet = facet, 
                                            palette_color = values$group_colo_palette, palette_fill = values$group_fill_palette),
                             combine_string(init_layer = init_layer, geoms = geoms, facet = facet, 
                                            palette_color = values$group_color_palette, palette_fill = values$group_fill_palette,
                                            theme_custom = legend_pos)
        )
      }
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
    
    
  }
  
  runApp(shinyApp(ui, server))
}


gglearn(dataset = iris)



