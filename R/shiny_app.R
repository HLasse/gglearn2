### Shiny app for teaching ggplot2


## choose plot toggle knap
#### i facet behold kun factor/fjern numeric

#'@title gglearn: Shiny App for learning ggplot2
#'@description
#'A shiny application for teaching and learning ggplot2
#'
#'@param df A dataframe object
#'
#'@author
#'L. Hansen
#'
#'@return
#'start a shiny application
#'
#'@references
#'
#'@import shiny shinythemes ggplot2 shinyAce shinyWidgets scales shinyFeedback
#'@importFrom stats setNames
#'
#'@export
gglearn <- function(dataset){

  # Get named list of columns in dataset
  columns <- setNames(as.list(names(dataset)), names(dataset))

  ui <- navbarPage("gglearn2", windowTitle = NULL, theme = "lumen",

                   # Tab 1: Intro and flowchart
                   tabPanel(title = "Introduction", icon = icon("stats", lib = "glyphicon"),
                            # Initialize shinyfeedback
                            useShinyFeedback(),
                            fluidPage(
                              fluidRow(
                                column(width = 8,
                                       panel(
                                         h4("gglearn is a tool for learning to code using ggplot2"),
                                         p("The app is structured into 3 categories to help you learn the fundamentals of statistical plotting
                                           as well as basic ways of making your plots look pretty."),
                                         p("All tabs have an interactive code block which shows you the code used to produce the currently shown plot."),
                                         p("Experiment with modifying the code and watch the plots change in response!"),
                                         p("Unsure which type of plot is right for your needs? Check out the flowchart below", icon("arrow-down"))
                                       )
                                ),
                                hr(),
                                column(width = 12,
                                       img(src = "img/flowchart.png", height = "70%", width = "100%")
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
                                         p("All plots start with a call to", code("ggplot()"), "with the dataset and ", "aesthetic", code("aes()"), "mappings between variables and visuals."),
                                         p("This is followed by one or more ", em("geoms"), "which tell ggplot how to display the information."),
                                         p(strong("Exercise:"), "Delete everything after and including the ", code("+"), "What do you think happens?"),
                                         p(strong("Exercise:"), "Play around with the different options and drop-downs to the right. Notice the changes to the code!"),
                                         p(strong("Exercise:"), "Once you are familiar with the geoms presented here, click the styling tab to customize their apperance.",
                                           "Notice what happens in the code block!")
                                       )
                                ),
                                column(width = 6,
                                       tabsetPanel(type = "tabs", id = "tab_1_var",
                                                   tabPanel("Layout", value = "l",
                                                            column(width = 6,
                                                                   selectInput("graph_1", "Geom",
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
                                       panel(
                                         p("Items added after the intial ", code("ggplot()"), "call are called", em("layers.")),
                                         p("As the name implies, ", em("layers"), "can be added on top of each other using ", code("+"), "to create more and more sophisticated plots."),
                                         p(strong("Exercise:"), "Experiment with using two different geoms. Does order matter?"),
                                         p(strong("Exercise:"), "Try adding a third geom of your own choice using the code block.")
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
                                                                   selectInput("geom_2_1", "Geom 1",
                                                                               c("Scatter" = "scatter",
                                                                                 "Line" = "line",
                                                                                 "Violin" = "violin",
                                                                                 "Boxplot" = "box",
                                                                                 "Smooth" = "smooth",
                                                                                 "Smooth (linear)" = "lm")
                                                                   )
                                                            ),
                                                            column(6,
                                                                   selectInput("geom_2_2", "Geom 2",
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
                                       panel(
                                         p("One of ggplot's strengths is its ability to easily show extra variables using the", em("color, fill, shape"), "and", em("size"), "aesthetics."),
                                         p("Using these aesthetics allows you to visualize differences between e.g. levels of a factor and 1 or more variables."),
                                         p(em("Facet"), "will split the plot into multiple panes, one for each group you", em("facet"), "by."),
                                         p(strong("Exercise:"), "Play around with color and fill. What's the difference between them?"),
                                         p(strong("Exercise:"), "Add the", em("size"), "or", em("shape"), "aesthetic. How does your plot change?")
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
                   ###############      Challenges
                   ##############
                   #############################################################
                   tabPanel(title = "Challenges",
                            fluidRow(
                              column(6,
                                     panel(
                                       h3("Exercises"),
                                       "Easy: Create a histogram and flip the axes.",
                                       tags$ul(
                                         tags$li("Fill by a factor variable (hint: check out the 'grouping' panel.")
                                       ),
                                       "Medium: Create a violin plot with a factor variable on the X-axis",
                                       tags$ul(
                                         tags$li("Flip the axes.")
                                       ),
                                       "Hard: Create a violin plot with a boxplot on top.",
                                       tags$ul(
                                         tags$li("Fill the plots with different colors.")
                                       )
                                     )
                              )
                            ),
                            icon = icon("trophy")
                   ),
                   tabPanel(title = "Resources",
                            fluidRow(
                              column(6,
                                     panel(
                                       h3("Resources"),
                                       h4("Books:"),
                                       p(a("ggplot2: Elegant Graphics for Data Analysis", href="https://ggplot2-book.org/")),
                                       br(),
                                       h4("Free Online Courses:"),
                                       p(a("Andrew Heiss' Data Visualization Course", href="https://datavizm20.classes.andrewheiss.com/")),
                                       br(),
                                       h4("Misc."),
                                       p(a("Cheatsheets from RStudio", href="https://rstudio.com/resources/cheatsheets/")),
                                       p(a("The R Graph Gallery", href="https://www.r-graph-gallery.com/")),
                                       p(a("Make beautiful ggplots", href="https://cedricscherer.netlify.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/"))
                                     )
                              )
                            ),
                            icon = icon("book")
                   )
  )



  # textAreaInput

  server <- function(input, output, session) {

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
        shinyFeedback::hideFeedback("x_1")
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

    observe( {
      feedbackWarning("p_2_shape", input$geom_2_1 != "scatter" & input$p_2_shape != "NULL",
                      "The shape parameter only works with geom_point()")
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
          geoms <- ifelse(!isTRUE(values$tab_group),
                           create_geom(as.character(values$graph_1)),
                           create_geom(as.character(values$graph_1) , alpha = values$group_alpha)
          )
          if(isTRUE(values$coord_flip_1)){
            geoms <- c(geoms, create_geom("coord_flip"))
          }

        }
        if(values$group_plot == "2_var_plot"){
          init_layer <- create_init(x = values$x_2, y = values$y_2, color = values$group_color, fill = values$group_fill)
          print(values$tab_group)
          # Creates bugs if ifelse..
          if (!isTRUE(values$tab_group)) {
            geoms <- create_geom(c(as.character(values$geom_2_1), as.character(values$geom_2_2)))
          } else {
            geoms <- create_geom(c(as.character(values$geom_2_1), as.character(values$geom_2_2)), alpha = c(values$group_alpha, "NULL"))
          }
        }

        facet <- create_facet(as.character(values$group_facet))
        legend_pos <- create_custom_theme(values$group_legend)

        final_str <- ifelse(!isTRUE(values$tab_group),
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
