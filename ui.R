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
                                     selectInput("graph_1", "Graph Type", 
                                                 c("Density" = "dens",
                                                   "Histogram" = "hist",
                                                   "Barplot" = "bar",
                                                   "QQ Plot" = "qq"))
                              ),
                              column(width = 3,
                                     varSelectInput("x_1", "Variable", iris)
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
                              column(width = 4,
                                     wellPanel(
                                       uiOutput("code_1_var")
                                     )
                              ),
                              column(width = 8,
                                     plotOutput("plot_1_var"))
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
                                     varSelectInput("x_2", "X", iris)
                              ),
                              column(3, 
                                     varSelectInput("y_2", "Y", iris)
                              ),
                              br(),
                              column(3, offset = 6, 
                                     selectInput("plot_2_1", "Plot Type",
                                                 c("Scatter" = "scatter",
                                                   "Line" = "line",
                                                   "Violin" = "violin",
                                                   "Boxplot" = "box",
                                                   "Bar" = "bar",
                                                   "Smooth" = "smooth")
                                     )
                              ),
                              column(3,
                                     selectInput("plot_2_1", "Plot Type 2",
                                                 c("None" = "none",
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
                              column(width = 4,
                                     wellPanel(
                                       uiOutput("code_2_var")
                                     )
                              ),
                              column(width = 8,
                                     plotOutput("plot_2_var")
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
                                     varSelectInput("group_color", "Color", iris)
                              ),
                              br(),
                              column(3, offset = 6, 
                                     varSelectInput("group_fill", "Fill", iris)
                              ),
                              column(3,
                                     varSelectInput("plot_2_1", "Plot Type 2", iris)
                                     
                              )         
                            ),
                            fluidRow(
                              column(width = 4,
                                     wellPanel(
                                       uiOutput("code_2_var")
                                     )
                              ),
                              column(width = 8,
                                     plotOutput("plot_2_var")
                              )
                            )
                          )
                 ),
                 tabPanel(title = "Making it pretty!", icon = icon("child")
                 ),
                 navbarMenu(title = "About", icon = icon("info"),
                            tabPanel("The app"),
                            tabPanel("The authors"))
                 
                 
)