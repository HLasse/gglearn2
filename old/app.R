### Shiny app for teaching ggplot2

library(shiny)
library(shinythemes)
library(ggplot2)
library(shinyAce)
library(shinyWidgets)
library(scales)
library(shinyFeedback)

library(tidyverse)
library(hash)
library(palmerpenguins)

create_init <- function(df = "dataset", x, y=NULL,
                        fill=NULL, color=NULL, shape=NULL){
  y <- str_to_null(y)
  fill <- str_to_null(fill)
  color <- str_to_null(color)
  shape <- str_to_null(shape)

  if (is.null(y)){
    e_string <- paste0("ggplot(", df, ", aes(x = ", x, "))")
  } else {
    e_string <- paste0("ggplot(", df, ", aes(x = ", x, ", y = ", y, "))")
  }
  if (!is.null(color)){
    rep_str <- paste0(", color = ", color, "))")
    e_string <- str_replace(e_string, "\\)\\)", rep_str)
  }
  if (!is.null(fill)){
    rep_str <- paste0(", fill = ", fill, "))")
    e_string <- str_replace(e_string, "\\)\\)", rep_str)
  }
  if (!is.null(shape)){
    rep_str <- paste0(", shape = ", shape, "))")
    e_string <- str_replace(e_string, "\\)\\)", rep_str)
  }
  return(e_string)
}



geom_replace_arg <- function(str, arg_to_rep = "x", replacement) {
  # # Example:
  # geom_replace_arg("geom(aes(x    =df$TOTO, y = df$duck))",
  #                  arg_to_rep = "x",
  #                  replacement = "log(df$TOTO)")

  t <- str_match(str, paste0(arg_to_rep, "\\s*=.*?(,|\\))"))
  end_sym <- t[length(t)]

  regex <- paste0(arg_to_rep, "\\s*=.*?[,|\\)]")
  rep_str <- paste0(arg_to_rep, " = ", replacement, end_sym)
  return (str_replace(str, pattern = regex, rep_str))
}

geom_extract_arg <- function(str, arg_to_extract = "x") {
  # Returns first match
  regex <- paste0(arg_to_extract, "\\s*=\\s*(.*?)[,|\\)]")
  t <- str_match(str, pattern = regex)
  return(t[2])
}


add_transform <- function(init_str, arg_to_trans = "x", transform) {
  transform <- str_to_null(transform)
  if (is.null(transform)){
    return(init_str)
  }
  arg <- geom_extract_arg(init_str, arg_to_trans)
  res <- geom_replace_arg(init_str,
                          arg_to_rep = arg_to_trans,
                          replacement = paste0(transform, "(", arg, ")", sep = "")
  )
  return(res)
}

geom_add_arg <- function(geom_str, arg, name){
  if (!is.null(arg)){
    x <- str_match(geom_str, "\\(.*\\)")
    if (x[1,1] == "()"){
      comma <- ""
    } else {
      comma = ", "
    }
    rep_str <- paste0(comma, name," = ", arg, ")")
    geom_str <- str_replace(geom_str, "\\)", rep_str)
  }
  return(geom_str)
}


check_length <- function(x, len){
  if (length(x) == len){
    return(x)
  } else if (length(x) == 1 | is.null(x)){
    x <- rep(x, len)
  } else {
    stop("length of geoms is above 1, but the length of the remainder of the arguments does not match")
  }
}



create_stat_geom <- function(type="overlay_norm", var = NULL){
  if (is.null(type)){
    return(NULL)
  }
  h <- hash("overlay_norm" = paste0("stat_function(fun = dnorm, ",
                                    "args = list(",
                                    "mean = mean(dataset$", var, ")",
                                    ", sd = sd(dataset$", var, ")), color = 'red')"))
  geom_str <- as.character(h[[type]])
  return(geom_str)
}


create_geom <- function(geom, color=NULL, fill=NULL, shape=NULL, alpha=NULL){
  if (length(geom)>1){
    n <- length(geom)
    color <- check_length(color, n)
    fill <- check_length(fill, n)
    shape <- check_length(shape, n)
    alpha <- check_length(alpha, n)

    res <- c()
    for (i in 1:n){
      geom_str <- create_geom(geom[i], color[i], fill[i], shape[i], alpha[i])
      res = c(res, geom_str)
    }
    return(res)
  }
  geom = str_to_null(geom)
  if (is.null(geom)){
    return(NULL)
  }

  color <- str_to_null(color)
  fill<- str_to_null(fill)
  shape <- str_to_null(shape)
  alpha <- str_to_null(alpha)
  h <- hash("dens" = "geom_density()",
            "hist" = "geom_histogram()",
            "qq" ="geom_qq()",
            "line" ="geom_line()",
            "bar" = "geom_bar()",
            "scatter" = "geom_point()",
            "violin" = "geom_violin()",
            "box" = "geom_boxplot()",
            "smooth" = "geom_smooth()",
            "lm" = "geom_smooth(method='lm')",
            "coord_flip" = "coord_flip()"
  )
  geom_str = h[[geom]]
  if (is.null(geom_str)){stop(paste("The geom,", geom, "is not implemented"))}

  geom_str <- geom_add_arg(geom_str, add_quatations(color), "color")
  geom_str <- geom_add_arg(geom_str, add_quatations(fill), "fill")
  geom_str <- geom_add_arg(geom_str, shape, "shape")
  geom_str <- geom_add_arg(geom_str, alpha, "alpha")
  return(geom_str)
}


add_layer <- function(e_string, geom){
  if (is.null(geom)){
    return(e_string)
  }
  e_string <- paste0(e_string, " + ", "\n", geom)
  return(e_string)
}


null_to_str <- function(str){
  if (is.null(str)){
    str <- ""
  }
  return(str)
}


str_to_null <- function(str){
  if (is.null(str)){
    return(str)
  }
  if (str == "NULL"){
    str <- NULL
  }
  return(str)
}


add_quatations <- function(str){
  if (is.null(str)){
    return(NULL)
  }
  str = paste0("'", str,"'")
  return(str)
}


create_labs <- function(title=NULL,
                        subtitle=NULL,
                        caption=NULL,
                        tag=NULL,
                        color=NULL,
                        fill=NULL,
                        shape=NULL,
                        x = NULL,
                        y = NULL
){
  title = null_to_str(title)

  labs_str <- paste0("labs(title = ", add_quatations(title), ")")
  labs_str <- geom_add_arg(labs_str, add_quatations(subtitle), "subtitle")
  labs_str <- geom_add_arg(labs_str, add_quatations(caption), "caption")
  labs_str <- geom_add_arg(labs_str, add_quatations(tag), "tag")
  labs_str <- geom_add_arg(labs_str, add_quatations(color), "color")
  labs_str <- geom_add_arg(labs_str, add_quatations(fill), "fill")
  labs_str <- geom_add_arg(labs_str, add_quatations(shape), "shape")
  labs_str <- geom_add_arg(labs_str, add_quatations(x), "x")
  labs_str <- geom_add_arg(labs_str, add_quatations(y), "y")
  return(labs_str)
}


create_std_theme <- function(theme){
  h <- hash("gray" = "theme_gray()",
            "bw" = "theme_bw()",
            "light" = "theme_light()",
            "dark" = "theme_dark()",
            "minimal" = "theme_minimal()",
            "classic" = "theme_classic()",
            "void" = "theme_void()"
  )
  theme_str <- h[[theme]]
  return(theme_str)
}


create_custom_theme <- function(rm_legend){

  theme_str <- "theme()"
  theme_str <- geom_add_arg(theme_str, name = 'legend.position', arg = add_quatations(rm_legend))
  if (theme_str == "theme()"){
    return(NULL)
  }
  return(theme_str)
}

create_facet <- function(by){
  by <- str_to_null(by)
  if(is.null(by)){return(NULL)}
  facet_str <- paste0("facet_wrap( ~ ", by, ")")
  return(facet_str)
}

create_palette <- function(palette, type="fill"){
  if (! type %in% c("fill", "color")){
    stop("invalid type, should be fill or color")
  }
  brewer <- c("Dark2", "Greens", "YlOrRd")
  viridis <- c("viridis", "inferno", "plasma")
  if (palette %in% brewer){
    lib = "library(RColorBrewer)"
    str = paste0("scale_", type, "_brewer(palette = '", palette, "')")
  } else if (palette %in% viridis){
    lib = "library(viridis)"
    str = paste0("scale_", type, "_viridis(option = '", palette, "')")
  } else {
    stop("invalid color palette")
  }
  return(list(str=str, lib=lib))
}




combine_string <- function(libraries = "library(ggplot2)",
                           init_layer,
                           geoms,
                           facet = NULL,
                           labs = NULL,
                           theme_std = NULL,
                           theme_custom = NULL,
                           palette_fill = NULL,
                           palette_color = NULL

){
  palette_fill = str_to_null(palette_fill)
  palette_color = str_to_null(palette_color)
  if (! is.null(palette_fill)){
    res = create_palette(palette_fill, type = "fill")
    palette_fill = res$str
    libraries = paste(libraries, res$lib, sep = "\n")
  }
  if (! is.null(palette_color)){
    res = create_palette(palette_color, type = "color")
    palette_color = res$str
    libraries = paste(libraries, res$lib, sep = "\n")
  }

  geoms <- paste0("\t", geoms)
  e_string <- paste(libraries, init_layer, sep ="\n\n")

  if (! is.null(facet)){
    geoms <- c(geoms, facet)
  }
  for (geom in geoms){
    if (! is.null(geom)){
      e_string <- add_layer(e_string, geom)
    }
  }

  e_string <- add_layer(e_string, labs)
  e_string <- add_layer(e_string, theme_std)
  e_string <- add_layer(e_string, theme_custom)
  e_string <- add_layer(e_string, palette_fill)
  e_string <- add_layer(e_string, palette_color)

  # exception handling for qq
  x <- str_match(e_string, "geom_qq")[1, 1]
  if (! is.na(x) & x == "geom_qq"){
    var <- str_match(e_string, "x = (.*)\\)")[, 2]
    e_string <- str_replace(e_string, "x = .*\\)",
                            paste0("sample = ", var, ")"))
  }

  return(e_string)
}

shape_opts <- function(){
  c("None" = "NULL",
    "Square" = 0,
    "Circle" = 1,
    "Triangle" = 2,
    "Plus" = 3,
    "Cross" = 4,
    "Diamond" = 5,
    "Downwards triangle" = 6,
    "Square cross" = 7,
    "Star" = 8)
}

palette_opts <- function(){
  list(
    "Default" = "NULL",
    Discrete = c(
      "Dark2" = "Dark2",
      "Greens" = "Greens",
      "YlOrRd" = "YlOrRd"),
    Continuous = c(
      "Viridis" = "viridis",
      "Inferno" = "inferno",
      "Plasma" = "plasma")
  )
}


dataset = penguins

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
                              column(width = 4,
                                     panel(
                                       h4("Note"),
                                       p("The online demo version uses the", a("penguins dataset", href="https://github.com/allisonhorst/palmerpenguins"),
                                         "and features such as editing code are disabled. For the full experience and to use your own dataset, download the R package",
                                         a("here", href ="https://github.com/HLasse/gglearn2"))
                                     )),
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
                                       p("All plots start with a call to", code("ggplot()"), "with the dataset and ", em("aes", .noWS = "after"),"thetic mappings between variables and visuals."),
                                       p("This is followed by one or more ", em("geoms"), "which tell ggplot how to display the information."),
                                       p(strong("Exercise:"), "Delete everything but the initial ", code("ggplot()"), "call. What happens?"),
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
                                     aceEditor("code_1_ace", "", wordWrap = T, theme = "dawn", mode = "r", readOnly = TRUE),
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
                                       p(strong("Exercise:"), "try adding a third geom of your own choice using the code block.")
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
                                                                               "Bar" = "bar",
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
                                     aceEditor("code_2_ace", "", wordWrap = T, theme = "dawn", mode = "r", readOnly = TRUE),
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
                                       p("One of ggplot's strengths is in way you can easily show extra variables using the", em("color, fill, shape, and size"), "aesthetics."),
                                       p("Using these aesthetics allows you to visualize differences between e.g. levels of a factor and 1 or more variables."),
                                       p("Facetting splits the plot into multiple panes, one for each group you facet by."),
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
                                     aceEditor("code_3_ace", "", wordWrap = T, theme = "dawn", mode = "r", readOnly = TRUE),
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

  ######################### OUTPUTS FOR TAB 1 - FLOWCHART

  output$flow_chart <- renderImage({
    list(src = "flowchart.png",
         height = "70%",
         width = "100%")
  }, deleteFile = FALSE)


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

shinyApp(ui, server)


