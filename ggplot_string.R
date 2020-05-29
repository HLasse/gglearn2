
pacman::p_load(tidyverse, hash)

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
            "smooth" = "geom_smooth()"
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


combine_string <- function(libraries = "library(ggplot2)",
                           init_layer,
                           geoms,
                           facet = NULL,
                           labs = NULL,
                           theme_std = NULL,
                           theme_custom = NULL
){
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
  
  # exception handling for qq
  x <- str_match(e_string, "geom_qq")[1, 1]
  if (! is.na(x) & x == "geom_qq"){
    var <- str_match(e_string, "x = (.*)\\)")[, 2]
    e_string <- str_replace(e_string, "x = .*\\)", 
                            paste0("sample = ", var, ")"))
  }
  
  return(e_string)
}

# init_layer <- create_init(x = "x")
# combine_string(init_layer =init_layer, geoms = create_geom("qq"))

# 
# 
# dataset <- iris
# x <- "Sepal.Length"
# y <- "Petal.Width"
# fill <- "Species"
# color <- "Species"
# shape <- "Species"
# 
# 
# libraries <- "library(ggplot2)"
# init_layer <- create_init(x = x, y = y, fill = fill, color = color, shape = shape)
# 
# geoms <- c(create_geom("scatter"), create_geom("line"))
# 
# std_theme <- create_std_theme(theme = "bw")
# custom_theme <- create_custom_theme(rm_legend = F)
# labs <- create_labs(title = "example", color = "BLOMSTER for helved")
# e_string <- combine_string(libraries,
#                            init_layer = init_layer,
#                            geoms = geoms,
#                            labs = labs,
#                            theme_std = std_theme,
#                            theme_custom = custom_theme)
# p <- eval(parse(text=e_string))
# p
# 
# 
# 
# cat(e_string)
# head(df)


