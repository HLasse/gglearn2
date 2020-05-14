
pacman::p_load(tidyverse, hash)

create_init <- function(df = "dataset", x, y=NULL,
                        fill=NULL, color=NULL, shape=NULL){
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


create_geom <- function(geom, color, fill, shape, alpha){
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

  geom_str <- geom_add_arg(geom_str, color, "color")
  geom_str <- geom_add_arg(geom_str, fill, "fill")
  geom_str <- geom_add_arg(geom_str, shape, "shape")
  geom_str <- geom_add_arg(geom_str, alpha, "alpha")
  return(geom_str)
}

add_layer <- function(e_string, geom){
  e_string <- paste0(e_string, " + ", "\n", geom)
  return(e_string)
}

combine_string <- function(libraries = "library(ggplot2)",
                           init_layer,
                           geoms,
                           legends = ""
                           ){
  geoms <- paste0("\t", geoms)
  e_string <- paste(libraries, init_layer, sep ="\n")
  for (geom in geoms){
    e_string <- add_layer(e_string, geoms)
  }
  e_string <- add_layer(e_string, legends)
  return(e_string)
}





dataset <- iris
x <- "Sepal.Length" 
y <- "Petal.Width"
fill <- "Species"
color <- "Species"
shape <- "Species"


libraries <- "library(ggplot2)"
init_layer <- create_init(x = x, y = y, fill = fill, color = color, shape = shape)
geoms <- c("geom_point()")
e_string <- combine_string(libraries, init_layer=init_layer, geoms=geoms)
p <- eval(parse(text=e_string))
p
head(df)
