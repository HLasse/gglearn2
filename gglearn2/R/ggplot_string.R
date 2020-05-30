
#'@title create_init
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
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
    e_string <- stringr::str_replace(e_string, "\\)\\)", rep_str)
  }
  if (!is.null(fill)){
    rep_str <- paste0(", fill = ", fill, "))")
    e_string <- stringr::str_replace(e_string, "\\)\\)", rep_str)
  }
  if (!is.null(shape)){
    rep_str <- paste0(", shape = ", shape, "))")
    e_string <- stringr::str_replace(e_string, "\\)\\)", rep_str)
  }
  return(e_string)
}


#'@title geom_replace_arg
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
geom_replace_arg <- function(str, arg_to_rep = "x", replacement) {
  # # Example:
  # geom_replace_arg("geom(aes(x    =df$TOTO, y = df$duck))",
  #                  arg_to_rep = "x",
  #                  replacement = "log(df$TOTO)")

  t <- stringr::str_match(str, paste0(arg_to_rep, "\\s*=.*?(,|\\))"))
  end_sym <- t[length(t)]

  regex <- paste0(arg_to_rep, "\\s*=.*?[,|\\)]")
  rep_str <- paste0(arg_to_rep, " = ", replacement, end_sym)
  return (stringr::str_replace(str, pattern = regex, rep_str))
}

#'@title geom_extract_arg
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
geom_extract_arg <- function(str, arg_to_extract = "x") {
  # Returns first match
  regex <- paste0(arg_to_extract, "\\s*=\\s*(.*?)[,|\\)]")
  t <- stringr::str_match(str, pattern = regex)
  return(t[2])
}

#'@title add_transform
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
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

#'@title geom_add_arg
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
geom_add_arg <- function(geom_str, arg, name){
  if (!is.null(arg)){
    x <- stringr::str_match(geom_str, "\\(.*\\)")
    if (x[1,1] == "()"){
      comma <- ""
    } else {
      comma = ", "
    }
    rep_str <- paste0(comma, name," = ", arg, ")")
    geom_str <- stringr::str_replace(geom_str, "\\)", rep_str)
  }
  return(geom_str)
}



#'@title check_length
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
check_length <- function(x, len){
  if (length(x) == len){
    return(x)
  } else if (length(x) == 1 | is.null(x)){
    x <- rep(x, len)
  } else {
    stop("length of geoms is above 1, but the length of the remainder of the arguments does not match")
  }
}


#'@title create_stat_geom
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
create_stat_geom <- function(type="overlay_norm", var = NULL){
  if (is.null(type)){
    return(NULL)
  }
  h <- hash::hash("overlay_norm" = paste0("stat_function(fun = dnorm, ",
                                    "args = list(",
                                    "mean = mean(dataset$", var, ")",
                                    ", sd = sd(dataset$", var, ")), color = 'red')"))
  geom_str <- as.character(h[[type]])
  return(geom_str)
}

#'@title create_geom
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
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
  fill  <- str_to_null(fill)
  shape <- str_to_null(shape)
  alpha <- str_to_null(alpha)
  h <- hash::hash("dens" = "geom_density()",
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

#'@title add_layer
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
add_layer <- function(e_string, geom){
  if (is.null(geom)){
    return(e_string)
  }
  e_string <- paste0(e_string, " + ", "\n", geom)
  return(e_string)
}

#'@title null_to_str
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
null_to_str <- function(str){
  if (is.null(str)){
    str <- ""
  }
  return(str)
}

#'@title str_to_null
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
str_to_null <- function(str){
  if (is.null(str)){
    return(str)
  }
  if (str == "NULL"){
    str <- NULL
  }
  return(str)
}

#'@title add_quatations
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
add_quatations <- function(str){
  if (is.null(str)){
    return(NULL)
  }
  str = paste0("'", str,"'")
  return(str)
}

#'@title create_labs
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
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

#'@title create_std_theme
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
create_std_theme <- function(theme){
  h <- hash::hash("gray" = "theme_gray()",
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

#'@title create_custom_theme
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
create_custom_theme <- function(rm_legend){

  theme_str <- "theme()"
  theme_str <- geom_add_arg(theme_str, name = 'legend.position', arg = add_quatations(rm_legend))
  if (theme_str == "theme()"){
    return(NULL)
  }
  return(theme_str)
}


#'@title create_facet
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
create_facet <- function(by){
  by <- str_to_null(by)
  if(is.null(by)){return(NULL)}
  facet_str <- paste0("facet_wrap( ~ ", by, ")")
  return(facet_str)
}


#'@title create_palette
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
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



#'@title combine_string
#'@description
#'
#'
#'@author
#'K. Enevoldsen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
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
  x <- stringr::str_match(e_string, "geom_qq")[1, 1]
  if (! is.na(x) & x == "geom_qq"){
    var <- str_match(e_string, "x = (.*)\\)")[, 2]
    e_string <- stringr::str_replace(e_string, "x = .*\\)",
                            paste0("sample = ", var, ")"))
  }

  return(e_string)
}

