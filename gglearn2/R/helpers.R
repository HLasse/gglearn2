
#'@title shape_opts
#'@description
#'
#'
#'@author
#'L. Hansen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
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

#'@title palette_opts
#'@description
#'
#'
#'@author
#'L. Hansen
#'
#'@return
#'unk
#'
#'@references
#'
#'@export
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

