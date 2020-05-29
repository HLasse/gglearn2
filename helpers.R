### help functions
capitalize <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# Options for rendering datatables
dt_options <- function(dom_settings = "tip"){
  list(
  dom = dom_settings,
  scrollX = TRUE,
  scrollY = TRUE,
  autoWidth = TRUE,
  columnDefs = list(list(width = '20%', targets = "_all")))
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