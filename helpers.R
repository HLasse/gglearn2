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