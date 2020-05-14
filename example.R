pacman::p_load(shiny)
binner <- function(dataset) {
  require(shiny)
  
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(sliderInput("n", "Bins", 5, 100, 20)),
      mainPanel(plotOutput("hist"))
    )
  )
  
  server = function(input, output) {
    output$hist <- renderPlot(
      ggplot(dataset, aes(x = eruptions)) + geom_histogram(bins = input$n) 
    )
  }
    
  shinyApp(ui, server)
}
binner(dataset = faithful)