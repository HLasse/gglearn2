server <- function(input, output, session) { 
  source("helpers.R")
  
  print(dataset)
  
  output$skim_factor <- renderDT({
    dataset %>%
      select(where(is.factor)) %>%
      skim() %>%
      as_tibble() %>%
      select(skim_variable, n_missing, factor.n_unique, factor.top_counts) %>% 
      rename_at(vars(starts_with("factor")),
                funs(str_replace(., "factor.", ""))) %>% 
      rename(variable = skim_variable) %>% 
      rename_all(funs(str_replace(., "_", " "))) %>% 
      rename_with(capitalize)
  }, 
  options = dt_options(), 
  rownames = F
  )
  
  
  output$skim_numeric <- renderDT({
    dataset %>%
      select(where(is.numeric)) %>%
      skim() %>%
      as_tibble() %>% 
      select(skim_variable, n_missing, numeric.mean, numeric.sd, numeric.hist) %>% 
      rename_at(vars(starts_with("numeric")),
                funs(str_replace(., "numeric.", ""))) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      rename(Variable = skim_variable,
             Histogram = hist) %>% 
      rename_all(funs(str_replace(., "_", " "))) %>%
      rename_with(capitalize)
  }, options = dt_options(), rownames = F
  )
  
  output$dataframe <- renderDT({
    head(dataset, 10)
  },
  options = dt_options()
  )
  
  output$code_1_var <- renderUI({
    code("This is code")
  })
  
  output$plot_1_var <- renderPlot({
    ggplot(dataset, aes(Petal.Length)) + 
      geom_density()
  })
  
  output$code_2_var <- renderUI({
    code("This is more code")
  })
  
  output$plot_2_var <- renderPlot({
    ggplot(dataset, aes(Petal.Length, Petal.Width)) + 
      geom_point()
  })
  
}
