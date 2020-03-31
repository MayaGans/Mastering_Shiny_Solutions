library(purrr)
library(dplyr)

make_sliders <- function(var) {
  selectInput(var, paste0('typeof: ', var), choices = 
                c("double", "integer", "character", "numeric", "complex"))
}

change_type <- function(x, val) {
  switch(val,
         "double" = x %>% mutate(val, as.double),
         "integer" = x %>% mutate(val, as.integer),
         "character" = x %>% mutate(val, as.character),
         "numeric" = x %>% mutate(val, as.numeric),
         "complex" = x %>% mutate(val, as.complex))
  return(x)
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      map(names(iris), ~ make_sliders(.x))
    ),
    mainPanel(
      tableOutput('type_table')
    )
  )
)

server <- function(input, output, session) {

  # this is definitely messed up
  df<- renderUI(
    map(colnames(iris), ~ change_type(iris[[.x]], input[[.x]]))
  )
  
  output$type_table <- renderTable(data.frame(
    names = names(df()),
    type = map_chr(df(), function(x) typeof(x)))
  )
}

shinyApp(ui = ui, server = server)