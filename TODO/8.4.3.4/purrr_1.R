make_sliders <- function(var) {
  selectInput(var, paste0('typeof: ', var), choices = 
                c("double", "integer", "character", "numeric", "complex"))
}


change_type <- function(x, val) {
  switch(val,
         "double" = x <- as.double(x),
         "integer" = x <- as.integer(x),
         "character" = x <- as.character(x),
         "numeric" = x <- as.numeric(x),
         "complex" = x <- as.complex(x))
  iris
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
  
  df <- reactive({
    map(names(iris), ~ change_type(iris[[.x]], input[[.x]]))
  })
  
  output$type_table <- renderTable(data.frame(
    names = names(df()),
    type = map_chr(df(), function(x) typeof(x)))
  )
}

shinyApp(ui = ui, server = server)
