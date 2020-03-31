library(shiny)

make_ui <- function(x, var) {
  selectInput(var, paste("Type: ",var), choices = c(colnames(x)))
}

ui <- fluidPage(
  tags$style("#wizard { display:none; }"),
  tabsetPanel(id = "wizard",
              tabPanel("page1", 
                       fileInput("input", "input"),
                       actionButton("page12", "next")
              ),
              tabPanel("page2", 
                       
                       actionButton("page21", "prev"),
                       actionButton("page23", "next")
              ),
              tabPanel("page3", 
                       renderTable("mytable"),
                       actionButton("page32", "prev")
              )
  )
)

server <- function(input, output, session) {
  switch_tab <- function(page) {
    updateTabsetPanel(session, "wizard", selected = page)
  }
  
  observeEvent(input$page12, switch_tab("page2"))
  observeEvent(input$page21, switch_tab("page1"))
  observeEvent(input$page23, switch_tab("page3"))
  observeEvent(input$page32, switch_tab("page2"))
}

shinyApp(ui = ui, server = server)
