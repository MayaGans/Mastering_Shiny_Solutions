---
date: "2020-03-27T00:00:00+01:00"
draft: false
linktitle: Chapter 8
menu:
  example:
    weight: 8
title: Chapter 8
toc: true
type: docs
weight: 8
---
  
## 8.1.5.1

Complete the user interface below with a server function that updates `input$date` so that you can only select dates in `input$year`.

```{r, eval=FALSE}
ui <- fluidPage(
  numericInput("year", "year", value = 2020),
  dateInput("date", "date")
)
```

This solution was a little wonky because it required shinyjs for the dateInput to properly update. I [opened up an issue here](https://github.com/rstudio/shiny/issues/2798) since I think this is not the most intuative answer.

```{r, eval=FALSE}
library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs() ,
  numericInput("year", "year", value = 2020),
  dateInput("date", "date", value = Sys.Date())
)

server <- function(input, output, session) {
  
  observeEvent(input$year, {
    
    req(isTruthy(input$year)) # stop if year is blank
    daterange <- range(as.Date(paste0(input$year, "-01-01")),as.Date(paste0(input$year, "-12-31")))
    updateDateInput(session, "date", min = daterange[1], max = daterange[2] )
    delay(250,  # delay 250ms
          updateDateInput(session,"date",
                          value = daterange[1]
          ))
  })
}

shinyApp(ui = ui, server = server)
```

## 8.1.5.2

Complete the user interface below with a server function that updates `input$county` choices based on `input$state`. For an added challenge, also change the label from “County” to “Parrish” for Louisana and “Borrough” for “Alaska”.

```{r, eval=FALSE}
library(openintro)
states <- unique(county$state)

ui <- fluidPage(
  selectInput("state", "State", choices = states),
  selectInput("county", "County", choices = NULL)
)
```

```{r, eval=FALSE}
library(shiny)
library(tidyverse)
library(openintro)

states <- unique(county$state)
counties <- unique(county$state)

ui <- fluidPage(
  selectInput("state", "State", choices = states),
  selectInput("county", "County", choices = NULL)
)


server <- function(input, output, session) {
  
  label <- reactive({
    switch(input$state,
           "Alaska" = "Burrough",
           "Louisiana" = "Parish",
           "County")
  })
   
  observeEvent( input$state, {
    updateSelectInput(session, "county", label = label(),
                      choices = county %>% 
                        filter(state == input$state) %>%
                        select(name) %>%
                        distinct())
  })

}


shinyApp(ui = ui, server = server)
```

## 8.1.5.3

Complete the user interface below with a server function that updates `input$country` choices based on the `input$continent`. Use `output$data` to display all matching rows.

```{r, eval=FALSE}
library(gapminder)
continents <- unique(gapminder$continent)

ui <- fluidPage(
  selectInput("continent", "Continent", choices = continents), 
  selectInput("country", "Country", choices = NULL),
  tableOutput("data")
)
```



```{r, eval=FALSE}
library(shiny)

library(gapminder)
continents <- unique(gapminder$continent)

ui <- fluidPage(
  selectInput("continent", "Continent", choices = c("", as.character(continents))), 
  selectInput("country", "Country", choices = NULL),
  tableOutput("data")
)



server <- function(input, output, session) {
  
  selected_data <- reactive({
    if(input$continent %in% continents) {
      gapminder %>% 
      filter(continent == input$continent)
    } else {
      gapminder
    }
  })
  
  
  observeEvent( input$continent, {
    updateSelectInput(session, "country", "Country",
                      choices = selected_data() %>% 
                        select(country) %>%
                        distinct())
  })
  
  
  output$data <- renderTable({ 
    selected_data() %>% 
      filter(country == input$country) 
  })

}


shinyApp(ui = ui, server = server)
```

## 8.1.5.4

Extend the previous app so that you can also choose to select no continent, and hence see all countries. You’ll need to add "" to the list of choices, and then handle that specially when filtering.

<div class="note">
Initially setting the choices to `c("", as.character(continents))` allows the user to see all the Country options prior to a continent being selected. That said, once a continent is selected this `""` option is no longer available?
</div>

```{r, eval=FALSE}
library(shiny)

library(gapminder)
continents <- unique(gapminder$continent)

ui <- fluidPage(
  selectInput("continent", "Continent", choices = c("", as.character(continents))), 
  selectInput("country", "Country", choices = NULL),
  tableOutput("data")
)


server <- function(input, output, session) {
  
  selected_data <- reactive({
    if(input$continent %in% continents) {
      gapminder %>% 
      filter(continent == input$continent)
    } else {
      gapminder
    }
  })
  
  
  observeEvent( input$continent, {
    updateSelectInput(session, "country", "Country",
                      choices = selected_data() %>% 
                        select(country) %>%
                        distinct())
  })
  
  
  output$data <- renderTable({ 
    selected_data() %>% 
      filter(country == input$country) 
  })

}


shinyApp(ui = ui, server = server)
```

## 8.1.5.5

What is at the heart of the problem described at https://community.rstudio.com/t/29307?

Updating all three sliders creates a circular reference!

## 8.4.3.1

Take this very simple app based on the initial example in the chapter:

```{r, eval=FALSE}
ui <- fluidPage(
  selectInput("type", "type", c("slider", "numeric")),
  uiOutput("numeric")
)
server <- function(input, output, session) {
  output$numeric <- renderUI({
    if (input$type == "slider") {
      sliderInput("n", "n", value = 0, min = 0, max = 100)
    } else {
      numericInput("n", "n", value = 0, min = 0, max = 100)  
    }
  })
}
```

How could you instead implement it using dynamic visibility? If you implement dynamic visiblity, how could you keep the values in sync when you change the controls?

```{r, eval=FALSE}
library(shiny)
parameter_tabs <- tagList(
  tags$style("#params { display:none; }"),
  tabsetPanel(id = "params",
              tabPanel("slider",
                       sliderInput("my_slider", "n", value = 0, min = 0, max = 100)
              ),
              tabPanel("numeric", 
                       numericInput("my_numeric", "n", value = 0, min = 0, max = 100)  
              )
  )
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("my_selector", "Input Type", 
                  choices = c("slider", "numeric")
      ),
      parameter_tabs,
    ),
    mainPanel()
  )
)

server <- function(input, output, session) {
  
  # if slider changes, update numeric
  observeEvent( input$my_slider, {
    updateNumericInput(session, "my_numeric", value = isolate(input$my_slider))
  })
  
  # if numeric changes update slider
  observeEvent( input$my_numeric, {
    updateSliderInput(session, "my_slider", value = isolate(input$my_numeric))
  })
  
  observeEvent(input$my_selector, {
    updateTabsetPanel(session, "params", selected = input$my_selector)
  }) 
  
}



shinyApp(ui = ui, server = server)
```

## 8.4.3.2

Add support for date and date-time columns `make_ui()` and `filter_var()`. 

In order to complete this, I had to 

1) make a new dummy dataframe I called `x` in order to test for dates
2) include checking for `is.Date` in the `make_ui` and `filter_var` functions
3) Change `tableOutput` and `renderTable` to `DT::renderTableOutput` and `DT::renderTableOutput` becuase `renderTable` was rendering the dates as numbers and I think this could be because it uses `xtable()` for HTML table rendering?

```{r, eval=FALSE}
# 8.4.3.2
library(shiny)
library(purrr)
library(tidyverse)

make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else if (lubridate::is.Date(x)) {
    rng <- range(x, na.rm = TRUE)
    dateInput(var, var, min = rng[1], max = rng[2], value = rng[1])
  } else {
    # No control, so don't filter
    NULL
  }
}


filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else if (lubridate::is.Date(x)) {
    x %in% val
  } else {
    TRUE
  }
}

library(shiny)

dfs <- keep(ls("package:datasets"), ~ is.data.frame(get(.x, "package:datasets")))

# add a dataframe with dates in it since I cant find one in the datasets above
# rep 5 dates five times, each include 1 factor a-e
x <- data.frame(date = c(rep(as.Date("2020/1/1"), 5),
                         rep(as.Date("2020/2/2"), 5),
                         rep(as.Date("2020/3/3"), 5),
                         rep(as.Date("2020/4/4"), 5),
                         rep(as.Date("2020/5/5"), 5)),
                fac = as.factor(c("a", "b", "c", "d", "e")))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      #selectInput("dataset", label = "Dataset", choices = c(dfs, "x")),
      uiOutput("filter")
    ),
    mainPanel(
      DT::dataTableOutput("data")
    )
  )
)

server <- function(input, output, session) {
  
  # data is either my dummy dataset or from datasets
  data <- reactive(x)
  
  vars <- reactive(names(data()))
  
  output$filter <- renderUI(
    # take eahc column name and make ui
    # data()[[.x]] is each column
    # and .x is each column name (vars())
    map(vars(), ~ make_ui(data()[[.x]], .x))
  )
  
  selected <- reactive({
    # take each column name and filer var
    # with the first argument the column in the data
    # and the second argument the input$vars()
    # so for date check that input[[date]] in data[[1]]
    each_var <- map(vars(), ~ filter_var(data()[[.x]], input[[.x]]))
    
    # what exactly is reduce doing here?
    print(each_var)
    print(reduce(each_var, `&`))
    reduce(each_var, `&`)
  })
  
  # subset the data by the vars that are true
  output$data <- DT::renderDataTable(data()[selected(), ])
}
# Run the application 
shinyApp(ui = ui, server = server)
```

## 8.4.3.3

(Advanced) If you know the S3 OOP system, consider how you could replace the if blocks in `make_ui()` and `filter_var()` with generic functions.

```{r, eval=FALSE}
library(shiny)
library(purrr)

make_ui <- function(obj, var) { UseMethod("make_ui") }

make_ui.numeric <- function(x, var) {
  rng <- range(x, na.rm = TRUE)
  sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
}

make_ui.factor <- function(x, var) { 
  levs <- levels(x) 
  selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
}

make_ui.default <- function(x, var) { NULL }

filter_var <- function(x, val) { UseMethod("filter_var") }
filter_var.numeric <- function(x, val) { !is.na(x) & x >= val[1] & x <= val[2] }
filter_var.factor <- function(x, val) { x %in% val }
filter_var.default <- function(x, val) { TRUE }

dfs <- keep(ls("package:datasets"), ~ is.data.frame(get(.x, "package:datasets")))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", label = "Dataset", choices = dfs),
      uiOutput("filter")
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)
server <- function(input, output, session) {
  data <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  vars <- reactive(names(data()))
  
  output$filter <- renderUI(
    map(vars(), ~ make_ui(data()[[.x]], .x))
  )
  
  selected <- reactive({
    each_var <- map(vars(), ~ filter_var(data()[[.x]], input[[.x]]))
    reduce(each_var, `&`)
  })
  
  output$data <- renderTable(head(data()[selected(), ], 12))
}

shinyApp(ui = ui, server = server)
```

## 8.4.3.4 TODO

(Hard) Make a wizard that allows the user to upload their own dataset. The first page should handle the upload. The second should handle reading it, providing one drop down for each variable that lets the user select the column type. The third page should provide some way to get a summary of the dataset.

```{r, eval=FALSE}

```

<style>
p {
  font-size: 12px;
}

pre {
    background-color: #bed3ec;
    border: solid 5px #dfedff;
    color: #1f5386;
    padding: 5px;
}

code {
  background-color: #bed3ec;
  color: #1f5386;
}

#TableOfContents {
  padding-left: 10px;
}

#TableOfContents li a, .toc-top li a {
    display: block;
    padding: .125rem 1.5rem;
    color: #436E9A;
}

body {
 background-color: #FFFFE0;
}

.docs-sidebar .docs-toc-item.active a, .docs-sidebar .nav>.active:hover>a, .docs-sidebar .nav>.active>a {
    font-weight: 700;
    color: #436E9A;
    background-color: transparent;
}

.note {
    padding: 1em;
    margin: 1em 0;
    padding-left: 100px;
    background-size: 70px;
    background-repeat: no-repeat;
    background-position: 15px center;
    min-height: 120px;
    color: #1f5386;
    background-color: #cae7c1;
    border: solid 5px #c3dac3;
    font-size: 15px;
  }
  
.note {
  background-image: url("/img/question.png");
}

</style>