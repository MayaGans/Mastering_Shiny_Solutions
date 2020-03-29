---
date: "2020-03-27T00:00:00+01:00"
draft: false
linktitle: Chapter 5
menu:
  example:
    weight: 5
title: Chapter 5
toc: true
type: docs
weight: 5
---

## 5.8.1

Draw the reactive graph for each app.

<img src="/img/reactlog1.png"></img>
<img src="/img/reactlog2.png"></img>
<img src="/img/reactlog3.png"></img>
    
## 5.8.2

What happens if you flip `fct_infreq()` and `fct_lump()` in the code that reduces the summary tables?

Using a test factor we see that we have 3 `a`s and 2 `c`s. if we first `fct_lump`, then every letter with a count of 2 will be placed into `Other`. `Other` now has 4 values so this becomes the 1st first factor when we factor based on frequency using `fct_infreq`. Because `Other` really represents different letters it should be placed as the last factor in our order.

```{r, eval=FALSE}
test <- factor(c("a", "a", "a", "c", "c", "b", "d", "e", "f"))
fct_infreq(fct_lump(test$a, n = 2))
```

## 5.8.3

Add an input control that lets the user decide how many rows to show in the summary tables.

Our function `count_top` is responsible for grouping our variables into a set number of factors, lumping the rest of the values into `other`. The function has an argument `n` which is set to `5`. By creating a `numericInput` called `rows` we can let the user set the number of `fct_infreq` dynamically. However because `fct_infreq` is the number of factors + `Other`, we need to subtract 1 from what the user selects in order to display the number of rows they input. 

<div class="note">
I tried downloading the data using the neiss package but I think that's already been cleaned. The data for this app can be found here: https://github.com/hadley/mastering-shiny/tree/master/neiss
</div>

```{r}
library(shiny)
library(tidyr)

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

fct_infreq(fct_lump(test$a, n = 2))

injuries <- vroom::vroom("/Users/mayagans/Downloads/injuries.tsv.gz")
products <- vroom::vroom("/Users/mayagans/Downloads/products.tsv")
population <- vroom::vroom("/Users/mayagans/Downloads/population.tsv")

selected <- injuries %>% filter(prod_code == 1842)

ui <- fluidPage(
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    column(2, numericInput("rows", "Number of Rows", min = 0, max = 10, value = 5)),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  # use n() for the n argument in count_top
  n <- reactive(input$rows - 1)
  
  output$diag <- renderTable(count_top(selected(), diag, n = n()), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part, n = n()), width = "100%")
  output$location <- renderTable(count_top(selected(), location, n = n()), width = "100%")
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries") +
        theme_grey(15)
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") +
        theme_grey(15)
    }
  })
  
  output$narrative <- renderText({
    input$story
    selected() %>% pull(narrative) %>% sample(1)
  })
}
shinyApp(ui, server)
```

## 5.8.4 TODO

Provide a way to step through every narrative systematically with forward and backward buttons.

```{r, eval=FALSE}
# data downloaded from https://github.com/hadley/mastering-shiny/tree/master/neiss
# this is not == the neiss package data!

library(shiny)
library(tidyr)

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

fct_infreq(fct_lump(test$a, n = 2))

injuries <- vroom::vroom("injuries.tsv.gz")
products <- vroom::vroom("products.tsv")
population <- vroom::vroom("population.tsv")

selected <- injuries %>% filter(prod_code == 1842)

ui <- fluidPage(
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    column(2, numericInput("rows", "Number of Rows", min = 0, max = 10, value = 5)),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("prev_story", "Go back a story")),
    column(2, actionButton("next_story", "Next story")),
    column(8, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  n <- reactive(input$rows - 1)
  
  output$diag <- renderTable(count_top(selected(), diag, n = n()), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part, n = n()), width = "100%")
  output$location <- renderTable(count_top(selected(), location, n = n()), width = "100%")
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries") +
        theme_grey(15)
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") +
        theme_grey(15)
    }
  })
  
  # set values$count to 1
  values <- reactiveValues(count = 1)
  
  # when next story is select increase values$count
  observeEvent(input$next_story, {
    values$count <- values$count + 1
  })
  
  # when next story is select decrease values$count
  observeEvent(input$prev_story, {
    values$count <- values$count - 1
  })
  
  # use values$count as the index of the story chosen
  output$narrative <- renderText({
    print(values$count)
    selected()$narrative[values$count]
  })
}
```

Advanced: Make the list of narratives “circular” so that advancing forward from the last narrative takes you to the first.

```{r, eval=FALSE}
# data downloaded from https://github.com/hadley/mastering-shiny/tree/master/neiss
# this is not == the neiss package data!

library(shiny)
library(tidyr)

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

fct_infreq(fct_lump(test$a, n = 2))

injuries <- vroom::vroom("injuries.tsv.gz")
products <- vroom::vroom("products.tsv")
population <- vroom::vroom("population.tsv")

selected <- injuries %>% filter(prod_code == 1842)

ui <- fluidPage(
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    column(2, numericInput("rows", "Number of Rows", min = 0, max = 10, value = 5)),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("prev_story", "Go back a story")),
    column(2, actionButton("next_story", "Next story")),
    column(8, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  # to check the code is working I changed the select code to only the head
  # in order to loop through just 6 values
  selected <- reactive(head(injuries %>% filter(prod_code == input$code)))
  
  n <- reactive(input$rows - 1)
  
  output$diag <- renderTable(count_top(selected(), diag, n = n()), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part, n = n()), width = "100%")
  output$location <- renderTable(count_top(selected(), location, n = n()), width = "100%")
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries") +
        theme_grey(15)
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") +
        theme_grey(15)
    }
  })
  
  values <- reactiveValues(count = 1)
  
  observeEvent(input$next_story, {
    # if the count is the max number of selected rows
    # then go back to 1
    if (values$count < nrow(selected())) {
      values$count <- values$count + 1
    } else {
      values$count <- 1
    }
  })
  
  observeEvent(input$prev_story, {
    # if you're at the first story go to the last story
    # otherwise can go down in index number
    if (values$count == 1) {
      values$count <- nrow(selected())
    } else {
      values$count <- values$count - 1
    }
  })
  
  output$narrative <- renderText({
    # check we're incrementing up and back to 1
    # and when we reach one we go back up to 6
    # print(values$count)
    selected()$narrative[values$count]
  })
  
}
shinyApp(ui, server)
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

</style>