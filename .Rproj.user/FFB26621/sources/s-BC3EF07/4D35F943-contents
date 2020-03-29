---
date: "2020-03-27T00:00:00+01:00"
draft: false
linktitle: Chapter 4
menu:
  example:
    weight: 4
title: Chapter 4
toc: true
type: docs
weight: 4
---

## 4.3.6.1

Draw the reactive graph for the following server functions:

```{r, eval=FALSE}
server1 <- function(input, output, session) {
  c <- reactive(input$a + input$b)
  e <- reactive(c() + input$d)
  output$f <- renderText(e())
}
```

<img src="/img/reactive1.png"></img>

```{r, eval=FALSE}
server2 <- function(input, output, session) {
  x <- reactive(input$x1 + input$x2 + input$x3)
  y <- reactive(input$y1 + input$y2)
  output$z <- renderText(x() / y())
}
```

<img src="/img/reactive2.png"></img>

```{r, eval=FALSE}
server3 <- function(input, output, session) {
  d <- reactive(c() ^ input$d)
  a <- reactive(input$a * 10)
  c <- reactive(b() / input$c) 
  b <- reactive(a() + input$b)
}
```

<img src="/img/reactive3.png"></img>

## 4.3.6.2

Can the reactive graph contain a cycle? Why/why not?

No! This will create circular references and a recursion loop!

## 4.4.6.1 TODO

Use reactive expressions to reduce the duplicated code in the following simple apps.

<div class="note">
Unclear what apps this question is referring to
</div>

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