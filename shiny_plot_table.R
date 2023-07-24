library(shiny)
library(plotly)
library(DT)
ui <- fluidPage(
  column(3, offset = 2, verticalLayout(plotlyOutput("p1"), dataTableOutput("d1"))),
  column(3, offset = 2, verticalLayout(plotlyOutput("p2"), dataTableOutput("d2"))),
  column(3, offset = 2, verticalLayout(plotlyOutput("p3"), dataTableOutput("d3"))),
  column(3, offset = 2, verticalLayout(plotlyOutput("p4"), dataTableOutput("d4")))
)
server <- function(input, output, session) {
  output$p1 <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~disp)
  })
  output$d1 <- DT::renderDataTable({
    datatable(head(mtcars))
  })
  output$p2 <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~disp)
  })
  output$d2 <- DT::renderDataTable({
    datatable(head(mtcars))
  })
  output$p3 <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~disp)
  })
  output$d3 <- DT::renderDataTable({
    datatable(head(mtcars))
  })
  output$p4 <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~disp)
  })
  output$d4 <- DT::renderDataTable({
    datatable(head(mtcars))
  })
}
shinyApp(ui, server)