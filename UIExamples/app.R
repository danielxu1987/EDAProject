library(shiny)

# Sample data for demonstration
set.seed(123)
data <- rnorm(100)

ui <- fluidPage(
  tabsetPanel(
    id = "tabs",
    tabPanel("EDA",
        sidebarLayout(
          sidebarPanel(
            selectInput("edaOption", "Select an option for EDA:",
                             choices = c("hist", "boxplot"))
        ),
        mainPanel(
          plotOutput("edaPlot")
        )
      )
    ),
    tabPanel("ML",
      sidebarLayout(
        sidebarPanel(
          selectInput("mlOption", "Select an option for ML:",
                         choices = c("auc", "precision"))
        ),
        mainPanel(
          plotOutput("mlPlot")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$edaPlot <- renderPlot({
    #req(input$edaOption)  # Ensure input is available
    if (input$edaOption == "hist") {
      hist(data, main = "Histogram", xlab = "Values", col = "lightblue")
    } else if (input$edaOption == "boxplot") {
      boxplot(data, main = "Boxplot", xlab = "Values", col = "lightgreen")
    }
  })
  
  output$mlPlot <- renderPlot({
    #req(input$mlOption)  # Ensure input is available
    if (input$mlOption == "auc") {
      plot(1:10, cumsum(runif(10)), type = "l", main = "Line Curve", 
           xlab = "X-axis", ylab = "Cumulative Sum", col = "red")
    } else if (input$mlOption == "precision") {
      plot(1:10, runif(10, min = 0, max = 1), type = "l", main = "Precision Curve", 
           xlab = "X-axis", ylab = "Precision", col = "blue")
    }
  })
}

shinyApp(ui, server)
