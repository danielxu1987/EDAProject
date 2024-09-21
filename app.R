# This is the shiny app to assist data exploration for EDA project in unit CITS4009
# Made by Sifeng Xu, 24525844

# Load  libraries
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gridExtra)
library(knitr)

# load dataset
df <- read.csv("./Countries and death causes.csv", header = T, sep=",")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("EDA for single variable"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "con", label = "Choose country", choices = unique(df$Entity)),
          selectInput(inputId = "var", label = "Choose death cause", choices = names(df[, -c(1:3)]))
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("histogram")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    generate_histogram <- function(data, con, x_var, title) {
      data <- data[data$Entity == con,]
      ggplot(data, aes_string(x = x_var)) +
        geom_histogram(bins = 20, fill = "gray") +
        labs(title = title, x = x_var, y = "Frequency", subtitle = 'Frequency per cause per country')
    }
    # Render the side-by-side histograms
    output$histogram <- renderPlot({
      generate_histogram(df, input$con, input$var, colnames(input$var))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
