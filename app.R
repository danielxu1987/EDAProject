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


### Single histogram

# # Define UI for application that draws a histogram
# ui <- fluidPage(
#   
#   # Application title
#   titlePanel("Histgram for single variable"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#       # selectInput(inputId = "con", label = "Choose country", choices = unique(df$Entity)),
#       selectInput(inputId = "x", label = "Choose death cause", choices = names(df[, -c(1:3)])),
#       # selectInput(inputId = "var2", label = "Choose death cause2", choices = names(df[, -c(1:3)]))
#     ),
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotOutput("histogram")
#     )
#   )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#   generate_histogram <- function(data, var, title) {
#     ggplot(data) +
#       geom_histogram(aes_string(x = var), bins = 40, fill = "gray") +
#       labs(title = title, x = var, y = "Frequency", subtitle = 'Frequency per cause for all countries')
#   }
#   # Render the side-by-side histograms
#   output$histogram <- renderPlot({
#     generate_histogram(df, input$x, colnames(input$var))
#   })
# }

# # Run the application 
# shinyApp(ui = ui, server = server)

##Single boxplot
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Boxplot for single death cause per country"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "con", label = "Choose country", choices = unique(df$Entity)),
      selectInput(inputId = "var", label = "Choose death cause", choices = names(df[, -c(1:3)])),
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("boxplot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Render the side-by-side histograms
  output$boxplot <- renderPlot({
    data <- df[df['Entity']==input$con,] #df[df['Entity']=='China',]
    boxplot(data[input$var], xlab=input$var, ylab = 'Count')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# ## Two variables histogram
# # Define UI for application that draws a histogram
# ui <- fluidPage(
#   
#   # Application title
#   titlePanel("EDA for single variable"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#       selectInput(inputId = "con", label = "Choose country", choices = unique(df$Entity)),
#       selectInput(inputId = "var1", label = "Choose death cause1", choices = names(df[, -c(1:3)])),
#       selectInput(inputId = "var2", label = "Choose death cause2", choices = names(df[, -c(1:3)]))
#     ),
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotOutput("histogram")
#     )
#   )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#   generate_histogram <- function(data, con, x_var1, x_var2, title) {
#     data <- data[data$Entity == con,]
#     
#     p1 <- ggplot(data, aes_string(x = x_var1)) +
#       geom_histogram(bins = 20, fill = "gray") +
#       labs(title = title, x = x_var1, y = "Frequency", subtitle = 'Frequency per cause per country')
#     
#     p2 <- ggplot(data, aes_string(x = x_var2)) +
#       geom_histogram(bins = 20, fill = "gray") +
#       labs(title = title, x = x_var2, y = "Frequency", subtitle = 'Frequency per cause per country')
#     
#     grid.arrange(p1, p2, nrow = 1)
#   }
#   # Render the side-by-side histograms
#   output$histogram <- renderPlot({
#     generate_histogram(df, input$con, input$var1, input$var2, colnames(input$var))
#   })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
