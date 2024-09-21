# EDA Project for CITS4009, compiled by Sifeng Xu, 24525488

# Load  libraries
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gridExtra)
library(knitr)

# load dataset
df <- read.csv("./Countries and death causes.csv", header = T, sep=",")

# data.frame(
#   colID = col(df),
#   variables = names(df),
#   class = sapply(df, typeof),
#   first_values = sapply(df, function(x) paste0(head(x), collapse = ', ')),
#   row.names = NULL) |>
# kable()

summary(df)
str(df)

df_2010 <- df[df['Year']==2010, 
              c('Entity', 'Outdoor.air.pollution', 'Smoking', 'Drug.use')]

# df_2010[sample(nrow(df_2010), 6),]

sc <- c('Afghanistan', 'Morocco', 'Nigeria', 'Australia', 'Belgium', 'India')

selected <- subset(df_2010, Entity %in% sc)
names(selected)

# plot
df_belgium <- df[df$Entity=='Belgium',]
summary(df_belgium$Outdoor.air.pollution)

ggplot(df_belgium, aes(x=Outdoor.air.pollution, y=..density..)) +
  geom_histogram(bins = 40, color='gray') +
  geom_density(colour = "blue", alpha = 0.5)


# Define the UI
ui <- fluidPage(
  titlePanel("Histogram Visualization"),    
  sidebarLayout(
    sidebarPanel(
      #selectInput(inputId = "country", label = "Choose x:", choices = unique(df['Entity']),
      selectInput("x1", "Choose variable:", choices = names(df_belgium))
    ),    
    mainPanel(
      plotOutput(outputId = "histogram")
    )
  )
)

server <- function(input, output) {
  generate_histogram <- function(data, x_var, title) {
    ggplot(data, aes(x = x_var)) + 
      geom_histogram(bins = 20, color = "gray") + 
      geom_density(colour = "blue", alpha = 0.5) +
      labs(title = title, x = x_var, y = "Frequency")
  }
  # Render the side-by-side histograms    
  output$histogram <- renderPlot({
    generate_histogram(df_belgium, input$x1, colnames(input$x1))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
