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
  titlePanel("EDA for single and mult variables"),
  
  # Sidebar with a slider input
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Select plot type:",
        choices = c('hist_single',
                    'box_single',
                    'box_compare',
                    "scatter",
                    "pairwise",
                    "geom_bin2d",
                    "colorplot",
                    "jitter",
                    "hist_density",
                    "stackedbar",
                    "sidebysidebar",
                    "filledbar",
                    "overlayingbar"
      )),
      conditionalPanel(
        condition = "input.plot_type == 'hist_single'",
        selectInput(inputId = "var", label = "Choose death cause", choices = names(df[, -c(1:3)]))
      ),
      conditionalPanel(
        condition = "input.plot_type == 'box_single'",
        selectInput(inputId = "con", label = "Choose country", choices = unique(df$Entity)),
        selectInput(inputId = "var", label = "Choose death cause", choices = names(df[, -c(1:3)]))
      ),
      conditionalPanel(
        condition = "input.plot_type == 'pairwise'",
        pickerInput(
          inputId = "selected_attributes",
          label = "Select attributes:",
          choices = names(a),
          multiple = TRUE,
          options = list('actions-box' = TRUE)
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'scatter'",
        selectInput("x_attr", "Select x-axis attribute:", names(a)),
        selectInput("y_attr", "Select y-axis attribute:", names(a))
      ),
      conditionalPanel(
        condition = "input.plot_type == 'geom_bin2d'",
        sliderInput('alp', 'Transparency', min = 0.1, max = 1.0, value = 0.35)
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot", height = '400px', width = '600px')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # in the server:
  output$plot <- renderPlot({
    if(input$plot_type == "hist_single"){
      ggplot(df) +
        geom_histogram(aes_string(x = input$var), bins = 40, fill = "gray") +
        labs(x = input$var, y = "Frequency", subtitle = 'Frequency per cause for all countries')
    }
    else if (input$plot_type == "box_single"){
      data <- df[df['Entity']==input$con,] #df[df['Entity']=='China',]
      boxplot(data[input$var], xlab=input$var, ylab = 'Count')
    }
    else if (input$plot_type == "pairwise") {
      selected_attrs <- input$selected_attributes
      if (length(selected_attrs) < 2) {
        return(NULL)
      }
      pairs(a[, selected_attrs])
      
    } else if(input$plot_type == "scatter") {
      x_attr <- input$x_attr
      y_attr <- input$y_attr
      ggplot(a, aes_string(x = x_attr, y = y_attr)) +
        geom_point()+geom_smooth()
      
    } else if(input$plot_type == "geom_bin2d"){
      x_attr <- input$x_attr
      y_attr <- input$y_attr
      alp <- input$alp
      ggplot(a, aes_string(x=x_attr, y=y_attr) ) +
        geom_bin2d(alpha=alp)
      
    } else if(input$plot_type == "colorplot") {
      ggplot(a, aes(x=residual.sugar, y=density, colour = quality.mod)) +
        geom_point(size=2.0) +
        #geom_point() +
        facet_wrap(~quality.mod, ncol = 3)
      # to separate scatter points in different graphs, 1 vector -> 2 dimensional
      
    } else if(input$plot_type == "jitter") {
      ggplot(a, aes(x=residual.sugar, y=density, colour = quality.mod)) +
        geom_jitter(width=0.2, height=0, alpha=0.5) +
        facet_wrap(~quality.mod, ncol = 3)
      # to separate scatter points in different graphs, 1 vector -> 2 dimensional
      
    } else if(input$plot_type == "hist_density"){
      ggplot(a, aes(x=residual.sugar, y=..density..)) +
        geom_histogram(binwidth = 1) +
        geom_density(colour = "blue", alpha = 0.5)
    }
    else if(input$plot_type == "stackedbar") {
      ggplot(a, aes(x=density_new, fill = quality.mod)) +
        geom_bar()
      # no extra param produces stacked bar chart,
      # to see distribution of values in categories
      
    } else if(input$plot_type == "sidebysidebar") {
      ggplot(a, aes(x=density_new, fill = quality.mod)) +
        geom_bar(position = 'dodge')
      # 'dodge' produces side-by-side chart,
      # to compare count of values across categories
      
    } else if(input$plot_type == "filledbar"){
      ggplot(a, aes(x=density_new, fill = quality.mod)) +
        geom_bar(position = 'fill')
      # fill the whole column,
      # to see ratios of one value over the other value
    } else if(input$plot_type == "overlayingbar"){
      ggplot(a, aes(x=density_new, fill = quality.mod, alpha = 0.15)) +
        geom_bar(position='identity')
      # fill the whole column,
      # to see ratios of one value over the other value
    }
  })
}



# Run the application
shinyApp(ui = ui, server = server)

