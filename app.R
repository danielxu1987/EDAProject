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

dataUK <- df[df['Entity']=='United Kingdom',] #df[df['Entity']=='China',]
dataTAN <- df[df['Entity']=='Tanzania',]
dataNLD <- df[df['Entity']=='Netherlands',]
dataCAM <- df[df['Entity']=='Cambodia',]

num_vars <- sort(names(df[, -c(1:3)]))
countries <- unique(df$Entity)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("EDA Project single-multi variables"),
  
  # Sidebar with a slider input
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Select plot type:",
        choices = c('hist_single',
                    'box_single',
                    'box_compare',
                    'trend_compare',
                    'bar_compare',
                    "pairwise",
                    "scatter",
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
        selectInput(inputId = "var", label = "Choose death cause", choices = num_vars)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'box_single'",
        selectInput(inputId = "con", label = "Choose country", choices = countries),
        selectInput(inputId = "var", label = "Choose death cause", choices = num_vars)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'box_compare'",
        selectInput(inputId = "var", 
          label = "Choose death cause", choices = c('Unsafe.water.source', 'Unsafe.sanitation', 'No.access.to.handwashing.facility')),
      ),
      conditionalPanel(
        condition = "input.plot_type == 'trend_compare'",
        selectInput(inputId = "var", label = "Choose death cause", choices = num_vars),
      ),
      conditionalPanel(
        condition = "input.plot_type == 'bar_compare'",
        selectInput(inputId = "year", label = "Choose year", choices = unique(df$Year)),
        selectInput(inputId = "var", label = "Choose death cause", choices = num_vars),
      ),
      conditionalPanel(
        condition = "input.plot_type == 'pairwise'",
        selectInput(inputId = "con", label = "Choose country", choices = countries),
        
        pickerInput(
          inputId = "selected_attributes",
          label = "Select attributes:",
          choices = names(df[, -c(1:3)]),
          multiple = TRUE,
          options = list('actions-box' = TRUE)
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'scatter'",
        #selectInput(inputId = "con", label = "Choose country", choices = countries),
        selectInput("x_attr", "Select x-axis attribute:", num_vars),
        selectInput("y_attr", "Select y-axis attribute:", num_vars)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'geom_bin2d'",
        #selectInput(inputId = "con", label = "Choose country", choices = countries),
        selectInput("x_attr", "Select x-axis attribute:", num_vars),
        selectInput("y_attr", "Select y-axis attribute:", num_vars),
        sliderInput('alp', 'Transparency', min = 0.1, max = 1.0, value = 0.35)
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot", height = '600px', width = '1000px')
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
    else if (input$plot_type == 'box_compare'){
      sel_var <- input$var
      
      p1 <- ggplot(dataUK) + geom_boxplot(aes_string(y=sel_var)) +
        labs(x = input$var, y= 'Count', title = 'UK')
      p2 <- ggplot(dataTAN) + geom_boxplot(aes_string(y=sel_var)) +
        labs(x = input$var, y= 'Count', title = 'Tanzania')
      p3 <- ggplot(dataNLD) + geom_boxplot(aes_string(y=sel_var)) +
        labs(x = input$var, y= 'Count', title = 'Netherlands')
      p4 <- ggplot(dataCAM) + geom_boxplot(aes_string(y=sel_var)) +
        labs(x = input$var, y= 'Count', title = 'Cambodia')
      
      grid.arrange(p1, p2, p3, p4, nrow=2)
    }
    else if(input$plot_type == 'trend_compare'){
      sel_var <- input$var
      
      p1 <- ggplot(dataUK, aes_string(x ='Year', y=sel_var)) + 
        geom_line(color='gray', size=2) + 
        labs(title = 'UK')
      p2 <- ggplot(dataTAN, aes_string(x ='Year', y=sel_var)) + 
        geom_line(color='gray', size=2) + 
        labs(title = 'Tanzania')
      p3 <- ggplot(dataNLD, aes_string(x ='Year', y=sel_var)) + 
        geom_line(color='gray', size=2) + 
        labs(title = 'Netherlands')
      p4 <- ggplot(dataCAM, aes_string(x ='Year', y=sel_var)) + 
        geom_line(color='gray', size=2) + 
        labs(title = 'Cambodia')
      
      grid.arrange(p1, p2, p3, p4, nrow=2)
    }
    else if(input$plot_type == 'bar_compare'){
      df_year <- df[df$Year==input$year,]
      sam_year <- df_year[sample(nrow(df_year), 20), ]
      # sel_var <- input$var
      rr_trans <- transform(sam_year, Entity = reorder(Entity, Drug.use))
      ggplot(rr_trans, aes(x = Entity, y = Drug.use)) +
        geom_bar(stat = "identity", fill = 'red') + 
        coord_flip()
    }
    else if (input$plot_type == "pairwise") {
      data <- df[df['Entity']==input$con,]
      selected_attrs <- input$selected_attributes
      if (length(selected_attrs) < 2) {
        return(NULL)
      }
      pairs(data[, selected_attrs], main = input$con)
      
    } 
    else if(input$plot_type == "scatter") {
      #data <- df[df['Entity']==input$con,]
      x_attr <- input$x_attr
      y_attr <- input$y_attr
      ggplot(df, aes_string(x = x_attr, y = y_attr)) +
        geom_jitter(width=0.6, height=3, alpha=0.5) + 
        geom_smooth() +
        xlim(1, 2500) +
        ylim(1, 1000)
    } 
    else if(input$plot_type == "geom_bin2d"){
      #data <- df[df['Entity']==input$con,]
      x_attr <- input$x_attr
      y_attr <- input$y_attr
      alp <- input$alp
      ggplot(df, aes_string(x=x_attr, y=y_attr) ) +
        geom_bin2d(alpha=alp)
      
    } 
    # else if(input$plot_type == "colorplot") {
    #   ggplot(df, aes(x=residual.sugar, y=density, colour = quality.mod)) +
    #     geom_point(size=2.0) +
    #     #geom_point() +
    #     facet_wrap(~quality.mod, ncol = 3)
    #   # to separate scatter points in different graphs, 1 vector -> 2 dimensional
    #   
    # } 
    # else if(input$plot_type == "jitter") {
    #   ggplot(df, aes(x=residual.sugar, y=density, colour = quality.mod)) +
    #     geom_jitter(width=0.2, height=0, alpha=0.5) +
    #     facet_wrap(~quality.mod, ncol = 3)
    #   # to separate scatter points in different graphs, 1 vector -> 2 dimensional
    #   
    # } 
    # else if(input$plot_type == "hist_density"){
    #   ggplot(df, aes(x=residual.sugar, y=..density..)) +
    #     geom_histogram(binwidth = 1) +
    #     geom_density(colour = "blue", alpha = 0.5)
    # }
    else if(input$plot_type == "stackedbar") {
      ggplot(df, aes(x=density_new, fill = quality.mod)) +
        geom_bar()
      # no extra param produces stacked bar chart,
      # to see distribution of values in categories
      
    } 
    else if(input$plot_type == "sidebysidebar") {
      ggplot(df, aes(x=density_new, fill = quality.mod)) +
        geom_bar(position = 'dodge')
      # 'dodge' produces side-by-side chart,
      # to compare count of values across categories
      
    } 
    else if(input$plot_type == "filledbar"){
      ggplot(df, aes(x=density_new, fill = quality.mod)) +
        geom_bar(position = 'fill')
      # fill the whole column,
      # to see ratios of one value over the other value
    } 
    else if(input$plot_type == "overlayingbar"){
      ggplot(df, aes(x=density_new, fill = quality.mod, alpha = 0.15)) +
        geom_bar(position='identity')
      # fill the whole column,
      # to see ratios of one value over the other value
    }
  })
}



# Run the application
shinyApp(ui = ui, server = server)

