library(shiny)
library(heatmaply)
library(plotly)

ui <- fluidPage(
  titlePanel("Make your own Combined Heatmap"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = ".csv"),
      actionButton("upload", "Upload File"),
      tags$p("Timestamp must be in format %d-%m-%y %H:%M"),
      
      checkboxGroupInput("variables", "Select your variables", choices = NULL),
      
      selectInput("missing_color", "Select colour of missing value :", 
                  choices = c("Grey", "White", "Black", "Red"), selected = "Grey"),
      
      checkboxGroupInput("colors", "Select range of colours", 
                         choices = c("White", "Navyblue", "Blue", "Cyan", 
                                     "Yellow", "Red", "Darkred", "Black"), 
                         selected = c("Red", "Darkred")),
      
      numericInput("scale_min", "Select variable scale minimum value :", 0),
      numericInput("scale_max", "Select variable scale maximum value :", 50),
      numericInput("legend_increment", "Select legend scale increment :", 10),
      
      selectInput("x_axis", "X-axis represented by :", 
                  choices = c("days", "months", "years")),
      
      numericInput("x_title_size", "Select size of X-axis title :", 10),
      numericInput("x_label_size", "Select size of X-axis label :", 10),
      numericInput("y_title_size", "Select size of Y-axis title :", 8),
      numericInput("y_label_size", "Select size of Y-axis label :", 7),
      
      radioButtons("date_format", "Choose your date format", 
                   choices = c("Abbreviated month - 2-digit year (Jan-17)", 
                               "Decimal month - 2-digit year (01-17)", 
                               "Decimal date - Abbreviated month (01-Jan)", 
                               "Decimal date - Decimal month - 2 digit year (01-01-17)", 
                               "Decimal date - Full month (01-January)", 
                               "Abbreviated weekday (Mon)", 
                               "Full weekday (Monday)", 
                               "Abbreviated month (Jan)", 
                               "Full month (January)", 
                               "Decimal date (01)", 
                               "Decimal month (01)", 
                               "2-digit year (17)", 
                               "4-digit year (2017)")),
      
      textInput("title", "Enter Title", "Title"),
      textInput("x_name", "Enter X-axis name", "X-axis"),
      textInput("y_name", "Enter Y-axis name", "Y-axis"),
      textInput("legend_name", "Enter Legend name", "Legend"),
      
      numericInput("img_width", "Select image width :", 1000),
      numericInput("img_height", "Select image height :", 600),
      numericInput("point_size", "Select image point size :", 15),
      
      downloadButton("download", "Download the plot")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Heatmap", plotlyOutput("heatmapPlot")),
        tabPanel("Data", tableOutput("table"))
      ),
      tags$hr(),
      tags$p(style = "color:red;", "Error: An error has occurred. Check your logs or contact the app author for clarification.")
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$upload, {
    req(input$file)
    
    df <- read.csv(input$file$datapath, header = TRUE)
    data(df)
    
    updateCheckboxGroupInput(session, "variables", choices = names(df), selected = names(df))
  })
  
  output$table <- renderTable({
    req(data())
    head(data(), 10)
  })
  
  output$heatmapPlot <- renderPlotly({
    req(data(), input$variables)
    
    df <- data()
    selected_vars <- df[, input$variables, drop = FALSE]
    
    heatmaply(
      as.matrix(selected_vars),
      main = input$title,
      xlab = input$x_name,
      ylab = input$y_name,
      colors = input$colors,
      scale_fill_gradient_fun = ggplot2::scale_fill_gradient(low = input$colors[1], high = input$colors[length(input$colors)]),
      fontsize_row = input$y_label_size,
      fontsize_col = input$x_label_size
    )
  })
  
  output$download <- downloadHandler(
    filename = function() { paste("heatmap", ".png", sep = "") },
    content = function(file) {
      g <- heatmaply(
        as.matrix(data()[, input$variables, drop = FALSE]),
        main = input$title,
        xlab = input$x_name,
        ylab = input$y_name,
        colors = input$colors,
        scale_fill_gradient_fun = ggplot2::scale_fill_gradient(low = input$colors[1], high = input$colors[length(input$colors)]),
        fontsize_row = input$y_label_size,
        fontsize_col = input$x_label_size
      )
      
      plotly_IMAGE(g, format = "png", out_file = file, width = input$img_width, height = input$img_height)
    }
  )
}

shinyApp(ui = ui, server = server)
