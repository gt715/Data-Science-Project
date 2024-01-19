

library(shiny)
library(ggplot2)


source("R/data_cleaning.R")
source("R/kmeans_analysis.R")
source("R/decision_tree_analysis.R")
source("R/visualization.R")
source("R/functions.R")


main_data <- read.csv("C:\\Games\\DB project\\Laptop-Dataset.csv")


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(class = "title-panel",
      HTML("<h1>Data Science</h1>"),
      windowTitle = "Data App"
  ),
  
  # Tabbed layout
  tabsetPanel(
    tabPanel("Data",
             fluidRow(
               column(3, textInput("limit_rows", "Limit Rows", value = "All")),
               column(12, br(), verbatimTextOutput("data_summary")),
               column(12, br(), dataTableOutput("main_table"))
             )
    ),
    
    tabPanel("Cleaning",
             fluidRow(
               column(3, textInput("limit_rows", "Limit Rows", value = "All")),
               column(12, br(), verbatimTextOutput("data_issue_message")),
               column(12, br(), verbatimTextOutput("clean_data_summary")),
               column(12, br(), dataTableOutput("cleaned_table"))
             )
    ),
    
    tabPanel("Visualization",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(12, selectizeInput("selected_visual", "Select Type of visual:", choices = c("Histogram", "Barplot", "Boxplot", "Dot"), selected = "Dot", multiple = FALSE)),
                   column(12, selectizeInput("selected_column_for_visual", "Select Column for visual:", choices = NULL, multiple = FALSE)),
                   column(12, actionButton("generate_visual", "generate visual"))
                 )
               ),
               mainPanel(
                 plotOutput("visualization_plot")
               )
             )
    ),
    
    tabPanel("K-Means",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(12, selectizeInput("selected_column", "Select Column for K-Means:", choices = NULL, multiple = FALSE)),
                   column(12, numericInput("kmeans_k", "Number of Clusters (K)", value = 3, min = 1)),
                   column(12, actionButton("run_kmeans", "Run K-Means"))
                 )
               ),
               mainPanel(
                   verbatimTextOutput("kmeans_result"),
                   plotOutput("kmeans_plot")
               )
             )
    ),
    
    tabPanel("Decision Tree",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(12, selectizeInput("selected_column_for_decision", "Select Output of decision:", choices = NULL, multiple = FALSE)),
                   column(12, actionButton("run_decision_tree", "Run decision tree"))
                 )
               ),
               mainPanel(
                 verbatimTextOutput("decision_tree_result"),
                 
                 plotOutput("decision_tree_plot", height = "800", width = 900)
               )
             )
    )
  )
)





server <- function(input, output, session) {

  # Main Data Tab
  output$main_table <- renderDataTable({
    limit_rows(main_data, input$limit_rows)
  }, options = list(scrollX = TRUE, autoWidth = FALSE))
  
  output$data_summary <- renderText({
    summary_data(main_data)
  })
  
  # Cleaning Tab
  cleaned_data <- reactive({
    limit_rows(clean_data(main_data), input$limit_rows)
  })
  
  output$cleaned_table <- renderDataTable({
    cleaned_data()
  }, options = list(scrollX = TRUE, autoWidth = FALSE))
  
  output$data_issue_message <- renderText({
    # Check for issues with the data
    return (display_issues(main_data))
  })
  
  output$clean_data_summary <- renderText({
    summary_data(clean_data(main_data))
  })
  
  # K-Means Tab
  
  observe({
    cleaned_data <- clean_data(main_data)
    determine_numeric_cols(session, cleaned_data)
    determine_decision_cols(session, cleaned_data)
  })
  
  observeEvent(input$run_kmeans, {
    kmeans_result <- perform_kmeans(clean_data(main_data), input$kmeans_k, input$selected_column)
    
    output$kmeans_result <- renderPrint({
      print(kmeans_result)  
    })
    
    output$kmeans_plot <- renderPlot({
      represent_kmeans(kmeans_result, clean_data(main_data))
    })
  })
  
  # Decision Tree Tab
  
  observeEvent(input$run_decision_tree, {
    decision_tree_result <- perform_decision_tree(clean_data(main_data), input$selected_column_for_decision)
    
    output$decision_tree_result <- renderPrint({
      print(decision_tree_result)  
    })
    
    output$decision_tree_plot <- renderPlot({
      rpart.plot(decision_tree_result)
    })
  })
  
  # Visualization Tab
  
  observe({
    selected_visual <- input$selected_visual
    determine_visual_cols(session, selected_visual)
  })
  
  observeEvent(input$generate_visual, {
    visiual <- perform_visualization(input$selected_visual, input$selected_column_for_visual)
    output$visualization_plot <- renderPlot({
      visiual
    })
  })
  
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
