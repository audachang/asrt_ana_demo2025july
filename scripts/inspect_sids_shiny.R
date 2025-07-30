require(shiny)

source('asrt_ana_functions.R')

# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tasktype", "Choose a task type", 
                  choices = c('motor', 'percept')),
      selectInput("unitx", "Choose the x unit", 
                  choices = c('block', 'epoch')),
      uiOutput("dynamicUI")  # dynamic subject selector
    ),
    mainPanel(
      fluidRow(
        column(8, plotOutput("rtPlot", height = "300px")),
        column(4, plotOutput("reportPlot", height = "300px"))
      ),
      fluidRow(
        column(6, plotOutput("accPlot", height = "300px")),
        column(6, plotOutput("learningPlot", height = "300px"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # --- Reactive: File and data loading ---
  d <- reactive({
    req(input$sid, input$tasktype)
    
    finfo <- list_file(input$tasktype)     # returns tibble (file + sid)
    req(nrow(finfo) > 0)
    
    # Get file path for selected subject
    fpth <- finfo$file[finfo$sid == input$sid]
    req(length(fpth) == 1 && file.exists(fpth))
    
    # Full preprocessing pipeline
    import_d(fpth, input$tasktype, input$sid)
  })  
  
  # --- Reactive: Add condition columns ---
  d_add_cond <- reactive({
    add_condition_cols(d(), input$tasktype)
  })
  
  # --- Reactive: Summarize cleaned data ---
  d_summary <- reactive({
    d_freq <- assign_freq(d_add_cond())
    d_cleaned <- clean_data(d_freq)
    create_summ4plot(d_cleaned, input$unitx)
  })
  
  # --- Reactive: Sequence report analysis ---
  seqreport <- reactive({
    report <- extr_stiseq_respseq(d(), d_add_cond()) # extract sti & response seq
    dfresp <- report$reportseq
    stiseq <- report$stiseq
    
    # Compute max repeated rotation info per block
    dfresp %>%
      group_by(blockID) %>%
      summarise(
        rotation_info = list(max_repeated_rotation_in_respseq(stiseq, seq_report_key.keys)),
        .groups = 'drop'
      ) %>%
      mutate(
        max_match_count = map_int(rotation_info, "max_match_count"),
        best_rotation = map(rotation_info, "best_rotation"),
        element_match_count = map_int(rotation_info, "element_match_count")
      ) %>%
      select(-rotation_info)
  })
  
  # --- Dynamic UI: subject list ---
  output$dynamicUI <- renderUI({
    finfo <- list_file(input$tasktype)
    selectInput("sid", "Choose a participant:", 
                choices = finfo$sid)  # tibble column name changed to sid
  })
  
  # --- Plot: Reaction Time curve ---
  output$rtPlot <- renderPlot({
    d_summ <- d_summary()
    finfo <- list_file(input$tasktype)
    
    asrt_plot(input$unitx, 
              input$tasktype, 
              d_summ, 
              finfo, 
              input$sid, 
              show_legend = TRUE, 
              legend_text_size = 12)
  })
  
  # --- Plot: Accuracy curve ---
  output$accPlot <- renderPlot({
    d_summ <- d_summary()
    finfo <- list_file(input$tasktype)
    
    asrt_acc_plot(input$unitx, 
                  input$tasktype, 
                  d_summ, 
                  finfo, 
                  input$sid,
                  show_legend = FALSE, 
                  legend_text_size = 10)
  })
  
  # --- Plot: Learning metrics (sequence vs statistical) ---
  output$learningPlot <- renderPlot({
    finfo <- list_file(input$tasktype)
    learning_res <- compute_learning_metrics(d_summary())
    
    plot_combined_learning_metrics(
      learning_res, 
      finfo, input$sid, input$tasktype,
      unitx = input$unitx, 
      fontsize = 12, show_legend = TRUE, 
      legend_text_size = 10
    )
  }) 
  
  # --- Plot: Sequence report match count ---
  output$reportPlot <- renderPlot({
    finfo <- list_file(input$tasktype)
    plot_element_match_count(seqreport(), finfo, input$sid, input$tasktype)
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)
