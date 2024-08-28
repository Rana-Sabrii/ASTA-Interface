library(shiny)
library(data.table)
library(MOFA2)
library(readr)
library(shinyBS)  # Add shinyBS for tooltips
library(ggplot2) # Add ggplot2 for theme

# Define UI (user interface)
ui <- fluidPage(
  titlePanel("Train Your Multi-Omics dataset using Multi Omics Factor Analysis"),
  sidebarLayout(
    sidebarPanel(
      h2("Notes:"),
      p("- Minimum number of samples required for each file is 30."),
      p("- Make sure the Sample IDs in all files are matching."),
      
      numericInput("num_files", "Number of Input Files (at least 2):", value = 2, min = 2),
      
      uiOutput("fileInputs"), # Dynamically generated file input area
      actionButton("start_training", "Start Training"),
      
      downloadButton("downloadModel", "Download MOFA Model"),
      
      downloadButton("downloadPlot", "Download Data Overview Plot"),
      
      h2("MOFA Options (Advanced)"),
      p("Note: Only change the default model options if you are familiar with the underlying mathematical model!"),
      
      h4("Data Options"),
      checkboxInput("scale_views", "Scale Views", value = FALSE),
      
      bsTooltip("scale_views", "If groups have different ranges/variances, it is good practice to scale each group to unit variance. Default is FALSE", placement = "right", trigger = "hover"),
      checkboxInput("scale_groups", "Scale Groups", value = FALSE),
      
      bsTooltip("scale_groups", "If views have different ranges/variances, it is good practice to scale each view to unit variance. Default is FALSE", placement = "right", trigger = "hover"),
      
      h4("Model Options"),
      selectInput("likelihoods", "Likelihoods:", choices = c("Auto", "gaussian", "poisson", "bernoulli"), selected = "Auto"),
      
      bsTooltip("likelihoods", "Likelihood per view - options are 'gaussian', 'poisson', 'bernoulli'. By default, they are learned automatically. We advise users to use 'gaussian' whenever possible!", placement = "right", trigger = "hover"),
      numericInput("num_factors", "Number of Factors:", value = 7, min = 1),
      
      h4("Training Options"),
      numericInput("maxiter", "Max Iterations:", value = 1000, min = 1),
      bsTooltip("maxiter", "Number of iterations. Default is 1000.", placement = "right", trigger = "hover"),
      
      selectInput("convergence_mode", "Convergence Mode:", choices = c("fast", "medium", "slow"), selected = "fast"),
      bsTooltip("convergence_mode", "'fast', 'medium', 'slow'. For exploration, the fast mode is good enough.", placement = "right", trigger = "hover"),
      
      numericInput("seed", "Random Seed:", value = 42, min = 1)
    ),
    mainPanel(
      
      h3("MOFA Object Summary"),
      verbatimTextOutput("mofa_summary"),
      
      
      plotOutput("data_overview"),
      
      
      # Table outputs
      h4("Data Options Table"),
      tableOutput("data_opts_table"),
      
      
      h4("Model Options Table"),
      tableOutput("model_opts_table"),
      
      
      h4("Training Options Table"),
      tableOutput("train_opts_table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Dynamically generate file input and text input UI elements
  output$fileInputs <- renderUI({
    num_files <- input$num_files
    lapply(1:num_files, function(i) {
      fluidRow(
        column(6, selectInput(paste0("omics_type", i), paste("Enter Omics Type", i, ":"), 
                              choices = c("Genome", "Transcriptome", "Metabolome", 
                                          "Cytokine", "Microbiome", "Gut 16s", "Nares 16s","Proteome"))),
        
        column(6, fileInput(paste0("file", i), paste("Upload Data File", i, "(CSV):")))
      )
    })
  })
  
  # Read input data (reactively)
  data_list <- reactive({
    req(input$num_files)
    lapply(1:input$num_files, function(i) {
      req(input[[paste0("file", i)]])
      read.csv(input[[paste0("file", i)]]$datapath, row.names = 1)
    })
  })
  
  
  # Reactive value to store the trained model
  trained_model <- reactiveVal(NULL)
  
  # Observe event when "Start Training" button is clicked
  observeEvent(input$start_training, {
    # Check for minimum samples in each file
    req(all(sapply(data_list(), nrow) >= 30))
    
    # Create MOFA data structure with dynamic names
    data <- lapply(1:input$num_files, function(i) {
      data.matrix(data_list()[[i]])
    })
    names(data) <- sapply(1:input$num_files, function(i) input[[paste0("omics_type", i)]])
    
    # Create and prepare MOFA object
    MOFAobject <- create_mofa(data)
    
    # Get default options and update with user inputs
    data_opts <- get_default_data_options(MOFAobject)
    model_opts <- get_default_model_options(MOFAobject)
    train_opts <- get_default_training_options(MOFAobject)
    
    # Update Data Options
    data_opts$scale_views <- input$scale_views
    data_opts$scale_groups <- input$scale_groups
    
    # Update Model Options
    likelihoods <- strsplit(input$likelihoods, ",")[[1]]
    if (length(likelihoods) == 1 && likelihoods[1] != "Auto") {
      model_opts$likelihoods <- rep(likelihoods[1], length(data))
    } else if (length(likelihoods) == length(data)) {
      model_opts$likelihoods <- likelihoods
    } 
    model_opts$num_factors <- input$num_factors
    
    # Update Training Options
    train_opts$maxiter <- input$maxiter
    train_opts$convergence_mode <- input$convergence_mode
    train_opts$seed <- input$seed
    
    # Prepare MOFA object with updated options
    MOFAobject <- prepare_mofa(
      object = MOFAobject,
      data_options = data_opts,
      model_options = model_opts,
      training_options = train_opts
    )
    
    # Train MOFA model using basilisk
    withProgress(message = 'Training MOFA model', value = 0, {
      MOFAobject.trained <- run_mofa(MOFAobject, outfile = "model.hdf5", use_basilisk = TRUE) 
    })
    
    # Store the trained model in the reactive value 
    trained_model(MOFAobject.trained) 
    
    # Output: MOFA summary
    output$mofa_summary <- renderPrint({
      print(MOFAobject.trained)
    })
    
    # Output: Data overview plot
    output$data_overview <- renderPlot({
      m <- MOFAobject.trained  # Use a shorter name for clarity
      
      # Use custom color palette
      shiny_palette <- c("#948979", "#3C5B6F", "#DFD0B8", "#B5C18E" ,"#DFDFDF" ,"#B99470" ,"#DBB5B5")
      n_views <- get_dimensions(m)$M 
      if (n_views <= length(shiny_palette)) {
        shiny_colours <- shiny_palette[seq_len(n_views)]
      } else {
        shiny_colours <- rainbow(n_views) 
      }
      names(shiny_colours) <- views_names(m)
      
      plot_data_overview(m, colors = shiny_colours) +
        theme(
          strip.text.x = element_text(size = 16, colour = "#333333"),
          axis.text.y = element_text(size = 16, colour = "#333333")
        )
    })
    
    # Output: Data options table (modified)
    output$data_opts_table <- renderTable({
      data_opts[c("scale_views", "scale_groups", "views", "groups")]
    })
    
    # Output: Model options table (modified)
    output$model_opts_table <- renderTable({
      model_opts[c("likelihoods", "num_factors")]
    })
    
    # Output: Training options table (modified)
    output$train_opts_table <- renderTable({
      train_opts[c("maxiter", "convergence_mode", "drop_factor_threshold", "startELBO", "freqELBO")]
    })
  })
  
  # Download handler for the model
  output$downloadModel <- downloadHandler(
    filename = function() { "model.hdf5" },
    content = function(file) {
      file.copy(file.path(getwd(), "model.hdf5"), file)
    }
  )
  
  # Download handler for the plot (Modified)
  output$downloadPlot <- downloadHandler(
    filename = function() { "data_overview_plot.png" },
    content = function(file) {
      
      # Use custom color palette (Same logic as in your data_overview output)
      shiny_palette <- c("#948979", "#3C5B6F", "#DFD0B8", "#B5C18E" ,"#DFDFDF" ,"#B99470" ,"#DBB5B5")
      n_views <- get_dimensions(trained_model())$M 
      if (n_views <= length(shiny_palette)) {
        shiny_colours <- shiny_palette[seq_len(n_views)]
      } else {
        shiny_colours <- rainbow(n_views)  # Fallback if more views than colors
      }
      names(shiny_colours) <- views_names(trained_model())
      
      png(file) 
      print(plot_data_overview(trained_model(), colors = shiny_colours))
      dev.off()
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)