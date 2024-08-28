library(shiny)
library(shinyWidgets)
library(tools)
library(ggplot2)
library(ggpubr)
library(MOFA2)
library(readr)
library(Matrix)
library(irlba)
library(tidyr) # Add tidyr library for pivot_wider

options(shiny.maxRequestSize = 1500*1024^2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Downstream Analysis your MOFA Model"),
  
  
  # Sidebar with main model parameters choices
  sidebarLayout(
    sidebarPanel(
      
      # Model file picker (HDF5 or RDS file)
      fileInput(inputId = "model_file",
                label = "Model file (.hdf5):",
                buttonLabel = "Model file",
                accept = c("hdf5")),
      uiOutput("viewsChoice"),
      #uiOutput("groupsChoice"),
      uiOutput("factorsChoice"),
      uiOutput("colourChoice"),
      
      width = 3
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        
        tabPanel("Data overview",
                 p("", class="description"),
                 plotOutput("dataOverviewPlot")
        ),
        
        tabPanel("Variance",
                 p("", class="description"),
                 # TODO: selector x, y
                 # TODO: selector group_by
                 # TODO: embed multiple plots on one page
                 plotOutput("varianceExplainedPlot")
        ),
        
        tabPanel("Weights",
                 p("Visualize factor weights as a first step to interpret factors.", class="description"),
                 fluidRow(
                   column(2, uiOutput("weightsViewSelection")),
                   
                   column(3, sliderInput(inputId = "nfeatures_to_label",
                                         label = "Number of top features to label:",
                                         min = 0,
                                         max = 100,
                                         value = 10,
                                         step = 1)),
                   column(3, uiOutput("weightsFeatureSelection"))
                 ),
                 plotOutput("weightsPlot"),
                 
                 p("Top features per factor in the current view are displayed below:", class="description"),
                 
                 plotOutput("topWeightsPlot")
        ),
        tabPanel("Data Heatmap",
                 p("Explore factors one by one by plotting original data values for their top weights.", class="description"),
                 fluidRow(
                   column(2, uiOutput("dataFactorSelection")),
                   column(2, uiOutput("dataViewSelection")),
                   column(3, sliderInput(inputId = "nfeatures_to_plot",
                                         label = "Number of top features to plot:",
                                         min = 0,
                                         max = 100,
                                         value = 10,
                                         step = 1)),
                   column(3, uiOutput("dataFeatureSelection"))
                 ),
                 plotOutput("dataHeatmapPlot"),
                 
                 
                 #plotOutput("dataScatterPlot")
        ),
        # tabPanel("Factors Violin",
        #          p("Visualize factor values and explore their distribution in different sets of samples", class="description"),
        #          # fluidRow(
        #          #   column(2, uiOutput("factorsAxisChoice_x"))
        #          #   
        #          # 
        #          # ),
        #          
        #          fluidRow(
        #            column(1, switchInput(inputId = "factorsAddDots", label = "Points", value = TRUE),
        #                   style = "margin-top: 25px; margin-right: 25px;"),
        #            
        #            column(2, sliderInput(inputId = 'factorsDotSize', label = 'Point size', value = 2, min = 1, max = 8, step = .5),
        #                   style = "margin-right: 5px;"),
        #            
        #            column(1, switchInput(inputId = "factorsAddViolins", label = "Violins", value = FALSE),
        #                   style = "margin-top: 25px; margin-right: 25px;"),
        #            
        #            # column(2, sliderInput(inputId = 'factorsViolinAlpha', label = 'Violin alpha', value = 1, min = .1, max = 1, step = .1),
        #            #        style = "margin-right: 5px;")
        #          ),
        #          hr(),
        #          plotOutput("factorsPlot")
        # ),
        tabPanel("Factors scatter",
                 p("Visualize pairs of factors to study how they separate different sets of samples.", class="description"),
                 fluidRow(
                   column(2, uiOutput("factorChoice_x")),
                   
                   column(1, actionButton("swapEmbeddings", "",
                                          icon("exchange-alt"),
                                          style = "margin: 25px auto; display: flex;")),
                   
                   column(2, uiOutput("factorChoice_y")),
                   
                   column(2, sliderInput(inputId = 'factorDotSize', label = 'Point size', value = 2, min = 1, max = 8, step = .5)),
                   
                   column(2, sliderInput(inputId = 'factorDotAlpha', label = 'Point alpha', value = 1, min = .1, max = 1, step = .1),
                          style = "margin-right: 5px;")
                   
                   
                 ),
                 hr(),
                 
                 plotOutput("embeddingsPlot",
                            brush = brushOpts(id = "plot_factors", fill = "#aaa")),
                 
                 verbatimTextOutput("embeddingsInfo")
        ),
        
        tabPanel("Export",
                 p("Download extracted data and results from the MOFA model."),
                 fluidRow(
                   column(12, # Make the column span the entire row
                          fluidRow(
                            column(3, downloadButton("downloadFactors", "Download Inferred Factors"))
                          ),
                          fluidRow(
                            column(3, img(src = "Inferred Factors.png", height = 150, width = 150))
                          ),
                          fluidRow(
                            column(3, downloadButton("downloadWeights", "Download Inferred Weights of Features"))
                          ),
                          fluidRow(
                            column(3, img(src = "Inferred Weights of Features.png", height = 150, width = 150))
                          ),
                          fluidRow(
                            column(3, downloadButton("downloadData", "Download Observed Measurements "))
                          ),
                          fluidRow(
                            column(3, img(src = "Observed Data.png", height = 150, width = 150))
                          )
                   )
                 )
        )
      ),
      width = 9
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  ###########################
  ### REACTIVE COMPONENTS ###
  
  ### GLOBAL VARIABLES ###
  
  model <- reactive({
    input_model <- input$model_file
    if (is.null(input_model)) return(NULL)
    print(file_ext(input_model$name))
    if (file_ext(input_model$name) %in% c("hdf5", "h5")) {
      load_model(input_model$datapath)
      
    } else if (file_ext(input_model$name) %in% c("rds", "RDS")) {
      readRDS(input_model$datapath)
    } else {
      return(NULL)
    }
  })
  
  factorsChoice <- reactive({
    m <- model()
    if (is.null(m)) return(NULL)
    factors_names(m)
  })
  
  viewsChoice <- reactive({
    m <- model()
    if (is.null(m)) return(NULL)
    views_names(m)
  })
  
  groupsChoice <- reactive({
    m <- model()
    if (is.null(m)) return(NULL)
    groups_names(m)
  })
  
  metaChoice <- reactive({
    m <- model()
    if (is.null(m)) return(NULL)
    metadata_names <- colnames(samples_metadata(m))
    metadata_names[metadata_names != "sample"]
  })
  
  metaAndFeatureChoice <- reactive({
    m <- model()
    if (is.null(m)) return(NULL)
    choices_names <- colnames(samples_metadata(m))
    choices_names <- choices_names[choices_names != "sample"]
    c(choices_names, features_names(m))
  })
  
  metaFeatureFactorChoice <- reactive({
    m <- model()
    if (is.null(m)) return(NULL)
    meta_names <- list()
    meta_names <- colnames(samples_metadata(m))
    meta_names <- meta_names[meta_names != "sample"]
    if (length(meta_names) > 1)
      # There's more information than just a group
      meta_names <- list("Metadata" = meta_names)
    c(meta_names,
      features_names(m),
      list("Factors" = factors_names(m)))
  })
  
  
  featuresChoice <- reactive({
    m <- model()
    if (is.null(m)) return(NULL)
    features_names(m)
  })
  
  
  
  groupsSelection <- reactive({
    if (is.null(input$groupsChoice))
      return("all")
    input$groupsChoice
  })
  
  factorsSelection <- reactive({
    if (is.null(input$factorsChoice))
      return("all")
    input$factorsChoice
  })
  
  colourSelection <- reactive({
    if (is.null(input$colourChoice))
      return("group_name")
    input$colourChoice
  })
  
  ### LOADINGS ###
  
  weightsViewSelection <- reactive({
    if (is.null(input$weightsViewSelection))
      return(1)
    input$weightsViewSelection
  })
  
  weightsFeatureSelection <- reactive({
    if (is.null(input$weightsFeatureSelection))
      return(NULL)
    input$weightsFeatureSelection
  })
  
  ### DATA ###
  
  dataFactorSelection <- reactive({
    if (is.null(input$dataFactorSelection))
      return(1)
    input$dataFactorSelection
  })
  
  dataViewSelection <- reactive({
    if (is.null(input$dataViewSelection))
      return(1)
    input$dataViewSelection
  })
  
  dataFeatureSelection <- reactive({
    if (is.null(input$dataFeatureSelection))
      return(NULL)
    input$dataFeatureSelection
  })
  
  ### EMBERDDINGS ###
  
  factorSelection_x <- reactive({
    selected_global <- input$factorsChoice
    if (is.null(selected_global)) {
      return(factorsChoice()[1])
    } else if (length(selected_global) >= 1) {
      return(selected_global[1])
    } else {
      return(factorsChoice()[1])
    }
    input$factorChoice_x
  })
  
  factorSelection_y <- reactive({
    selected_global <- input$factorsChoice
    if (is.null(selected_global)) {
      return(factorsChoice()[2])
    } else if (length(selected_global) > 1) {
      return(selected_global[2])
    } else {
      return(factorsChoice()[2])
    }
    input$factorChoice_y
  })
  
  ### FACTOR VALUES ###
  
  factorsAxisSelection_x <- reactive({
    selected_global <- input$factorsAxisChoice_x
    if (is.null(selected_global)) {
      return(metaChoice()[1])
    } else if (length(selected_global) >= 1) {
      return(selected_global[1])
    } else {
      return(metaChoice()[1])
    }
  })
  
  
  factorsGroupsChoice <- reactive({
    m <- model()
    if (is.null(m)) return(NULL)
    as.character(unique(samples_metadata(m)[,factorsAxisSelection_x()]))
  })
  
  
  ### MANIFOLD VALUES ###
  manifoldSelection <- reactive({
    selected_global <- input$manifoldChoice
    if (is.null(selected_global)) {
      return(dimredChoice()[1])
    } else if (length(selected_global) >= 1) {
      return(selected_global[1])
    } else {
      return(dimredChoice()[1])
    }
  })
  
  
  #################
  ### RENDERING ###
  #################
  
  
  output$viewsChoice <- renderUI({
    selectInput('viewsChoice', 'Views:', choices = viewsChoice(), selected = viewsChoice(), multiple = TRUE, selectize = TRUE)
  })
  
  output$groupsChoice <- renderUI({
    selectInput('groupsChoice', 'Groups:', choices = groupsChoice(), multiple = TRUE, selectize = TRUE)
  })
  
  output$factorsChoice <- renderUI({
    selectInput('factorsChoice', 'Factors:', choices = factorsChoice(), multiple = TRUE, selectize = TRUE)
  })
  
  output$colourChoice <- renderUI({
    selectInput('colourChoice', 'Colour samples:', choices = metaFeatureFactorChoice(), selected = "group", multiple = FALSE, selectize = FALSE)
  })
  
  ### MODEL OVERVIEW ###
  
  output$dataOverviewPlot <- renderPlot({
    m <- model()
    if (is.null(m)) return(NULL)
    
    # Use custom colour palette
    shiny_palette <- c("#948979", "#3C5B6F", "#DFD0B8", "#B5C18E" ,"#DFDFDF" ,"#B99470" ,"#DBB5B5")
    n_views <- get_dimensions(m)$M
    if (n_views <= length(shiny_palette)) {
      shiny_colours <- shiny_palette[seq_len(n_views)]
    } else {
      shiny_colours <- rainbow(n_views)
    }
    names(shiny_colours) <- views_names(m)
    plot_data_overview(m, colors = shiny_colours) +
      theme(strip.text.x = element_text(size = 16, colour = "#333333"),
            axis.text.y = element_text(size = 16, colour = "#333333"))
  })
  
  ### VARIANCE EXPLAINED ###
  
  output$varianceExplainedPlot <- renderPlot({
    m <- model()
    if (is.null(m)) return(NULL)
    plot_variance_explained(m,
                            factors = factorsSelection(),
                            plot_total = FALSE, use_cache = TRUE) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            strip.text.x = element_text(size = 14, colour = "#333333"))
  })
  
  ### WEIGHTS (LOADINGS) ###
  
  output$weightsViewSelection <- renderUI({
    selectInput('weightsViewSelection', 'View:', choices = viewsChoice(), multiple = FALSE, selectize = TRUE)
  })
  
  output$weightsFeatureSelection <- renderUI({
    selectInput('weightsFeatureSelection', 'Label manually:', choices = featuresChoice()[weightsViewSelection()], multiple = TRUE, selectize = TRUE)
  })
  
  output$weightsPlot <- renderPlot({
    m <- model()
    if (is.null(m)) return(NULL)
    if (!is.null(weightsFeatureSelection()) && (length(weightsFeatureSelection()) > 0)) {
      # Some features are selected manually
      plot_weights(m, view = weightsViewSelection(), factors = factorsSelection(), manual = weightsFeatureSelection())
    } else {
      plot_weights(m, view = weightsViewSelection(), factors = factorsSelection(), nfeatures = input$nfeatures_to_label)
    }
  })
  
  output$topWeightsPlot <- renderPlot({
    m <- model()
    if (is.null(m)) return(NULL)
    plot_top_weights(m, view = weightsViewSelection(),
                     factors = factorsSelection(),
                     nfeatures = input$nfeatures_to_label)
  })
  
  
  ### FACTOR VALUES ###
  
  output$factorsPlot <- renderPlot({
    m <- model()
    if (is.null(m)) return(NULL)
    # Removed dot alpha from the function call
    p <- plot_factor(m, factors = factorsSelection(), color_by = colourSelection(), group_by = factorsAxisSelection_x(),
                     groups = groupsSelection(), add_dots = input$factorsAddDots, add_violin = input$factorsAddViolins,
                     dot_size = input$factorsDotSize)
    # Always rotate labels
    p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
  })
  
  output$factorsAxisChoice_x <- renderUI({
    selectInput('factorsAxisChoice_x', 'X axis:',
                choices = metaChoice(), multiple = FALSE, selectize = TRUE,
                selected = factorsAxisSelection_x())
  })
  
  ### DATA (SINGLE FACTOR EXPLORATION) ###
  
  output$dataFactorSelection <- renderUI({
    selectInput('dataFactorSelection', 'Factor:', choices = factorsChoice(), multiple = FALSE, selectize = TRUE)
  })
  
  output$dataViewSelection <- renderUI({
    selectInput('dataViewSelection', 'View:', choices = viewsChoice(), multiple = FALSE, selectize = TRUE)
  })
  
  output$dataFeatureSelection <- renderUI({
    selectInput('dataFeatureSelection', 'Select features manually:',
                choices = featuresChoice()[dataViewSelection()], multiple = TRUE, selectize = TRUE)
  })
  
  output$dataHeatmapPlot <- renderPlot({
    m <- model()
    if (is.null(m)) return(NULL)
    
    # Figure out if samples should be annotated
    annotation_samples <- NULL
    if (colourSelection() %in% colnames(samples_metadata(m))) annotation_samples <- colourSelection()
    
    # Figure out if features are provided manually
    selection_features <- input$nfeatures_to_plot
    if (!is.null(dataFeatureSelection()) && (length(dataFeatureSelection()) > 0))
      selection_features <- dataFeatureSelection()
    
    plot_data_heatmap(m, view = dataViewSelection(), groups = groupsSelection(),
                      factor = dataFactorSelection(), features = selection_features,
                      annotation_samples = annotation_samples)
  })
  
  output$dataScatterPlot <- renderPlot({
    m <- model()
    if (is.null(m)) return(NULL)
    
    # Figure out if features are provided manually
    selection_features <- input$nfeatures_to_plot
    if (!is.null(dataFeatureSelection()) && (length(dataFeatureSelection()) > 0))
      selection_features <- dataFeatureSelection()
    
    plot_data_scatter(m, view = dataViewSelection(), groups = groupsSelection(),
                      factor = dataFactorSelection(), features = selection_features)
  })
  
  
  ### FACTORS SCATTERPLOT (EMBEDDINGS) ###
  
  output$factorChoice_x <- renderUI({
    selectInput('factorChoice_x', 'X axis factor:',
                choices = factorsChoice(), multiple = FALSE, selectize = TRUE,
                selected = factorSelection_x())
  })
  
  output$factorChoice_y <- renderUI({
    selectInput('factorChoice_y', 'Y axis factor:',
                choices = factorsChoice(), multiple = FALSE, selectize = TRUE,
                selected = factorSelection_y())
  })
  
  output$embeddingsPlot <- renderPlot({
    m <- model()
    if (is.null(m)) return(NULL)
    plot_factors(m, groups = groupsSelection(), factors = c(input$factorChoice_x, input$factorChoice_y), color_by = colourSelection(),
                 dot_size = input$factorDotSize, alpha = input$factorDotAlpha)
  })
  
  output$embeddingsInfo <- renderPrint({
    m <- model()
    if (is.null(m)) return(NULL)
    
    df <- plot_factors(m, groups = groupsSelection(), 
                       factors = c(input$factorChoice_x, input$factorChoice_y), 
                       color_by = colourSelection(), return_data = TRUE)
    
    brushedPoints(df, input$plot_factors, xvar = "x", yvar = "y") 
  })
  
  
  
  
  
  
  
  observeEvent(input$swapEmbeddings, {
    x_sel <- input$factorChoice_x
    y_sel <- input$factorChoice_y
    if (!is.null(x_sel) && !is.null(y_sel)) {
      output$factorChoice_y <- renderUI({
        selectInput('factorChoice_y', 'Factor on Y axis:',
                    choices = factorsChoice(), multiple = FALSE, selectize = TRUE,
                    selected = x_sel)
      })
      
      output$factorChoice_x <- renderUI({
        selectInput('factorChoice_x', 'Factor on X axis:',
                    choices = factorsChoice(), multiple = FALSE, selectize = TRUE,
                    selected = y_sel)
      })
    }
  })
  
  
  
  # Download handlers
  output$downloadFactors <- downloadHandler(
    filename = function() {
      paste0("factors_", Sys.Date(), ".csv")
    },
    content = function(file) {
      m <- model()
      factors <- get_factors(m, factors = "all")
      
      # Convert the list of matrices to a data frame with row names
      factors_df <- lapply(seq_along(factors), function(factor_index) {
        factor_name <- names(factors)[factor_index]
        factor_data <- as.data.frame(factors[[factor_index]])
        factor_data$sample <- rownames(factor_data) # Add sample IDs as a column
        factor_data$factor <- factor_name
        factor_data[, c("sample", setdiff(names(factor_data), "sample"))] # Reorder columns
      }) %>%
        do.call(rbind, .)
      
      write_csv(factors_df, file)
    }
  )
  
  output$downloadWeights <- downloadHandler(
    filename = function() {
      paste0("weights_", Sys.Date(), ".csv")
    },
    content = function(file) {
      m <- model()
      weights <- get_weights(m, views = "all", factors = "all")
      
      # Convert the list of matrices to a data frame with row names
      weights_df <- lapply(seq_along(weights), function(weight_index) {
        weight_name <- names(weights)[weight_index]
        weight_data <- as.data.frame(weights[[weight_index]])
        weight_data$feature <- rownames(weight_data)  # Add feature names as a column
        weight_data$view <- weight_name
        weight_data[, c("feature", setdiff(names(weight_data), "feature"))] # Reorder columns
      }) %>%
        do.call(rbind, .)
      
      write_csv(weights_df, file)
    }
  )
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      m <- model()
      data <- get_data(m)
      
      # Create a list to store data for the data frame
      data_list <- list(sampleID = character(), feature = character(), view = character(), value = numeric(), group = character())
      
      # Iterate over views
      for (view_name in names(data)) {
        view_data <- data[[view_name]]
        
        # Iterate over groups within the view
        for (group_name in names(view_data)) {
          group_data <- view_data[[group_name]]
          
          # Iterate over features (using row names)
          for (feature_name in rownames(group_data)) {
            # Extract data for the current feature, view, and group
            feature_values <- group_data[feature_name, ]
            
            # Get sample IDs for the current group
            sample_ids <- colnames(group_data)
            
            # Append data to the list
            data_list$sampleID <- c(data_list$sampleID, sample_ids)
            data_list$feature <- c(data_list$feature, rep(feature_name, length(feature_values)))
            data_list$view <- c(data_list$view, rep(view_name, length(feature_values)))
            data_list$value <- c(data_list$value, feature_values)
            data_list$group <- c(data_list$group, rep(group_name, length(feature_values)))
          }
        }
      }
      
      # Create a data frame from the list
      data_df <- as.data.frame(data_list)
      
      # Reshape the data frame using pivot_wider
      data_df <- data_df %>%
        pivot_wider(names_from = sampleID, values_from = value)
      
      # Reorder columns to have desired format: feature, samples, view, group
      sample_cols <- colnames(data_df)[!colnames(data_df) %in% c("feature", "view", "group")]
      data_df <- data_df[, c("feature", sample_cols, "view", "group")] 
      
      write_csv(data_df, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

