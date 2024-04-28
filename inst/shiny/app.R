library(shiny)
library(shinyjs)
library(bslib)

ui <- page_navbar(
  useShinyjs(),
  title = "Random Forest Classification - Project 3",
  nav_panel(title = "Construct Model",
    layout_columns(
    card(
    selectInput("dropdown_dataset", "Select Dataset:",
                  choices = "Obesity Dataset", selected = "Obesity Dataset"),
    "Select Features (Select at least 2):",
    checkboxInput("Gender",
                    "Gender of individual",
                    value = FALSE),
      checkboxInput("Age",
                    "Age of individual",
                    value = FALSE),
      checkboxInput("Height",
                    "Height of individual",
                    value = FALSE),
      checkboxInput("Weight",
                    "Weight of individual",
                    value = FALSE),
      checkboxInput("family_history_with_overweight",
                    "Familial history of overweight",
                    value = FALSE),
      checkboxInput("FAVC",
                    "Frequent consumption of high caloric food",
                    value = FALSE),
      checkboxInput("FCVC",
                    "Frequent consumption of vegetables",
                    value = FALSE),
      checkboxInput("NCP",
                    "Number of main meals",
                    value = FALSE),
      checkboxInput("CAEC",
                    "Consumption of food between meals",
                    value = FALSE),
      checkboxInput("SMOKE",
                    "Smoking status",
                    value = FALSE),
      checkboxInput("CH20",
                    "Consumption of water",
                    value = FALSE),
      checkboxInput("SCC",
                    "Caloric intake monitoring status",
                    value = FALSE),
      checkboxInput("FAF",
                    "Frequency of physical activity",
                    value = FALSE),
      checkboxInput("TUE",
                    "Time using technology devices",
                    value = FALSE),
      checkboxInput("CALC",
                    "Alcohol consumption",
                    value = FALSE),
      checkboxInput("MTRANS",
                    "Mode of transportion",
                    value = FALSE)
    ,
      selectInput("dropdown_scaling", "Select Scaling Method:",
                  choices = c("None","Min-Max","Standardization","Robust Scaling"), selected = "None")
    ,
      sliderInput("ntree",
                  "Number of trees in forest:",
                  min = 1,
                  max = 1000,
                  value = 100),
      sliderInput("mtry",
                  "Number of variables per tree:",
                  min = 1,
                  max = 16,
                  value = 1),
      sliderInput("max_depth",
                  "Maximum depth of trees:",
                  min = 1,
                  max = 100,
                  value = 10),
      sliderInput("nodesize",
                  "Minimum size of terminal nodes:",
                  min = 1,
                  max = 100,
                  value = 10),
      actionButton("start_model", "Run Model")
    ),
    card(
      card_header("Confusion Matrices"),
      plotOutput("train_heatmap"),
      plotOutput("test_heatmap")
    ),
    card(
      card_header("Model Metrics"),
      "Training Metrics:",
      tableOutput("train_metrics"),
      "Testing Metrics:",
      tableOutput("test_metrics")
    ),
    col_widths = c(2,4,6)
    )
  ),
  nav_panel(title = "Analyze Features",
            layout_columns(
              card(
                "Select Features (Select at least 2):",
                checkboxInput("Gender_inv",
                              "Gender of individual",
                              value = FALSE),
                checkboxInput("Age_inv",
                              "Age of individual",
                              value = FALSE),
                checkboxInput("Height_inv",
                              "Height of individual",
                              value = FALSE),
                checkboxInput("Weight_inv",
                              "Weight of individual",
                              value = FALSE),
                checkboxInput("family_history_with_overweight_inv",
                              "Familial history of overweight",
                              value = FALSE),
                checkboxInput("FAVC_inv",
                              "Frequent consumption of high caloric food",
                              value = FALSE),
                checkboxInput("FCVC_inv",
                              "Frequent consumption of vegetables",
                              value = FALSE),
                checkboxInput("NCP_inv",
                              "Number of main meals",
                              value = FALSE),
                checkboxInput("CAEC_inv",
                              "Consumption of food between meals",
                              value = FALSE),
                checkboxInput("SMOKE_inv",
                              "Smoking status",
                              value = FALSE),
                checkboxInput("CH20_inv",
                              "Consumption of water",
                              value = FALSE),
                checkboxInput("SCC_inv",
                              "Caloric intake monitoring status",
                              value = FALSE),
                checkboxInput("FAF_inv",
                              "Frequency of physical activity",
                              value = FALSE),
                checkboxInput("TUE_inv",
                              "Time using technology devices",
                              value = FALSE),
                checkboxInput("CALC_inv",
                              "Alcohol consumption",
                              value = FALSE),
                checkboxInput("MTRANS_inv",
                              "Mode of transportion",
                              value = FALSE),
                checkboxInput("NObeyesdad_inv",
                              "Obesity Classification (Target)",
                              value = FALSE)
                ,
                selectInput("dropdown_scaling_inv", "Select Scaling Method:",
                            choices = c("None","Min-Max","Standardization","Robust Scaling"), selected = "None")
                ,
                actionButton("start_model_inv", "Run Analysis")
              ),
              card(
                card_header("Feature Covariance and Correlation"),
                plotOutput("cor_heatmap"),
                plotOutput("cov_heatmap")
              ),
              card(
                card_header("Feature Summary Statistics"),
                uiOutput("plot_placeholder"),
                uiOutput("sum_placeholder")
              ),
              card(
                card_header("PCA Analysis"),
                uiOutput("PCA_anal")
              ),
              col_widths = c(2,4,3,3)
            )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Detect number of features selected to limit appropriate parameter selection
  observe({
    num_features_selected <- sum(c(input$Gender, input$Age, input$Height, input$Weight,
                                   input$family_history_with_overweight, input$FAVC,
                                   input$FCVC, input$NCP, input$CAEC, input$SMOKE,
                                   input$CH20, input$SCC, input$FAF, input$TUE,
                                   input$CALC, input$MTRANS))
    updateSliderInput(session, "mtry", max = num_features_selected)
  })

  # Status of model
  model_status <- reactiveVal("")

  # Detect Start Model button to start the RF classifier with correct features, scaling, and parameters
  observeEvent(input$start_model, {

    # Prevent the start_model button from being activated again
    shinyjs::disable("start_model")

    # Convert features to list for output to function
    featureList <- c(input$Gender, input$Age, input$Height, input$Weight, input$family_history_with_overweight,
                     input$FAVC,input$FCVC, input$NCP, input$CAEC, input$SMOKE, input$CH20, input$SCC, input$FAF,
                     input$TUE, input$CALC, input$MTRANS)

    # Check to see if at least one feature has been selected
    if(!any(featureList)) {
      warning("Please select at least one feature for analysis.")
      session$reload()
    }

    dataset <- input$dropdown_dataset

    # Check if valid dataset was selected
    if(dataset != "Obesity Dataset") {
      warning("Please choose a currently supported dataset.")
      session$reload()
    }

    # Update model_status
    model_status("Running model...")

    scaling <- input$dropdown_scaling
    parameters <- c(input$ntree,input$mtry,input$max_depth,input$nodesize)
    model_results <- RForest_Model(dataset, featureList, scaling, parameters)

    output$train_heatmap <- renderPlot({
      model_results$Train_hmap  # Return the plot
    }, height = 500, width = 575)

    output$test_heatmap <- renderPlot({
      model_results$Test_hmap  # Return the plot
    }, height = 500, width = 575)

    output$train_metrics <- renderTable({
      model_results$Metrics_train
    }, rownames = TRUE)

    output$test_metrics <- renderTable({
      model_results$Metrics_test
    }, rownames = TRUE)

    # Update model_status
    model_status("Model finished.")

    # Enable the start_model button again
    shinyjs::enable("start_model")
  })

  # Detect Start Model button to start the feature analysis with correct features and scaling.
  observeEvent(input$start_model_inv, {

    # Prevent the start_model button from being activated again
    shinyjs::disable("start_model_inv")

    # Convert features to list for output to function
    featureList_inv <- c(input$Gender_inv, input$Age_inv, input$Height_inv, input$Weight_inv, input$family_history_with_overweight_inv,
                     input$FAVC_inv,input$FCVC_inv, input$NCP_inv, input$CAEC_inv, input$SMOKE_inv, input$CH20_inv, input$SCC_inv, input$FAF_inv,
                     input$TUE_inv, input$CALC_inv, input$MTRANS_inv, input$NObeyesdad_inv)

    # Check to see if at least one feature has been selected
    if(!any(featureList_inv)) {
      warning("Please select at least one feature for analysis.")
      session$reload()
    }


    scaling_inv <- input$dropdown_scaling_inv
    feat_anal_results <- Feat_Anal(featureList_inv, scaling_inv)

    output$cor_heatmap <- renderPlot({
      feat_anal_results$Cor_HM  # Return the plot
    }, height = 500, width = 500)

    output$cov_heatmap <- renderPlot({
      feat_anal_results$Cov_HM  # Return the plot
    }, height = 500, width = 500)

    output$plot_placeholder <- renderUI({
      plot_output_list <- lapply(names(feat_anal_results$Feat_Plots), function(col_name) {
        tagList(
          h3(col_name),  # Display the column name as a heading
          renderPlot({feat_anal_results$Feat_Plots[[col_name]]})
        )
      })
      tagList(plot_output_list)
    })


    output$sum_placeholder <- renderUI({
      summary_output_list <- lapply(names(feat_anal_results$Sum_List), function(col_name) {
        tagList(
          h3(col_name),  # Display the column name as a heading
          renderPrint({feat_anal_results$Sum_List[[col_name]]})
        )
      })
      tagList(summary_output_list)
    })

    output$PCA_anal <- renderUI({
      verbatimTextOutput("pca_output")
    })

    output$pca_output <- renderPrint({
      feat_anal_results$PCA
    })

    # Enable the start_model button again
    shinyjs::enable("start_model_inv")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
