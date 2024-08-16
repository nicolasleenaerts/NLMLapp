NLMLapp <- function(){
  #### Load libraries ####
  library(bslib)
  library(caret)
  library(dplyr)
  library(DT)
  library(ensr)
  library(forcats)
  library(ggplot2)
  library(glmnet)
  library(openxlsx)
  library(pkgload)
  library(pROC)
  library(readxl)
  library(readxl)
  library(scales)
  library(shiny)
  library(shinythemes)
  library(shinyWidgets)
  library(splitTools)
  library(tidyr)
  
  #### Define UI ####
  ui <- fluidPage(
    # Theme of the app
    theme=bs_theme(primary = "#3BA688", secondary = "#F39498", 
                   font_scale = NULL, preset = "minty"),
    # App title
    titlePanel(h1(HTML("Person-specific and Pooled Prediction Models <br/> with Elastic Net Regularized Regression"),align = "center")),
    
    # Create panels for the sidebar
    navlistPanel(
      id = "tabset",
      # Tab 1: Uploading data
      tabPanel("Data", 
               # Upload data
               fileInput("upload", "Upload your data set",multiple = T,accept='.csv')),
      
      # Tab 2: Setting preprocessing parameters
      tabPanel("Preprocessing",  
               
               # Create bootstrap switch for either a pooled of person-specific model
               'Pick a model type',
               switchInput(inputId = "model_type",label = 'Model type',onLabel = "Pooled",offLabel = "Person-specific",value = TRUE,width= 'auto'),
               
               # Conditional panels for the pooled or person-specific models
               tabsetPanel(
                 id = "switcher_pooled_person",
                 type = "hidden",
                 # Pooled model
                 tabPanelBody("pooled", 
                              fluidRow(
                                column(6,
                                       # Create checkboxes
                                       pickerInput(inputId = 'predictors_within_con_pooled', label = 'Select within-person continuous predictor(s)', choices = NULL, 
                                                   options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = TRUE),
                                       pickerInput(inputId = 'predictors_between_con_pooled', label = 'Select between-person continuous predictor(s)', choices = NULL, 
                                                   options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = TRUE),
                                       pickerInput(inputId = 'predictors_within_cat_pooled', label = 'Select within-person categorical predictor(s)', choices = NULL, 
                                                   options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = TRUE),
                                       pickerInput(inputId = 'predictors_between_cat_pooled', label = 'Select between-person categorical predictor(s)', choices = NULL, 
                                                   options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = TRUE),
                                       pickerInput(inputId = 'outcome_pooled', label = 'Select outcome', choices = NULL, 
                                                   options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = FALSE),
                                       # Split and cross-validation
                                       'Pick a split type',
                                       switchInput(inputId = "split_type_pooled",label = 'Split type',onLabel = "Cross-validation",offLabel = "Train-test split",value = TRUE,width= 'auto'),
                                       tabsetPanel(
                                         id = "switcher_split_pooled",
                                         type = "hidden",
                                         tabPanelBody("cv_pooled",
                                                      numericInput("nfolds_pooled", "Number of folds", value = 5)),
                                         tabPanelBody("split_pooled",
                                                      numericInput("split_percentage_pooled", "Percentage training data", value = 80))),
                                       # Inner folds
                                       numericInput("ensr_cv_pooled", "Number of inner folds", value = 10, min = 1),
                                       # Number of inner cross-validation repetition
                                       numericInput("repeated_cv_pooled", "Number of inner cross-validation repetitions", value = 1, min = 1)
                                ),
                                column(6,
                                       # Stratified
                                       materialSwitch(inputId = "stratified_pooled",label = "Stratified", status = "default",right = F,value=T),
                                       # Scaled
                                       materialSwitch(inputId = "scaled_pooled",label = "Scaled     ", status = "default",right = F,value=T),
                                       # Shuffle
                                       materialSwitch(inputId = "shuffle_pooled",label = "Shuffle     ", status = "default",right = F,value=T),
                                       # Number of alphas
                                       numericInput("nalphas_pooled", "Number of alphas", value = 10, min = 2),
                                       # Number of lambdas
                                       numericInput("nlambdas_pooled", "Number of lambdas", value = 10, min = 1),
                                       # Seed
                                       numericInput("seed_pooled", "Seed", value = 404, min = 1),
                                       # Analysis family
                                       'Pick a family type for the analyses',
                                       switchInput(inputId = "family_pooled",label = 'Family',onLabel = "Binary",offLabel = "Continuous",value = TRUE,width= 'auto'),
                                       tabsetPanel(
                                         id = "switcher_family_pooled",
                                         type = "hidden",
                                         tabPanelBody("binary_family_pooled",
                                                      numericInput("stop_train_pooled", "Minimum number of positive observations in the training data", value = 5),
                                                      numericInput("stop_test_pooled", "Minimum number of positive observations in the testing data", value = 1)),
                                         tabPanelBody("continuous_family_pooled",
                                                      numericInput("pred_min_pooled", "Minimum value of the predictions", value = NULL),
                                                      numericInput("pred_max_pooled", "Maximum value of the predictions", value = NULL)))
                                )
                              )),
                 # Person-specific model
                 tabPanelBody("person_specific",
                              fluidRow(
                                column(6,         
                                       # Create checkboxes
                                       pickerInput(inputId = 'predictors_con_person', label = 'Select continuous predictor(s)', choices = NULL, 
                                                   options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = TRUE),
                                       pickerInput(inputId = 'predictors_cat_person', label = 'Select categorical predictor(s)', choices = NULL, 
                                                   options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = TRUE),
                                       pickerInput(inputId = 'outcome_person', label = 'Select outcome', choices = NULL, 
                                                   options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = FALSE),
                                       
                                       # Split and cross-validation
                                       'Pick a split type',
                                       switchInput(inputId = "split_type_person",label = 'Split type',onLabel = "Cross-validation",offLabel = "Train-test split",value = TRUE,width= 'auto'),
                                       tabsetPanel(
                                         id = "switcher_split_person",
                                         type = "hidden",
                                         tabPanelBody("cv_person",
                                                      numericInput("nfolds_person", "Number of outer folds", value = 5)),
                                         tabPanelBody("split_person",
                                                      numericInput("split_percentage_person", "Percentage training data", value = 80))),
                                       # Inner folds
                                       numericInput("ensr_cv_person", "Number of inner folds", value = 10, min = 1),
                                       # Number of inner cross-validation repetition
                                       numericInput("repeated_cv_person", "Number of inner cross-validation repetitions", value = 1, min = 1)
                                ),
                                column(6,
                                       # Stratified
                                       materialSwitch(inputId = "stratified_person",label = "Stratified", status = "default",right = F,value=T),
                                       # Scaled
                                       materialSwitch(inputId = "scaled_person",label = "Scaled     ", status = "default",right = F,value=T),
                                       # Shuffle
                                       materialSwitch(inputId = "shuffle_person",label = "Shuffle     ", status = "default",right = F,value=T),
                                       # Number of alphas
                                       numericInput("nalphas_person", "Number of alphas", value = 10, min = 2),
                                       # Number of lambdas
                                       numericInput("nlambdas_person", "Number of lambdas", value = 10, min = 1),
                                       # Seed
                                       numericInput("seed_person", "Seed", value = 404, min = 1),
                                       # Analysis family
                                       'Pick a family type for the analyses',
                                       switchInput(inputId = "family_person",label = 'Family',onLabel = "Binary",offLabel = "Continuous",value = TRUE,width= 'auto'),
                                       tabsetPanel(
                                         id = "switcher_family_person",
                                         type = "hidden",
                                         tabPanelBody("binary_family_person",
                                                      numericInput("stop_train_person", "Minimum number of positive observations in the training data", value = 5),
                                                      numericInput("stop_test_person", "Minimum number of positive observations in the testing data", value = 1)),
                                         tabPanelBody("continuous_family_person",
                                                      numericInput("pred_min_person", "Minimum value of the predictions", value = NULL),
                                                      numericInput("pred_max_person", "Maximum value of the predictions", value = NULL)))
                                )
                              )))),
      # Tab 3: Analysis
      tabPanel("Model Training and Testing", 
               HTML('Press button to start the analyses <br/>'),
               actionButton("start_analysis", "Start!"),
               progressBar(id = "pb", value = 0, display_pct = T)),
      # Tab : Displaying the results
      tabPanel("Results",
               tabsetPanel(
                 id = "switcher_family_results",
                 type = "hidden",
                 tabPanelBody("results_family_pooled",
                              HTML('Download results <br/>'),
                              downloadButton("download_pooled"),
                              DT::DTOutput("results_table_pooled_average"),style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                              DT::DTOutput("results_table_pooled_metrics"),style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                              DT::DTOutput("results_table_pooled_estimates"),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"
                 ),
                 tabPanelBody("results_family_person",
                              HTML('Download results <br/>'),
                              downloadButton("download_person"),
                              DT::DTOutput("results_table_person_average"),style = "height:800px; overflow-y: scroll;overflow-x: scroll;",
                              DT::DTOutput("results_table_person"),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"
                 ))
      ),
      tabPanel("Plots", 
               HTML('Download plot <br/>'),
               downloadButton("download_plot"),
               HTML('<br/>'),
               'Pick a plot type',
               switchInput(inputId = "plot_type",label = 'Plot type',onLabel = "Parametric",offLabel = "Nonparametric",value = TRUE,width= 'auto'),
               fluidRow(
                 column(6,
                        numericInput("percentile", "Percentile", value = 0.9,min = 0,max = 1),
                        materialSwitch(inputId = "range",label = "Display Range", status = "default",right = F,value=T),
                        sliderInput("xlim", "Plot range", value = c(-2,2), min = -10, max = 10,step=0.5),
                        sliderInput('gradient', "Color range", value = c(-1.5,2.5), min = -5, max = 5,step=0.5)
                 ),
                 column(6,
                        textInput("title", "Title",value=NULL),
                        textInput("subtitle", "Subtitle",value=NULL),
                        textInput("ylab", "Label y-axis",value='Predictor'),
                        textInput("xlab", "Label x-axis",value='Estimate')
                 )
               ),
               plotOutput("plot_estimates"))
    )
  )
  
  #### Define server logic ####
  server <- function(input, output, session) {
    
    # Create reactive element for data
    data <- reactive({ 
      # Waiting for input
      req(input$upload) 
      # Making list of data sets
      data_lists <- list()
      for (entry in input$upload$datapath){
        data_lists[[length(data_lists)+1]]<-read.csv(entry)
      } 
      # Returning list of data sets
      data_lists
    })
    
    # Create reactive elements for split/CV
    split <- reactiveValues(value = NA)
    ext_cv_folds <- reactiveValues(value = NA)
    
    # Create reactive elements for results
    results_person <- reactiveValues(results = data.frame(NA),results_ranked=data.frame(NA),
                                     results_metrics_average = data.frame(NA),results_metrics_median = data.frame(NA))
    results_pooled <- reactiveValues(results_estimates = data.frame(NA),results_estimates_ranked = data.frame(NA), results_metrics = data.frame(NA),
                                     results_metrics_average = data.frame(NA),results_metrics_median = data.frame(NA))
    
    # Switch preprocessing panels for model type
    observeEvent(input$model_type, {
      updateTabsetPanel(inputId = "switcher_pooled_person", selected = ifelse(input$model_type,'pooled','person_specific'))
    })
    
    # Switch preprocessing panels for split/CV
    observeEvent(input$split_type_pooled, {
      updateTabsetPanel(inputId = "switcher_split_pooled", selected = ifelse(input$split_type_pooled,'cv_pooled','split_pooled'))
      if(input$split_type_pooled==T){  
        split$value <- NULL
        ext_cv_folds$value <- input$nfolds_pooled}
      else{
        split$value <- input$split_percentage_pooled
        ext_cv_folds$value <-NULL}
    })
    observeEvent(input$split_type_person, {
      updateTabsetPanel(inputId = "switcher_split_person", selected = ifelse(input$split_type_person,'cv_person','split_person'))
      if(input$split_type_person==T){  
        split$value <- NULL
        ext_cv_folds$value <- input$nfolds_person}
      else{
        split$value <- input$split_percentage_person
        ext_cv_folds$value <-NULL}
    })
    
    # Switch preprocessing panels for family
    observeEvent(input$family_pooled, {
      updateTabsetPanel(inputId = "switcher_family_pooled", selected = ifelse(input$family_pooled,'binary_family_pooled','continuous_family_pooled'))
    })
    observeEvent(input$family_person, {
      updateTabsetPanel(inputId = "switcher_family_person", selected = ifelse(input$family_person,'binary_family_person','continuous_family_person'))
    })
    
    # Update selection list for predictors and outcome
    observe({
      # Create common predictors
      predictors <- colnames(data()[[1]])
      for (i in 2:length(data())){
        predictors<- intersect(predictors, colnames(data()[[i]]))
      }
      # Update predictor selectors
      updatePickerInput(session, 'predictors_con_person', choices = predictors)   
      updatePickerInput(session, 'predictors_cat_person', choices = predictors)   
      updatePickerInput(session, 'predictors_within_con_pooled', choices = predictors)   
      updatePickerInput(session, 'predictors_within_cat_pooled', choices = predictors) 
      updatePickerInput(session, 'predictors_between_con_pooled', choices = predictors)   
      updatePickerInput(session, 'predictors_between_cat_pooled', choices = predictors) 
      updatePickerInput(session, 'outcome_person', choices = predictors)
      updatePickerInput(session, 'outcome_pooled', choices = predictors)  
    })
    
    # Run analyses
    observeEvent(input$start_analysis, {
      # Perform pooled analyses
      if (input$model_type==T){
        # Create one data set
        data_pooled <- list()
        for (entry in 1:length(data())){
          data_pooled[[entry]] <- dplyr::select(data()[[entry]],c(input$predictors_within_con_pooled,input$predictors_between_con_pooled,
                                                                  input$predictors_within_cat_pooled,input$predictors_between_cat_pooled,
                                                                  input$outcome_pooled))
          data_pooled[[entry]]$entry <- entry
        }
        data_pooled <- dplyr::bind_rows(data_pooled)
        # run model
        updateProgressBar(session = session, id = "pb", value = 0)
        results_model <- elastic_net_wrapper_pooled(data=data_pooled,by='entry',predictors_con=input$predictors_within_con_pooled,predictors_cat=input$predictors_within_cat_pooled,
                                                    between_predictors_con = input$predictors_between_con_pooled,between_predictors_cat = input$predictors_between_cat_pooled,
                                                    outcome=input$outcome_pooled,split=split$value, outer_cv=ext_cv_folds$value, stratified=input$stratified_pooled,scaling=input$scaled_pooled,
                                                    repeated_cv=input$repeated_cv_pooled,ensr_cv=input$ensr_cv_pooled,ensr_alphas=seq(0, 1, length = input$nalphas_pooled),ensr_lambdas=input$nlambdas_pooled,seed=input$seed_pooled,
                                                    shuffle=input$shuffle_pooled,stop_test=input$stop_test_pooled,family=ifelse(input$family_pooled,'binary','continuous'),pred_min=input$pred_min_pooled,pred_max=input$pred_max_pooled)
        updateProgressBar(session = session, id = "pb", value = 100)
        # Return results
        results_pooled$results_estimates <- results_model$results_pooled_model
        results_pooled$results_metrics <- results_model$results_by
        results_pooled$results_metrics_average <- aggregate(dplyr::select(results_model$results_by,-c('by','fold')),list(by=results_model$results_by$by),mean,na.rm=T)
        results_pooled$results_metrics_average$type <- 'mean'
        results_pooled$results_metrics_average <- aggregate(dplyr::select(results_pooled$results_metrics_average,-c('by','type')),list(type=results_pooled$results_metrics_average$type),mean,na.rm=T)
        results_pooled$results_metrics_median <- aggregate(dplyr::select(results_model$results_by,-c('by','fold')),list(by=results_model$results_by$by),median,na.rm=T)
        results_pooled$results_metrics_median$type <- 'median'
        results_pooled$results_metrics_median <- aggregate(dplyr::select(results_pooled$results_metrics_median,-c('by','type')),list(type=results_pooled$results_metrics_median$type),median,na.rm=T)
        
        # Rename columns
        results_pooled$results_metrics <- results_pooled$results_metrics %>%
          dplyr::rename(
            participant = by
          )
        
        # Create ranked results
        results_pooled$results_estimates_ranked <- results_pooled$results_estimates
        results_pooled$results_estimates_ranked[,c(input$predictors_within_con_pooled,input$predictors_between_con_pooled,input$predictors_within_cat_pooled,input$predictors_between_cat_pooled)] <- coefficient_ranker(results_pooled$results_estimates_ranked[,c(input$predictors_within_con_pooled,input$predictors_between_con_pooled,input$predictors_within_cat_pooled,input$predictors_between_cat_pooled)])
      }
      # Perform person-specific analyses
      else if (input$model_type==F){
        # Create final results dataframe
        results_all <- data.frame()
        updateProgressBar(session = session, id = "pb", value = 0)
        # Loop over data sets
        for (entry in 1:length(data())){
          results_model <- elastic_net_wrapper(data=data()[[entry]],predictors_con=input$predictors_con_person,predictors_cat=input$predictors_cat_person,
                                               outcome=input$outcome_person,split=split$value, outer_cv=ext_cv_folds$value, stratified=input$stratified_person,scaling=input$scaled_person,
                                               repeated_cv=input$repeated_cv_person,ensr_cv=input$ensr_cv_person,ensr_alphas=seq(0, 1, length = input$nalphas_person),ensr_lambdas=input$nlambdas_person,seed=input$seed_person,
                                               shuffle=input$shuffle_person,stop_train=input$stop_train_person,stop_test=input$stop_test_person,family=ifelse(input$family_person,'binary','continuous'),
                                               pred_min=input$pred_min_person,pred_max=input$pred_max_person)
          updateProgressBar(session = session, id = "pb", value = (entry/length(data()))*100)
          # Store the results
          if (nrow(results_model$metrics)!=0){
            results_model$metrics$entry <- entry
            results_all[((nrow(results_all)+1):(nrow(results_all)+nrow(results_model$metrics))),1:ncol(results_model$metrics)] <- results_model$metrics}
        }
        # Return results
        results_person$results <- results_all
        results_person$results_metrics_average <- aggregate(dplyr::select(results_all,-c('entry','fold')),list(entry=results_all$entry),mean,na.rm=T)
        results_person$results_metrics_average$type <- 'mean'
        results_person$results_metrics_average <- aggregate(dplyr::select(results_person$results_metrics_average,-c('entry','type')),list(type=results_person$results_metrics_average$type),mean,na.rm=T)
        results_person$results_metrics_median <- aggregate(dplyr::select(results_all,-c('entry','fold')),list(entry=results_all$entry),median,na.rm=T)
        results_person$results_metrics_median$type <- 'median'
        results_person$results_metrics_median <- aggregate(dplyr::select(results_person$results_metrics_median,-c('entry','type')),list(type=results_person$results_metrics_median$type),median,na.rm=T)
        
        # Reorder columns
        results_person$results <- results_person$results[,c(ncol(results_person$results),1:(ncol(results_person$results)-1))]
        
        # Rename columns
        results_person$results <- results_person$results %>%
          dplyr::rename(
            participant = entry
          )
        
        # Create ranked results
        results_person$results_ranked <- results_person$results
        results_person$results_ranked[,c(input$predictors_con_person,input$predictors_cat_person)] <- coefficient_ranker(results_person$results_ranked[,c(input$predictors_con_person,input$predictors_cat_person)])
      }
    })
    
    # Show table
    observeEvent(input$model_type, {
      # Switch panels
      updateTabsetPanel(inputId = "switcher_family_results", selected = ifelse(input$model_type,'results_family_pooled','results_family_person'))
      # Results for pooled model
      if (input$model_type==T){
        # Show table
        output$results_table_pooled_average <- DT::renderDT({
          datatable(rbind(results_pooled$results_metrics_average,results_pooled$results_metrics_median), 
                    options = list(paging = FALSE),caption = htmltools::tags$caption( style = 'caption-side: top; color:black;  font-size:150% ;','Table 1: Average and median of the metrics'))
        })
        output$results_table_pooled_metrics <- DT::renderDT({
          datatable(results_pooled$results_metrics, options = list(paging = FALSE),
                    caption = htmltools::tags$caption(style = 'caption-side: top; color:black;  font-size:150% ;','Table 2: Full metrics'))
        })
        output$results_table_pooled_estimates <- DT::renderDT({
          datatable(results_pooled$results_estimates, options = list(paging = FALSE),
                    caption = htmltools::tags$caption(style = 'caption-side: top; color:black;  font-size:150% ;','Table 3: Estimates'))
        })
      }
      # Results for person-specific model
      else if (input$model_type==F){
        output$results_table_person_average <- DT::renderDT({
          datatable(rbind(results_person$results_metrics_average,results_person$results_metrics_median), options = list(paging = FALSE),
                    caption = htmltools::tags$caption(style = 'caption-side: top; color:black;  font-size:150% ;','Table 1: Average and median of the metrics and estimates'))
        })
        output$results_table_person <- DT::renderDT({
          datatable(results_person$results, options = list(paging = FALSE),
                    caption = htmltools::tags$caption(style = 'caption-side: top; color:black;  font-size:150% ;','Table 2: Full metrics and estimates'))
        })
      }
    })
    
    # Download data pooled
    output$download_pooled <- downloadHandler(
      filename = function() {'results_pooled.xlsx'},
      content = function(file) {
        # Create workbook
        OUT <- createWorkbook()
        
        # Add some sheets to the workbook
        addWorksheet(OUT, "average_metrics")
        addWorksheet(OUT, "full_metrics")
        addWorksheet(OUT, "full_estimates")
        
        # Write the data to the sheets
        writeData(OUT, sheet = "average_metrics", x = rbind(results_pooled$results_metrics_average,results_pooled$results_metrics_median))
        writeData(OUT, sheet = "full_metrics", x = results_pooled$results_metrics)
        writeData(OUT, sheet = "full_estimates", x = results_pooled$results_estimates)
        
        # Export the file
        saveWorkbook(OUT, file)
      }
    )
    
    # Download data person
    output$download_person <- downloadHandler(
      filename = function() {'results_person.xlsx'},
      content = function(file) {
        # Create workbook
        OUT <- createWorkbook()
        
        # Add some sheets to the workbook
        addWorksheet(OUT, "average_metrics_and_estimated")
        addWorksheet(OUT, "full_metrics_and_estmates")
        
        # Write the data to the sheets
        writeData(OUT, sheet = "average_metrics_and_estimated", x = rbind(results_person$results_metrics_average,results_person$results_metrics_median))
        writeData(OUT, sheet = "full_metrics_and_estmates", x = results_person$results)
        
        # Export the file
        saveWorkbook(OUT, file)
      }
    )
    
    # Create reactive elements for plots
    plot_estimates_pooled_parametric <- reactive({
      NLML_plot(data.frame(t(apply(dplyr::select(results_pooled$results_estimates,c(input$predictors_within_con_pooled,input$predictors_between_con_pooled,
                                                                                    input$predictors_within_cat_pooled,input$predictors_between_cat_pooled)),2,mean,na.rm=T))),
                percentile = input$percentile,range = input$range,title = input$title,subtitle = input$subtitle,
                xlim = input$xlim,ylab = input$ylab,xlab = input$xlab,gradient_values = seq(from=input$gradient[1],to=input$gradient[2],length.out=11))}) 
    
    plot_estimates_pooled_nonparametric <- reactive({
      NLML_plot(dplyr::select(aggregate(dplyr::select(results_person$results,-c('participant','fold')),list(participant=results_person$results$participant),mean,na.rm=T),c(input$predictors_con_person,input$predictors_cat_person)),
                percentile = input$percentile,range = input$range,title = input$title,subtitle = input$subtitle,
                xlim = input$xlim,ylab = input$ylab,xlab = input$xlab,gradient_values = seq(from=input$gradient[1],to=input$gradient[2],length.out=11))})
    
    plot_estimates_person_parametric <- reactive({
      NLML_plot_ranked_split(data.frame(t(apply(dplyr::select(results_pooled$results_estimates_ranked,c(input$predictors_within_con_pooled,input$predictors_between_con_pooled,
                                                                                                        input$predictors_within_cat_pooled,input$predictors_between_cat_pooled)),2,mean,na.rm=T))),
                             percentile = input$percentile,range = input$range,title = input$title,subtitle = input$subtitle,
                             xlim = input$xlim,ylab = input$ylab,xlab = input$xlab,gradient_values = seq(from=input$gradient[1],to=input$gradient[2],length.out=11))})
    
    plot_estimates_person_nonparametric <- reactive({
      NLML_plot_ranked_split(dplyr::select(aggregate(dplyr::select(results_person$results_ranked,-c('participant','fold')),list(participant=results_person$results_ranked$participant),mean,na.rm=T),c(input$predictors_con_person,input$predictors_cat_person)),
                             percentile = input$percentile,range = input$range,title = input$title,subtitle = input$subtitle,
                             xlim = input$xlim,ylab = input$ylab,xlab = input$xlab,gradient_values = seq(from=input$gradient[1],to=input$gradient[2],length.out=11))
    })
    # Plot estimates
    observeEvent(req(input$tabset == 'Plots'),{
      observeEvent(input$plot_type,{
        if (input$plot_type==T){
          if (input$model_type==T){
            output$plot_estimates <- renderPlot({plot_estimates_pooled_parametric()},res = 144)
          }
          else if (input$model_type==F){
            output$plot_estimates <- renderPlot({plot_estimates_pooled_nonparametric()},res = 144)
          }
        }
        else if (input$plot_type==F){
          if (input$model_type==T){
            output$plot_estimates <- renderPlot({plot_estimates_person_parametric()},res = 144)
          }
          else if (input$model_type==F){
            output$plot_estimates <- renderPlot({plot_estimates_person_nonparametric()},res = 144)
          }
        }
      })
    })
    
    # Download plot
    observeEvent(req(input$tabset == 'Plots'),{
      output$download_plot <- downloadHandler(
        filename = function() {'plot.png'},
        content = function(file) {
          png(file,units='in',width=4, height=2.5, res = 144)
          if (input$plot_type==T){
            if (input$model_type==T){
              plot(plot_estimates_pooled_parametric())
            }
            else if (input$model_type==F){
              plot(plot_estimates_pooled_nonparametric())
            }
          }
          else if (input$plot_type==F){
            if (input$model_type==T){
              plot(plot_estimates_person_parametric())
            }
            else if (input$model_type==F){
              plot(plot_estimates_person_nonparametric())
            }
          }
          dev.off()
        })
    })
  }
  
  #### Run the application ####
  shinyApp(ui = ui, server = server)
}

