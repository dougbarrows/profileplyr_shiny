library(shiny)
library(DT)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)

options(shiny.maxRequestSize=200*1024^2) 

ui <- dashboardPage(
  header = dashboardHeader(title = "profileplyr"),
  sidebar = dashboardSidebar(
    sidebarMenu(
    menuItem("Input data", tabName = "input_data", icon = icon("upload")),
    menuItem("Explore Ranges", tabName = "explore_ranges", icon = icon("plane")),
    menuItem("Manupulate Ranges", tabName = "manipulate_ranges", icon = icon("cut")),
    menuItem("Visualize", tabName = "visualize", icon = icon("eye")))),
  
  body = dashboardBody(
    
    tabItems(
      tabItem("input_data",
              box(width = 8,
                htmlOutput("generate_headline"),
                  radioButtons(inputId = "inputType",
                               label = "What are you starting from?",
                               choices = c("BAM/bigwig/bed files" = "bam_bigwig_bed",
                                           "deepTools matrix (from computeMatrix)" = "deepTools",
                                           "profileplyr object (e.g. RData file)" = "profileplyr"),
                               selected = character(0)),
                  
                  
                  ###################################################
                  # conditions if user selects bam/bigwig/bed input
                  ###################################################
                  conditionalPanel(
                    condition = "input.inputType == 'bam_bigwig_bed'",
                    radioButtons(inputId = "format",
                                 label = "Signal File Format:",
                                 choices = c("BAM" = "bam",
                                             "bigwig" = "bigwig"),
                                 inline = TRUE),
                    textInput(inputId = "signalFiles",
                              label = "Enter paths to bigwig or bam files (each separated by a comma):",
                              value = "/Users/douglasbarrows 1/Desktop/R_functions/profileplyr/inst/extdata/Sorted_Liver_day_12_1_filtered.bam"),
                    textInput(inputId = "testRanges",
                              label = "Enter paths to bed files (each separated by a comma):",
                              value = "/Users/douglasbarrows 1/Desktop/R_functions/profileplyr/inst/extdata/newranges.bed"),
                    actionButton(inputId = "go_bam_bigwig",
                                 label = "Generate profileplyr object"),
                  # option to download
                    uiOutput("download_bam_bigwig_bed")),
                  # 
                  # ###################################################
                  # # conditions if the user selects deepTools matrix
                  # ###################################################
                  conditionalPanel(
                    condition = "input.inputType == 'deepTools'",
                    textInput(inputId = "deepTools_mat_path",
                              label = "Enter path to deeptools matrix (output of computeMartix):",
                              value = "/Users/douglasbarrows 1/Desktop/R_functions/profileplyr/inst/extdata/example_deepTools_MAT"),
                    actionButton(inputId = "go_deepTools",
                                 label = "Generate profileplyr object"),
                  
                  # the option to download the profileplyr object
                    uiOutput("download_deepTools")),
                  
                  ###################################################
                  # conditions if the user selects direct profileplyr object upload
                  ###################################################
                  conditionalPanel(
                    condition = "input.inputType == 'profileplyr'",
                    fileInput(inputId = "profileplyr_upload",
                              label = "Upload profileplyr object")),
                  
                  htmlOutput("object_headline"),
                  verbatimTextOutput("proplyr_print")
              )
      ),
      tabItem("explore_ranges",
              
              box(DT::dataTableOutput("rangeTable"), width = 12)
      ),
      tabItem("manipulate_ranges",
              box(
                checkboxGroupInput(
                  'select', 'Select Functions:', 
                  choices = c("change metadata column used for grouping" = "change_groups_in_use",
                              "group by list of genes" = "groupby_genes_choice",
                              "cluster ranges" = "cluster_choice",
                              "ChIPseeker annotate" = "annotate_cs_choice",
                              "GREAT gene annotation" = "annotate_great_choice")
                ),
                conditionalPanel(
                  condition = "input.select.includes('change_groups_in_use')",
                  uiOutput("columns_for_groups")
                ),
                
                conditionalPanel(
                  condition = "input.select.includes('groupby_genes_choice')",
                  uiOutput("gene_list_instruct"),
                  fileInput(inputId = "gene_list",
                            label = "Upload one or more gene lists",
                            multiple = TRUE,
                            accept = c(".csv")),
                  radioButtons(inputId = "include_nonoverlapping",
                               label = "Do you want to include the ranges that do not overlap with your gene lists?",
                               choices = c("no" = FALSE,
                                           "yes" = TRUE),
                               inline = TRUE),
                  actionButton(inputId = "perform_groupby_genes",
                               label = "Group the ranges by gene list")),
                
                conditionalPanel(
                  condition = "input.select.includes('cluster_choice')",
                  htmlOutput("cluster_title"),
                  radioButtons(inputId = "cluster_method",
                               label = "What clustering method to use:",
                               choices = c("kmeans" = "kmeans",
                                           "hierarchical clustering" = "hclust")),
                  numericInput(inputId = "cluster_number", 
                               label = "Number of clusters:",
                               value = 1,
                               min = 1,
                               max = NA),
                  actionButton(inputId = "perform_cluster",
                               label = "Perform Clustering")),
                
                conditionalPanel(
                  condition = "input.select.includes('annotate_cs_choice')",
                  htmlOutput("annotate_cs_title"),
                  selectInput(inputId = "cs_genome",
                              label = "Select a genome:",
                              choices = c("hg19", "hg38", "mm9", "mm10"),
                              selected = c("hg19"),
                              multiple = FALSE),
                  checkboxGroupInput(inputId = "annotation_subset",
                                     label = "Specific annotation types to use:",
                                     choices = c("Promoter", "Exon", "Intron", "Downstream", "Distal Intergenic", "3p UTR", "5p UTR"),
                                     selected = c("Promoter", "Exon", "Intron", "Downstream", "Distal Intergenic", "3p UTR", "5p UTR")),
                  sliderInput(inputId = "tss_region",
                              label = "Transcription Start Site Region: \n *TSS = 0",
                              min = -5000,
                              max = 5000,
                              value = c(-3000, 3000),
                              step = 100),
                  actionButton(inputId = "perform_annotate_cs",
                               label = "Perform annotation with ChIPseeker")),
                
                conditionalPanel(
                  condition = "input.select.includes('annotate_great_choice')",
                  htmlOutput("annotate_great_title"),
                  selectInput(inputId = "great_species",
                              label = "Select a genome:",
                              choices = c("hg19", "mm10", "mm9", "danRer7"),
                              selected = c("hg19"),
                              multiple = FALSE),
                  actionButton(inputId = "perform_annotate_great",
                               label = "Perform gene annotation with GREAT"))
                
              )
              
      ),
      tabItem("visualize",
              box(width = 4,
                radioButtons(inputId = "extra_annotation_columns_query",
                             label = "Do you want to add any extra heatmaps from a range metadata column?",
                             choices = c("yes" = "yes",
                                         "no" = "no"),
                             selected = "no",
                             inline = TRUE),
                conditionalPanel(
                  condition = "input.extra_annotation_columns_query == 'yes'",
                  uiOutput("extra_annotation_columns")
                ),
                radioButtons(inputId = "include_group_annotation",
                             label = "Include group annotation:",
                             choices = c("TRUE" = TRUE,
                                         "FALSE" = FALSE),
                             inline = TRUE),
                htmlOutput("sample_names_title"),
                checkboxInput(inputId = "sample_names_query",
                              label = "Click to change heatmap sample names"),
                conditionalPanel(
                  condition = "input.sample_names_query != 0",
                  uiOutput("sample_names")
                ),
                htmlOutput("matrices_color_title"),
                checkboxInput(inputId = "matrices_color_query",
                              label = "Click to change heatmap colors"),
                conditionalPanel(
                  condition = "input.matrices_color_query != 0",
                  uiOutput("matrices_color")
                ),
                radioButtons(inputId = "ylim_query",
                             label = "Y-axis limit setting:",
                             choices = c("Common Max (Default)" = "common_max",
                                         "Inferred spearately" = "inferred", # since this is NULL, had toruble buidling in, can essentially do this with custom, so leave out for now
                                         "Custom" = "custom"),
                             selected = "common_max",
                             inline = FALSE),
                conditionalPanel(
                  condition = "input.ylim_query == 'custom'",
                  uiOutput("ylim")
                )
                
                
                
              ),
             
              box(width = 8,                         
                actionButton(inputId = "MakeEnrichedHeatmap_direct_file",
                             label = "Make range heatmap"),
                plotOutput("EnrichedHeatmap_direct_file") %>% withSpinner(type = 4),
                uiOutput("download_heatmap_button")
                
                
              )
      )
    )
  )
)




server <- function(input, output, session) {
  library(profileplyr)
  library(dplyr)
  library(shinyjs)

  output$sample_names_title <- renderText("<b> Heatmap Sample Names: <b>")
  output$matrices_color_title <- renderText("<b> Heatmap Color Schemes: <b>")
  output$cluster_title <- renderText("<br><br><b> Clustering of Ranges: <b> <br><br>")
  output$annotate_cs_title <- renderText("<br><br><b> Annotation with ChIPseeker: <b> <br><br>")
  output$annotate_great_title <- renderText("<br><br><b> Annotation with GREAT: <b> <br><br>")
  output$gene_list_instruct <- renderText("<b> NOTE: Gene lists must be .csv files with the gene symbols in the first column. <br> Additional columns (e.g. gene expression values) will be included in the new range metadata. <b> <br><br>")
  
  output$object_headline <- renderText("<br><b> Current profileplyr object for analysis: <b> <br><br>")
  output$generate_headline <- renderText("<b> Generate, upload, or refresh profileplyr object: <b> <br><br>")
  
  observeEvent(input$resetAll, {
    reset("inputType")
  })
  
  rv <- reactiveValues(proplyr = "No profileplyr object has been uploaded/generated")
  
 
  # read in signal files and bed files to make profileplyr object from Bam/bigwig/bed
  from_bam_bigwig_bed <- observeEvent(input$go_bam_bigwig, {
    withProgress(message = 'Calculation in progress',
                 value = 0.5, {

      signalFiles <- strsplit(input$signalFiles, split = ",") %>%
        unlist() %>%
        trimws()
      testRanges <- strsplit(input$testRanges, split = ",") %>%
        unlist() %>%
        trimws()
      rv$proplyr <- BamBigwig_to_chipProfile(signalFiles = signalFiles,
                                             testRanges = testRanges,
                                             format = input$format) %>%
        as_profileplyr()
    })
  })
  

  # read in from deeptools
  from_deepTools <- observeEvent(input$go_deepTools, {
    withProgress(message = 'Calculation in progress',
                 value = 0.5, {
      rv$proplyr <- import_deepToolsMat(input$deepTools_mat_path)
    })
  })
  
  
  ## read in from profileplyr object upload
  from_direct_file <- observeEvent(input$profileplyr_upload, {
      file <- input$profileplyr_upload
      rv$proplyr <- readRDS(file$datapath)

  })
  
  # print object to screen if through direct upload
  output$proplyr_print <- renderPrint({
 
      rv$proplyr

  })
 

  # make interactive data table in 'Explore Ranges' tab for direct profileplyr option
  update_mcol <- eventReactive(rv$proplyr, {
    rv$mcol <- rowRanges(rv$proplyr) %>%
      as.data.frame()
  })
  
  # ranges_direct_file <- eventReactive(input$update_range_table, {
  #   rv$mcol
  # })
  
  output$rangeTable <- DT::renderDataTable({
    DT::datatable(update_mcol(), 
                  options = list(pageLength = 25,
                                 scrollX = TRUE))
  })
  
  # set parameters for generateEnrichedHeatmap()
  output$extra_annotation_columns <- renderUI({
    selectInput(inputId = "extra_annotation_columns_input",
              label = "Name of column used for extra annotation column:",
              choices = colnames(mcols(rv$proplyr)),
              multiple = TRUE,
              selectize = TRUE)
  })
  extra_annotation_columns <- reactive({
    ifelse(input$extra_annotation_columns_query %in% "no", list(NULL), list(input$extra_annotation_columns_input))
  })
  
  include_group_annotation <- reactive({
    input$include_group_annotation
  })

  # produce the text boxes for user to input sample names
  output$sample_names <- renderUI({
    numSamples <- length(assays(rv$proplyr))
    lapply(1:numSamples, function(i) {
      textInput(inputId = paste0("sample_names_list", i),
                label = paste0("Heatmap ", i, " name"),
                value = rownames(sampleData(rv$proplyr))[i])
    })
  })

  # create list of names user enters, or sets to default names if user unclicks the box
  sample_names_new <- reactive({
    numSamples <- length(assays(rv$proplyr))
    temp <- list()
    if (input$sample_names_query != 0){
      for(i in seq(numSamples)){
        temp[[i]] <- input[[paste0("sample_names_list", i)]]
      }
    }else{ 
      for(i in seq(numSamples)){
        temp[[i]] <-  rownames(sampleData(rv$proplyr))[i]
      }
    }
    temp
  })
  
  # produce the text boxes for user to input colors
  output$matrices_color <- renderUI({
    numSamples <- length(assays(rv$proplyr))
    lapply(1:numSamples, function(i) {
      textInput(inputId = paste0("matrices_color_list", i),
                label = paste0("Heatmap ", i, " colors (either two or three colors separated by commas)"),
                value = "blue, white, red")
    })
  })
  
  
  # this creates a list of the colors that the use entered
  # this was tricky because we wanted this to remain a list if not NULL, but not be a list if NULL (Default), so we couldn't just make everything a list, then unlist in heatmap call. 
  # So we just construct the default list anyway, don't use NULL and don't unlist in heatmap call
  matrices_color_new <- reactive({
    numSamples <- length(assays(rv$proplyr))
    temp <- list()
    #new_colors <- list()
    if (input$matrices_color_query != 0){
      for(i in seq(numSamples)){
        temp[[i]] <- input[[paste0("matrices_color_list", i)]]
        temp[[i]] <- strsplit(temp[[i]], split = ",") %>%
          unlist() %>%
          trimws()
      }
    }else{
      for(i in seq(numSamples)){
        temp[[i]] <-  c("blue", "white", "red")
      }
    }
      temp
    })

  
  output$ylim <- renderUI({
    numSamples <- length(assays(rv$proplyr))
    
    # from the geenrateEnrichedHeatmap code to get mins and maxes for slider
    scoreMat <- do.call(cbind,
                        as.list(assays(rv$proplyr)))
    group_boundaries <- c(which(!duplicated(rowData(rv$proplyr)[params(rv$proplyr)$rowGroupsInUse]))-1,length(rv$proplyr))
    
    group_sub <- vector(mode = "list", length = length(group_boundaries)-1)
    for(i in seq_along(group_boundaries[-(length(group_boundaries))])){
      if(i==1){
        group_sub[[i]] <- scoreMat[group_boundaries[i]:group_boundaries[i+1], ]
      }else{
        group_sub[[i]] <- scoreMat[(group_boundaries[i]+1):group_boundaries[i+1], ]
      }
    }
      
      # get the max and min col mean accounting for groups
      col_means <- vector(mode = "list", length = length(group_sub))
      for (i in seq_along(group_sub)){
        col_means[[i]] <- colMeans(group_sub[[i]])
      }
      col_means_unlist <- unlist(col_means)
      
      col_means_max <- max(col_means_unlist)
      # get the value for which this would be 90% to giv esome room in the figure
      max_for_figure = col_means_max/0.8
      
      col_means_min <- min(col_means_unlist)
      min_for_figure <- col_means_min/0.8
      
      if (min_for_figure < 0){
        min_for_figure <- col_means_min/0.8
      }else {
        min_for_figure <- 0
      }
      
      lapply(1:numSamples, function(i) {
        sliderInput(inputId = paste0("ylim_list", i),
                    label = paste0("Y-limits for Heatmap ", i),
                    min = min_for_figure,
                    max = max_for_figure,
                    value = c(min_for_figure, max_for_figure))})
    })
  
  # when the button to make the enrichedheatmap is pressed, the ranges from the sliders will be taken into account
  ylim_new <- eventReactive(input$MakeEnrichedHeatmap_direct_file, {
    if(input$ylim_query == "custom")
           { numSamples <- length(assays(rv$proplyr))
           temp <- list()
           for(i in seq(numSamples)){
             temp[[i]] <- input[[paste0("ylim_list", i)]]
           }
           temp }
  
    else if (input$ylim_query == "common_max") {
      "common_max"
    } # will default to NULL (inferred) if it gets past these if statements

  })
  
  # make heatmap in 'Visualize' tab for direct file upload
  makeHeatmap_direct_file <- eventReactive(input$MakeEnrichedHeatmap_direct_file, {
    rv$heatmap <- generateEnrichedHeatmap(object = rv$proplyr, 
                            extra_annotation_columns = unlist(extra_annotation_columns()),
                            include_group_annotation = include_group_annotation(),
                            sample_names = unlist(sample_names_new()),
                            ylim = ylim_new(),
                            matrices_color = matrices_color_new())
                          
  })
  
  output$EnrichedHeatmap_direct_file <- renderPlot({
    rv$heatmap
  })
  
  output$download_heatmap_button <- renderUI({
    if(!is.null(makeHeatmap_direct_file())) {
      downloadButton("download_heatmap", "Download Heatmap")
    }
  })
  
  options(shiny.usecairo=TRUE)
  
  output$download_heatmap <- downloadHandler(
    filename = function() {
      paste("EnrichedHeatmap", "_",Sys.time(),".pdf",sep="")
    },
    content = function(file) {
        cairo_pdf(filename = file)
        print(makeHeatmap_direct_file())
        dev.off()

    }
  )
 
  ############
  # range manipulation
  ############
  
  ######change grouping
  
  output$columns_for_groups <- renderUI(
    textInput(inputId = "columns_for_groups_dropdown",
                label = "Enter the name of the column from the range matadata to be used for grouping:",
                value = params(rv$proplyr)$rowGroupsInUse)
  )
  
  change_grouping <- observeEvent(input$columns_for_groups_dropdown, {
    rv$proplyr <- groupBy(object = rv$proplyr,
                          group = input$columns_for_groups_dropdown)
  })
  
  ##### groupBy gene list

  
  make_gene_list <- observeEvent(input$gene_list, {
    rv$genes <- lapply(input$gene_list$datapath, 
                       read.csv, 
                       header=TRUE,
                       stringsAsFactors = FALSE)
    for(i in seq_along(rv$genes)){
      rv$genes[[i]] <- rv$genes[[i]][!duplicated(rv$genes[[i]][,1]),]
      rownames(rv$genes[[i]]) <- NULL
      rownames(rv$genes[[i]]) <- rv$genes[[i]][,1]
      rv$genes[[i]] <- as.data.frame(rv$genes[[i]][-1])
    }
  })
  
  include_nonoverlapping <- reactive({
    include_nonoverlapping = input$include_nonoverlapping
  })
  
  groupby_gene_list_go <- observeEvent(input$perform_groupby_genes, {
    withProgress(message = 'Calculation in progress',
                 value = 0.5, {
                   rv$proplyr <- groupBy(rv$proplyr,
                                         group = rv$genes,
                                         include_nonoverlapping = include_nonoverlapping())
                 })
    
  }, ignoreInit = TRUE)
  
  #### clustering
  
  kmeans_k <- reactive({
    if(input$cluster_method == "kmeans"){
      kmeans_k = input$cluster_number
    } else{
      kmeans_k = NULL
    }
  })
    
    cutree_rows <- reactive({
      if(input$cluster_method == "hclust"){
        cutree_rows = input$cluster_number
      } else{
        cutree_rows = NULL
      }

      })

    cluster <- observeEvent(input$perform_cluster, {
      withProgress(message = 'Calculation in progress',
                   value = 0.5, {
                     set.seed(0)
                     rv$proplyr <- clusterRanges(rv$proplyr,
                                                 fun = "rowMeans",
                                                 kmeans_k = kmeans_k(),
                                                 cutree_rows = cutree_rows())
                   })
      
    }, ignoreInit = TRUE)
  

  ##### annotate with chipseeker
  annotate_cs <- observeEvent(input$perform_annotate_cs, {
    withProgress(message = 'Calculation in progress',
                 value = 0.5, {
        rv$proplyr <- annotateRanges(rv$proplyr,
                                     TxDb = input$cs_genome,
                                     annotation_subset = input$annotation_subset,
                                     tssRegion = input$tss_region)

    })
  }, ignoreInit = TRUE)
  
  ##### annotate with great
  annotate_great <- observeEvent(input$perform_annotate_great, {
    withProgress(message = 'Calculation in progress',
                 value = 0.5, {
                   rv$proplyr <- annotateRanges_great(rv$proplyr,
                                                      species = input$great_species)
                   
                 })
  }, ignoreInit = TRUE)
  
}

shinyApp( ui = ui, server = server)