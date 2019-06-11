library(shiny)
library(DT)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(rtracklayer)
library(GenomicRanges)
library(grDevices)



options(shiny.maxRequestSize=1000*1024^2) 

ui <- dashboardPage(
  header = dashboardHeader(title = "profileplyr"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Input data", tabName = "input_data", icon = icon("upload")),
      menuItem("Explore Ranges", tabName = "explore_ranges", icon = icon("plane")),
      menuItem("Manupulate Ranges", tabName = "manipulate_ranges", icon = icon("cut")),
      menuItem("Visualize", tabName = "visualize", icon = icon("eye")))
    #sidebarMenuOutput("other_tabs"))
    ),
  
  body = dashboardBody(
    
    tabItems(
      tabItem("input_data", class = "active",
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
                              value = ""),
                    textInput(inputId = "testRanges",
                              label = "Enter paths to bed files (each separated by a comma):",
                              value = ""),
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
                    fileInput(inputId = "deepTools_mat_upload",
                              label = "Upload deeptools matrix (output of computeMartix):"),
                    # actionButton(inputId = "go_deepTools",
                    #              label = "Generate profileplyr object"),
                  
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
                  verbatimTextOutput("proplyr_print"),
                uiOutput("download_object_button"),
                uiOutput("clear_object"),
                uiOutput("clear_warning")
              )
      ),
      tabItem("explore_ranges",
              uiOutput("explore_ranges_render")  
              
              # box(DT::dataTableOutput("rangeTable"), width = 12)
      
      ),
      tabItem("manipulate_ranges",
              uiOutput("manipulate_render")
              # box(width = 12,
              # 
              #     checkboxGroupInput(
              #       'select', 'Select Functions:',
              #       choices = c("cluster ranges" = "cluster_choice",
              #                   "annotate ranges with genomic regions (promoters, exons, etc.) and closest gene with ChIPseeker" = "annotate_cs_choice",
              #                   "annotate ranges with genes using GREAT" = "annotate_great_choice",
              #                   "filter or group the ranges by lists of genes (NOTE: must annotate with ChIPseeker or GREAT first)" = "groupby_genes_choice")
              #     ),
              #     
              #     
              #     conditionalPanel(
              #       condition = "input.select.includes('cluster_choice')",
              #       htmlOutput("cluster_title"),
              #       radioButtons(inputId = "cluster_method",
              #                    label = "What clustering method to use:",
              #                    choices = c("kmeans" = "kmeans",
              #                                "hierarchical clustering" = "hclust")),
              #       numericInput(inputId = "cluster_number",
              #                    label = "Number of clusters:",
              #                    value = 1,
              #                    min = 1,
              #                    max = NA),
              #       actionButton(inputId = "perform_cluster",
              #                    label = "Perform Clustering")),
              #     
              #     conditionalPanel(
              #       condition = "input.select.includes('annotate_cs_choice')",
              #       htmlOutput("annotate_cs_title"),
              #       selectInput(inputId = "cs_genome",
              #                   label = "Select a genome:",
              #                   choices = c("hg19", "hg38", "mm9", "mm10"),
              #                   selected = c("hg19"),
              #                   multiple = FALSE),
              #       checkboxGroupInput(inputId = "annotation_subset",
              #                          label = "Specific annotation types to use:",
              #                          choices = c("Promoter", "Exon", "Intron", "Downstream", "Distal Intergenic", "3p UTR", "5p UTR"),
              #                          selected = c("Promoter", "Exon", "Intron", "Downstream", "Distal Intergenic", "3p UTR", "5p UTR")),
              #       sliderInput(inputId = "tss_region",
              #                   label = "Transcription Start Site Region: \n *TSS = 0",
              #                   min = -5000,
              #                   max = 5000,
              #                   value = c(-3000, 3000),
              #                   step = 100),
              #       actionButton(inputId = "perform_annotate_cs",
              #                    label = "Perform annotation with ChIPseeker")),
              #     
              #     conditionalPanel(
              #       condition = "input.select.includes('annotate_great_choice')",
              #       htmlOutput("annotate_great_title"),
              #       selectInput(inputId = "great_species",
              #                   label = "Select a genome:",
              #                   choices = c("hg19", "mm10", "mm9", "danRer7"),
              #                   selected = c("hg19"),
              #                   multiple = FALSE),
              #       actionButton(inputId = "perform_annotate_great",
              #                    label = "Perform gene annotation with GREAT")),
              #     
              #     conditionalPanel(
              #       condition = "input.select.includes('groupby_genes_choice')",
              #       uiOutput("gene_list_instruct"),
              #       fileInput(inputId = "gene_list",
              #                 label = "Upload one or more gene lists",
              #                 multiple = TRUE,
              #                 accept = c(".csv")),
              #       radioButtons(inputId = "include_nonoverlapping",
              #                    label = "Do you want to include the ranges that do not overlap with your gene lists?",
              #                    choices = c("no" = FALSE,
              #                                "yes" = TRUE),
              #                    inline = TRUE),
              #       actionButton(inputId = "perform_groupby_genes",
              #                    label = "Group the ranges by gene list"))
              #     
              # )
              # 
      ),
      tabItem("visualize",
              uiOutput("visualize_render_box1"),
              uiOutput("visualize_render_box2")
              # box(width = 4,
              #     uiOutput("columns_for_groups"),
              #     radioButtons(inputId = "include_group_annotation",
              #                  label = "Include group annotation on left side of heatmaps:",
              #                  choices = c("include" = TRUE,
              #                              "don't include" = FALSE),
              #                  inline = TRUE),
              #     htmlOutput("extra_annotation_title"),
              #   htmlOutput("extra_annotation_inputbox_label"),
              #   uiOutput("extra_annotation_columns"),
              #   htmlOutput("sample_names_title"),
              #   checkboxInput(inputId = "sample_names_query",
              #                 label = "Click to change heatmap sample names"),
              #   conditionalPanel(
              #     condition = "input.sample_names_query != 0",
              #     uiOutput("sample_names")
              #   ),
              #   htmlOutput("matrices_color_title"),
              #   checkboxInput(inputId = "matrices_color_query",
              #                 label = "Click to change heatmap colors"),
              #   conditionalPanel(
              #     condition = "input.matrices_color_query != 0",
              #     uiOutput("matrices_color")
              #   ),
              #   radioButtons(inputId = "ylim_query",
              #                label = "Y-axis limit setting:",
              #                choices = c("Common Max (Default)" = "common_max",
              #                            "Inferred spearately" = "inferred", # since this is NULL, had toruble buidling in, can essentially do this with custom, so leave out for now
              #                            "Custom" = "custom"),
              #                selected = "common_max",
              #                inline = FALSE),
              #   conditionalPanel(
              #     condition = "input.ylim_query == 'custom'",
              #     uiOutput("ylim")
              #   )
              # 
              # 
              # 
              # ),
              # 
              # box(width = 8,
              #   actionButton(inputId = "MakeEnrichedHeatmap",
              #                label = "Generate and download range heatmap"),
              #   textInput(inputId = "heatmap_path",
              #             label = "Enter path for heatmap location after download:",
              #             value = paste0(getwd(), "/EnrichedHeatmap", "_",Sys.time(),".pdf")),
              #   plotOutput("EnrichedHeatmap") %>% withSpinner(type = 4)
              #   #uiOutput("download_heatmap_button")
              #   
              # 
              # 
              # )
      )
    )
  )
)




server <- function(input, output, session) {
  library(profileplyr)
  library(dplyr)
  library(shinyjs)
 
  output$explore_ranges_render <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      box(DT::dataTableOutput("rangeTable"), width = 12)
    } else {
      htmlOutput("no_object_message_explore")
    }
  })
  
  output$manipulate_render <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      box(width = 12,
          
          checkboxGroupInput(
            'select', 'Select Functions:',
            choices = c("cluster ranges" = "cluster_choice",
                        "annotate ranges with genomic regions (promoters, exons, etc.) and closest gene with ChIPseeker" = "annotate_cs_choice",
                        "annotate ranges with genes using GREAT" = "annotate_great_choice",
                        "filter or group the ranges based on whether they overlap with other sets of ranges (i.e. bed files)" = "groupby_granges_choice",
                        "filter or group the ranges by lists of genes (NOTE: must annotate with ChIPseeker or GREAT first)" = "groupby_genes_choice")
          ),
          
          
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
                         label = "Perform gene annotation with GREAT")),
          
          conditionalPanel(
            condition = "input.select.includes('groupby_granges_choice')",
            fileInput(inputId = "granges",
                      label = "Upload one or more bed files",
                      multiple = TRUE),
            conditionalPanel(
              condition = "output.bed_file_uploaded",
                radioButtons(inputId = "include_nonoverlapping_granges",
                         label = "Do you want to include the ranges that do not overlap with these bed files in the heatmap?",
                         choices = c("no" = FALSE,
                                     "yes" = TRUE),
                         inline = TRUE),
            uiOutput("granges_names"),
            actionButton(inputId = "perform_groupby_granges",
                         label = "Group the ranges by bed file overlap"))),
          
          conditionalPanel(
            condition = "input.select.includes('groupby_genes_choice')",
            uiOutput("gene_list_instruct"),
            fileInput(inputId = "gene_list",
                      label = "Upload one or more gene lists",
                      multiple = TRUE,
                      accept = c(".csv")),
            conditionalPanel(
              condition = "output.gene_list_uploaded",
            radioButtons(inputId = "include_nonoverlapping_gene_list",
                         label = "Do you want to include the ranges that do not overlap with your gene lists in the heatmap?",
                         choices = c("no" = FALSE,
                                     "yes" = TRUE),
                         inline = TRUE),
            uiOutput("gene_list_names"),
            actionButton(inputId = "perform_groupby_genes",
                         label = "Group the ranges by gene lists"))
          )   
      )
      
    } else {
      htmlOutput("no_object_message_manipulate")
    }
  })
  
  output$visualize_render_box1 <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      box(width = 4,
          uiOutput("columns_for_groups"),
          radioButtons(inputId = "include_group_annotation",
                       label = "Include group annotation on left side of heatmaps:",
                       choices = c("include" = TRUE,
                                   "don't include" = FALSE),
                       inline = TRUE),
          htmlOutput("extra_annotation_title"),
          htmlOutput("extra_annotation_inputbox_label"),
          uiOutput("extra_annotation_columns"),
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
          
          
          
      )
    }
  })
      
  output$visualize_render_box2 <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      box(width = 8,
          radioButtons(inputId = "local_vs_internet",
                       label = "Are you deploying this app from a local machine or from the internet?",
                       choices = c("local", "internet"),
                       selected = "local"),

          conditionalPanel(
            condition = "input.local_vs_internet == 'local'",
            #uiOutput("download_heatmap_button")
            actionButton(inputId = "MakeEnrichedHeatmap_local",
                         label = "Generate range heatmap and download"),
            textInput(inputId = "heatmap_path_local",
                      label = "Enter path for heatmap location if downloading from local launch:",
                      value = paste0(getwd(), "/EnrichedHeatmaplocal", "_",Sys.time(),".pdf")),
            plotOutput("EnrichedHeatmap_local")


          ),
          conditionalPanel(
            condition = "input.local_vs_internet == 'internet'",
            actionButton(inputId = "MakeEnrichedHeatmap",
                         label = "Generate range heatmap only"),
            # textInput(inputId = "heatmap_path",
            #           label = "Enter path for heatmap location if downloading from local launch:",
            #           value = paste0(getwd(), "/EnrichedHeatmap", "_",Sys.time(),".pdf")),
            plotOutput("EnrichedHeatmap"), #%>% withSpinner(type = 4)
            downloadButton(outputId = "download_heatmap",
                           label = "Download range heatmap")
          )
      )
    } else {
      htmlOutput("no_object_message_visualize")
    }
  })
  
  output$no_object_message_explore <- renderText("<b> No profileplyr object currently loaded <b>")
  output$no_object_message_manipulate <- renderText("<b> No profileplyr object currently loaded <b>")
  output$no_object_message_visualize <- renderText("<b> No profileplyr object currently loaded <b>")
  output$extra_annotation_title <- renderText("<b> Add extra annotation heatmaps: <b><br><br>")
  output$extra_annotation_inputbox_label <- renderText("Name of column used for extra annotation column:")
  output$sample_names_title <- renderText("<b> Heatmap Sample Names: <b>")
  output$matrices_color_title <- renderText("<b> Heatmap Color Schemes: <b>")
  output$cluster_title <- renderText("<br><br><b> Clustering of Ranges: <b> <br><br>")
  output$annotate_cs_title <- renderText("<br><br><b> Annotation with ChIPseeker: <b> <br><br>")
  output$annotate_great_title <- renderText("<br><br><b> Annotation with GREAT: <b> <br><br>")
  output$gene_list_instruct <- renderText("<b> NOTE: Gene lists must be .csv files with the gene symbols in the first column. <br> Additional columns (e.g. gene expression values) will be included in the new range metadata. <b> <br><br>")
  
  output$object_headline <- renderText("<br><b> Current profileplyr object for analysis: <b> <br><br>")
  output$generate_headline <- renderText("<b> Generate, upload, or refresh profileplyr object: <b> <br><br>")
  
  
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
  from_deepTools <- observeEvent(input$deepTools_mat_upload, {
    withProgress(message = 'Calculation in progress',
                 value = 0.5, {
      file <- input$deepTools_mat_upload
      rv$proplyr <- import_deepToolsMat(file$datapath)
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
 
  
  # this reactive expression will reset the tabs whenever the profileplyr object is not in the appropirate slot of 'rv'
  # this allows us to remove these tabs when 
  # show_other_tabs <- reactive({
  #   if(is(rv$proplyr, "profileplyr")){
  #     sidebarMenu(
  #       menuItem("Explore Ranges", tabName = "explore_ranges", icon = icon("plane")),
  #       menuItem("Manupulate Ranges", tabName = "manipulate_ranges", icon = icon("cut")),
  #       menuItem("Visualize", tabName = "visualize", icon = icon("eye"))
  #     )} else {
  #       sidebarMenu()
  #     }
  # })
  # 
  # 
  # 
  # # render other tables once the object is present
  # output$other_tabs <- renderMenu({
  #   show_other_tabs()
  # })
  # 
  output$download_object_button <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      downloadButton("download_object", "Download profileplyr object")
    }
  })

  output$download_object <- downloadHandler(
      filename = function() {
        paste("proplyrObject", "_",Sys.time(),".RData",sep="")
      },
      content = function(file) {
        saveRDS(rv$proplyr, file)
      }
  )

  output$clear_object <- renderUI({
    if(is(rv$proplyr, "profileplyr")){
      actionButton(inputId = "clear_object_button",
                   label = "Clear current profileplyr object")
    }
  })

  output$clear_warning <- renderText({
    if(is(rv$proplyr, "profileplyr")){
      "Warning: make sure you download object before clearing if you want to keep this object for further analysis! It will be much faster to load the RData file as opposed to starting from BAM/bigwig files in the future. Not necessary to download if starting from RData file already."
    }
  })

  observeEvent(input$clear_object_button, {
    rv$proplyr <- "No profileplyr object has been uploaded/generated"
    rv$mcol <- NULL
    rv$heatmap <- NULL
    rv$heatmap_local <- NULL
  })
  
  
  # make interactive data table in 'Explore Ranges' tab for direct profileplyr option
  update_mcol <- eventReactive(rv$proplyr, {
    rv$mcol <- rowRanges(rv$proplyr) %>%
      as.data.frame()
  })
  
  output$rangeTable <- DT::renderDataTable({
    DT::datatable(update_mcol(), 
                  options = list(pageLength = 25,
                                 scrollX = TRUE))
  })
  
  # set parameters for generateEnrichedHeatmap()
  output$extra_annotation_columns <- renderUI({
    selectInput(inputId = "extra_annotation_columns_input",
                label = "",
                choices = colnames(mcols(rv$proplyr)),
                multiple = TRUE,
                selectize = TRUE)
  })
  extra_annotation_columns <- reactive({
    ifelse(is.null(input$extra_annotation_columns_input), list(NULL), list(input$extra_annotation_columns_input))
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
  ylim_new <- eventReactive(input$MakeEnrichedHeatmap, {
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
  
  # when the button to make the enrichedheatmap is pressed, the ranges from the sliders will be taken into account
  ylim_new_local <- eventReactive(input$MakeEnrichedHeatmap_local, {
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
  
  options(shiny.usecairo=TRUE)
  #make heatmap in 'Visualize' tab for direct file upload
  makeHeatmap <- observeEvent(input$MakeEnrichedHeatmap, {
    withProgress(message = 'Calculation in progress',
                 value = 0.5, {
                   # file = input$heatmap_path
                   # cairo_pdf(filename = file)
                   rv$heatmap <- generateEnrichedHeatmap(object = rv$proplyr,
                                                         extra_annotation_columns = unlist(extra_annotation_columns()),
                                                         include_group_annotation = include_group_annotation(),
                                                         sample_names = unlist(sample_names_new()),
                                                         ylim = ylim_new(),
                                                         matrices_color = matrices_color_new()
                   )
                   # dev.off()
                 })
  })

  output$EnrichedHeatmap <- renderPlot(
    rv$heatmap
  )
  
 

  makeHeatmap_local <- observeEvent(input$MakeEnrichedHeatmap_local, {
    withProgress(message = 'Calculation in progress',
                 value = 0.5, {
                   file = input$heatmap_path_local
                   cairo_pdf(filename = file)
                   rv$heatmap_local <- generateEnrichedHeatmap(object = rv$proplyr,
                                                         extra_annotation_columns = unlist(extra_annotation_columns()),
                                                         include_group_annotation = include_group_annotation(),
                                                         sample_names = unlist(sample_names_new()),
                                                         ylim = ylim_new_local(),
                                                         matrices_color = matrices_color_new()
                   )
                   dev.off()
                 })
  })



  output$EnrichedHeatmap_local <- renderPlot(
    rv$heatmap_local
  )

  output$download_heatmap <- downloadHandler(
    filename = function() {
          paste("heatmap", "_",Sys.time(),".pdf",sep="")
        },
        content = function(file) {
          # temp_file <- file.path(tempdir(), "heatmap_test_ddd.pdf")
          # file.copy("heatmap_test_ddd.pdf", temp_file, overwrite = TRUE)

          #pdf(file)
          cairo_pdf(filename = file)
          rv$heatmap <- generateEnrichedHeatmap(object = rv$proplyr,
                                        extra_annotation_columns = unlist(extra_annotation_columns()),
                                        include_group_annotation = include_group_annotation(),
                                        sample_names = unlist(sample_names_new()),
                                        ylim = ylim_new(),
                                        matrices_color = matrices_color_new()
          )

          dev.off()
        },
        contentType = "application/pdf"
    )


  # output$download_heatmap_button <- renderUI({
  #   if(!is.null(makeHeatmap())) {
  #     downloadButton("download_heatmap", "Download Heatmap")
  #   }
  # })
  #

  #
  # output$download_heatmap <- downloadHandler(
  #   filename = function() {
  #     paste("EnrichedHeatmap", "_",Sys.time(),".pdf",sep="")
  #   },
  #   content = function(file) {
  #       cairo_pdf(filename = file)
  #       print(makeHeatmap())
  #       dev.off()
  #
  #   }
  # )
 
  ############
  # range manipulation
  ############
  
  ######change grouping
  
  output$columns_for_groups <- renderUI(
    selectInput(inputId = "columns_for_groups_dropdown",
                label = "Enter the name of the column from the range matadata to be used for grouping:",
                choices = colnames(mcols(rv$proplyr)),
                selected = params(rv$proplyr)$rowGroupsInUse)
  )
  
  change_grouping <- observeEvent(input$columns_for_groups_dropdown, {
    rv$proplyr <- groupBy(object = rv$proplyr,
                          group = input$columns_for_groups_dropdown)
  })
  
  
  ##### groupBy GRanges
  
  output$bed_file_uploaded <- reactive({
    return(!is.null(input$granges))
  })
  outputOptions(output, "bed_file_uploaded", suspendWhenHidden = FALSE)
  
  make_GRanges <- observeEvent(input$granges, {
    rv$granges <- lapply(input$granges$datapath, 
                       import.bed)
    
    rv$granges <- GRangesList(rv$granges)
    
  })

    
  include_nonoverlapping_granges <- reactive({
    include_nonoverlapping_granges = input$include_nonoverlapping_granges
  })
  
  
  # produce the text boxes for user to input  granges names
  output$granges_names <- renderUI({
    numBedFiles <- length(input$granges$name)
    lapply(1:numBedFiles, function(i) {
      textInput(inputId = paste0("granges_names_list", i),
                label = paste0("Bed File #", i, " name"),
                value = input$granges$name[[i]])
    })
  })
  
  # create list of names user enters, or sets to default names if user unclicks the box
  granges_names_new <- reactive({
    numBedFiles <- length(input$granges$name)
    temp <- list()
      for(i in seq(numBedFiles)){
        temp[[i]] <- input[[paste0("granges_names_list", i)]]
      }
    temp
  })
  
  groupby_granges_go <- observeEvent(input$perform_groupby_granges, {
    withProgress(message = 'Calculation in progress',
                 value = 0.5, {
                   rv$proplyr <- groupBy(rv$proplyr,
                                         group = rv$granges,
                                         GRanges_names = unlist(granges_names_new()),
                                         include_nonoverlapping = include_nonoverlapping_granges())
                 })
    
  }, ignoreInit = TRUE)
  
  ##### groupBy gene list

  output$gene_list_uploaded <- reactive({
    return(!is.null(input$gene_list))
  })
  outputOptions(output, "gene_list_uploaded", suspendWhenHidden = FALSE)
  
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
  
  include_nonoverlapping_gene_list <- reactive({
    include_nonoverlapping_gene_list = input$include_nonoverlapping_gene_list
  })
  
  # produce the text boxes for user to input  granges names
  output$gene_list_names <- renderUI({
    numGeneLists <- length(input$gene_list$name)
    lapply(1:numGeneLists, function(i) {
      textInput(inputId = paste0("gene_list_names_list", i),
                label = paste0("Gene List #", i, " name"),
                value = input$gene_list$name[[i]])
    })
  })
  
  # create list of names user enters, or sets to default names if user unclicks the box
  gene_list_names_new <- reactive({
    numGeneLists <- length(input$gene_list$name)
    temp <- list()
    for(i in seq(numGeneLists)){
      temp[[i]] <- input[[paste0("gene_list_names_list", i)]]
    }
    unlist(temp)
  })
  

  groupby_gene_list_go <- observeEvent(input$perform_groupby_genes, {
    withProgress(message = 'Calculation in progress',
                 value = 0.5, {
                   names(rv$genes) <- gene_list_names_new()
                   rv$proplyr <- groupBy(rv$proplyr,
                                         group = rv$genes,
                                         include_nonoverlapping = include_nonoverlapping_gene_list())
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