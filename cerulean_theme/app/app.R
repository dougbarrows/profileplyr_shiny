
#BiocManager::install(version='devel')

library(profileplyr)
library(shiny)
library(DT)
library(shinydashboard)
#library(shinycssloaders)
library(dplyr)
library(rtracklayer)
library(GenomicRanges)
library(grDevices)
library(shinythemes)
library(shinyWidgets)
library(grid)
library(shinyjs)
library(shinyFiles)

options(shiny.maxRequestSize=5000*1024^2) 

ui <- fluidPage(theme = shinytheme("cerulean"),
                useShinyjs(),
                navbarPage("profileplyr",
                           tabPanel("Input Data",
                                    
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
                                      radioButtons(inputId = "input_local_vs_server",
                                                   label = "Are you deploying this app locally, using docker/a server?",
                                                   choices = c("locally" = "locally",
                                                               "using docker or through a server" = "docker_server"),
                                                   inline = FALSE),
                                      radioButtons(inputId = "format",
                                                   label = "Signal File Format:",
                                                   choices = c("BAM" = "bam",
                                                               "bigwig" = "bigwig"),
                                                   inline = TRUE),
                                      conditionalPanel(
                                        condition = "input.input_local_vs_server == 'docker_server'",
                                        uiOutput("numSignalFiles_ui_server"),
                                        uiOutput("SignalFileUpload"), 

                                        br(),
                                        
                                        uiOutput("numBedFiles_ui_server"),
                                        uiOutput("BedFileUpload")
                                        ), 

                                      conditionalPanel(
                                        condition = "input.input_local_vs_server == 'locally'",
                                      uiOutput("numSignalFiles_ui"),
                                      
                                        fluidRow(
                                          column(2,
                                                 uiOutput("SignalFilePathButton")), 
                                          column(10,
                                                 htmlOutput("SignalFilePathText"))
                                        ),
                                        br(),
                                        
                                        uiOutput("numBedFiles_ui"),
                                        fluidRow(
                                          column(2,
                                                 uiOutput("BedFilePathButton")), 
                                          column(10,
                                                 uiOutput("BedFilePathText"))
                                        )
                                      ),
                                      br(),
                                      # textInput(inputId = "signalFiles",
                                      #           label = "Enter paths to bigwig or bam files (each separated by a comma):",
                                      #           value = ""),
                                      # textInput(inputId = "testRanges",
                                      #           label = "Enter paths to bed files (each separated by a comma):",
                                      #           value = ""),
                                      radioButtons(inputId = "soggi_style",
                                                   label = "How do you want the ranges from the bed file to be defined?",
                                                   choices = c("all ranges in bed files normalized to the same length" = "percentOfRegion",
                                                               "defined by a specified distance from the center of each range in the bed files" = "point"),
                                                   selected = "percentOfRegion"),
                                      conditionalPanel(
                                        condition = "input.soggi_style == 'percentOfRegion'",
                                        numericInput(inputId = "distanceAround",
                                                     label = "Define the distance to extend to either side of each range (as a percentage of each range):",
                                                     value = 100,
                                                     min = 1,
                                                     max = 500,
                                                     step = 10),
                                        numericInput(inputId = "nOfWindows",
                                                     label = "Enter the desired number of bins the normalized regions should be split into:",
                                                     value = 100,
                                                     min = 1,
                                                     max = 500,
                                                     step = 10)
                                      ),
                                      conditionalPanel(
                                        condition = "input.soggi_style == 'point'",
                                        numericInput(inputId = "distanceUp",
                                                     label = "Enter the distance (in base pairs) upstream from the center of each range to include in the quantification:",
                                                     value = 1500,
                                                     min = 1,
                                                     max = 10000,
                                                     step = 100),
                                        numericInput(inputId = "distanceDown",
                                                     label = "Enter the distance (in base pairs) downstream from the center of each range to include in the quantification:",
                                                     value = 1500,
                                                     min = 1,
                                                     max = 10000,
                                                     step = 100),
                                        numericInput(inputId = "bin_size",
                                                     label = "Enter the size of each bin in which the signal should be quantified (in base pairs):",
                                                     value = 20,
                                                     min = 1,
                                                     max = 1000, # could make this an uiOutput and make this value to be euql to the size of the window?
                                                     step = 10)
                                      ),
                                      
                                      
                                      actionButton(inputId = "go_bam_bigwig",
                                                   label = "Generate profileplyr object", 
                                                   icon("check"),
                                                   class = "btn btn-primary"),
                                      # option to download
                                      uiOutput("download_bam_bigwig_bed")),
                                    # 
                                    # ###################################################
                                    # # conditions if the user selects deepTools matrix
                                    # ###################################################
                                    conditionalPanel(
                                      condition = "input.inputType == 'deepTools'",
                                      fileInput(inputId = "deepTools_mat_upload",
                                                label = "Upload deeptools matrix (output of computeMatrix):"),
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
                                                label = "Upload profileplyr object:")),
                                    
                                    #htmlOutput("object_headline"),
                                    #verbatimTextOutput("proplyr_print"),
                                    uiOutput("download_object_button_inputTab"),
                                    uiOutput("clear_object"),
                                    uiOutput("clear_warning"),
                                    br(),
                                    br(),
                                    uiOutput("sample_table_input")
                           ),
                           
                           tabPanel("Select/Annotate Samples",
                                    uiOutput("download_object_button_selectTab"),
                                    br(),
                                    # htmlOutput("select_samples_message"),
                                    # fluidRow(
                                    #   column(4,
                                    uiOutput("select_samples_box"),
                                    # ),
                                    # column(8,
                                    uiOutput("new_sample_column_box"),
                                    br(),
                                    uiOutput("selected_samples_render_table")
                                    # )
                                    # )
                           ),
                           tabPanel("Manupulate Ranges",
                                    uiOutput("download_object_button_manipulateTab"),
                                    br(),
                                    uiOutput("manipulate_render"),
                                    br(),
                                    fluidRow(
                                      column(7,
                                             (uiOutput("explore_ranges_render"))),
                                      column(5,
                                             (uiOutput("selected_samples_table_render_manipulate")))
                                    )
                                    
                           ),
                           tabPanel("Visualize",
                                    fluidRow(
                                      column(5,
                                             uiOutput("download_object_button_visualizeTab"),
                                             br(),
                                             uiOutput("labeled_image"),
                                             uiOutput("visualize_render_box1")),
                                      column(7, 
                                             uiOutput("selected_samples_table_render_visualize"),
                                             uiOutput("visualize_render_box2"))
                                    )
                           )
                )
)




server <- function(input, output, session) {
  
  rv <- reactiveValues(proplyr = "No profileplyr object has been uploaded/generated")
  
  output$numSignalFiles_ui_server <- renderUI({
    numericInput(inputId = "numSignalFiles_server", 
                 label = paste0("How many ", input$format, " files would you like to use?"), 
                 value = 1, 
                 min = 1, 
                 step = 1)
  })
  
  output$SignalFileUpload <- renderUI({
    req(input$numSignalFiles_server)
    signalFiles_upload <- as.list(1:input$numSignalFiles_server)
    signalFiles_upload <- lapply(signalFiles_upload, function(i)
    {
      
      fileInput(inputId = paste0("UploadSignalFile", i),
                label = paste0("Select ", input$format," file # ", i),
                multiple = FALSE
                )
      
    })
    
  })
  
  output$numBedFiles_ui_server <- renderUI({
    numericInput(inputId = "numBedFiles_server", 
                 label = paste0("How many bed files would you like to use?"), 
                 value = 1, 
                 min = 1, 
                 step = 1)
  })
  
  output$BedFileUpload <- renderUI({
    req(input$numBedFiles_server)
    BedFiles_upload <- as.list(1:input$numBedFiles_server)
    BedFiles_upload <- lapply(BedFiles_upload, function(i)
    {
      
      fileInput(inputId = paste0("UploadBedFile", i),
                label = paste0("Select bed file # ", i),
                multiple = FALSE
      )
      
    })
    
  })
  
  output$numSignalFiles_ui <- renderUI({
    numericInput(inputId = "numSignalFiles", 
                 label = paste0("How many ", input$format, " files would you like to use?"), 
                 value = 1, 
                 min = 1, 
                 step = 1)
  })
  
  output$SignalFilePathButton <- renderUI({
    req(input$numSignalFiles)
    signalFiles_buttons <- as.list(1:input$numSignalFiles)
    signalFiles_buttons <- lapply(signalFiles_buttons, function(i)
    {
      
      shinyFilesButton(paste0("GetSignalFile", i),
                       paste0("Select ", input$format," file # ", i),
                       title = "Please select a file:", multiple = FALSE,
                       class = "btn btn-primary")
      
    })
    
  })
  
  # this function is used to get the paths to a 'shinyFile' link, it is used below to both print the file paths and store them to be used later on 
  selected_files_function <- function(input_name, x) {
    volumes = c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    
    shinyFileChoose(input, 
                    paste0(input_name, x), 
                    roots = volumes, 
                    session = session)
    
    if(!is.null(input[[paste0(input_name, x)]])){
      file_selected <-parseFilePaths(volumes, 
                                     input[[paste0(input_name, x)]])
      return(file_selected$datapath)
    }
    
  }
  
  # render the paths just to the right of the links
  output$SignalFilePathText <- renderUI({
    req(input$numSignalFiles)
    signalFiles_text <- as.list(1:input$numSignalFiles)
    signalFiles_text <- lapply(signalFiles_text, function(i)
    {
      # use the function defined above to get the selected files
      signalFiles_files <- selected_files_function(input_name = "GetSignalFile", x = i)
      if (!is.null(signalFiles_files)){
        renderText(paste0(input$format, " file ", i, ": ", as.character(signalFiles_files)))
      }
    })
    
  })
  
  #use the same function that we used to print the paths to the files to make a reactive value that contains a vector of paths
  signalFiles_paths <- reactive({
    req(input$numSignalFiles)
    if(input$input_local_vs_server == "locally"){
      signalFiles_text2 <- as.list(1:input$numSignalFiles)
      signalFiles_text2 <- lapply(signalFiles_text2, function(i)
      {
        # use the function defined above to get the selected files
        selected_files_function(input_name = "GetSignalFile", x = i)
        
      })
      unlist(signalFiles_text2)
    } else {
      signalFiles_text2 <- as.list(1:input$numSignalFiles_server)
      signalFiles_text2 <- lapply(signalFiles_text2, function(i)
      {
        input[[paste0("UploadSignalFile", i)]]$datapath
      })
      unlist(signalFiles_text2)
    }
    
  })
  
  output$numBedFiles_ui <- renderUI({
    
    numericInput(inputId = "numBedFiles", 
                 label = paste0("How many bed files would you like to use?"), 
                 value = 1, 
                 min = 1, 
                 step = 1)
  })
  
  output$BedFilePathButton <- renderUI({
    req(input$numBedFiles)
    testRanges_buttons <- as.list(1:input$numBedFiles)
    testRanges_buttons <- lapply(testRanges_buttons, function(i)
    {
      
      shinyFilesButton(paste0("GetBedFile", i),
                       paste0("Select bed file # ", i),
                       title = "Please select a file:", multiple = FALSE,
                       #buttonType = "default", 
                       class = "btn btn-primary")
      
    })
    
  })
  
  # render the paths just to the right of the links
  output$BedFilePathText <- renderUI({
    req(input$numBedFiles)
    testRanges_text <- as.list(1:input$numBedFiles)
    testRanges_text <- lapply(testRanges_text, function(i)
    {
      # use the function defined above to get the selected files
      testRanges_files <- selected_files_function(input_name = "GetBedFile", x = i)
      if (!is.null(testRanges_files)){
        renderText(paste0("bed file ", i, ": ", as.character(testRanges_files)))
      }
    })
    
  })
  
  #use the same function that we used to print the paths to the files to make a reactive value that contains a vector of paths
  testRanges_paths <- reactive({
    req(input$numBedFiles)
    if(input$input_local_vs_server == "locally"){
    testRanges_text2 <- as.list(1:input$numBedFiles)
    testRanges_text2 <- lapply(testRanges_text2, function(i)
    {
      # use the function defined above to get the selected files
      selected_files_function(input_name = "GetBedFile", x = i)
      
    })
    unlist(testRanges_text2)
    } else {
      testRanges_text2 <- as.list(1:input$numBedFiles_server)
      testRanges_text2 <- lapply(testRanges_text2, function(i)
      {
        input[[paste0("UploadBedFile", i)]]$datapath
      })
      unlist(testRanges_text2)
    }
    
  }) 
  
  output$labeled_image <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      wellPanel(
        tags$h4("Example range heatmap:"),
        img(src=rv$labeled_image,
            height = "100%",
            width = "100%")
      )
    }
  })
  
  output$explore_ranges_render <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      wellPanel(
        tags$h4("Current range information and annotation:"),
        #htmlOutput("rangeTable_title"),
        DT::dataTableOutput("rangeTable"))
    } 
  })
  
  output$select_samples_box <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      wellPanel(
        htmlOutput("select_sample_table_title"), 
        pickerInput(inputId = "select_sample_choose", 
                    choices = c(rownames(rv$sampleData_original)),
                    options = list(`actions-box` = TRUE),
                    #label = "",
                    multiple = TRUE 
                    #selectize = TRUE
        ), 
        actionButton("select_samples_action" ,"Select samples", 
                     icon("check"),
                     class = "btn btn-primary")
      )
      
    } else {
      htmlOutput("no_object_message_select")
    }
  })
  
  
  
  output$new_sample_column_box <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      fluidRow(
        wellPanel(
          checkboxInput(inputId = "new_sample_column_query",
                        label = HTML("<b>Click to add a column to the sample metadata below. This information can be used for grouping samples during visualization (e.g. with colors).<b>"),
                        value = 0)),
        conditionalPanel(
          condition = "input.new_sample_column_query != 0",
          column(6, uiOutput("column_name")),
          column(6,
                 htmlOutput("new_sample_column_render_title"),
                 uiOutput("new_sample_column_render"),
                 uiOutput("add_sample_column_button")
          )
        )
      )
    }
  })
  
  output$column_name <- renderUI({
    textInput(inputId = "column_name_input",
              label = "Enter the name of the new column:")
  })
  
  output$new_sample_column_render_title <- renderText("<b> Enter the contents for each row of this column: <b><br><br>")
  output$new_sample_column_render <- renderUI({
    numSamples <- length(assays(rv$proplyr))
    lapply(1:numSamples, function(i) {
      textInput(inputId = paste0("new_sample_column_list", i),
                label = rownames(sampleData(rv$proplyr))[i])
    })
  })
  
  
  output$add_sample_column_button <- renderUI({
    actionButton(inputId = "add_sample_column_yes",
                 label = "Add Column to sample data", 
                 icon("check"),
                 class = "btn btn-primary")
  })
  
  
  output$selected_samples_render_table <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      wellPanel(
        htmlOutput("selected_samples_table_title"),
        DT::dataTableOutput("selected_samples_table")
      )
    } 
  })
  
  output$select_samples_message <- renderText("<b> After selecting the rows of the samples you want to use for this analysis from the table on the left, click the 'Select samples' button and samples shown in the table on the right are those that will be used for any subsequent analysis. To revert back to the original samples, just click the 'Select samples' button with no rows highlighted in the table above. <b>")
  
  output$selected_samples_table_render_manipulate <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      wellPanel(
        tags$h4("Currently selected samples for annotation:"),
        #htmlOutput("selected_samples_table_manipulate_title"),
        DT::dataTableOutput("selected_samples_table_manipulate"))
    }
  })
  
  output$manipulate_render <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      
      fluidRow(
        column(4,
               
               wellPanel(
                 tags$h4("Select functions to annotate and group ranges:"),
                 radioButtons(
                   'select', '',
                   choices = c("cluster ranges" = "cluster_choice",
                               "annotate ranges with genomic regions (promoters, exons, etc.) and closest gene with ChIPseeker" = "annotate_cs_choice",
                               "annotate ranges with genes using GREAT" = "annotate_great_choice",
                               "filter or group the ranges based on whether they overlap with other sets of ranges (i.e. bed files)" = "groupby_granges_choice",
                               "filter or group the ranges by lists of genes (NOTE: must annotate with ChIPseeker or GREAT first)" = "groupby_genes_choice")
                 ))),
        
        
        column(8,
               fluidRow(
                 column(12,
                        conditionalPanel(
                          condition = "input.select.includes('cluster_choice')",
                          wellPanel(
                            tags$h4("Clustering of Ranges:"),
                            #htmlOutput("cluster_title"),
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
                                         label = "Perform Clustering", 
                                         icon("check"),
                                         class = "btn btn-primary")
                          )
                        ))),
               fluidRow(
                 column(12,
                        conditionalPanel(
                          condition = "input.select.includes('annotate_cs_choice')",
                          wellPanel(
                            tags$h4("Annotation with ChIPseeker:"),
                            #htmlOutput("annotate_cs_title"),
                            fluidRow(
                              column(6,
                                     selectInput(inputId = "cs_genome",
                                                 label = "Select a genome:",
                                                 choices = c("hg19", "hg38", "mm9", "mm10"),
                                                 selected = c("hg19"),
                                                 multiple = FALSE),
                                     checkboxGroupInput(inputId = "annotation_subset",
                                                        label = "Specific annotation types to use:",
                                                        choices = c("Promoter", "Exon", "Intron", "Downstream", "Distal Intergenic", "3p UTR", "5p UTR"),
                                                        selected = c("Promoter", "Exon", "Intron", "Downstream", "Distal Intergenic", "3p UTR", "5p UTR")),
                                     actionButton(inputId = "perform_annotate_cs",
                                                  label = "Perform annotation with ChIPseeker", 
                                                  icon("check"),
                                                  class = "btn btn-primary")
                              ),
                              column(6,
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     sliderInput(inputId = "tss_region",
                                                 label = "Transcription Start Site Region: \n *TSS = 0",
                                                 min = -5000,
                                                 max = 5000,
                                                 value = c(-3000, 3000),
                                                 step = 100)
                              )
                            )
                          )
                        ))),
               
               fluidRow(
                 column(12,
                        conditionalPanel(
                          condition = "input.select.includes('annotate_great_choice')",
                          wellPanel(
                            tags$h4("Annotation with GREAT:"),
                            #htmlOutput("annotate_great_title"),
                            selectInput(inputId = "great_species",
                                        label = "Select a genome:",
                                        choices = c("hg19", "mm10", "mm9", "danRer7"),
                                        selected = c("hg19"),
                                        multiple = FALSE),
                            actionButton(inputId = "perform_annotate_great",
                                         label = "Perform gene annotation with GREAT", 
                                         icon("check"),
                                         class = "btn btn-primary"))
                        ))),
               fluidRow(
                 column(12,
                        conditionalPanel(
                          condition = "input.select.includes('groupby_granges_choice')",
                          wellPanel(
                            tags$h4("Group ranges by bed files:"),
                            #htmlOutput("groupby_granges_title"),
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
                                           label = "Group the ranges by bed file overlap", 
                                           icon("check"),
                                           class = "btn btn-primary")))
                        )
                 )
               ),
               
               fluidRow(
                 column(12,
                        conditionalPanel(
                          condition = "input.select.includes('groupby_genes_choice')",
                          wellPanel(
                            tags$h4("Group ranges by overlap with gene lists:"),
                            #htmlOutput("groupby_gene_list_title"),
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
                                           label = "Group the ranges by gene lists", 
                                           icon("check"),
                                           class = "btn btn-primary"))
                          )
                        )
                 )   
               )
        )
      )
    } else {
      htmlOutput("no_object_message_manipulate")
    }
    
  })
  
  
  output$selected_samples_table_render_visualize <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      wellPanel(
        tags$h4("Currently selected samples for visualization:"),
        #htmlOutput("selected_samples_table_visualize_title"),
        DT::dataTableOutput("selected_samples_table_visualize")
      )
    } 
  })
  
  output$visualize_render_box1 <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      wellPanel( 
        tags$h4("Options for the optimization of the range heatmap:"),
        #htmlOutput("visualization_options_title"),
        br(),
        # tags$h4("1) Ordering of the ranges (rows) of the heatmap:"),
        checkboxInput(inputId = "row_ordering_option",
                      label = HTML("<b>Change the ordering of the ranges (rows) of the heatmap<b>")),
        conditionalPanel(
          condition = "input.row_ordering_option != 0",
          
          radioButtons(inputId = "sort_by_mean_or_column",
                       label = "Do you want to sort the ranges by mean signal or by a range metadata column?",
                       choices = c("mean signal" = "mean",
                                   "range metadata column" = "metadata_column"),
                       inline = TRUE),
          conditionalPanel(
            condition = "input.sort_by_mean_or_column == 'mean'",
            selectInput(inputId = "sort_by_mean_subset",
                        label = "When sorting by mean signal, which samples should be used:",
                        choices = c(rownames(rv$sampleData_active_subset)),
                        selectize = TRUE,
                        multiple = TRUE),
            tags$p("NOTE: if this is left blank, mean signal across all samples will be used to sort. This is the default setting.")
          ),
          conditionalPanel(
            condition = "input.sort_by_mean_or_column == 'metadata_column'",
            selectInput(inputId = "sort_by_metadata_column",
                        label = "Which range metadata column should be used for sorting:",
                        choices = c("", colnames(mcols(rv$proplyr))),
                        multiple = FALSE
            ),
            radioButtons(inputId = "increasing_or_decreasing",
                         label = "Should the values of the column be put in increasing or decreasing order (for either categorical or numeric values)?",
                         choices = c("increasing order" = FALSE,
                                     "decreasing order" = TRUE)),
            br()
          )
        ),
        #tags$h4("2) Modify Sample names at the top of each heatmap"),
        #tags$h5("Change the sample names (i.e. the labels on top of each main heatmap):"),
        checkboxInput(inputId = "sample_names_option",
                      label = HTML("<b>Change and customize sample names (i.e. the labels on top of each main heatmap)<b>")),
        conditionalPanel(
          condition = "input.sample_names_option != 0",
          numericInput(inputId = "sample_label_fontsize_entry",
                       label = "Font size of sample labels:",
                       min = 1, 
                       max = 50,
                       step = 1,
                       value = 10),
          radioButtons(inputId = "sample_label_fontface_entry",
                       label = "Font type of sample labels",
                       choices = c("plain text" = "plain",
                                   "bold" = "bold"),
                       inline = TRUE,
                       selected = "bold"),
          htmlOutput("change_sample_names_title"),
          uiOutput("sample_names")
          
        ),
        checkboxInput(inputId = "heatmap_colors_option",
                      label = HTML("<b>Customize the colors of the main heatmaps<b>")),
        conditionalPanel(
          condition = "input.heatmap_colors_option != 0",
          
          radioButtons(inputId = "individual_or_group_colors",
                       label = "Do you want to specify colors for each individual heatmap, or color them by sample grouping?",
                       choices = c("Color by individual heatmap (default)" = "individual",
                                   "Color by sample groups" = "sample_groups") 
                       #selected = character(0)
          ),
          conditionalPanel(
            condition = "input.individual_or_group_colors == 'individual'",
            tags$b("Enter two or three colors separated by commas for each sample heatmap:"),
            br(),
            br(),
            uiOutput("matrices_color_individual")
          ),
          conditionalPanel(
            condition = "input.individual_or_group_colors == 'sample_groups'",
            uiOutput("sample_column_for_colors"),
            br(),
            tags$b("Enter two or three colors separated by commas for each sample group:"),
            br(),
            br(),
            uiOutput("user_group_colors")
          ),
          #tags$h5("Should the colors for the range heatmap all have a common scale, or should they be scaled individually?"),
          radioButtons(inputId = "all_color_scales_equal",
                       label = "Should the colors for the range heatmap all have a common scale, or should they be scaled individually?",
                       choices = c("same scale" = TRUE,
                                   "individual scales" = FALSE),
                       inline = TRUE),
          conditionalPanel(
            condition = "input.all_color_scales_equal == 'FALSE'",
            uiOutput("legend_show_select_notEqual_render")
          ),
          conditionalPanel(
            condition = "input.all_color_scales_equal == 'TRUE'",
            uiOutput("legend_show_select_equal_render")
          )
        ),
        
        #tags$h4("3) Range/row grouping parameters:"),
        #br(),
        #tags$h5("Enter the name of the column from the range metadata to be used for grouping:"),
        checkboxInput(inputId = "range_grouping_option",
                      label = HTML("<b>Change the how the ranges (rows) are grouped<b>")
        ),
        conditionalPanel(
          condition = "input.range_grouping_option != 0",
          
          selectInput(inputId = "columns_for_groups_dropdown",
                      label = "Enter the name of the column from the range metadata to be used for grouping",
                      choices = colnames(mcols(rv$proplyr)),
                      selected = params(rv$proplyr)$rowGroupsInUse),
          br(),
          #tags$h5("Do you want to include group annotation on left side of heatmaps?"),
          radioButtons(inputId = "include_group_annotation",
                       label = "Do you want to include group annotation on left side of heatmaps?",
                       choices = c("include" = TRUE,
                                   "don't include" = FALSE),
                       inline = TRUE),
          conditionalPanel(
            condition = "input.include_group_annotation == 'TRUE'",
            # tags$h5("Set the paratmeters of the group annotation labels:"),
            numericInput(inputId = "group_anno_width_entry",
                         label = "Width of the group annotations (in mm):",
                         min = 1, 
                         max = 50,
                         step = 1,
                         value = 3)
          ),
          br(),
          conditionalPanel(
            condition = "input.include_group_annotation == 'TRUE'",
            tags$b("Set the paratmeters of the group annotation labels:"),
            numericInput(inputId = "group_anno_label_fontsize_entry",
                         label = "Font size of group labels:",
                         min = 1, 
                         max = 50,
                         step = 1,
                         value = 10),
            radioButtons(inputId = "group_anno_label_fontface_entry",
                         label = "Font type of group labels",
                         choices = c("plain text" = "plain",
                                     "bold" = "bold"),
                         inline = TRUE,
                         selected = "bold") 
          ),
          br(),
          conditionalPanel(
            condition = "input.include_group_annotation == 'TRUE'",
            tags$b("Customize colors of group annotations:"),
            checkboxInput(inputId = "group_color_query",
                          label = "Click to change the colors of the group annotation panel",
                          value = 0)
          ),
          conditionalPanel(
            condition = "input.group_color_query != 0",
            uiOutput("group_color")
          ),
          br()
        ),
        #tags$h4("4) Set the parameters of the signal line plots on the top of each sample heatmap"),
        checkboxInput(inputId = "signal_plots_option",
                      label = HTML("<b>Set the parameters of the signal line plots on the top of each sample heatmap<b>")),
        conditionalPanel(
          condition = "input.signal_plots_option != 0",
          
          radioButtons(inputId = "ylim_query",
                       label = "Y-axis limit setting:",
                       choices = c("Common max of all heatmaps (Default)" = "common_max_all",
                                   "Common max by sample grouping" = "common_max_group",
                                   "Inferred spearately" = "inferred", # since this is NULL, had toruble buidling in, can essentially do this with custom, so leave out for now
                                   "Custom" = "custom"),
                       selected = "common_max_all",
                       inline = FALSE),
          conditionalPanel(
            condition = "input.ylim_query == 'custom'",
            uiOutput("ylim")),
          conditionalPanel(
            condition = "input.ylim_query == 'common_max_group'",
            uiOutput("ylim_group_column")
          ),
          numericInput(inputId = "top_anno_axis_font_entry",
                       label = "Font size of y-axis tick labels:",
                       min = 1, 
                       max = 50,
                       step = 1,
                       value = 8),
          numericInput(inputId = "top_anno_height_entry",
                       label = "Height of signal plot (in cm)",
                       min = 1, 
                       max = 10,
                       step = 1,
                       value = 2),
          br()
        ),
        #tags$h5("Enter/select the names of columns from the range metadata for extra annotation heatmaps:"),
        checkboxInput(inputId = "extra_annotation_option",
                      label = HTML("<b>Add/customize any extra annotation columns on the right side of the heatmap<b>")),
        conditionalPanel(
          condition = "input.extra_annotation_option != 0",
          
          selectInput(inputId = "extra_annotation_columns_input",
                      label = "Enter/select the names of columns from the range metadata for extra annotation heatmaps:",
                      choices = colnames(mcols(rv$proplyr)),
                      multiple = TRUE,
                      selectize = TRUE),
          br(),
          uiOutput("extra_anno_param_title"),
          uiOutput("extra_anno_width_query"),
          uiOutput("extra_anno_width"),
          uiOutput("extra_anno_color_query"),
          uiOutput("extra_anno_color_numeric"),
          uiOutput("extra_anno_color_factor")
        ),
        uiOutput("genes_to_label_option_render"),
        uiOutput("genes_to_label_direct_or_upload_render"),
        uiOutput("direct_type_inputbox_render"),
        uiOutput("genes_to_label_fontsize")
        
      )
      
    }
  })
  
  # show various versions of the labeled example heatmap
  observe({
    req(input$row_ordering_option)
    if(input$row_ordering_option != 0){
      rv$labeled_image <- "labeled_figure_for_app_row_order.png"
    } 
  })
  
  observe({
    req(input$extra_annotation_option)
    if(input$extra_annotation_option != 0){
      rv$labeled_image <- "labeled_figure_for_app_extra_anno.png"
    } 
  })
  
  observe({
    req(input$sample_names_option)
    if(input$sample_names_option != 0){
      rv$labeled_image <- "labeled_figure_for_app_sample_names.png"
    } 
  })
  
  observe({
    req(input$signal_plots_option)
    if(input$signal_plots_option != 0){
      rv$labeled_image <- "labeled_figure_for_app_signal_plots.png"
    } 
  })
  
  observe({
    req(input$range_grouping_option)
    if(input$range_grouping_option != 0){
      rv$labeled_image <- "labeled_figure_for_app_groups.png"
    } 
  })
  
  observe({
    req(input$genes_to_label_option)
    if(input$genes_to_label_option != 0){
      rv$labeled_image <- "labeled_figure_for_app_labeled_genes.png"
    } 
  })
  
  observe({
    req(input$heatmap_colors_option)
    if(input$heatmap_colors_option != 0){
      rv$labeled_image <- "labeled_figure_for_app_main_heatmaps.png"
    } 
  })
  
  output$visualize_render_box2 <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      wellPanel(
        tags$h4("Generate range heatmap:"),
        radioButtons(inputId = "local_vs_browser",
                     label = "Are you deploying this app from a local machine or from the browser?",
                     choices = c("local", "browser"),
                     selected = character(0)
        ),
        br(),
        conditionalPanel(
          condition = "input.local_vs_browser == 'local'",
          actionButton(inputId = "MakeEnrichedHeatmap_local",
                       label = "Click to generate range heatmap and download", 
                       icon("check"),
                       class = "btn btn-primary"),
          textInput(inputId = "heatmap_path_local",
                    label = "Enter path for heatmap location if downloading from local launch:",
                    value = paste0(getwd(), "/EnrichedHeatmaplocal", "_",Sys.time(),".pdf")),
          plotOutput("EnrichedHeatmap_local")
        ),
        conditionalPanel(
          condition = "input.local_vs_browser == 'browser'",
          actionButton(inputId = "MakeEnrichedHeatmap",
                       label = "Click to generate range heatmap only", 
                       icon("check"),
                       class = "btn btn-primary"),
          plotOutput("EnrichedHeatmap"),
          downloadButton(outputId = "download_heatmap",
                         label = "Click to download range heatmap")
        )
      )
    } else {
      htmlOutput("no_object_message_visualize")
    }
  })
  
  output$no_object_message_explore <- renderText("<b> No profileplyr object currently loaded <b>")
  output$no_object_message_manipulate <- renderText("<b> No profileplyr object currently loaded <b>")
  output$no_object_message_visualize <- renderText("<b> No profileplyr object currently loaded <b>")
  output$no_object_message_select <- renderText("<b> No profileplyr object currently loaded <b>")
  
  output$select_sample_table_title <- renderText("<b> Select the samples to be used for further annotation and visualization: <b><br>")
  output$selected_samples_table_title <- renderText("<b> Selected samples (these will be the samples used for any manipulation or visualization) <b><br><br>")
  output$extra_annotation_title <- renderText("<b> Add extra annotation heatmaps: <b><br><br>")
  output$extra_annotation_inputbox_label <- renderText("Name of column used for extra annotation column:")
  output$sample_names_title <- renderText("<b> Heatmap Sample Names: <b>")
  output$matrices_color_title <- renderText("<b> Heatmap Color Schemes: <b>")
  output$change_sample_names_title <- renderText("<b>Change current sample names<b>")
  
  output$rangeTable_title <- renderText("<b> Current range information and annotation: <b><br><br>")
  output$selected_samples_table_manipulate_title <- renderText("<b> Currently selected samples for annotation: <b><br><br>")
  output$cluster_title <- renderText("<b> Clustering of Ranges: <b> <br><br>")
  output$annotate_cs_title <- renderText("<b> Annotation with ChIPseeker: <b> <br><br>")
  output$annotate_great_title <- renderText("<b> Annotation with GREAT: <b> <br><br>")
  output$groupby_granges_title <- renderText("<b> Group ranges by bed files: <b> <br><br>")
  output$groupby_gene_list_title <- renderText("<b> Group ranges by gene lists: <b> <br><br>")
  output$gene_list_instruct <- renderText("<b> NOTE: Gene lists must be .csv files with the gene symbols in the first column. <br> Additional columns (e.g. gene expression values) will be included in the new range metadata. <b> <br><br>")
  
  output$visualization_options_title <- renderText("<b> Options for the optimization of the range heatmap: <b><br><br>")
  output$selected_samples_table_visualize_title <- renderText("<b> Currently selected samples for visualization: <b><br><br>")
  output$generate_heatmap_title <- renderText("<b> Generate range heatmap: <b><br><br>")
  
  output$object_headline <- renderText("<br><b> Current profileplyr object for analysis: <b> <br><br>")
  #output$generate_headline <- renderText("<b> Generate, upload, or refresh profileplyr object: <b> <br><br>")
  
  
  rv$labeled_image <- 'labeled_figure_for_app.png'
  
  # read in signal files and bed files to make profileplyr object from Bam/bigwig/bed
  from_bam_bigwig_bed <- observeEvent(input$go_bam_bigwig, {
    withProgress(message = 'Action in progress',
                 value = 0.5, {
                   
                   signalFiles <- signalFiles_paths()
                   testRanges <- testRanges_paths()
                   # signalFiles <- strsplit(input$signalFiles, split = ",") %>%
                   #   unlist() %>%
                   #   trimws()
                   # testRanges <- strsplit(input$testRanges, split = ",") %>%
                   #   unlist() %>%
                   #   trimws()
                   rv$proplyr <- rv$proplyr_original <- BamBigwig_to_chipProfile(signalFiles = signalFiles,
                                                                                 testRanges = testRanges,
                                                                                 format = input$format,
                                                                                 style = input$soggi_style,
                                                                                 nOfWindows = input$nOfWindows,
                                                                                 bin_size = input$bin_size,
                                                                                 distanceAround = input$distanceAround,
                                                                                 distanceUp = input$distanceUp,
                                                                                 distanceDown = input$distanceDown) %>%
                     as_profileplyr()
                 })
  })
  
  
  # read in from deeptools
  from_deepTools <- observeEvent(input$deepTools_mat_upload, {
    withProgress(message = 'Action in progress',
                 value = 0.5, {
                   file <- input$deepTools_mat_upload
                   rv$proplyr <- rv$proplyr_original <- import_deepToolsMat(file$datapath)
                 })
  })
  
  
  ## read in from profileplyr object upload
  from_direct_file <- observeEvent(input$profileplyr_upload, {
    file <- input$profileplyr_upload
    rv$proplyr <- rv$proplyr_original <- readRDS(file$datapath)
    
  })
  
  # print object to screen if through direct upload
  output$proplyr_print <- renderPrint({
    
    rv$proplyr
    
  })
  
  output$download_object_button_inputTab <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      downloadButton("download_object_inputTab", "Download current profileplyr object")
    }
  })
  
  output$download_object_button_selectTab <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      downloadButton("download_object_selectTab", "Download current profileplyr object")
    }
  })
  
  output$download_object_button_manipulateTab <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      downloadButton("download_object_manipulateTab", "Download current profileplyr object")
    }
  })
  
  output$download_object_button_exploreTab <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      downloadButton("download_object_exploreTab", "Download current profileplyr object")
    }
  })
  
  
  output$download_object_button_visualizeTab <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      downloadButton("download_object_visualizeTab", "Download current profileplyr object")
    }
  })
  
  output$download_object_inputTab <- downloadHandler(
    filename = function() {
      paste("proplyrObject", "_",Sys.time(),".RData",sep="")
    },
    content = function(file) {
      saveRDS(rv$proplyr, file)
    }
  )
  
  output$download_object_selectTab <- downloadHandler(
    filename = function() {
      paste("proplyrObject", "_",Sys.time(),".RData",sep="")
    },
    content = function(file) {
      saveRDS(rv$proplyr, file)
    }
  )
  
  output$download_object_manipulateTab <- downloadHandler(
    filename = function() {
      paste("proplyrObject", "_",Sys.time(),".RData",sep="")
    },
    content = function(file) {
      saveRDS(rv$proplyr, file)
    }
  )
  
  output$download_object_exploreTab <- downloadHandler(
    filename = function() {
      paste("proplyrObject", "_",Sys.time(),".RData",sep="")
    },
    content = function(file) {
      saveRDS(rv$proplyr, file)
    }
  )
  
  output$download_object_visualizeTab <- downloadHandler(
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
    rv$proplyr_original <- NULL
    rv$mcol <- NULL
    rv$heatmap <- NULL
    rv$heatmap_local <- NULL
    rv$row_select_index <- NULL
  })
  
  
  output$sample_table_input <- renderUI({
    if(is(rv$proplyr, "profileplyr")) {
      box(DT::dataTableOutput("sample_table"),
          title = "Samples from imported data:",
          width = 12,
          solidHeader = TRUE,
          status = "primary")
    } 
  })
  
  
  # make interactive data table for sampleData (original input)
  observeEvent(rv$proplyr_original, {
    rv$sampleData_original <- rv$sampleData_active_subset <- sampleData(rv$proplyr_original) %>% 
      as.data.frame() %>%
      dplyr::select(sample_labels)
  })
  
  
  # create list of entries for new column 
  add_to_sampleData <- observeEvent(input$add_sample_column_yes, {
    numSamples <- length(assays(rv$proplyr))
    temp <- list()
    if (input$new_sample_column_query != 0){
      for(i in seq(numSamples)){
        temp[[i]] <- input[[paste0("new_sample_column_list", i)]]
      }
    }
    new_sample_column <- unlist(temp)
    column_name <- input$column_name_input
    sampleData(rv$proplyr)[,column_name] <- new_sample_column
    rv$sampleData_active_subset[,column_name] <- sampleData(rv$proplyr)[,column_name]
  })
  
  output$sample_table <- DT::renderDataTable({
    DT::datatable(rv$sampleData_original, 
                  options = list(pageLength = 10,
                                 scrollX = TRUE))
  })
  
  output$select_sample_table <- DT::renderDataTable({
    DT::datatable(rv$sampleData_original, 
                  options = list(pageLength = 10,
                                 scrollX = TRUE))
  })
  
  row_index_update <- observeEvent(input$select_samples_action, {
    #rv$row_select_index <- input$select_sample_table_rows_selected
    rv$row_select_index <- rownames(sampleData(rv$proplyr_original)) %in% input$select_sample_choose
    if(is.null(rv$row_select_index)){
      rv$row_select_index <- seq(nrow(sampleData(rv$proplyr_original)))
    }
  })
  
  update_proplyr_after_index <- observeEvent(rv$row_select_index, {
    rv$proplyr <- rv$proplyr_original[, ,rv$row_select_index]
    rv$sampleData_active_subset <- sampleData(rv$proplyr) %>%
      as.data.frame() %>%
      dplyr::select(sample_labels)
  })
  
  output$selected_samples_table <- DT::renderDataTable({
    DT::datatable(rv$sampleData_active_subset, 
                  options = list(pageLength = 10,
                                 scrollX = TRUE),
                  selection = "none") 
  })
  
  output$selected_samples_table_manipulate <- DT::renderDataTable({
    DT::datatable(rv$sampleData_active_subset, 
                  options = list(pageLength = 10,
                                 scrollX = TRUE),
                  selection = "none")  
  })
  
  output$selected_samples_table_visualize <- DT::renderDataTable({
    DT::datatable(rv$sampleData_active_subset, 
                  options = list(pageLength = 10,
                                 scrollX = TRUE),
                  selection = "none")  
  })
  
  # make interactive data table in 'Explore Ranges' tab for direct profileplyr option
  update_mcol <- eventReactive(rv$proplyr, {
    rv$mcol <- rowRanges(rv$proplyr) %>%
      as.data.frame()
  })
  
  output$rangeTable <- DT::renderDataTable({
    DT::datatable(update_mcol(), 
                  options = list(pageLength = 10,
                                 scrollX = TRUE))
  })
  
  
  
  # set parameters for generateEnrichedHeatmap()
  
  samples_to_sortby <- reactive ({
    if(input$sort_by_mean_or_column == "mean") {
      rv$proplyr <- orderBy(rv$proplyr, column = NULL)
      return(input$sort_by_mean_subset)
    } else if (input$sort_by_mean_or_column == "metadata_column") {
      NULL
    } 
  })
  
  change_sort_column_local <- observeEvent(input$MakeEnrichedHeatmap_local, {
    if(input$sort_by_mean_or_column == "metadata_column"){
      if(!(input$sort_by_metadata_column == "")) {
        rv$proplyr <- orderBy(rv$proplyr, column = input$sort_by_metadata_column)
      } else {
        rv$proplyr <- orderBy(rv$proplyr, column = NULL)
      }
    }
  })
  
  change_sort_column <- observeEvent(input$MakeEnrichedHeatmap, {
    if(input$sort_by_mean_or_column == "metadata_column"){
      if(!(input$sort_by_metadata_column == "")) {
        rv$proplyr <- orderBy(rv$proplyr, column = input$sort_by_metadata_column)
      } else {
        rv$proplyr <- orderBy(rv$proplyr, column = NULL)
      }
    }
  })
  
  decreasing <- reactive({
    input$increasing_or_decreasing
  })
  
  
  include_group_annotation <- reactive({
    input$include_group_annotation
  })
  
  all_color_scales_equal <- reactive({
    input$all_color_scales_equal
  })
  
  color_by_sample_group <- reactive({
    if (input$individual_or_group_colors == "sample_groups") {
      return(input$select_sample_column_for_colors)
    } else {
      return(NULL)
    }
  })
  
  # produce the text boxes for user to input group colors 
  output$group_color <- renderUI({
    groupLevels <- mcols(rv$proplyr) %>%
      .[, colnames(.) %in% params(rv$proplyr)$rowGroupsInUse] %>%
      as.factor() %>%
      levels(.)
    numGroups <- length(groupLevels)
    colors <- palette()
    while(length(colors) < numGroups){
      colors <- c(colors,colors)
    }
    lapply(1:numGroups, function(i) {
      textInput(inputId = paste0("group_color_list", i),
                label = groupLevels[i],
                value = colors[i])
    })
  })
  
  group_color_new <- reactive({
    groupLevels <- mcols(rv$proplyr) %>%
      .[, colnames(.) %in% params(rv$proplyr)$rowGroupsInUse] %>%
      as.factor() %>%
      levels(.)
    numGroups <- length(groupLevels)
    colors <- palette()
    while(length(colors) < numGroups){
      colors <- c(colors,colors)
    }
    temp <- list()
    if (input$group_color_query != 0){
      for(i in seq(numGroups)){
        temp[[i]] <- input[[paste0("group_color_list", i)]]
      }
    }else{
      for(i in seq(numGroups)){
        temp[[i]] <-  colors[i]
      }
    }
    temp
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
    if (input$sample_names_option != 0){
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
  
  observeEvent(input$MakeEnrichedHeatmap_local, {
    rownames(sampleData(rv$proplyr)) <- sample_names_new()
  })
  
  observeEvent(input$MakeEnrichedHeatmap, {
    rownames(sampleData(rv$proplyr)) <- sample_names_new()
  })
  
  # produce the text boxes for user to input colors
  output$matrices_color_individual <- renderUI({
    numSamples <- length(assays(rv$proplyr))
    lapply(1:numSamples, function(i) {
      textInput(inputId = paste0("matrices_color_list", i),
                label = rownames(sampleData(rv$proplyr))[i],
                value = "blue, white, red")
    })
  })
  
  sampleData_colnames <- reactive({
    colnames(rv$sampleData_active_subset)
  })
  
  output$sample_column_for_colors <- renderUI({
    selectInput(inputId = "select_sample_column_for_colors",
                label = "Select the name of the column from the sample metadata to be used for color grouping:",
                choices = sampleData_colnames())
  })
  
  
  output$user_group_colors <- renderUI({
    
    # get number of levels in the columns selected by the user
    column_levels <- sampleData(rv$proplyr) %>%
      .[, colnames(.) %in% input$select_sample_column_for_colors] %>%
      as.factor() %>%
      levels() 
    
    numGroups <- length(column_levels)
    
    # the list of color options for various defaults 
    matrices_color_levels_default <- list(c("white, black"), c("white, red"), c("white, #6C7EDA"), c("white, #1B5B14"), c("white, purple"), c("white, #CC0066"), c("white, #009999"), c("white, #CC6600"))
    
    # this will fill in the default colors if there are more levels in the column than I have set defaults fro above
    # this will allow us to fill in some default colors before user inputs
    while(length(matrices_color_levels_default) < numGroups){
      matrices_color_levels_default <- c(matrices_color_levels_default, matrices_color_levels_default)
    }
    matrices_color_levels <- matrices_color_levels_default[seq(numGroups)]
    
    
    lapply(1:numGroups, function(i) {
      textInput(inputId = paste0("matrices_color_groups_list", i),
                label = paste0(input$select_sample_column_for_colors, " = ", column_levels[i]),
                value = matrices_color_levels[[i]])
    })
  })
  
  
  # this creates a list of the colors that the use entered
  # this was tricky because we wanted this to remain a list if not NULL, but not be a list if NULL (Default), so we couldn't just make everything a list, then unlist in heatmap call. 
  # So we just construct the default list anyway, don't use NULL and don't unlist in heatmap call
  
  rv$individual_or_group_colors <- "default"
  
  observe({
    
    req(input$individual_or_group_colors)
    req(input$matrices_color_list1)
    temp <- list()
    if (input$individual_or_group_colors == "individual"){
      numSamples <- length(assays(rv$proplyr))
      for(i in seq(numSamples)){
        temp[[i]] <- input[[paste0("matrices_color_list", i)]]
        temp[[i]] <- strsplit(temp[[i]], split = ",") %>%
          unlist() %>%
          trimws()
      }
      rv$individual_or_group_colors <- temp
    }
  })
  
  observe({
    req(input$individual_or_group_colors)
    req(input$select_sample_column_for_colors)
    req(input$matrices_color_groups_list1)
    temp <- list()
    if (input$individual_or_group_colors == "sample_groups"){
      # get number of levels in the columns selected by the user
      column_levels <- sampleData(rv$proplyr) %>%
        .[, colnames(.) %in% input$select_sample_column_for_colors] %>%
        as.factor() %>%
        levels()
      
      numGroups <- length(column_levels)
      
      for(i in seq(numGroups)){
        temp[[i]] <- input[[paste0("matrices_color_groups_list", i)]]
        temp[[i]] <- strsplit(temp[[i]], split = ",") %>%
          unlist() %>%
          trimws()
      }
      rv$individual_or_group_colors <- temp
    }
    
  })
  
  matrices_color_new <- reactive({
    if( rv$individual_or_group_colors %in% "default"){
      NULL
    } else {
      rv$individual_or_group_colors
    }
  })
  extra_annotation_columns <- reactive({
    ifelse(is.null(input$extra_annotation_columns_input), list(NULL), list(input$extra_annotation_columns_input))
  })
  
  output$extra_anno_param_title <- renderUI ({
    if (!is.null(input$extra_annotation_columns_input)) {
      tags$h5("Customize the parameters of the extra annotation columns:")
    }
  })
  
  output$extra_anno_width_query <- renderUI ({
    if (!is.null(input$extra_annotation_columns_input)) {
      checkboxInput(inputId = "extra_anno_width_yes",
                    label = "Click to change the width of the extra annotation columns",
                    value = 0)
    }
  })
  
  output$extra_anno_width <- renderUI ({
    if (!is.null(input$extra_annotation_columns_input)) {
      numExtra <- length(input$extra_annotation_columns_input)
      conditionalPanel(
        condition = "input.extra_anno_width_yes != 0",
        lapply(1:numExtra, function(i) {
          numericInput(inputId = paste0("extra_anno_width", i),
                       label = input$extra_annotation_columns_input[i],
                       value = 6)
        })
      )
    }
  })
  
  
  extra_anno_width_input <- reactive({
    numExtra <- length(input$extra_annotation_columns_input)
    temp <- list()
    if (input$extra_anno_width_yes !=0){
      for(i in seq(numExtra)){
        temp[[i]] <- input[[paste0("extra_anno_width", i)]]
      }
    }else{ 
      for(i in seq(numExtra)){
        temp[[i]] <-  6
      }
    }
    temp
  })
  
  output$extra_anno_color_title <- renderUI ({
    if (!is.null(input$extra_annotation_columns_input)) {
      tags$h5("Customize colors of extra annotation columns:")
    }
  })
  output$extra_anno_color_query <- renderUI ({
    if (!is.null(input$extra_annotation_columns_input)) {
      checkboxInput(inputId = "extra_anno_color_yes",
                    label = "Click to change the colors of the extra annotation columns",
                    value = 0)
      # numExtra <- length(input$extra_annotation_columns_input)
      
    }
  })
  
  output$extra_anno_color_numeric <- renderUI ({
    if (!is.null(input$extra_annotation_columns_input)) {
      numExtra <- length(input$extra_annotation_columns_input)
      conditionalPanel(
        condition = "input.extra_anno_color_yes != 0",
        lapply(1:numExtra, function(i) {
          column <- mcols(rv$proplyr)[colnames(mcols(rv$proplyr)) %in% input$extra_annotation_columns_input[i]][,1]
          if (is.integer(column) | is.numeric(column)){
            column <- as.numeric(column)
            class <- "numeric"
            textInput(inputId = input$extra_annotation_columns_input[i],
                      label = HTML(paste0(input$extra_annotation_columns_input[i], " - ", class, "<br/> Enter colors, separated by commas. The number of colors entered will specify the number of breaks in the color spectrum")))
          }
        })
      )
    }
  })
  
  output$extra_anno_color_factor <- renderUI ({
    if (!is.null(input$extra_annotation_columns_input)) {
      numExtra <- length(input$extra_annotation_columns_input)
      conditionalPanel(
        condition = "input.extra_anno_color_yes != 0",
        lapply(1:numExtra, function(i) {
          column <- mcols(rv$proplyr)[colnames(mcols(rv$proplyr)) %in% input$extra_annotation_columns_input[i]][,1]
          if (is.factor(column) | is.character(column)){
            column <- ordered(column, levels = levels(column)[order(levels(column))]) # Heatmap function seems to automatically order based on alphabetical (or numeric i guess), and am not sure how to change, so for now just force it to do this here so that the inpout boxes actually line up with the colors in the figure
            column <- droplevels(column)
            class <- "factor"
            level_names <- levels(column)
            numLevels <- length(level_names)
            lapply(1:numLevels, function(x) {
              if (x == 1){
                textInput(inputId = paste0(input$extra_annotation_columns_input[i], "_", level_names[x]),
                          label = HTML(paste0(input$extra_annotation_columns_input[i], " - ", class, " with ", numLevels, " level(s) <br/> level = ", level_names[x])))
              }else {
                textInput(inputId =  paste0(input$extra_annotation_columns_input[i], "_", level_names[x]),
                          label = paste0("level = ", level_names[x]))
              }
              
            })
          }
        })
      )
    }
  })
  
  extra_anno_color_input <- reactive({
    
    numExtra <- length(input$extra_annotation_columns_input)
    color_list <- vector(mode = "list", 
                         length = numExtra)
    
    
    if (!is.null(input$extra_annotation_columns_input)){ 
      for (i in seq(numExtra)){
        column <- mcols(rv$proplyr)[colnames(mcols(rv$proplyr)) %in% input$extra_annotation_columns_input[i]][,1]
        if (is.factor(column) | is.character(column)){
          column <- ordered(column, levels = levels(column)[order(levels(column))])  # Heatmap function seems to automatically order based on alphabetical (or numeric i guess), and am not sure how to change, so for now just force it to do this here so that the inpout boxes actually line up with the colors in the figure
          column <- droplevels(column)
          class <- "factor"
          level_names <- levels(column)
          numLevels <- length(level_names)
          
          # make default color vector to pull from
          colors <- palette()
          while(length(colors) < numLevels){
            colors <- c(colors,colors)
          }
          
          temp_factor_colors <- vector()
          for(x in seq(numLevels)){
            if (input$extra_anno_color_yes != 0) {
              temp_factor_colors[x] <- input[[paste0(input$extra_annotation_columns_input[i], "_", level_names[x])]]
            } else {
              temp_factor_colors[x] <- colors[x] # default for factor
            }
          }
          color_list[[i]] <- temp_factor_colors
        } else if(is.integer(column) | is.numeric(column)) {
          if (input$extra_anno_color_yes != 0) {
            color_list[[i]] <- strsplit(input[[input$extra_annotation_columns_input[i]]], split = ",") %>%
              unlist() %>%
              trimws()
          } else {
            color_list[[i]] <- c("white", "red") # default numeric (would b e ideal to make this a function factoring in zero?)
          }
        }
        
      }
    } 
    color_list
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
  
  output$ylim_group_column <- renderUI({
    selectInput(inputId = "ylim_group_column_name",
                label = "Select the name of the column from the sample metadata to be used for ylim grouping:",
                choices = sampleData_colnames())
  })
  
  # when the button to make the enrichedheatmap is pressed, the ranges from the sliders will be taken into account
  ylim_new <- eventReactive(rv$proplyr, {
    if(input$ylim_query == "custom") { 
      numSamples <- length(assays(rv$proplyr))
      temp <- list()
      for(i in seq(numSamples)){
        temp[[i]] <- input[[paste0("ylim_list", i)]]
      }
      temp 
    } else if (input$ylim_query == "common_max_all") {
      "common_max"
    } else if (input$ylim_query == "common_max_group") {
      input$ylim_group_column_name
    } # will default to NULL (inferred) if it gets past these if statements
    
  })
  
  # when the button to make the enrichedheatmap is pressed, the ranges from the sliders will be taken into account
  ylim_new_local <- eventReactive(rv$proplyr, {
    if(input$ylim_query == "custom"){ 
      numSamples <- length(assays(rv$proplyr))
      temp <- list()
      for(i in seq(numSamples)){
        temp[[i]] <- input[[paste0("ylim_list", i)]]
      }
      temp 
    }else if (input$ylim_query == "common_max_all") {
      "common_max"
    } else if (input$ylim_query == "common_max_group") {
      input$ylim_group_column_name
    }# will default to NULL (inferred) if it gets past these if statements
    
  })
  
  top_anno_height <- reactive ({
    input$top_anno_height_entry
  })
  
  top_anno_axis_font <- reactive ({
    input$top_anno_axis_font_entry
  })
  
  sample_label_fontsize <- reactive ({
    input$sample_label_fontsize_entry
  })
  sample_label_fontface <- reactive ({
    input$sample_label_fontface_entry
  })
  
  group_anno_label_fontsize <- reactive ({
    input$group_anno_label_fontsize_entry
  })
  group_anno_label_fontface <- reactive ({
    input$group_anno_label_fontface_entry
  })
  
  group_anno_width <- reactive ({
    input$group_anno_width_entry
  })
  
  output$legend_show_select_notEqual_render <- renderUI({
    checkboxGroupInput(inputId = "legend_show_notEqual_select",
                       label = "For each sample/heatmap, select whether you want to show the legend for the color scales",
                       choices = rownames(sampleData(rv$proplyr)),
                       selected = rownames(sampleData(rv$proplyr))
    )
  })
  
  output$legend_show_select_equal_render <- renderUI({
    radioButtons(inputId = "legend_show_equal_select",
                 label = "Do you want to show the legend for the heatmap color scales?",
                 choices = c("yes" = TRUE,
                             "no" = FALSE)
    )
  })
  
  # this got pretty complicated for me, it throws an error if it runs the cde and the variables inside the iff statment are not defined yet
  # usually you can use the req() function, but this just stops the code chunk, whereas we want it to return NULL if these code chunks dont run becuase this must be the default for show_heatmap_legend arguemtn
  # here I just define a dummy variable as a reactive, and then keep track of what it is, then in the end set the show_heatmap_legend argument to NULL if the dummy variable hasnt changes (i.e. the code chunks didnt run)
  
  rv$show_heatmap_legend <- "default"
  
  observe({
    req(input$all_color_scales_equal)
    req(input$legend_show_notEqual_select)
    if(input$all_color_scales_equal == FALSE){
      rv$show_heatmap_legend <- rownames(sampleData(rv$proplyr)) %in% input$legend_show_notEqual_select 
    }
  })
  
  observe({
    req(input$all_color_scales_equal)
    req(input$legend_show_equal_select)
    if (input$all_color_scales_equal == TRUE){
      if(input$legend_show_equal_select == FALSE) {
        rv$show_heatmap_legend <- c(rep(FALSE, nrow(sampleData(rv$proplyr))))
      } else if (input$legend_show_equal_select == TRUE){
        rv$show_heatmap_legend <- "default"
      }
    }
  })
  
  show_heatmap_legend <- reactive({
    if(rv$show_heatmap_legend %in% "default"){
      NULL
    } else {
      rv$show_heatmap_legend
    }
  })  
  
  output$genes_to_label_option_render <- renderUI({
    if("SYMBOL" %in% colnames(mcols(rv$proplyr))){
      checkboxInput(inputId = "genes_to_label_option",
                    label = HTML("<b>Enter a list of gene to label the rows that have been annotated with these genes<b>"))
    }
  })
  
  # output$genes_to_label_direct_or_upload_render <- renderUI({
  #     #req(input$genes_to_label_option)
  #     if(input$genes_to_label_option != 0){
  # 
  #     radioButtons(inputId = "genes_to_label_direct_or_upload",
  #                  label = "Do you want to directly enter the genes to overlap with the annotated ranges, or upload a list of genes?",
  #                  choices = c("Type in genes" = "direct",
  #                              "Upload gene list" = "upload"),
  #                  inline = TRUE)
  #     }
  # 
  # })
  
  
  rv$genes_to_label <- "default"
  
  output$direct_type_inputbox_render <- renderUI({
    req(input$genes_to_label_option)
    if(input$genes_to_label_option != 0){
      textInput(inputId = "direct_type_inputbox",
                label = "Enter the genes (separated by spaces or commas):")
      
    }
  })
  
  output$genes_to_label_fontsize <- renderUI({
    req(input$genes_to_label_option)
    if(input$genes_to_label_option != 0){
      numericInput(inputId = "genes_to_label_fontsize_query",
                   label = "Font size of gene labels:",
                   min = 1, 
                   max = 50,
                   step = 1,
                   value = 6)
      
    }
  })
  
  genes_to_label_fontsize_input <- reactive({
    input$genes_to_label_fontsize_query
  })
  
  observeEvent(input$MakeEnrichedHeatmap_local, {
    req(input$genes_to_label_option)
    rv$genes_to_label <- strsplit(input$direct_type_inputbox, split = ",|\ ") %>%
      unlist() %>%
      trimws()
  })
  
  genes_to_label_local <- eventReactive(rv$genes_to_label, {
    if(rv$genes_to_label %in% "default"){
      NULL
    } else {
      rv$genes_to_label
    }
  })
  
  observeEvent(input$MakeEnrichedHeatmap, {
    req(input$genes_to_label_option)
    rv$genes_to_label <- strsplit(input$direct_type_inputbox, split = ",|\ ") %>%
      unlist() %>%
      trimws()
  })
  
  genes_to_label <- eventReactive(rv$genes_to_label, {
    if(rv$genes_to_label %in% "default"){
      NULL
    } else {
      rv$genes_to_label
    }
  })
  
  # genes_to_label <- eventReactive(input$MakeEnrichedHeatmap, {
  #   #req(input$direct_type_inputbox)
  #   strsplit(input$direct_type_inputbox, split = ",|\ ") %>%
  #     unlist() %>%
  #     trimws()
  # })
  
  
  # plot_input <- function(){
  #   
  #   generateEnrichedHeatmap(object = rv$proplyr,
  #                           extra_annotation_columns = unlist(extra_annotation_columns()),
  #                           extra_anno_color = extra_anno_color_input(),
  #                           extra_anno_width = unlist(extra_anno_width_input()),
  #                           include_group_annotation = include_group_annotation(),
  #                           group_anno_color = unlist(group_color_new()),
  #                           group_anno_row_title_gp = gpar(fontsize = group_anno_label_fontsize(),
  #                                                          fontface = group_anno_label_fontface()),
  #                           group_anno_width = group_anno_width(),
  #                           sample_names = unlist(sample_names_new()),
  #                           ylim = ylim_new(),
  #                           color_by_sample_group = color_by_sample_group(),
  #                           matrices_color = matrices_color_new(),
  #                           all_color_scales_equal = all_color_scales_equal(),
  #                           top_anno_height = unit(top_anno_height(), "cm"),
  #                           top_anno_axis_font = gpar(fontsize = top_anno_axis_font()),
  #                           matrices_column_title_gp = gpar(fontsize = sample_label_fontsize(),
  #                                                           fontface = sample_label_fontface()),
  #                           samples_to_sortby = samples_to_sortby(),
  #                           show_heatmap_legend  = show_heatmap_legend(),
  #                           genes_to_label = genes_to_label(),
  #                           gene_label_font_size = genes_to_label_fontsize_input()
  #   )
  #   
  # }
  
  
  #make heatmap in 'Visualize' tab for direct file upload
  makeHeatmap <- observeEvent(input$MakeEnrichedHeatmap, {
    withProgress(message = 'Action in progress',
                 value = 0.5, {
                   # file = input$heatmap_path
                   # cairo_pdf(filename = file)
                   rv$heatmap <- generateEnrichedHeatmap(object = rv$proplyr,
                                                         extra_annotation_columns = unlist(extra_annotation_columns()),
                                                         extra_anno_color = extra_anno_color_input(),
                                                         extra_anno_width = unlist(extra_anno_width_input()),
                                                         include_group_annotation = include_group_annotation(),
                                                         group_anno_color = unlist(group_color_new()),
                                                         group_anno_row_title_gp = gpar(fontsize = group_anno_label_fontsize(),
                                                                                        fontface = group_anno_label_fontface()),
                                                         group_anno_width = group_anno_width(),
                                                         sample_names = unlist(sample_names_new()),
                                                         ylim = ylim_new(),
                                                         color_by_sample_group = color_by_sample_group(),
                                                         matrices_color = matrices_color_new(),
                                                         all_color_scales_equal = all_color_scales_equal(),
                                                         top_anno_height = unit(top_anno_height(), "cm"),
                                                         top_anno_axis_font = gpar(fontsize = top_anno_axis_font()),
                                                         matrices_column_title_gp = gpar(fontsize = sample_label_fontsize(),
                                                                                         fontface = sample_label_fontface()),
                                                         samples_to_sortby = samples_to_sortby(),
                                                         show_heatmap_legend  = show_heatmap_legend(),
                                                         genes_to_label = genes_to_label(),
                                                         gene_label_font_size = genes_to_label_fontsize_input()
                   )
                   
                   # dev.off()
                 })
  })
  
  pdf(NULL)
  dev.off()
  output$EnrichedHeatmap <- renderPlot({
    rv$heatmap
  })
  
  
  options(shiny.usecairo=TRUE)
  makeHeatmap_local <- observeEvent(input$MakeEnrichedHeatmap_local, {
    withProgress(message = 'Action in progress',
                 value = 0.5, {
                   file = input$heatmap_path_local
                   cairo_pdf(filename = file)
                   rv$heatmap_local <- generateEnrichedHeatmap(object = rv$proplyr,
                                                               extra_annotation_columns = unlist(extra_annotation_columns()),
                                                               extra_anno_color = extra_anno_color_input(),
                                                               extra_anno_width = unlist(extra_anno_width_input()),
                                                               include_group_annotation = include_group_annotation(),
                                                               group_anno_color = unlist(group_color_new()),
                                                               group_anno_row_title_gp = gpar(fontsize = group_anno_label_fontsize(),
                                                                                              fontface = group_anno_label_fontface()),
                                                               group_anno_width = group_anno_width(),
                                                               sample_names = unlist(sample_names_new()),
                                                               ylim = ylim_new_local(),
                                                               color_by_sample_group = color_by_sample_group(),
                                                               matrices_color = matrices_color_new(),
                                                               all_color_scales_equal = all_color_scales_equal(),
                                                               top_anno_height = unit(top_anno_height(), "cm"),
                                                               top_anno_axis_font = gpar(fontsize = top_anno_axis_font()),
                                                               matrices_column_title_gp = gpar(fontsize = sample_label_fontsize(),
                                                                                               fontface = sample_label_fontface()),
                                                               samples_to_sortby = samples_to_sortby(),
                                                               show_heatmap_legend  = show_heatmap_legend(),
                                                               genes_to_label = genes_to_label_local(),
                                                               gene_label_font_size = genes_to_label_fontsize_input()
                   )
                   dev.off()
                 })
  })
  
  
  
  output$EnrichedHeatmap_local <- renderPlot(
    rv$heatmap_local
  )
  
  
  
  
  options(shiny.usecairo=TRUE)
  output$download_heatmap <- downloadHandler(
    filename = function(){
      paste("heatmap", "_",Sys.time(),".pdf",sep="")
    },
    content = function(file) {
      # temp_file <- file.path(tempdir(), "heatmap_test_ddd.pdf")
      # file.copy("heatmap_test_ddd.pdf", temp_file, overwrite = TRUE)
      
      #pdf(file)
      cairo_pdf(filename = file)
      generateEnrichedHeatmap(object = rv$proplyr,
                              extra_annotation_columns = unlist(extra_annotation_columns()),
                              extra_anno_color = extra_anno_color_input(),
                              extra_anno_width = unlist(extra_anno_width_input()),
                              include_group_annotation = include_group_annotation(),
                              group_anno_color = unlist(group_color_new()),
                              group_anno_row_title_gp = gpar(fontsize = group_anno_label_fontsize(),
                                                             fontface = group_anno_label_fontface()),
                              group_anno_width = group_anno_width(),
                              sample_names = unlist(sample_names_new()),
                              ylim = ylim_new(),
                              color_by_sample_group = color_by_sample_group(),
                              matrices_color = matrices_color_new(),
                              all_color_scales_equal = all_color_scales_equal(),
                              top_anno_height = unit(top_anno_height(), "cm"),
                              top_anno_axis_font = gpar(fontsize = top_anno_axis_font()),
                              matrices_column_title_gp = gpar(fontsize = sample_label_fontsize(),
                                                              fontface = sample_label_fontface()),
                              samples_to_sortby = samples_to_sortby(),
                              show_heatmap_legend  = show_heatmap_legend(),
                              genes_to_label = genes_to_label(),
                              gene_label_font_size = genes_to_label_fontsize_input()
      )
      
      dev.off()
    },
    contentType = "application/pdf"
    
  )
  
  
  
  ############
  # range manipulation
  ############
  
  
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
    withProgress(message = 'Action in progress',
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
    withProgress(message = 'Action in progress',
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
    withProgress(message = 'Action in progress',
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
    withProgress(message = 'Action in progress',
                 value = 0.5, {
                   rv$proplyr <- annotateRanges(rv$proplyr,
                                                TxDb = input$cs_genome,
                                                annotation_subset = input$annotation_subset,
                                                tssRegion = input$tss_region)
                   
                 })
  }, ignoreInit = TRUE)
  
  ##### annotate with great
  annotate_great <- observeEvent(input$perform_annotate_great, {
    withProgress(message = 'Action in progress',
                 value = 0.5, {
                   rv$proplyr <- annotateRanges_great(rv$proplyr,
                                                      species = input$great_species)
                   
                 })
  }, ignoreInit = TRUE)
  
}

shinyApp( ui = ui, server = server)
