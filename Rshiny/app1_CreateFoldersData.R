#!usr/bin/Rscript

####################################################################################################
#######################################   USER INTERFACE   #########################################
####################################################################################################

ui <- fluidPage(
  # Title
  tags$head(tags$style(".title {margin: auto; width: 750px}")),
  tags$div(class="title", titlePanel(tags$h1("Create and Run CFA/MGCFA Mplus Files"))),
  # Choose the setup file
  tags$h2("Choose the study's set up file"),#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Change the name 'set up' and 'setup'? <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ## Read in the setup file...
  fileInput(inputId="fileSetup", label=tags$h4("Choose set up Excel file"), multiple=F, accept=".xlsx"),
  
  ################################################
  
  # 01. Option to create folder structure
  tags$h2("Create folder structure"),
  radioButtons(inputId="radioFolders", label=tags$h4("Create a folder structure for the data?"), c("No","Yes"), inline=T),
  conditionalPanel(
    condition="output.radioFolders='Yes'",
    tags$h2("Please read the README file here: <LINK>"), #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Need to add the link, option to open? <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    wellPanel(
      actionButton(inputId="submitCreateFolderStructure", label="Create folder structure") #<<<<<<<< source1a_CreateFolderStructure.R, open the log file <<<<<<<<<<<<<<<<<<<<<
    )
  ),
  # 02. Option to merge the raw data files
  tags$h2("Create a single data set for the study"),
  radioButtons(inputId="radioMerge", label=tags$h4("Merge raw data into a single file?"), c("No","Yes"), inline=T),
  conditionalPanel(
    condition="output.radioMerge='Yes'",
    tags$h2("<Text explaining merging format: file inputs, file outputs; based on setup file. Text explaining what scale/item statistics will be computed and where (folder location, Excel file, sheet)>"), #<< Add this later <<<<<
    wellPanel(
      actionButton(inputId="submitCreateMergeSyntax", label="Create and open SPSS syntax") #<<<<<<<< source1b_MergeRawData.R (D:\3_LSCE\00_ShinyTesting\Rreplacements\1.Merge_Files_Rescale_Weights_Rtesting.r), (D:\3_LSCE\00_ShinyTesting\Rreplacements\scaleStats_JW.R) <<<<<
    )
  ),
  tags$h3("NOTE: The merged SPSS data set must be created to use the options below."),
  tags$h2("Modify data"),
  # 03. Option to rescale a weight in the data #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Option to rescale more than one?
  tags$h3("Rescale weights"),
  radioButtons(inputId="radioRescaleWeights", label=tags$h4("Rescale a weight?"), c("No","Yes"), inline=T),
  conditionalPanel(
    condition="output.radioRescaleWeights='Yes'",
    tags$h2("<Text noting methods of rescaling (see Mplus webnote from Agnes)"), #<<<<<<<<<<<<<<<<<< Add this later <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    wellPanel(
      radioButtons(inputId="radioChooseMethod", label=tags$h4("Choose the method for rescaling"), choices=c("Method A", "Method B", "Method C"), inline=T),
      radioButtons(inputId="radioChooseWeight", label=tags$h4("Choose the weight to rescale"), choices=unique(na.omit(dfSetup()$Weight)), inline=T),
      actionButton(inputId="submitRescale", label="Rescale the weights") #<<<<<<<<<<<<<<<<<<<<<<<<<< source1c_RescaleWeights.R (D:\3_LSCE\00_ShinyTesting\Rreplacements\dataRecodeRescale_JW.R) <<<<<
    )
  ),
  # 04. Option to recode scale items
  tags$h3("Reverse coding"),
  radioButtons(inputId="radioReverseCodeItems", label=tags$h4("Are there items that need reverse coding?"), c("No","Yes"), inline=T),
  conditionalPanel(
    condition="output.radioReverseCodeItems='Yes'",
    tags$h2("<Text noting recodes to perform and source Excel file and sheet: Option to reload scale list?"), # Add this later <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    wellPanel(
      actionButton(inputId="submitRecoding", label="Recode scale items") #<<<<<<<<<<<<<<<<<<<<<<<<<< source1d_RecodeItems.R (D:\3_LSCE\00_ShinyTesting\Rreplacements\dataRecodeRescale_JW.R) <<<<<
      #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Open before and after frequency tables for each item (using "capture.output" function?) <<<<<
    )
  ),
  # Option to create individual scale data sets
  tags$h2("Create individual scale data sets"),
  radioButtons(inputId="radioScaleDataSets", label=tags$h4("Would you like to create individual scale data sets?"), c("No","Yes"), inline=T),
  conditionalPanel(
    condition="output.radioScaleDataSets='Yes'",
    tags$h2("<Text listing variables that will be included in all data sets and source Excel file and sheet: Option to reload setup information?"), # Add this later <<<<<<<<<<
    wellPanel(
      radioButtons(inputId="radioFileTypes", label=tags$h4("Please choose which types of files you would like to create"), choices=c("*.dat (Mplus use)", "*.Rda (R use)", "*.csv", "*.xlsx"), inline=T),
      tags$h3("<Text noting the files names and where each type of file will be stored (also, if folder will be created as a result)>"),
      actionButton(inputId="submitCreateScaleData", label="Create individual scale data sets") #<<<< source1e_CreateScaleData.R (D:\3_LSCE\00_ShinyTesting\Rreplacements\dataRecodeRescale_JW.R) <<<<<
    )
  )
)
####################################################################################################
#######################################       SERVER       #########################################
####################################################################################################

server <- function(input, output, session){
  # Create datasets from the set up file
  ## Make the setup dataset 'reactive' for use in other UIs
  dfSetup <- reactive({
    infile <- input$fileSetup
    if(is.null(infile)){
      return(NULL)
    } else {
      read.xlsx(infile$datapath, sheet="SetUp", startRow=1, colNames=T, rowNames=F, rows=1:21, cols=c(3,5:15), skipEmptyRows=F)
    }
  })
  ## Make the country dataset 'reactive' for use in other UIs
  dfCntry <- reactive({
    infile <- input$fileSetup
    if(is.null(infile)){
      return(NULL)
    } else {
      read.xlsx(infile$datapath, sheet="Country", startRow=1, colNames=T, rowNames=F)
    }
  })
  # Create a statement that recognizes when the file is uploaded to produce conditional UIs
  output$uploadedFileSetup <- reactive({
    return(!is.null(dfSetup()))
  })
  outputOptions(output, "uploadedFileSetup", suspendWhenHidden=F)
  
  ################################################
  
  # 04. Create UI for selecting the population group (e.g. teachers, principals, students) #<<<<<<<< Need to add this <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  output$selectGroup <- renderUI({
    if(is.null(dfSetup())){
      return(NULL)
    } else if(!is.null(dfSetup()) & length(unique(na.omit(dfSetup()$Group)))>1){
      selectInput(inputId="selectGroup", label=tags$h4("Select the group for analysis"), choices=unique(na.omit(dfSetup()$Group)))
    } else {
      # Ignore if only one population group is given in the setup file
      return(NULL)
    }
  })
  ## Make the scale dataset 'reactive' for use in other UIs
  dfScale <- reactive({
    infile <- input$fileSetup
    if(is.null(infile)){
      return(NULL)
    } else if(!is.null(dfSetup()) & length(unique(na.omit(dfSetup()$Group)))>1){
      read.xlsx(infile$datapath, sheet=paste0("Scale_",gsub(" ","",input$selectGroup)), startRow=1, colNames=T, rowNames=F)
    } else {
      read.xlsx(infile$datapath, sheet=paste0("Scale_",dfSetup()$Group[1]), startRow=1, colNames=T, rowNames=F)
    }
  })
  # Create UIs for choosing other options
  # 06. Using adjudication variable
  output$radioAdjudicateVar <- renderUI({
    if(is.null(dfSetup())){
      return(NULL)
    } else if(!is.null(dfSetup()) & !is.na(dfSetup()$FixedInformation[9])){
      radioButtons(inputId="radioAdjudicateVar", label=tags$h4(paste0('Should the adjudication variable "',dfSetup()$FixedInformation[9],'" be used for selecting cases?')), c("No","Yes"), inline=T)
    } else {
      return(NULL)
    }
  })
  # 07. Including improvements
  output$radioImprove <- renderUI({
    if(is.null(dfSetup())){
      return(NULL)
    } else {
      radioButtons(inputId="radioImprove", label=tags$h4("Should model improvements be included in the model?"), c("No","Yes"), inline=T)
    }
  })
  # 08. Create factor scores
  ## Make conditional on the type of analysis
  appearRadioFS <- eventReactive(input$selectAnalysis, {
    if(input$selectAnalysis=="Cross-country MGCFA with MI testing" | input$selectAnalysis=="Cross-level  MGCFA with MI testing"){
      return(tags$div(tags$hr(),tags$h4("Note: Factor scores cannot be produced for the type of analysis selected"),tags$hr())) # Keep or leave blank? <<<<<<<<<<<<<<<<<<<<<<<<
    } else {
      radioButtons(inputId="radioFS", label=tags$h4("Should factor scores be produced from the model?"), c("No","Yes"), inline=T)
    }
  })
  output$radioFS <- renderUI({
    if(is.null(dfSetup())){
      return(NULL)
    } else {
      appearRadioFS()
    }
  })
  # 09. Run Mplus input files
  output$radioMplusAuto <- renderUI({
    if(is.null(dfSetup())){
      return(NULL)
    } else {
      radioButtons(inputId="radioMplusAuto", label=tags$h4("Should Mplus outputs be produced?"), c("No","Yes"), inline=T)
    }
  })
  # 10. Add filename suffix
  output$radioSuffix <- renderUI({
    if(is.null(dfSetup())){
      return(NULL)
    } else {
      radioButtons(inputId="radioSuffix", label=tags$h4("Do you want to include a common filename suffix for the Mplus input files?"), c("No","Yes"), inline=T)
    }
  })
  # 11. Write filename suffix
  output$writeSuffix <- renderUI({
    if(is.null(dfSetup())){
      return(NULL)
    } else {
      textInput(inputId="writeSuffix", label=tags$h4("Please write the suffix here:"), value="")
    }
  })
  ################################################
  
  # CREATE OUTPUT FILES
  # Create a 'reactive event' for when the "submitAnalysisOptions" action button is clicked
  textFromInputs <- eventReactive(input$submitAnalysisOptions, {
    ## Study name
    nameStudy  <- dfSetup()$FixedInformation[1]
    ## Directories
    ### Raw data directory
    dirRawData <- gsub("\\\\","/",dfSetup()$FixedInformation[3])
    if(is.na(dirRawData)){dirRawData <- "Not provided"}
    ### Root 'implementation' directory
    dirRoot    <- gsub("\\\\","/",dfSetup()$FixedInformation[4])
    if(is.na(dirRoot)){dirRoot <- "Not provided"}
    ### Reporting directory (e.g. technical reports) #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< possibly fold this into implementation <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    dirReport  <- gsub("\\\\","/",dfSetup()$FixedInformation[5])
    if(is.na(dirReport)){dirReport <- "Not provided"}
    ### R script source files directory
    dirSource  <- gsub("\\\\","/",dfSetup()$FixedInformation[6])
    if(is.na(dirSource)){dirSource <- "Not provided"}
    ### Setup Excel file directory
    dirSetup   <- gsub("\\\\","/",dfSetup()$FixedInformation[7])
    selectionFilename               <- paste0(gsub(" ","",selectionAnalysis),"_",scl)
    date <- unlist(strsplit(as.character(Sys.time()), " "))[1]
    time <- unlist(strsplit(as.character(Sys.time()), " "))[2]
    time <- gsub(":", "'", time)
    selectionFilenameDate           <- paste0(selectionFilename,"_",selectionSuffix,"_",date,"_",time)
    # Call source files and run their functions
    ## Log files
    source(paste0(dirSource,"/source2a_LogFiles.R"))
    run.source.logs(nameStudy, dirRawData, dirRoot, dirReport, dirSource, dirSetup, selectionAnalysisPhase, selectionGroup, selectionAnalysis, selectionRound, selectionGroupCodeVar, selectionStrat, selectionCluster, selectionWeight, selectionIDVars, selectionGlobalDropCountry, selectionGlobalDropLevel, selectionGlobalDropPop, selectionAdjudicateVar, selectionAdjudicateCode, selectionCountryCodeVar, selectionLevelCodeVar, selectionTargetLevelCode, selectionPopCodeVar, selectionImprove, selectionFS, selectionMplusAuto, selectionSuffix, selectionListScale, selectionListScalePI, dirAnalysisPhase, dirGroup, dirAnalysis, dirRound, selectionScale, selectionScaleDropItems, selectionScaleDropSpecifics, selectionScaleDropCountry, selectionScaleDropLevel, selectionScaleDropPop, selectionFilename, selectionFilenameDate)
    ## Mplus input files
    source(paste0(dirSource,"/source2a_i-ivCFAs.R"))
    run.CFA(nameStudy,dirRoot,dirAnalysisPhase,dirGroup,selectionGroup,dirAnalysis,selectionAnalysis,dirRound,selectionIDVars,selectionGroupCodeVar,selectionStrat,selectionCluster,selectionWeight,selectionGlobalDropCountry,selectionGlobalDropLevel,selectionGlobalDropPop,selectionAdjudicateVar,selectionAdjudicateCode,selectionCountryCodeVar,selectionLevelCodeVar,selectionTargetLevelCode,selectionPopCodeVar,selectionImprove,selectionFS,selectionMplusAuto,selectionSuffix,selectionListScalePI,selectionScale,selectionScaleDropItems,selectionScaleDropSpecifics,selectionScaleDropCountry,selectionScaleDropLevel,selectionScaleDropPop)
    # Provide output for user
    tags$h5(paste0("Scale ",scl," is done :o)"))
  })
  textInputField #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Missing something here <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  # 41. R output notes
  output$Routput <- renderUI({
    textFromInputs() #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Research how to output the console, directly <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  })
}
shinyApp(ui=ui, server=server)

# END





#####
