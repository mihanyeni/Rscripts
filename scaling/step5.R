#!usr/bin/Rscript

# Syntax author:   Justin Wild, Ph.D.                            #
# email:           justin.wild@iea-hamburg.de                    #
# Affiliation:     IEA Hamburg                                   #
# Syntax language: R (3.4.4)                                     #
# Project:         TALIS 2018                                    #
# Analysis:        Testing for Main Study (MS)                   #
# Last update:     01-Nov-2018                                   #

# Title:   MG CFA: Cross Country, Cross Population
# Purpose: Create 2018 TALIS Scales cross country, cross population MG CFA Mplus input & output for
#          invariance testing

###################################################################################################

# This syntax is the located in the directories:
#   Z:\TALIS\TALIS2018\3_MS\Analysis\02Implementation\1PreliminaryAnalysis\Teacher\2PRG\2Validation\05MGCFACrossCntCrossPopMI
#   Z:\TALIS\TALIS2018\3_MS\Analysis\02Implementation\1PreliminaryAnalysis\Principal\2PRG\2Validation\05MGCFACrossCntCrossPopMI
# and
#   Z:\TALIS\TALIS2018\3_MS\Analysis\02Implementation\2FinalAnalysis\Teacher\2PRG\2Validation\05MGCFACrossCntCrossPopMI
#   Z:\TALIS\TALIS2018\3_MS\Analysis\02Implementation\2FinalAnalysis\Principal\2PRG\2Validation\05MGCFACrossCntCrossPopMI

# The syntax relies on data sets created by the R script: "Prelim_Create_Text_Files.R"
# The R script and data sets can be found at:

#   Z:\TALIS\TALIS2018\3_MS\Analysis\02Implementation\1PreliminaryAnalysis\Teacher\2PRG\2Validation
#   Z:\TALIS\TALIS2018\3_MS\Analysis\02Implementation\1PreliminaryAnalysis\Principal\2PRG\2Validation
# and
#   Z:\TALIS\TALIS2018\3_MS\Analysis\02Implementation\2FinalAnalysis\Teacher\2PRG\2Validation
#   Z:\TALIS\TALIS2018\3_MS\Analysis\02Implementation\2FinalAnalysis\Principal\2PRG\2Validation

###################################################################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
########################                 BEGIN USER INPUT                  ########################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

# Needed package: Remove "#"s to INSTALL as needed
# NOTE: If a non-default directory for installation is desired, new path must be set

# This package runs Mplus code in Mplus software (version 7.3)
#install.packages("MplusAutomation") # Code written with package version 0.7-2

# Set directory path keywords
# NOTE: This section has the user choose between two sets of options: Analysis Stage & Population
#       It relies on the folder structure of TALIS 2018. The TALIS 2018 MS study Implementation
#       folder structure can be found at:
#       Z:\TALIS\TALIS2018\3_MS\Analysis\01Preparation\folders.txt

# Which analysis are you working on?                  Remove the "#" of the appropriate stage.
#setAnalysisStage <- "1PreliminaryAnalysis"
setAnalysisStage <- "2FinalAnalysis"

# Are you interested in Teacher or Principal data?    Remove the "#" of the appropriate data.
setPopulationDta <- "Teacher"
#setPopulationDta <- "Principal"

# Which round of analysis are you working on?         Remove the "#" of the appropriate round.
#setAnalysisRound <- "1FirstRound"
#setAnalysisRound <- "2SecondRound"
setAnalysisRound <- "3FinalScaleResults"

# Which data will you be working with?                Remove the "#" of the appropriate data folder.
## Raw data
#setDataFileFoldr <- ""
## Data with cases removed that should not be in the analysis (e.g. South Africa, TALIS-PISA link)
#setDataFileFoldr <- "\\\\1RemovedCases"
## Data with added phantom indicators for missing items or items with zero covariance
#setDataFileFoldr <- "\\\\2AddedPhantoms"
## Data with added ID variables for each separate country/population case (for cross country, cross
## population MG CFA)
#setDataFileFoldr <- "\\\\3AddedIDs"
## Data with special IDs
#setDataFileFoldr <- "\\\\4Scores"
setDataFileFoldr <- "\\\\1AddedSuperGroupID"

#-------------------------------------------------------------------------------------------------#
########################                OPTIONAL USER INPUT                ########################
#-------------------------------------------------------------------------------------------------#

# Directories: should only need changes if working with the data off the Z drive
# NOTE: For this code, please use "\\" between folders; R has special uses for "\" with certain
#       characters; if you use "\", you will likely get an error
## Directory where text files are stored
dirText <- paste0("Z:\\TALIS\\TALIS2018\\3_MS\\Analysis\\02Implementation\\",setAnalysisStage,"\\",
                  setPopulationDta,"\\2PRG\\2Validation")
## Directory where the data is stored
dirData <- paste0("Z:\\TALIS\\TALIS2018\\3_MS\\Analysis\\02Implementation\\",setAnalysisStage,"\\",
                  setPopulationDta,"\\1Data\\3Validation",setDataFileFoldr)
## Directory where the master scale MGCFA crosstable is stored
dirScal <- paste0("Z:\\TALIS\\TALIS2018\\3_MS\\Analysis\\02Implementation\\",setAnalysisStage,"\\",
                  setPopulationDta,"\\5ArchiveAndDoc\\03_MGCFA MI Improvements")
## Directory where improvements are stored
dirImpr <- paste0("Z:\\TALIS\\TALIS2018\\3_MS\\Analysis\\02Implementation\\",setAnalysisStage,"\\",
                  setPopulationDta,"\\4Validation")
## Directory where CFA scale files will be stored
dirCFAs <- paste0("Z:\\TALIS\\TALIS2018\\3_MS\\Analysis\\02Implementation\\",setAnalysisStage,"\\",
                  setPopulationDta,"\\4Validation\\05MGCFACrossCntCrossPopMI\\",setAnalysisRound)

# Names of needed files
## Names of the scale lists for each type of scale
nameScl <- paste0("Scales_",setPopulationDta,"_automated.Rda")
namCntr <- paste0("CountryList_modified.Rda")
namSTab <- paste0("CCCP_table_",tolower(setPopulationDta),".xlsx")
namImpr <- paste0("model improvements_",tolower(setPopulationDta),".xlsx")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
########################                  END USER INPUT                   ########################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
###################################################################################################

# 1: Read in the file which contains Scale information
setwd(dirText)
load(nameScl)
load(namCntr)

###################################################################################################

# 2: Create function to write Mplus code for Scales
## Create Mplus syntax loop
writeMGCFA_CCCP <- function(scale, items, catgr, estmt, invar){
  mplusSyntax   <- 
    c('TITLE:',
      '<tabspace>MG CFA for <scl.name> - Cross-Country, Cross Population.',
      '',
      'DATA: FILE IS',
      '<tabspace>"Z:\\TALIS\\TALIS2018\\3_MS\\Analysis\\02Implementation\\',
      '<tabspace><data.pth>',
      '<tabspace><scl.name>.dat";',
      '',
      'VARIABLE: NAMES ARE',
      '<tabspace>IDCNTRY <id.varbl> ISCED <stratify> <clusters> SENWGT0 <itm.nams> SUPERID SUPERGROUPID;',
      '<tabspace>GROUPING IS SUPERGROUPID (<spl.code>);',
      '',
      'IDVARIABLE IS SUPERID;',
      '',
      'USEVARIABLES ARE <itm.nams>;',#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      '<categrcl>',
      '',
      'MISSING ARE <itm.nams> (9);',
      '',
      'WEIGHT IS SENWGT0;',
      'STRATIFICATION IS <stratify>;',
      'CLUSTER IS <clusters>;',
      '',
      'ANALYSIS:',
      'ITERATIONS = 1000;',
      'TYPE IS COMPLEX;',
      'ESTIMATOR = <estimatr>;',
      'PROCESSORS = 8;',
      '!MODEL = <inv.test>;',
      '',
      'MODEL:',
      '',# This is line 32 of the syntax, where we will place the model statement (see 2k below)
      '',
      'OUTPUT: SAMPSTAT STANDARDIZED RESIDUAL TECH1 FSDETERMINACY SVALUES;',
      '',
      'SAVEDATA: FILE IS <scl.name>_2levelMGCFA_FactorScore.dat;',
      'SAVE IS FSCORES;')

  ### 2a: Replace <scl.name> with scale
  mplusSyntax <- gsub("<scl.name>", scale, mplusSyntax)
  
  ### 2b: Replace <data.pth> with dPath
  mplusSyntax <- gsub("<data.pth>", dPath, mplusSyntax)

  ### 2c: Replace <id.varbl> with idvar
  mplusSyntax <- gsub("<id.varbl>", idvar, mplusSyntax)
  
  ### 2d: Replace <stratify> with strat
  mplusSyntax <- gsub("<stratify>", strat, mplusSyntax)
  
  ### 2e: Replace <clusters> with clust
  mplusSyntax <- gsub("<clusters>", clust, mplusSyntax)
  
  ### 2f: Replace <itm.nams> with items
  mplusSyntax <- gsub("<itm.nams>", items, mplusSyntax)
  
  ### 2f: Replace <itm.nams> with items
  #mplusSyntax <- gsub("<new.itm.nams>", newitems, mplusSyntax)#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  ### 2g: Replace <spl.code> with splcd
  mplusSyntax <- gsub("<spl.code>", splcd, mplusSyntax)

  ### 2h: Replace <categrcl> with catgr
  mplusSyntax <- gsub("<categrcl>", catgr, mplusSyntax)

  ### 2i: Replace <estimatr> with estmt
  mplusSyntax <- gsub("<estimatr>", estmt, mplusSyntax)
  
  ### 2j: Replace <inv.test> with invar
  mplusSyntax <- gsub("<inv.test>", invar, mplusSyntax)# NOT NEEDED?!?<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  ### 2k: Insert the model statement
  mplusSyntax <- c(mplusSyntax[1:31],model,mplusSyntax[33:37])
  
  ### Ensure no lines are greater than 90 characters
  mplusSyntax <- paste(strwrap(mplusSyntax,90, simplify=T), collapse="\n")
  
  ### 2l: Replace <tabspace> with tabSp
  mplusSyntax <- gsub("<tabspace>", tabSp, mplusSyntax)

  writeLines(text=mplusSyntax, con=paste(scale,"_2levelMGCFA.inp", sep=""))
}
###################################################################################################

# 3: Write Mplus files to respective Scale folders
## Set fixed replacement variables
### Set correct path folders for the analysis and a spacing variable for "nice looking" Mplus code
dPath <- paste0(setAnalysisStage,"\\\\",setPopulationDta,"\\\\1Data\\\\3Validation",setDataFileFoldr,"\\\\")
tabSp <- "     "
### Set ID, stratification, and cluster variables
if(setPopulationDta== "Teacher"){
  idvar <- "IDTEACH"
  strat <- "BRRTZONE"
  clust <- "BRRTREP"
}
if(setPopulationDta== "Principal"){
  idvar <- "IDSCHOOL"
  strat <- "BRRSZONE"
  clust <- "BRRSREP"
}
## Import improvment Excel sheet
setwd(dirImpr)
library(openxlsx)
PoolImpr <- read.xlsx(namImpr, sheet="Improvements", colNames=T)

## Create scale folders and replacement strings in the syntax for each respective scale
setwd(dirCFAs)
#for(z in c(1:4,7:9,11,15:nrow(scalesInfo))){# No TTCONTEMP (27), PDLEAD (5), PEQB (6), PMULTC (10), PSEQP (14)<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#for(z in c(1,6:8,10:17,23:26,28:nrow(scalesInfo))){
#for(z in c(2:5,9,18:22)){
for(z in c(6)){
  ### Create scale name
  scale <- scalesInfo$Name[z]
  ### Create item list
  items <- scalesInfo$Vars[z]
  ### Create "Categorical" Mplus statement
  if(!is.na(scalesInfo$MCat[z])){
    catgr <- paste0(scalesInfo$MCat[z],scalesInfo$Vars[z],";")
  } else {
    catgr <- ""
  }
  ### Create estimator type
  estmt <- scalesInfo$MEst[z]
  ### Create list of invariance testing statements
  invar <- scalesInfo$MInv[z]
  ### Create model statement
  ### UNIDIMENSIONAL SCALES
  if(is.na(scalesInfo$MDsc[z])){
    #### Create a list of the scale variables
    itemL <- unlist(strsplit(items, " "))
    #### Add "*" after the first element of the scale variables (freeing the parameter)
    itemL[1] <- paste0(itemL[1], "*")
    #### Recombine the list of scale variables into a string
    itemL <- paste(itemL, collapse=" ")
    #### Create "BY" statement for the scale
    model1 <- paste0(scale, " BY ", itemL, ";")# Changed "modl1" to "model1"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    #### Create "scale variance" statement
    #modl2 <- paste0(scale, "@1;")<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    #### Create full model statement
    #model <- c(modl1, "", modl2)<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  } else {
    ### MULTIDIMENSIONAL SCALES
    itemL <- c()
    tmpModel <- c()
    tmpSubsc <- c()
    for(j in 1:nrow(scalesInfo)){
      if(scalesInfo$ptMD[j] %in% scale){
        tmpScale <- scalesInfo$Name[j]
        tmpItemL <- scalesInfo$Vars[j]
        tmpItemL <- unlist(strsplit(tmpItemL, " "))
        tmpItemL[1] <- paste0(tmpItemL[1], "*")
        tmpItemL <- paste(tmpItemL, collapse=" ")
        itemL    <- c(itemL, tmpItemL)
        tmpModl1 <- paste0(tmpScale," BY ",tmpItemL,";")
        tmpModel <- c(tmpModel, tmpModl1)
        #tmpSubsc <- c(tmpSubsc, tmpScale)
      }
    }
    #for(j in 1:length(tmpSubsc)){
    #  tmpSubsc[j] <- paste0(tmpSubsc[j],"@1;") 
    #}
    model1 <- c(tmpModel)#, "", tmpSubsc)
  }
  #### Add improvement(s)<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ### Pooled CFA improvements
  tmpImprv <- c()
  for(j in 1:nrow(PoolImpr)){
    if(scale %in% PoolImpr$Scale[j]){
      tmpImprv1 <- PoolImpr$PooledImprovement[j]
      tmpImprv1 <- unlist(strsplit(tmpImprv1, "\n"))
      #tmpImprv2 <- PoolImpr$CCCPImprovement[j]<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      #tmpImprv2 <- unlist(strsplit(tmpImprv2, "\n"))
    } else {
      tmpImprv1 <- NA
      tmpImprv2 <- NA
    }
    tmpImprv <- c(tmpImprv, tmpImprv1)#, tmpImprv2)<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    tmpImprv <- tmpImprv[!is.na(tmpImprv)]
  }
  model1 <- c(model1, tmpImprv)
  ### Begin creating need information and syntax for model definitions and constraints
  #### Import the dataset to set special ID variables<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  tempData <- read.table(paste0(dirData,"\\",scale,".dat"), header=F, sep="")
  itemSplit <- unlist(strsplit(items, " "))
  colnames(tempData) <- c("IDCNTRY","IDSCHOOL","ISCED","BRRTZONE","BRRTREP","SENWGT0",itemSplit,"SUPERID","SUPERGROUP")
  cntryData <- cntryModfy[,c(1:4,10)]
  tempCntry <- tempData[,c(1,3,length(tempData))]
  tempCntry <- unique(tempCntry)
  for(i in 1:nrow(cntryData)){
    for(j in 1:nrow(tempCntry)){
      if((cntryData$CountryCode[i] %in% tempCntry$IDCNTRY[j]) & (cntryData$ISCED[i] %in% tempCntry$ISCED[j])){
        cntryData$SuperGroupID[i] <- tempCntry$SUPERGROUP[j]
      }
    }
  }
  #### Create string of unique country/population combinations
  ##### Must remove Belgium-FlemmishCommunity @ ISCED2 level<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  #cntryData <- cntryData[-56,]
  listSpclID <- c()
  for(i in 1:nrow(cntryData)){
    listSpclID <- c(listSpclID, paste0(cntryData$SuperGroupID[i],"=",paste0(cntryData$CountryAlpha3[i],"_ISCED",cntryData$ISCED[i])))#cntryData$spclStr))
  }
  splcd      <- paste(listSpclID, collapse=", ")
  #### Import scale table
  scaleTable <- read.xlsx(paste0(dirScal,"\\",namSTab), sheet=scale , colNames=T, cols=c(1:5))#, rowNames=T)#Check specifications<<<<<<<<<<<<<<<<<<<<<<<<<<<
  for(i in 1:nrow(scaleTable)){
    for(j in 2:ncol(scaleTable)){
      if(is.na(scaleTable[i,j])){
        scaleTable[i,j] <- "X"
      }
      scaleTable[i,j] <- toupper(scaleTable[i,j])
    }
  }
  #### Create a list of relevant populations for the Mplus syntax
  listPops <- c()
  for(k in 1:(nrow(scaleTable)-2)){
    for(l in 2:(ncol(scaleTable)-1)){
      if(scaleTable[k,l]!="X"){
        listPops <- c(listPops, paste0(scaleTable$Country[k],"_",colnames(scaleTable)[l]))
      }
    }
  }
  # Get UD and MD scales
  for(i in 1:nrow(scalesInfo)){
    if(scalesInfo$Name[i] %in% scale){
      scaleRow <- as.numeric(i)
    }
  }
  usedScaleFull <- c()
  usedScales <- c()
  if(is.na(scalesInfo$MDsc[scaleRow])){
    usedScaleFull <- scale
    usedScales <- substr(gsub("[[:digit:]]+", "", scale),1,6)
  } else {
    for(j in 1:nrow(scalesInfo)){
      #### If the MD scale name is in the "ptMD" column...
      if(scalesInfo$ptMD[j] %in% scale){
        #### Get the sub-scale name
        scaleName <- substr(gsub("[[:digit:]]+", "", scalesInfo$Name[j]),1,6)
        scaleNameFull <- scalesInfo$Name[j]
        usedScales <- c(usedScales, scaleName)
        usedScaleFull <- c(usedScaleFull, scaleNameFull)
      }
    }
  }
  # Set parameter names
  modelPrmNm <- c()
  listVarnc0 <- c()
  listMeans0 <- c()
  for(i in 1:length(listPops)){
    load1 <- unlist(strsplit(tolower(unlist(strsplit(listPops[i], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
    #load1 <- unlist(strsplit(tolower(unlist(strsplit(listPops[i], "-", perl=T))[1]), "_"))[1]<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    load2 <- toupper(letters[as.numeric(substr(listPops[i], nchar(listPops[i]), nchar(listPops[i])))])
    modelPrmNm <- c(modelPrmNm, paste0("MODEL ",listPops[i],":"))
    for(u in 1:length(usedScales)){
      itemScale <- items# IF DELETED what to do?<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      itemNum <- as.numeric(length(unlist(strsplit(itemScale, " "))))
      intercept <- paste0("[",itemScale,"]")
      scaleVari <- paste0(usedScaleFull[u],"*")
      scaleMean <- paste0("[",usedScaleFull[u],"*]")
      
      modelPrmNm <- c(modelPrmNm,
                      paste0(usedScaleFull[u]," BY ",itemL[u]," (",usedScales[u],load1,load2,"load1-",usedScales[u],load1,load2,"load",as.character(itemNum),");"),
                      paste0(intercept," (",usedScales[u],load1,load2,"int1-",usedScales[u],load1,load2,"int",as.character(itemNum),");"),
                      paste0(scaleVari," (",usedScales[u],load1,load2,"var);"),
                      paste0(scaleMean," (",usedScales[u],load1,load2,"mean);"),
                      "")
      cntryVari <- paste0(usedScales[u],load1,"Bvar")
      cntryMean <- paste0(usedScales[u],load1,"Bmean")
      listVarnc0 <- c(listVarnc0, cntryVari)
      listMeans0 <- c(listMeans0, cntryMean)
    }
  }
  listVarnc0 <- unique(listVarnc0)
  listMeans0 <- unique(listMeans0)
  listVarnc <- c()
  listMeans <- c()
  for(u in 1:length(usedScales)){
    templistVar <- c()
    templistMns <- c()
    for(i in 1:length(listVarnc0)){
      if(grepl(usedScales[u], listVarnc0[i])){
        templistVar <- c(templistVar, listVarnc0[i])
      }
    }
    listVarnc[[as.numeric(u)]] <- templistVar
    for(i in 1:length(listMeans0)){
      if(grepl(usedScales[u], listMeans0[i])){
        templistMns <- c(templistMns, listMeans0[i])
      }
    }
    listMeans[[as.numeric(u)]] <- templistMns
  }
  # Mean and variance constraints
  modelConst1 <- c("MODEL CONSTRAINT:")
  ## Determine reference population
  if(scaleTable[(nrow(scaleTable)-1),2]=="C" & scaleTable[(nrow(scaleTable)-1),3]=="C" & scaleTable[(nrow(scaleTable)-1),4]=="C"){
    refPop <- 2
  } else {
    if(scaleTable[(nrow(scaleTable)-1),3]=="S" | scaleTable[(nrow(scaleTable)-1),3]=="M"){
      refPop <- 2
    } else {
      if((scaleTable[(nrow(scaleTable)-1),2]=="S" & scaleTable[(nrow(scaleTable)-1),4]=="S") | (scaleTable[(nrow(scaleTable)-1),2]=="M" & scaleTable[(nrow(scaleTable)-1),4]=="M") | (scaleTable[(nrow(scaleTable)-1),2]=="C" & scaleTable[(nrow(scaleTable)-1),4]=="C")){
        pop1miss <- 0
        pop3miss <- 0
        for(i in 1:(nrow(scaleTable)-2)){
          if(scaleTable[i,2]=="X"){
            pop1miss <- pop1miss + 1
          }
          if(scaleTable[i,4]=="X"){
            pop3miss <- pop3miss + 1
          }
        }
        if(pop1miss>pop3miss){
          refPop <- 3
        } else {
          refPop <- 1
        }
      } else {
        if((scaleTable[(nrow(scaleTable)-1),2]=="S" & (scaleTable[(nrow(scaleTable)-1),4]=="M" | scaleTable[(nrow(scaleTable)-1),4]=="C")) | (scaleTable[(nrow(scaleTable)-1),2]=="M" & scaleTable[(nrow(scaleTable)-1),4]=="C")){
          refPop <- 1
        } else {
          if((scaleTable[(nrow(scaleTable)-1),4]=="S" & (scaleTable[(nrow(scaleTable)-1),2]=="M" | scaleTable[(nrow(scaleTable)-1),2]=="C")) | (scaleTable[(nrow(scaleTable)-1),4]=="M" & scaleTable[(nrow(scaleTable)-1),2]=="C")){
            refPop <- 3
          }
        }
      }
    }
  }
  ## Mean and variance constraints for reference pop
  if(scaleTable[(nrow(scaleTable)-1),(refPop+1)]=="S"){
    modelConst1 <- c(modelConst1,paste0("!Model constraints for reference population: ISCED",as.character(refPop)," - Scalar (none)"),"")
  } else {
    if(scaleTable[(nrow(scaleTable)-1),(refPop+1)]=="M"){
      modelConst1 <- c(modelConst1, paste0("!Model constraints for reference population: ISCED",as.character(refPop)," - Metric (means)"))
      for(i in 1:(nrow(scaleTable)-2)){
        if(scaleTable[i,(refPop+1)]!="X"){
          for(u in 1:length(usedScales)){
            load1 <- tolower(scaleTable$Country[i])
            load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[(refPop+1)], nchar(colnames(scaleTable)[(refPop+1)]), nchar(colnames(scaleTable)[(refPop+1)])))])
            modelConst1 <- c(modelConst1, paste0(usedScales[u],load1,load2,"mean = 0;"))
          }
        }
        modelConst1 <- c(modelConst1)#, "")
      }
    } else {
      if(scaleTable[(nrow(scaleTable)-1),(refPop+1)]=="C"){
        modelConst1 <- c(modelConst1, paste0("!Model constraints for reference population: ISCED",as.character(refPop)),"! - Configural (means and variances)")
        for(i in 1:(nrow(scaleTable)-2)){
          if(scaleTable[i,(refPop+1)]!="X"){
            for(u in 1:length(usedScales)){
              load1 <- tolower(scaleTable$Country[i])
              load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[(refPop+1)], nchar(colnames(scaleTable)[(refPop+1)]), nchar(colnames(scaleTable)[(refPop+1)])))])
              modelConst1 <- c(modelConst1, paste0(usedScales[u],load1,load2,"var = 1;"), paste0(usedScales[u],load1,load2,"mean = 0;"))
            }
          }
          modelConst1 <- c(modelConst1, "")
        }
      }
    }
  }
  ## Mean and variance constraints elsewhere
  modelConst1 <- c(modelConst1,"!Other model constraints for other populations")
  for(i in 1:(nrow(scaleTable)-2)){
    for(j in 2:(ncol(scaleTable)-1)){
      if(j!=(refPop+1)){
        if(scaleTable[i,j]!="X"){
          if(refPop!=2 & scaleTable[i,(refPop+1)]=="X" & j==3){
            for(u in 1:length(usedScales)){
              load1 <- tolower(scaleTable$Country[i])
              load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[j], nchar(colnames(scaleTable)[j]), nchar(colnames(scaleTable)[j])))])
              modelConst1 <- c(modelConst1, paste0(usedScales[u],load1,load2,"var = 1;"), paste0(usedScales[u],load1,load2,"mean = 0;"),"")
            }
          } else {
            for(u in 1:length(usedScales)){
              # Both configural
              if(scaleTable[i,ncol(scaleTable)]=="C" & scaleTable[(nrow(scaleTable)-1),j]=="C"){
                load1 <- tolower(scaleTable$Country[i])
                load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[j], nchar(colnames(scaleTable)[j]), nchar(colnames(scaleTable)[j])))])
                modelConst1 <- c(modelConst1, paste0(usedScales[u],load1,load2,"var = 1;"), paste0(usedScales[u],load1,load2,"mean = 0;"),"")
              }
              # One configural, one metric
              if(scaleTable[i,ncol(scaleTable)]=="C" & scaleTable[(nrow(scaleTable)-1),j]=="M"){
                load1 <- tolower(scaleTable$Country[i])
                load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[j], nchar(colnames(scaleTable)[j]), nchar(colnames(scaleTable)[j])))])
                modelConst1 <- c(modelConst1, paste0("!",usedScales[u],load1,load2,"var = 1;"), paste0(usedScales[u],load1,load2,"mean = 0;"),"")
              }
              if(scaleTable[i,ncol(scaleTable)]=="M" & scaleTable[(nrow(scaleTable)-1),j]=="C"){
                load1 <- tolower(scaleTable$Country[i])
                load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[j], nchar(colnames(scaleTable)[j]), nchar(colnames(scaleTable)[j])))])
                modelConst1 <- c(modelConst1, paste0("!",usedScales[u],load1,load2,"var = 1;"), paste0(usedScales[u],load1,load2,"mean = 0;"),"")
              }
              # One configural, one scalar
              if(scaleTable[i,ncol(scaleTable)]=="C" & scaleTable[(nrow(scaleTable)-1),j]=="S"){
                load1 <- tolower(scaleTable$Country[i])
                load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[j], nchar(colnames(scaleTable)[j]), nchar(colnames(scaleTable)[j])))])
                modelConst1 <- c(modelConst1, paste0("!",usedScales[u],load1,load2,"var = 1;"), paste0("!",usedScales[u],load1,load2,"mean = 0;"),"")
              }
              if(scaleTable[i,ncol(scaleTable)]=="S" & scaleTable[(nrow(scaleTable)-1),j]=="C"){
                load1 <- tolower(scaleTable$Country[i])
                load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[j], nchar(colnames(scaleTable)[j]), nchar(colnames(scaleTable)[j])))])
                modelConst1 <- c(modelConst1, paste0("!",usedScales[u],load1,load2,"var = 1;"), paste0("!",usedScales[u],load1,load2,"mean = 0;"),"")
              }
              # Both metric
              if(scaleTable[i,ncol(scaleTable)]=="M" & scaleTable[(nrow(scaleTable)-1),j]=="M"){
                load1 <- tolower(scaleTable$Country[i])
                load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[j], nchar(colnames(scaleTable)[j]), nchar(colnames(scaleTable)[j])))])
                modelConst1 <- c(modelConst1, paste0("!",usedScales[u],load1,load2,"var = 1;"), paste0(usedScales[u],load1,load2,"mean = 0;"),"")
              }
              # One metric, one scalar
              if(scaleTable[i,ncol(scaleTable)]=="M" & scaleTable[(nrow(scaleTable)-1),j]=="S"){
                load1 <- tolower(scaleTable$Country[i])
                load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[j], nchar(colnames(scaleTable)[j]), nchar(colnames(scaleTable)[j])))])
                modelConst1 <- c(modelConst1, paste0("!",usedScales[u],load1,load2,"var = 1;"), paste0("!",usedScales[u],load1,load2,"mean = 0;"),"")
              }
              if(scaleTable[i,ncol(scaleTable)]=="S" & scaleTable[(nrow(scaleTable)-1),j]=="M"){
                load1 <- tolower(scaleTable$Country[i])
                load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[j], nchar(colnames(scaleTable)[j]), nchar(colnames(scaleTable)[j])))])
                modelConst1 <- c(modelConst1, paste0("!",usedScales[u],load1,load2,"var = 1;"), paste0("!",usedScales[u],load1,load2,"mean = 0;"),"")
              }
              # Both scalar
              if(scaleTable[i,ncol(scaleTable)]=="S" & scaleTable[(nrow(scaleTable)-1),j]=="S"){
                load1 <- tolower(scaleTable$Country[i])
                load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[j], nchar(colnames(scaleTable)[j]), nchar(colnames(scaleTable)[j])))])
                modelConst1 <- c(modelConst1, paste0("!",usedScales[u],load1,load2,"var = 1;"), paste0("!",usedScales[u],load1,load2,"mean = 0;"),"")
              }
            }
          }
        } else {
          next
        }
      } else {next}
    }
  }
  # Fix average mean and variance
  modelFixVr <- c("! Fix average variance to 1 (sum of variances to the number of countries):","! for ISCED 2 metric and scalar invariance")
  modelFixMn <- c("! Fix average latent mean to 0: for ISCED 2 scalar invariance only")
  for(u in 1:length(usedScales)){
    #itemScale <- scalesInfo$Vars[which(scalesInfo$Name %in% usedScaleFull[u])]
    #itemNum <- as.numeric(length(unlist(strsplit(itemScale, " "))))
    if((scaleTable[(nrow(scaleTable)-1),2]=="M" | scaleTable[(nrow(scaleTable)-1),2]=="S") | (scaleTable[(nrow(scaleTable)-1),3]=="M" | scaleTable[(nrow(scaleTable)-1),3]=="S") | (scaleTable[(nrow(scaleTable)-1),4]=="M" | scaleTable[(nrow(scaleTable)-1),4]=="S")){
      modelFixVr <- c(modelFixVr, paste0("0 = ",paste(unique(listVarnc[[u]]), collapse=" + ")," - ",as.character(length(unique(listVarnc[[u]]))-1),";"),"")
    } else {
      modelFixVr <- ""
    }
    if(scaleTable[(nrow(scaleTable)-1),2]=="S" | scaleTable[(nrow(scaleTable)-1),3]=="S" | scaleTable[(nrow(scaleTable)-1),4]=="S"){
      modelFixMn <- c(modelFixMn, paste0("0 = ",paste(unique(listMeans[[u]]), collapse=" + "),";"),"")
    } else {
      modelFixMn <- ""
    }
  }
  # Loading and intercepts constraints
  modelConst2 <- c()
  for(i in 2:(ncol(scaleTable)-1)){
    if(scaleTable[as.numeric(((nrow(scaleTable)-1))),i]=="S"){
      modelConst2 <- c(modelConst2, paste0("!Cross country, scalar ISCED",as.numeric(substr(colnames(scaleTable)[i], nchar(colnames(scaleTable)[i]), nchar(colnames(scaleTable)[i]))),":"),"! (equal loadings, equal intercepts)")
      indexRow <- which(scaleTable[,i]!="X")
      indexRow <- as.numeric(indexRow[1])
      for(j in 1:(nrow(scaleTable)-2)){
        if(j!=indexRow){
          if(scaleTable[j,i]!="X"){
            for(u in 1:length(usedScales)){
              itemScale <- scalesInfo$Vars[which(scalesInfo$Name %in% usedScaleFull[u])]
              itemNum <- as.numeric(length(unlist(strsplit(itemScale, " "))))
              for(k in 1:itemNum){
                load1 <- unlist(strsplit(tolower(unlist(strsplit(scaleTable$Country[indexRow], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
                load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[i], nchar(colnames(scaleTable)[i]), nchar(colnames(scaleTable)[i])))])
                fullLoad1 <- paste0(usedScales[u],load1,load2,"load",as.character(k))
                load3 <- unlist(strsplit(tolower(unlist(strsplit(scaleTable$Country[j], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
                fullLoad2 <- paste0(usedScales[u],load3,load2,"load",as.character(k))
                modelConst2 <- c(modelConst2, paste0(fullLoad1," = ",fullLoad2,";"))
              }
              modelConst2 <- c(modelConst2, "")
            }
          } else {next}
        }
      }
      for(j in 1:(nrow(scaleTable)-2)){
        if(j!=indexRow){
          if(scaleTable[j,i]!="X"){
            for(u in 1:length(usedScales)){
              itemScale <- scalesInfo$Vars[which(scalesInfo$Name %in% usedScaleFull[u])]
              itemNum <- as.numeric(length(unlist(strsplit(itemScale, " "))))
              for(k in 1:itemNum){
                load1 <- unlist(strsplit(tolower(unlist(strsplit(scaleTable$Country[indexRow], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
                load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[i], nchar(colnames(scaleTable)[i]), nchar(colnames(scaleTable)[i])))])
                fullLoad1 <- paste0(usedScales[u],load1,load2,"int",as.character(k))
                load3 <- unlist(strsplit(tolower(unlist(strsplit(scaleTable$Country[j], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
                fullLoad2 <- paste0(usedScales[u],load3,load2,"int",as.character(k))
                modelConst2 <- c(modelConst2, paste0(fullLoad1," = ",fullLoad2,";"))
              }
              modelConst2 <- c(modelConst2, "")
            }
          } else {next}
        }
      }
    } else {
      if(scaleTable[as.numeric(((nrow(scaleTable)-1))),i]=="M"){
        modelConst2 <- c(modelConst2, paste0("!Cross country, metric ISCED",as.numeric(substr(colnames(scaleTable)[i], nchar(colnames(scaleTable)[i]), nchar(colnames(scaleTable)[i]))),": (equal loadings)"))
        indexRow <- which(scaleTable[,i]!="X")
        indexRow <- as.numeric(indexRow[1])
        for(j in 1:(nrow(scaleTable)-2)){
          if(j!=indexRow){
            if(scaleTable[j,i]!="X"){
              for(u in 1:length(usedScales)){
                itemScale <- scalesInfo$Vars[which(scalesInfo$Name %in% usedScaleFull[u])]
                itemNum <- as.numeric(length(unlist(strsplit(itemScale, " "))))
                for(k in 1:itemNum){
                  load1 <- unlist(strsplit(tolower(unlist(strsplit(scaleTable$Country[indexRow], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
                  load2 <- toupper(letters[as.numeric(substr(colnames(scaleTable)[i], nchar(colnames(scaleTable)[i]), nchar(colnames(scaleTable)[i])))])
                  fullLoad1 <- paste0(usedScales[u],load1,load2,"load",as.character(k))
                  load3 <- unlist(strsplit(tolower(unlist(strsplit(scaleTable$Country[j], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
                  fullLoad2 <- paste0(usedScales[u],load3,load2,"load",as.character(k))
                  modelConst2 <- c(modelConst2, paste0(fullLoad1," = ",fullLoad2,";"))
                }
                modelConst2 <- c(modelConst2, "")
              }
            } else {next}
          }
        }
      } else {
        if(scaleTable[as.numeric(((nrow(scaleTable)-1))),i]=="C"){
          modelConst2 <- c(modelConst2, paste0("!Cross country, configural ISCED",as.numeric(substr(colnames(scaleTable)[i], nchar(colnames(scaleTable)[i]), nchar(colnames(scaleTable)[i]))),": (no equalities)"))
        }
      }
    }
  }
  # Reference country
  ## Get list of countries that have two populations with identical invariance levels
  scalarTwoCntry <- c()
  metricTwoCntry <- c()
  configTwoCntry <- c()
  ## Get list of coutries that have all three populations and identical invariance levels
  scalarFullCntry <- c()
  metricFullCntry <- c()
  configFullCntry <- c()
  for(i in 1:(nrow(scaleTable)-2)){
    listTemp <- c(scaleTable[i,2], scaleTable[i,3], scaleTable[i,4])
    for(j in length(listTemp):1){
      if(listTemp[j]=="X"){
        listTemp <- listTemp[-j]
      } else {next}
    }
    if(length(listTemp)==1 | length(listTemp)==0){
      next
    } else {
      if(all(sapply(listTemp,function(x){x==listTemp[1]}))){
        if(length(listTemp)==3){
          if(all(sapply(listTemp,function(x){x=="S"}))){scalarFullCntry <- c(scalarFullCntry, scaleTable$Country[i])}
          if(all(sapply(listTemp,function(x){x=="M"}))){metricFullCntry <- c(metricFullCntry, scaleTable$Country[i])}
          if(all(sapply(listTemp,function(x){x=="C"}))){configFullCntry <- c(configFullCntry, scaleTable$Country[i])}
        } else {
          if(all(sapply(listTemp,function(x){x=="S"}))){scalarTwoCntry <- c(scalarTwoCntry, scaleTable$Country[i])}
          if(all(sapply(listTemp,function(x){x=="M"}))){metricTwoCntry <- c(metricTwoCntry, scaleTable$Country[i])}
          if(all(sapply(listTemp,function(x){x=="C"}))){configTwoCntry <- c(configTwoCntry, scaleTable$Country[i])}
        }
      }
    }
  }
  ## Get the "best" reference country
  if(!is.null(scalarFullCntry)){
    refCntry <- scalarFullCntry[1]
    scalar <- refCntry
    scalarFullCntry <- scalarFullCntry[-1]
    load1 <- unlist(strsplit(tolower(unlist(strsplit(refCntry, "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
    modelRef <- c(paste0("!Cross population in ",toupper(load1)," (Reference Country: Scalar - "), "! equal loadings, equal intercepts)")
    for(u in 1:length(usedScales)){
      for(i in 1:itemNum){
        load2 <- paste0(usedScales[u],load1,"Bload",as.character(i)," = ",usedScales[u],load1,"Aload",as.character(i),";")
        modelRef <- c(modelRef, load2)
      }
      modelRef <- c(modelRef, "")
      for(i in 1:itemNum){
        load2 <- paste0(usedScales[u],load1,"Bload",as.character(i)," = ",usedScales[u],load1,"Cload",as.character(i),";")
        modelRef <- c(modelRef, load2)
      }
      modelRef <- c(modelRef, "")
      for(i in 1:itemNum){
        load2 <- paste0(usedScales[u],load1,"Bint",as.character(i)," = ",usedScales[u],load1,"Aint",as.character(i),";")
        modelRef <- c(modelRef, load2)
      }
      modelRef <- c(modelRef, "")
      for(i in 1:itemNum){
        load2 <- paste0(usedScales[u],load1,"Bint",as.character(i)," = ",usedScales[u],load1,"Cint",as.character(i),";")
        modelRef <- c(modelRef, load2)
      }
      modelRef <- c(modelRef, "")
    }
  } else {
    if(!is.null(metricFullCntry)){
      refCntry <- metricFullCntry[1]
      metric <- refCntry
      metricFullCntry <- metricFullCntry[-1]
      load1 <- unlist(strsplit(tolower(unlist(strsplit(refCntry, "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
      modelRef <- c(paste0("!Cross population in ",load1," (Reference Country: Metric - equal loadings)"))
      for(u in 1:length(usedScales)){
        for(i in 1:itemNum){
          load2 <- paste0(usedScales[u],load1,"Bload",as.character(i)," = ",usedScales[u],load1,"Aload",as.character(i),";")
          modelRef <- c(modelRef, load2)
        }
        modelRef <- c(modelRef, "")
        for(i in 1:itemNum){
          load2 <- paste0(usedScales[u],load1,"Bload",as.character(i)," = ",usedScales[u],load1,"Cload",as.character(i),";")
          modelRef <- c(modelRef, load2)
        }
        modelRef <- c(modelRef, "")
      }
    }
  }
  scalarCntry <- c(scalarFullCntry, scalarTwoCntry)
  metricCntry <- c(metricFullCntry, metricTwoCntry)
  configCntry <- c(configFullCntry, configTwoCntry)
  ## Set other countries' equalities
  if(!is.null(scalarCntry)){
    for(i in 1:length(scalarCntry)){
      load1 <- unlist(strsplit(tolower(unlist(strsplit(scalarCntry[i], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
      modelRef <- c(modelRef, paste0("!Cross population in ",toupper(load1)," (Scalar:"), "! equal loadings, equal intercepts [no cross equalizations])")
      popNum <- c()
      cntryRow <- which(scaleTable$Country %in% scalarCntry[i])
      for(j in 2:(ncol(scaleTable)-1)){
        #if(j!=(refPop+1)){
          if(length(cntryRow)!=0){
            if(scaleTable[cntryRow,j]!="X"){
              popNum <- c(popNum, as.numeric(j))
            }
          }
        #}
      }
      if(!is.null(popNum) & length(popNum)!=1){
        if((refPop+1) %in% popNum){
          for(j in 1:length(popNum)){
            if(popNum[j]!=(refPop+1)){
              for(u in 1:length(usedScales)){
                if(scaleTable[(nrow(scaleTable)-1),popNum[j]]=="S"){
                  next
                } else {
                  if(scaleTable[(nrow(scaleTable)-1),popNum[j]]=="M"){
                    for(k in 1:itemNum){
                      load2 <- paste0(usedScales[u],load1,toupper(as.character(letters[refPop])),"int",as.character(k)," = ",usedScales[u],load1,toupper(as.character(letters[popNum[j]-1])),"int",as.character(k),";")
                      modelRef <- c(modelRef, load2)
                    }
                    modelRef <- c(modelRef, "")
                  } else {
                    if(scaleTable[(nrow(scaleTable)-1),popNum[j]]=="C"){
                      for(k in 1:itemNum){
                        load2 <- paste0(usedScales[u],load1,toupper(as.character(letters[refPop])),"load",as.character(k)," = ",usedScales[u],load1,toupper(as.character(letters[popNum[j]-1])),"load",as.character(k),";")
                        modelRef <- c(modelRef, load2)
                      }
                      modelRef <- c(modelRef, "")
                      for(k in 1:itemNum){
                        load2 <- paste0(usedScales[u],load1,toupper(as.character(letters[refPop])),"int",as.character(k)," = ",usedScales[u],load1,toupper(as.character(letters[popNum[j]-1])),"int",as.character(k),";")
                        modelRef <- c(modelRef, load2)
                      }
                      modelRef <- c(modelRef, "")
                    }
                  }
                }
              }
            }
          }
        } else {
          for(u in 1:length(usedScales)){
            if(scaleTable[cntryRow,5]=="C"){
              next
            } else {
              if(scaleTable[cntryRow,5]=="M"){
                for(k in 1:itemNum){
                  load2 <- paste0(usedScales[u],load1,toupper(as.character(letters[popNum[1]-1])),"load",as.character(k)," = ",usedScales[u],load1,toupper(as.character(letters[popNum[2]-1])),"load",as.character(k),";")
                  modelRef <- c(modelRef, load2)
                }
                modelRef <- c(modelRef, "")
              } else {
                if(scaleTable[cntryRow,5]=="S"){
                  for(k in 1:itemNum){
                    load2 <- paste0(usedScales[u],load1,toupper(as.character(letters[popNum[1]-1])),"load",as.character(k)," = ",usedScales[u],load1,toupper(as.character(letters[popNum[2]-1])),"load",as.character(k),";")
                    modelRef <- c(modelRef, load2)
                  }
                  modelRef <- c(modelRef, "")
                  for(k in 1:itemNum){
                    load2 <- paste0(usedScales[u],load1,toupper(as.character(letters[popNum[1]-1])),"int",as.character(k)," = ",usedScales[u],load1,toupper(as.character(letters[popNum[2]-1])),"int",as.character(k),";")
                    modelRef <- c(modelRef, load2)
                  }
                  modelRef <- c(modelRef, "")
                }
              }
            }
          }
        }
      }
    }
  }
  if(!is.null(metricCntry)){
    for(i in 1:length(metricCntry)){
      load1 <- unlist(strsplit(tolower(unlist(strsplit(metricCntry[i], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
      modelRef <- c(modelRef, paste0("!Cross population in ",toupper(load1)," (Metric: equal loadings [no cross equalizations])"))
      popNum <- c()
      cntryRow <- which(scaleTable$Country %in% metricCntry[i])
      for(j in 2:(ncol(scaleTable)-1)){
        if(j!=(refPop+1)){
          if(length(cntryRow)!=0){
            if(scaleTable[cntryRow,j]!="X"){
              popNum <- c(popNum, as.numeric(j))
            }
          }
        }
      }
      if(!is.null(popNum)){
        for(j in 1:length(popNum)){
          for(u in 1:length(usedScales)){
            if(scaleTable[(nrow(scaleTable)-1),popNum[j]]=="S"){
              next
            } else {
              if(scaleTable[(nrow(scaleTable)-1),popNum[j]]=="M"){
                next
              } else {
                if(scaleTable[(nrow(scaleTable)-1),popNum[j]]=="C"){
                  for(k in 1:itemNum){
                    load2 <- paste0(usedScales[u],load1,toupper(as.character(letters[refPop])),"load",as.character(k)," = ",usedScales[u],load1,toupper(as.character(letters[popNum[j]-1])),"load",as.character(k),";")
                    modelRef <- c(modelRef, load2)
                  }
                  modelRef <- c(modelRef, "")
                }
              }
            }
          }
        }
      }
    }
  }
  if(is.null(scalarFullCntry) & is.null(metricFullCntry) & is.null(scalarCntry) & is.null(metricCntry)){
    modelRef <- ""
  }
  model <- c(model1,"",modelPrmNm,modelConst1,modelFixVr,modelFixMn,modelConst2,modelRef)
  ### Create Scale directory
  dir.create(paste("Z:/TALIS/TALIS2018/3_MS/Analysis/02Implementation/2FinalAnalysis/Principal/4Validation/05MGCFACrossCntCrossPopMI/1FirstRound/separateMD", scale, sep="\\"), showWarnings=T, recursive=T)
  ### Set the working directory to the created Scale directory
  setwd(file.path("Z:/TALIS/TALIS2018/3_MS/Analysis/02Implementation/2FinalAnalysis/Principal/4Validation/05MGCFACrossCntCrossPopMI/1FirstRound/separateMD", scale))
  ### Use the function to create the Mplus input in the directory
  writeMGCFA_CCCP(scale, items, catgr, estmt, invar)
}
###################################################################################################

# 4: Run all the Mplus input files recursively to generate CFA output
library(MplusAutomation)
setwd(dirCFAs)
system.time({runModels(getwd(), recursive=T)})

# END





#####
