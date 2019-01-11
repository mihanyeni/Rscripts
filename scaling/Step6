#!usr/bin/Rscript

# Syntax author:   Justin Wild, Ph.D.                            #
# email:           **************************                    #
# Affiliation:     ***********                                   #
# Syntax language: R (3.4.4)                                     #
# Project:         **********                                    #
# Analysis:        Testing for Main Study (MS)                   #
# Last update:     16-Oct-2018                                   #

# Title:   MG CFA: Cross Cycle
# Purpose: Create ************************ cross cycle MG CFA Mplus input & output with fixed
#          factor loadings and intercepts from the 2013 final MG CFA data (by country and population)

###################################################################################################

# This syntax is the located in the directories:
#   Z:\*****************_MS\Analysis\02Implementation\1PreliminaryAnalysis\Teacher\2PRG\2Validation\06MGCFACrossCycleMI
#   Z:\****************3_MS\Analysis\02Implementation\1PreliminaryAnalysis\Principal\2PRG\2Validation\06MGCFACrossCycleMI
# and
#   Z:\***************\3_MS\Analysis\02Implementation\2FinalAnalysis\Teacher\2PRG\2Validation\06MGCFACrossCycleMI
#   Z:\***************\3_MS\Analysis\02Implementation\2FinalAnalysis\Principal\2PRG\2Validation\06MGCFACrossCycleMI

# The syntax relies on data sets created by the R script: "Prelim_Create_Text_Files.R"
# The R script and data sets can be found at:

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

# Which round of analysis are you working on?         Remove the "#" of the appropriate round.
#setAnalysisRound <- "1FirstRound"
#setAnalysisRound <- "2SecondRound"
setAnalysisRound <- "3FinalScaleResults/*********"

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
## For the cross cycle subsetted data, the data is in a new location
dirData <- paste0("Z:\\****************\\3_MS\\Analysis\\02Implementation\\",setAnalysisStage,#<<<<<<<<<<<<<<<<<
                  "\\",setPopulationDta,"\\2PRG\\2Validation\\06MGCFACrossCycleMI")

#-------------------------------------------------------------------------------------------------#
########################                OPTIONAL USER INPUT                ########################
#-------------------------------------------------------------------------------------------------#

# Directories: should only need changes if working with the data off the Z drive
# NOTE: For this code, please use "\\" between folders; R has special uses for "\" with certain
#       characters; if you use "\", you will likely get an error
## Directory where text files are stored


# Names of needed files
## Names of the scale lists for each type of scale
nameScl <- paste0("Scales_",setPopulationDta,"_automated.Rda")
namCntr <- "CountryList_modified.Rda"
nameCNm <- "CrossCycleCountryList_automated.Rda"
nameCmp <- paste0("CompareScales13to18_",setPopulationDta,".Rda")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
########################                  END USER INPUT                   ########################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
###################################################################################################

# 1: Read in the files which contain Scale and Country information, and create needed lists
setwd(dirText)
load(nameScl)
load(namCntr)
setwd(dirMtch)
load(nameCmp)
load(nameCNm)
#cntryNames <- sort(cntryNames)<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Get list of scales names
# if(setPopulationDta== "Teacher"){
#   listScales <- c("TECAO","TJSO","TPERDC","TPRODIV","TPROPD","TSEO","TSTAKEV","TTSTUD")
# }
# if(setPopulationDta== "Principal"){
#   listScales <- c("PCDELV1","PDLEAD","PINLEAD","PJSO")
# }
###################################################################################################

# 2: Create function to write Mplus code for scales
## Create Mplus syntax loop
writeTALISpisaCFA <- function(scale, items, catgr, estmt){
  mplusSyntax  <-
    c('TITLE:',
      '<tabSpace>Cross-Cycle MG CFA for <scl.name>.',
      '',
      'DATA: FILE IS',
      '<tabSpace>"Z:\\****************\\3_MS\\Analysis\\02Implementation\\',
      '<tabSpace><data.pth>\\',
      '<tabSpace><scl.name>.dat";',
      '',
      'VARIABLE: NAMES ARE',
      '<tabSpace>IDCNTRY <id.varbl> ISCED IDSTRATE <stratify> <clusters> SENWGT0 <itm.nams>;',
      '<tabSpace>GROUPING IS IDCNTRY (<cntry.cd>);',
      '',
      'USEVARIABLES ARE <itm.nams>;',
      '<categrcl>',
      '',
      'USEOBSERVATIONS IS ISCED EQ 2;',
      '',
      'MISSING ARE <itm.nams> (9);',
      '',
      'WEIGHT IS SENWGT0;',
      'STRATIFICATION IS <stratify>;',
      'CLUSTER IS <clusters>;',
      '',
      'ANALYSIS:',
      'TYPE IS COMPLEX;',
      'ESTIMATOR = <estimatr>;',
      'MODEL = <inv.test>;',
      '',
      'MODEL:',
      '',# This is line 30 of the syntax, where we will place the model statement (see 2k below)
      '',
      'OUTPUT: SAMPSTAT STANDARDIZED RESIDUAL TECH1 FSCOEFFICIENT FSDETERMINACY <mod.inds>;',
      '',
      'SAVEDATA: FILE IS <scl.name>_FactorScore.dat;',
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
  
  ### 2g: Replace <cntry.cd> with ccode
  mplusSyntax <- gsub("<cntry.cd>", ccode, mplusSyntax)
  
  ### 2h: Replace <categrcl> with catgr
  mplusSyntax <- gsub("<categrcl>", catgr, mplusSyntax)
  
  ### 2i: Replace <estimatr> with estmt
  mplusSyntax <- gsub("<estimatr>", estmt, mplusSyntax)
  
  ### 2j: Replace <inv.test> with invar
  mplusSyntax <- gsub("<inv.test>", invar, mplusSyntax)
  
  ### 2k: Insert the model statement
  mplusSyntax <- c(mplusSyntax[1:29],model,mplusSyntax[30:35])
  
  ### 2m: Replace <mod.inds> with modif
  mplusSyntax <- gsub("<mod.inds>", modif, mplusSyntax)
  
  ### Ensure no lines are greater than 90 characters
  mplusSyntax <- paste(strwrap(mplusSyntax, 90, simplify=T), collapse="\n")
  
  ### 2n: Replace <tabSpace> with tabSp
  mplusSyntax <- gsub("<tabSpace>", tabSp, mplusSyntax)
  
  writeLines(text=mplusSyntax, con=paste(scale,"_CrossCycle13to18.inp", sep=""))
}
###################################################################################################

# 3: Write Mplus files to respective scale folders
## Set fixed replacement variables
### Set correct path folders for the analysis and a spacing variable for "nice looking" Mplus code
dPath <- paste0(setAnalysisStage,"\\\\",setPopulationDta,"\\\\2PRG\\\\2Validation\\\\06MGCFACrossCycleMI")
tabSp <- "     "
### Set ID, stratification, and cluster variables
if(setPopulationDta== "*******"){
  idvar <- "IDTEACH"
  strat <- "BRRTZONE"
  clust <- "BRRTREP"
}
if(setPopulationDta== "*********"){
  idvar <- "IDSCHOOL"
  strat <- "BRRSZONE"
  clust <- "BRRSREP"
}
### Set the country code grouping statement
#### Countries in the 2013 cycle (p. 79 of technical report)
cntryLst13 <- c()
cntryGroup <- c()
cntryList  <- c()
for(i in 1:length(cntryLst13)){
  for(j in 1:nrow(cntryModfy)){
    if(cntryLst13[i] %in% cntryModfy$Country[j]){
      cntryGroup <- c(cntryGroup, paste0(cntryModfy$CountryCode[j],"=",cntryModfy$Country[j]))
      cntryList  <- c(cntryList, cntryLst13[i])
    } else {next}
  }
}
cntryGroup <- toupper(unique(cntryGroup))
cntryList  <- toupper(unique(cntryList))
ccode <- paste(cntryGroup, collapse=" ")
### Set invariance groups
Metric <- c("TECAO","TPERDC","TPRODIV","TSTAKEV","PDLEAD","PJSO")
Scalar <- c("TJSO","TPROPD","TSEO","TTSTUD","PCDELV1","PINLEAD")

## Create scale folders and replacement strings in the syntax for each respective scale
## Get reduced scale data set
scalesInfo18 <- scalesInfo[which(scalesInfo$Name %in% listScales),]
library(MplusAutomation)

setwd(dirCFAs)
 for(i in 1:nrow(scalesInfo18)){
  ### Create scale name
  scale <- scalesInfo18$Name[i]
  ### Create item list
  items <- scalesInfo18$Vars[i]
  ### Create "Categorical" Mplus statement
  if(!is.na(scalesInfo18$MCat[i])){
    catgr <- paste0(scalesInfo18$MCat[i],scalesInfo18$Vars[i],";")
  } else {
    catgr <- ""
  }
  ### Create estimator type
  estmt <- scalesInfo18$MEst[i]
  ### Create invariance statement
  if(scale %in% Metric){
    invar <- "METRIC"
  }
  if(scale %in% Scalar){
    invar <- "SCALAR"
  }
  ### Create modification indices output request
  modif <- scalesInfo18$ModI[i]
  ### Create general model statement
  ### METRIC MODELS
  ### 2.5: Create country specific model statement
  dirTemp <- paste0(dirMtch,"\\",scale)
  setwd(dirTemp)
  tempData <- readModels(target=getwd(), recursive=T, what="parameters")
  tempDSet <- tempData$parameters$unstandardized
  if(scale %in% Scalar){
    model <- c()
    ## Get parameters from any country
    cntry <- cntryList[1]
    ## Load country information into temporary data set
    tempDataC <- as.data.frame(tempDSet[which(tempDSet$Group %in% cntry),])
    if(nrow(tempDataC)==0){next}
    ## Create fixed loadings
    tempScale <- c()
    for(k in 1:nrow(tempDataC)){
      if(grepl("BY", tempDataC$paramHeader[k])){
        tempScale <- c(tempScale, tempDataC$paramHeader[k])
      }
    }
    tempScale <- unique(tempScale)
    for(k in 1:length(tempScale)){
      tempScale[k] <- unlist(strsplit(tempScale[k], "[.]"))[1]
    }
    tempLoads <- c()
    for(k in 1:length(tempScale)){
      tempLoads1 <- c()
      for(l in 1:nrow(scalesParm)){
        if(tempScale[k] %in% scalesParm$UDscale13[l]){
          tempItem18 <- scalesParm$Vars18[l]
          tempItem13 <- scalesParm$Vars13[l]
          for(m in 1:nrow(tempDataC)){
            if((grepl("BY", tempDataC$paramHeader[m])) & (tempItem13 %in% tempDataC$param[m])){
              tempLoads1 <- c(tempLoads1, paste0(tempItem18,"@",tempDataC$est[m]))
            } else {
              next
            }
          }
        } else {
          next
      }
      }
      scaleTemp <- unique(scalesParm$UDscale18[which(scalesParm$UDscale13 %in% tempScale[k])])
      scaleTemp <- paste0(scaleTemp," BY ")
      tempLoads <- c(tempLoads, paste0(scaleTemp, paste(tempLoads1, collapse=" "), ";"))
    }
    ## Create fixed intercepts
    tempIntcp <- c()
    for(k in 1:length(tempScale)){
      tempIntcp1 <- c()
      for(l in 1:nrow(scalesParm)){
        if(tempScale[k] %in% scalesParm$UDscale13[l]){
          tempItem18 <- scalesParm$Vars18[l]
          tempItem13 <- scalesParm$Vars13[l]
          for(m in 1:nrow(tempDataC)){
            if((grepl("Intercepts", tempDataC$paramHeader[m])) & (tempItem13 %in% tempDataC$param[m])){
              tempIntcp1 <- c(tempIntcp1, paste0(tempItem18,"@",tempDataC$est[m]))
            } else {
              next
            }
          }
        } else {
          next
        }
      }
      tempIntcp <- c(tempIntcp, paste0("[", paste(tempIntcp1, collapse=" "), "];"))
    }
    ## Create correlation statements
    tempWiths <- c()
    for(k in 1:nrow(tempDataC)){
      tempWiths1 <- c()
      if(grepl("WITH", tempDataC$paramHeader[k])){
        for(l in 1:nrow(scalesParm)){
          if(tempDataC$param[k] %in% scalesParm$Vars13[l]){
            tempWiths1 <- unlist(strsplit(tempDataC$paramHeader[k], "[.]"))
            tempWiths1 <- c(tempWiths1, tempDataC$param[k])
            for(m in 1:length(tempWiths1)){
              for(n in 1:nrow(scalesParm)){
                if(tempWiths1[m] %in% scalesParm$Vars13[n]){
                  tempWiths1[m] <- scalesParm$Vars18[n]
                } else {next}
              }
            }
            tempWiths1 <- paste0(paste(tempWiths1, collapse=" "), ";")
          } else {next}
        }
      } else {next}
      tempWiths <- c(tempWiths, tempWiths1)
    }
    ## Create statements to free the latent variable means and variances
    tempMeans <- c()
    tempMeans1 <- c()
    tempMeans2 <- c()
    for(k in 1:length(tempScale)){
      scaleTemp <- unique(scalesParm$UDscale18[which(scalesParm$UDscale13 %in% tempScale[k])])
      tempMeans3 <- paste0(scaleTemp,"*;")
      tempMeans1 <- c(tempMeans1,tempMeans3)
      tempMeans4 <- paste0("[",scaleTemp,"*];")
      tempMeans2 <- c(tempMeans2,tempMeans4)
    }
    tempMeans <- c(tempMeans1,tempMeans2)
    ## Release latent mean of the first group
    tempCntry <- c()
    ## Get country
    cntry <- cntryList[1]
    ## Load country information into temporary data set
    tempDataC <- as.data.frame(tempDSet[which(tempDSet$Group %in% cntry),])
    ## Create model statement intro
    tempCntry1 <- paste0("MODEL ",cntry, ":")
    ## Release latent mean
    tempCntry <- c(tempCntry1, tempMeans2)
    ## Create full statement
    model <- c("",tempLoads,"",tempIntcp,"",tempWiths,"",tempMeans,"",tempCntry)
  }
  if(scale %in% Metric){
    # Create model statement
    model <- c()
    ## Get parameters from any country
    cntry <- cntryList[1]
    ## Load country information into temporary data set
    tempDataC <- as.data.frame(tempDSet[which(tempDSet$Group %in% cntry),])
    if(nrow(tempDataC)==0){next}
    ## Create fixed loadings
    tempScale <- c()
    for(k in 1:nrow(tempDataC)){
      if(grepl("BY", tempDataC$paramHeader[k])){
        tempScale <- c(tempScale, tempDataC$paramHeader[k])
      }
    }
    tempScale <- unique(tempScale)
    for(k in 1:length(tempScale)){
      tempScale[k] <- unlist(strsplit(tempScale[k], "[.]"))[1]
    }
    tempLoads <- c()
    for(k in 1:length(tempScale)){
      tempLoads1 <- c()
      for(l in 1:nrow(scalesParm)){
        if(tempScale[k] %in% scalesParm$UDscale13[l]){
          tempItem18 <- scalesParm$Vars18[l]
          tempItem13 <- scalesParm$Vars13[l]
          for(m in 1:nrow(tempDataC)){
            if((grepl("BY", tempDataC$paramHeader[m])) & (tempItem13 %in% tempDataC$param[m])){
              tempLoads1 <- c(tempLoads1, paste0(tempItem18,"@",tempDataC$est[m]))
            } else {
              next
            }
          }
        } else {
          next
        }
      }
      scaleTemp <- unique(scalesParm$UDscale18[which(scalesParm$UDscale13 %in% tempScale[k])])
      scaleTemp <- paste0(scaleTemp," BY ")
      tempLoads <- c(tempLoads, paste0(scaleTemp, paste(tempLoads1, collapse=" "), ";"))
    }
    ## Create correlation statements
    tempWiths <- c()
    for(k in 1:nrow(tempDataC)){
      tempWiths1 <- c()
      if(grepl("WITH", tempDataC$paramHeader[k])){
        for(l in 1:nrow(scalesParm)){
          if(tempDataC$param[k] %in% scalesParm$Vars13[l]){
            tempWiths1 <- unlist(strsplit(tempDataC$paramHeader[k], "[.]"))
            tempWiths1 <- c(tempWiths1, tempDataC$param[k])
            for(m in 1:length(tempWiths1)){
              for(n in 1:nrow(scalesParm)){
                if(tempWiths1[m] %in% scalesParm$Vars13[n]){
                  tempWiths1[m] <- scalesParm$Vars18[n]
                } else {next}
              }
            }
            tempWiths1 <- paste0(paste(tempWiths1, collapse=" "), ";")
          } else {next}
        }
      } else {next}
      tempWiths <- c(tempWiths, tempWiths1)
    }
    ## Create statements to free the latent variable means and variances
    tempMeans <- c()
    tempMeans1 <- c()
    tempMeans2 <- c()
    for(k in 1:length(tempScale)){
      scaleTemp <- unique(scalesParm$UDscale18[which(scalesParm$UDscale13 %in% tempScale[k])])
      tempMeans3 <- paste0(scaleTemp,"*;")
      tempMeans1 <- c(tempMeans1,tempMeans3)
    }
    tempMeans <- c(tempMeans1)
    ## Create country statements
    tempCntry <- c()
    for(j in 1:length(cntryList)){
      ## Get country
      cntry <- cntryList[j]
      ## Load country information into temporary data set
      tempDataC <- as.data.frame(tempDSet[which(tempDSet$Group %in% cntry),])
      if(nrow(tempDataC)==0){next}
      ## Create model statement intro
      tempCntry1 <- paste0("MODEL ",cntry, ":")
      ## Create fixed intercepts
      tempIntcp <- c()
      for(k in 1:length(tempScale)){
        tempIntcp1 <- c()
        for(l in 1:nrow(scalesParm)){
          if(tempScale[k] %in% scalesParm$UDscale13[l]){
            tempItem18 <- scalesParm$Vars18[l]
            tempItem13 <- scalesParm$Vars13[l]
            for(m in 1:nrow(tempDataC)){
              if((grepl("Intercepts", tempDataC$paramHeader[m])) & (tempItem13 %in% tempDataC$param[m])){
                tempIntcp1 <- c(tempIntcp1, paste0(tempItem18,"@",tempDataC$est[m]))
              } else {
                next
              }
            }
          } else {
            next
          }
        }
        tempIntcp <- c(tempIntcp, paste0("[", paste(tempIntcp1, collapse=" "), "];"))
      }
      tempMeans2 <- c()
      for(k in 1:length(tempScale)){
        scaleTemp <- unique(scalesParm$UDscale18[which(scalesParm$UDscale13 %in% tempScale[k])])
        tempMeans4 <- paste0("[",scaleTemp,"*];")
        tempMeans2 <- c(tempMeans2,tempMeans4)
      }
      tempCntry <- c(tempCntry,tempCntry1,tempIntcp,tempMeans2,"")
    }
    ## Create full statement
    model <- c("",tempLoads,"",tempWiths,"",tempMeans,"",tempCntry)
  }
  ### Create Scale directory
  #dir.create(paste(dirCFAs, scale, sep="\\"), showWarnings=T, recursive=T)
  dir.create(paste("X:\\*************************_34CrossCycleBugTesting", scale, sep="\\"), showWarnings=T, recursive=T)
  ### Set the working directory to the created Scale directory
  #setwd(file.path(dirCFAs, scale))
  setwd(file.path("X:\\*************************_34CrossCycleBugTesting", scale))
  ### Use the function to create the Mplus input in the directory
  writeTALISpisaCFA(scale, items, catgr, estmt)
}
 ###################################################################################################

# 6: Run all the Mplus input files recursively to generate CFA output
setwd(dirCFAs)
system.time({runModels(getwd(), recursive=T)})

# End





#####
