#!usr/bin/Rscript

# Syntax author:   ******************                            #
# email:           **************************                    #
# Affiliation:     ***********                                   #
# Syntax language: R (3.4.4)                                     #
# Project:         **********                                    #
# Analysis:        ***************************                   #
# Last update:     19-Oct-2018                                   #

# Title:   MG CFA: Cross Country, Cross Population
# Purpose: Create ***************** cross country, cross population MG CFA Mplus input & output for
#          invariance testing

###################################################################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
########################                 BEGIN USER INPUT                  ########################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

# Needed package: Remove "#"s to INSTALL as needed
# NOTE: If a non-default directory for installation is desired, new path must be set

# This package runs Mplus code in Mplus software (version 7.3)
#install.packages("MplusAutomation") # Code written with package version 0.7-2




sheetName <- "Sheet1"

# Which analysis are you working on?                  Remove the "#" of the appropriate stage.
#setAnalysisStage <- "1PreliminaryAnalysis"
setAnalysisStage <- "2FinalAnalysis"

# Are you interested in ******************** data?    Remove the "#" of the appropriate data.
setPopulationDta <- "*******"
#setPopulationDta <- "*********"

# Which round of analysis are you working on?         Remove the "#" of the appropriate round.
setAnalysisRound <- "1FirstRound"
#setAnalysisRound <- "2SecondRound"
#setAnalysisRound <- "3FinalScaleResults"

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
setDataFileFoldr <- "\\\\4Scores"
#-------------------------------------------------------------------------------------------------#
########################                OPTIONAL USER INPUT                ########################
#-------------------------------------------------------------------------------------------------#

# Directories: should only need changes if working with the data off the Z drive
# NOTE: For this code, please use "\\" between folders; R has special uses for "\" with certain
#       characters; if you use "\", you will likely get an error
## Directory where text files are stored
dirText <- paste0("Z:\\****************\\3_MS\\Analysis\\02Implementation\\",setAnalysisStage,"\\",
                  setPopulationDta,"\\2PRG\\2Validation")
## Directory where CFA scale files will be stored
dirCFAs <- paste0("Z:\\****************\\3_MS\\Analysis\\02Implementation\\",setAnalysisStage,"\\",
                  setPopulationDta,"\\4Validation\\05MGCFACrossCntCrossPopMI\\",setAnalysisRound)

# Names of needed files
## Names of the scale lists for each type of scale
nameScl <- paste0("Scales_",setPopulationDta,"_automated.Rda")
namCntr <- paste0("CountryList_modified.Rda")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
########################                  END USER INPUT                   ########################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
###################################################################################################

# 1: Read in the file which contains Scale information
setwd(dirText)
load(nameScl)
load(namCntr)

###################################################################################################

# Import test table
library(openxlsx)
dirTTab <- "Z:\\****************\\3_MS\\Analysis\\10_Work\\Hynek\\05CCCP\\TSATEN\\Scenarios"
testTable <- read.xlsx(paste0(dirTTab,"\\TestTable.xlsx"), sheet=sheetName , colNames=T, rowNames=T)
# Import test data
dirTDta <- "Z:\\****************\\3_MS\\Analysis\\02Implementation\\2FinalAnalysis\\Teacher\\1Data\\3Validation\\4Scores"
testData <- read.table(paste0(dirTDta,"\\TSATEN.dat"), header=F, sep="")
items <- c("TT3G53C","TT3G53E","TT3G53G","TT3G53J")
colnames(testData) <- c("IDCNTRY","IDSCHOOL","ISCED","BRRTZONE","BRRTREP","SENWGT0",items,"SUPERID","SUPERGROUP")
cntryData <- cntryModfy[,c(1:4,10)]
tempCntry <- testData[,c(1,3,12)]
tempCntry <- unique(tempCntry)
for(i in 1:nrow(cntryData)){
  for(j in 1:nrow(tempCntry)){
    if((cntryData$CountryCode[i] %in% tempCntry$IDCNTRY[j]) & (cntryData$ISCED[i] %in% tempCntry$ISCED[j])){
      cntryData$SuperGroupID[i] <- tempCntry$SUPERGROUP[j]
    }
  }
}
# Limit country data to countries in the table
for(i in nrow(cntryData):1){
  if(!(cntryData$Country[i] %in% rownames(testTable))){
    cntryData <- cntryData[-i,]
  }
}
# Compare to relevant populations
listPops <- c()
for(k in 1:(nrow(testTable)-1)){
  for(l in 1:(ncol(testTable)-1)){
    if(is.na(testTable[k,l])){
      listPops <- c(listPops, paste0(rownames(testTable)[k],"_",colnames(testTable)[l]))
    } else {
      if(testTable[k,l]!="X"){
        listPops <- c(listPops, paste0(rownames(testTable)[k],"_",colnames(testTable)[l]))
      }
    }
  }
}
listPops
#[1] "CT_ISCED1" "CT_ISCED2" "CT_ISCED3" "CrISCED2"      
#[5] "Cr_ISCED3"       "CR_ISCED2" "De_ISCED1"       "De_ISCED2"      
#[9] "De_ISCED3"   
cntryData <- cntryData[order(cntryData$spclStr),]
cntryData$spclStr
#[1] "CT_ISCED1" "CT_ISCED2" "CT_ISCED3" "Cr_ISCED2"      
#[5] "Cr_ISCED3"       "CR_ISCED2" "De_ISCED1"       "De_ISCED2"      
#[9] "De_ISCED3" 





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
      'USEOBSERVATIONS ARE (<use.obsv>); !delete for actual analysis',
      '<tabspace>GROUPING IS SUPERGROUPID (<spl.code>);',
      '',
      'USEVARIABLES ARE <itm.nams>;',
      '<categrcl>',
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
      '!MODEL = <inv.test>;',
      '',
      'MODEL:',
      '',# This is line 29 of the syntax, where we will place the model statement (see 2k below)
      '',
      'OUTPUT: SAMPSTAT RESIDUAL TECH1;',
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
  
  ### 2g: Replace <spl.code> with splcd
  mplusSyntax <- gsub("<spl.code>", splcd, mplusSyntax)

  ### 2h: Replace <categrcl> with catgr
  mplusSyntax <- gsub("<categrcl>", catgr, mplusSyntax)

  ### 2i: Replace <estimatr> with estmt
  mplusSyntax <- gsub("<estimatr>", estmt, mplusSyntax)
  
  ### 2j: Replace <inv.test> with invar
  mplusSyntax <- gsub("<inv.test>", invar, mplusSyntax)
  
  ### 2k: Insert the model statement
  mplusSyntax <- c(mplusSyntax[1:28],model,mplusSyntax[30:34])
  
  mplusSyntax <- gsub("<use.obsv>", useOb, mplusSyntax) # Delete<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  ### Ensure no lines are greater than 90 characters
  mplusSyntax <- paste(strwrap(mplusSyntax,90, simplify=T), collapse="\n")
  
  ### 2l: Replace <tabspace> with tabSp
  mplusSyntax <- gsub("<tabspace>", tabSp, mplusSyntax)

  writeLines(text=mplusSyntax, con=paste(scale,"_2levelMGCFA.inp", sep=""))
}
###################################################################################################

# 3: Write Mplus files to respective Scale folders
## Create string of unique country/population combinations
#cntryModfy <- cntryModfy[with(cntryModfy, order(CountryCode, ISCED)),]
listSpclID <- c()
for(i in 1:nrow(cntryData)){
  listSpclID <- c(listSpclID, paste0(cntryData$SuperGroupID[i],"=",cntryData$spclStr[i]))
}
#cntryModfy$MplusSpclStr
splcd      <- paste(listSpclID, collapse=", ")

useOb <- c()#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
for(i in 1:nrow(cntryData)){
  useOb <- c(useOb, paste0("SUPERGROUPID == ",cntryData$SuperGroupID[i]))
}
useOb <- paste(useOb, collapse=" OR ")#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
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

## Import improvment Excel sheet<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#setwd("Z:\\*****************\\3_MS\\Analysis\\02Implementation\\2FinalAnalysis\\Teacher\\2PRG\\2Validation\\01CFAPooled")
#library(openxlsx)
#PoolImpr <- read.xlsx("01CFAPooled_scale_improvement.xlsx", sheet="01CFAPooled", colNames=T)
#dirCFAs <- "X:/*****************/ImprovementTesting"#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## Create scale folders and replacement strings in the syntax for each respective scale
setwd(dirCFAs)

###################################################################################################
scale <- scalesInfo$Name[18]
items <- scalesInfo$Vars[18]
if(!is.na(scalesInfo$MCat[18])){
  catgr <- paste0(scalesInfo$MCat[18],scalesInfo$Vars[18],";")
} else {
  catgr <- ""
}
estmt <- scalesInfo$MEst[18]
invar <- scalesInfo$MInv[18]
if(is.na(scalesInfo$MDsc[18])){
  itemL <- unlist(strsplit(items, " "))
  itemL[1] <- paste0(itemL[1], "*")
  itemL <- paste(itemL, collapse=" ")
  modl1 <- paste0(scale, " BY ", itemL, ";")
  #modl2 <- paste0(scale, "@1;")
  #model <- c(modl1, "", modl2)
  model1 <- modl1#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
} else {
  ### MULTIDIMENSIONAL SCALES
  tmpModel <- c()
  tmpSubsc <- c()
  for(j in 1:nrow(scalesInfo)){
    if(scalesInfo$ptMD[j] %in% scale){
      tmpScale <- scalesInfo$Name[j]
      tmpItemL <- scalesInfo$Vars[j]
      tmpItemL <- unlist(strsplit(tmpItemL, " "))
      tmpItemL[1] <- paste0(tmpItemL[1], "*")
      tmpItemL <- paste(tmpItemL, collapse=" ")
      tmpModl1 <- paste0(tmpScale," BY ",tmpItemL,";")
      tmpModel <- c(tmpModel, tmpModl1)
      tmpSubsc <- c(tmpSubsc, tmpScale)
    }
  }
  for(j in 1:length(tmpSubsc)){
    tmpSubsc[j] <- paste0(tmpSubsc[j],"@1;") 
  }
  model1 <- c(tmpModel, "", tmpSubsc)
}

# Set parameter names
parmNames <- c()
listVarnc <- c()
listMeans <- c()
itemNum <- as.numeric(length(unlist(strsplit(items, " "))))
intercept <- paste0("[",items,"]")
scaleVari <- paste0(scale,"*")
scaleMean <- paste0("[",scale,"*]")
for(i in 1:length(listPops)){
  load1 <- unlist(strsplit(tolower(unlist(strsplit(listPops[i], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
  load2 <- toupper(letters[as.numeric(substr(listPops[i], nchar(listPops[i]), nchar(listPops[i])))])
  parmNames <- c(parmNames,
                 paste0("MODEL ",listPops[i],":"),
                 paste0(scale," BY ",itemL," (",load1,load2,"load1-",load1,load2,"load",as.character(itemNum),");"),
                 paste0(intercept," (",load1,load2,"int1-",load1,load2,"int",as.character(itemNum),");"),
                 paste0(scaleVari," (",load1,load2,"_",scale,"var);"),
                 paste0(scaleMean," (",load1,load2,"_",scale,"mean);"),
                 "")
  cntryVari <- paste0(load1,"B_",scale,"var")
  cntryMean <- paste0(load1,"B_",scale,"mean")
  listVarnc <- c(listVarnc, cntryVari)
  listMeans <- c(listMeans, cntryMean)
}
# Mean and variance constraints
modelConst1 <- c("MODEL CONSTRAINT:")
for(i in 1:(nrow(testTable)-1)){
  for(j in 1:(ncol(testTable)-1)){
    if(is.na(testTable[i,j])){
      load1 <- unlist(strsplit(tolower(unlist(strsplit(rownames(testTable)[i], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
      load2 <- toupper(letters[as.numeric(substr(colnames(testTable)[j], nchar(colnames(testTable)[j]), nchar(colnames(testTable)[j])))])
      modelConst1 <- c(modelConst1, paste0(load1,load2,"_",scale,"var = 1;"), paste0(load1,load2,"_",scale,"mean = 0;"))
    } else {
      if(testTable[i,j]=="m" | testTable[i,j]=="M"){
        load1 <- unlist(strsplit(tolower(unlist(strsplit(rownames(testTable)[i], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
        load2 <- toupper(letters[as.numeric(substr(colnames(testTable)[j], nchar(colnames(testTable)[j]), nchar(colnames(testTable)[j])))])
        modelConst1 <- c(modelConst1, paste0(load1,load2,"_",scale,"mean = 0;"))
      } else {
        if(testTable[i,j]=="s" | testTable[i,j]=="S"){
          next
        } else {
          if(testTable[i,j]=="x" | testTable[i,j]=="X"){
            next
          }
        }
      }
    }
  }
}
# Mean and variance constraints based on overall invariance achieved
for(i in 2){#1:(ncol(testTable)-1)){<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  if(testTable[nrow(testTable),i]=="s" | testTable[nrow(testTable),i]=="S"){
    next
  } else {
    if(testTable[nrow(testTable),i]=="m" | testTable[nrow(testTable),i]=="M"){
      for(j in 1:(nrow(testTable)-1)){
        if(testTable[j,i]!="x" | testTable[j,i]!="X"){
          load1 <- unlist(strsplit(tolower(unlist(strsplit(rownames(testTable)[j], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
          load2 <- toupper(letters[as.numeric(substr(colnames(testTable)[i], nchar(colnames(testTable)[i]), nchar(colnames(testTable)[i])))])
          modelConst1 <- c(modelConst1, paste0(load1,load2,"_",scale,"mean = 0;"))
        }
      }
    } else {
      if(testTable[nrow(testTable),i]=="c" | testTable[nrow(testTable),i]=="C"){
        for(j in 1:(nrow(testTable)-1)){
          if(testTable[j,i]!="x" | testTable[j,i]!="X"){
            load1 <- unlist(strsplit(tolower(unlist(strsplit(rownames(testTable)[j], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
            load2 <- toupper(letters[as.numeric(substr(colnames(testTable)[i], nchar(colnames(testTable)[i]), nchar(colnames(testTable)[i])))])
            modelConst1 <- c(modelConst1, paste0(load1,load2,"_",scale,"var = 1;"), paste0(load1,load2,"_",scale,"mean = 0;"))
          }
        }
      }
    }
  }
}



# Fix average mean and variance
if(testTable[nrow(testTable),2]=="m" | testTable[nrow(testTable),2]=="M" | testTable[nrow(testTable),2]=="s" | testTable[nrow(testTable),2]=="S"){
  modelFixVr <- paste0("0 = ",paste(unique(listVarnc), collapse=" + ")," - ",as.character(length(unique(listVarnc))-1),"; ! Fixed average variance to 1 (sum of variances to the number of countries)")
} else {
  modelFixVr <- ""
}
if(testTable[nrow(testTable),2]=="s" | testTable[nrow(testTable),2]=="S"){
  modelFixMn <- paste0("0 = ",paste(unique(listMeans), collapse=" + "),"; ! Fixed average latent mean to 0")
} else {
  modelFixMn <- ""
}

# Loading and intercepts constraints
modelConst2 <- c("MODEL CONSTRAINT:")
for(i in 1:(ncol(testTable)-1)){
  if(testTable[as.numeric((nrow(testTable))),i]=="s" | testTable[as.numeric((nrow(testTable))),i]=="S"){
    modelConst2 <- c(modelConst2, paste0("!Cross country, scalar ISCED",as.numeric(substr(colnames(testTable)[i], nchar(colnames(testTable)[i]), nchar(colnames(testTable)[i])))))
    for(j in 2:(nrow(testTable)-1)){
      if(testTable[j,i]!="x" & testTable[j,i]!="X"){
        for(k in 1:itemNum){
          load1 <- unlist(strsplit(tolower(unlist(strsplit(rownames(testTable)[1], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
          load2 <- toupper(letters[as.numeric(substr(colnames(testTable)[i], nchar(colnames(testTable)[i]), nchar(colnames(testTable)[i])))])
          fullLoad1 <- paste0(load1,load2,"load",as.character(k))
          load3 <- unlist(strsplit(tolower(unlist(strsplit(rownames(testTable)[j], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
          fullLoad2 <- paste0(load3,load2,"load",as.character(k))
          modelConst2 <- c(modelConst2, paste0(fullLoad1," = ",fullLoad2,";"))
        }
        modelConst2 <- c(modelConst2, "")
      } else {next}
    }
    for(j in 2:(nrow(testTable)-1)){
      if(testTable[j,i]!="x" & testTable[j,i]!="X"){
        for(k in 1:itemNum){
          load1 <- unlist(strsplit(tolower(unlist(strsplit(rownames(testTable)[1], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
          load2 <- toupper(letters[as.numeric(substr(colnames(testTable)[i], nchar(colnames(testTable)[i]), nchar(colnames(testTable)[i])))])
          fullLoad1 <- paste0(load1,load2,"int",as.character(k))
          load3 <- unlist(strsplit(tolower(unlist(strsplit(rownames(testTable)[j], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
          fullLoad2 <- paste0(load3,load2,"int",as.character(k))
          modelConst2 <- c(modelConst2, paste0(fullLoad1," = ",fullLoad2,";"))
        }
        modelConst2 <- c(modelConst2, "")
      } else {next}
    }
  } else {
    if(testTable[as.numeric((nrow(testTable))),i]=="m" | testTable[as.numeric((nrow(testTable))),i]=="M"){
      modelConst2 <- c(modelConst2, paste0("!Cross country, metric ISCED",as.numeric(substr(colnames(testTable)[i], nchar(colnames(testTable)[i]), nchar(colnames(testTable)[i])))))
      for(j in 2:(nrow(testTable)-1)){
        if(testTable[j,i]!="x" & testTable[j,i]!="X"){
          for(k in 1:itemNum){
            load1 <- unlist(strsplit(tolower(unlist(strsplit(rownames(testTable)[1], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
            load2 <- toupper(letters[as.numeric(substr(colnames(testTable)[i], nchar(colnames(testTable)[i]), nchar(colnames(testTable)[i])))])
            fullLoad1 <- paste0(load1,load2,"load",as.character(k))
            load3 <- unlist(strsplit(tolower(unlist(strsplit(rownames(testTable)[j], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
            fullLoad2 <- paste0(load3,load2,"load",as.character(k))
            modelConst2 <- c(modelConst2, paste0(fullLoad1," = ",fullLoad2,";"))
          }
          modelConst2 <- c(modelConst2, "")
        } else {next}
      }
    } else {
      if(testTable[as.numeric((nrow(testTable))),i]=="c" | testTable[as.numeric((nrow(testTable))),i]=="C"){
        next
      }
    }
  }
}
# Reference country
scrCntry <- c()
refCntry <- c()
for(i in 1:(nrow(testTable)-1)){
  if(all(testTable[i,]!="c" & testTable[i,]!="C" & testTable[i,]!="m" & testTable[i,]!="M" & !is.na(testTable[i,]))){
    scrCntry <- c(scrCntry, rownames(testTable)[i])
  }
  if(all(testTable[i,]=="s" | testTable[i,]=="S")){
    refCntry <- c(refCntry, rownames(testTable)[i])
  }
}
scrCntry <- scrCntry[-which(scrCntry %in% refCntry)]

# Set reference country
load1 <- unlist(strsplit(tolower(unlist(strsplit(refCntry[1], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
modelRef <- c(paste0("!Cross population in ",load1))
for(i in 1:itemNum){
  load2 <- paste0(load1,"Bload",as.character(i)," = ",load1,"Aload",as.character(i),";")
  modelRef <- c(modelRef, load2)
}
modelRef <- c(modelRef, "")
for(i in 1:itemNum){
  load2 <- paste0(load1,"Bload",as.character(i)," = ",load1,"Cload",as.character(i),";")
  modelRef <- c(modelRef, load2)
}
modelRef <- c(modelRef, "")
for(i in 1:itemNum){
  load2 <- paste0(load1,"Bint",as.character(i)," = ",load1,"Aint",as.character(i),";")
  modelRef <- c(modelRef, load2)
}
modelRef <- c(modelRef, "")
for(i in 1:itemNum){
  load2 <- paste0(load1,"Bint",as.character(i)," = ",load1,"Cint",as.character(i),";")
  modelRef <- c(modelRef, load2)
}
modelRef <- c(modelRef, "")
# Set other scalar countries
for(i in 1:length(scrCntry)){
  load1 <- unlist(strsplit(tolower(unlist(strsplit(scrCntry[i], "(?<=[a-z])(?=[A-Z])", perl=T))[1]), "_"))[1]
  modelRef <- c(modelRef, paste0("!Cross population in ",load1))
  popNum <- c()
  cntryRow <- which(rownames(testTable) %in% scrCntry[i])
  for(j in 1:(ncol(testTable)-1)){
    if(j!=2){
      if(testTable[cntryRow,(as.numeric(substr(colnames(testTable)[j], nchar(colnames(testTable)[j]), nchar(colnames(testTable)[j]))))]!="X"){#####<<<<<<<<<<<<<<<<<<<<<<
        popNum <- c(popNum, as.numeric(j))
      }
    }
  }
  for(j in 1:length(popNum)){
    for(k in 1:itemNum){
      load2 <- paste0(load1,"Bint",as.character(k)," = ",load1,toupper(as.character(letters[popNum[j]])),"int",as.character(k),";")
      modelRef <- c(modelRef, load2)
    }
    modelRef <- c(modelRef, "")
  }
}

model <- c(model1,"",parmNames,modelConst1,"",modelFixVr,modelFixMn,"",modelConst2,"",modelRef)

dirCFAs <- "X:\\justin.wild\\TALIS\\TALIS_Numbered\\TALIS_38NewCCCPmgcfa"
#dir.create(paste(dirCFAs, scale, sep="\\"), showWarnings=T, recursive=T)
setwd(file.path(dirCFAs, scale))
writeMGCFA_CCCP(scale, items, catgr, estmt, invar)


##########























###################################################################################################

for (i in 1:nrow(scalesInfo)){
  ### Create scale name
  scale <- scalesInfo$Name[i]
  ### Create item list
  items <- scalesInfo$Vars[i]
  ### Create "Categorical" Mplus statement
  if(!is.na(scalesInfo$MCat[i])){
    catgr <- paste0(scalesInfo$MCat[i],scalesInfo$Vars[i],";")
  } else {
    catgr <- ""
  }
  ### Create estimator type
  estmt <- scalesInfo$MEst[i]
  ### Create list of invariance testing statements
  invar <- scalesInfo$MInv[i]
  ### Create model statement
  ### UNIDIMENSIONAL SCALES
  if(is.na(scalesInfo$MDsc[i])){
    #### Create a list of the scale variables
    itemL <- unlist(strsplit(items, " "))
    #### Add "*" after the first element of the scale variables (freeing the parameter)
    itemL[1] <- paste0(itemL[1], "*")
    #### Recombine the list of scale variables into a string
    itemL <- paste(itemL, collapse=" ")
    #### Create "BY" statement for the scale
    modl1 <- paste0(scale, " BY ", itemL, ";")
    #### Create "scale variance" statement
    modl2 <- paste0(scale, "@1;")
    #### Create full model statement
    model1 <- c(modl1, "", modl2)
  } else {
    ### MULTIDIMENSIONAL SCALES
    #### Create temporary lists to store subscale information
    tmpModel <- c()
    tmpSubsc <- c()
    #### Use a loop to generate the model statement (list of lines)
    for(j in 1:nrow(scalesInfo)){
      #### If the MD scale name is in the "ptMD" column...
      if(scalesInfo$ptMD[j] %in% scale){
        #### Get the sub-scale name
        tmpScale <- scalesInfo$Name[j]
        #### Get the sub-scale variables
        tmpItemL <- scalesInfo$Vars[j]
        #### Create a list of the sub-scale variables
        tmpItemL <- unlist(strsplit(tmpItemL, " "))
        #### Add "*" after the first element of the sub-scale variables (freeing the parameter)
        tmpItemL[1] <- paste0(tmpItemL[1], "*")
        #### Recombine the list of sub-scale variables into a string
        tmpItemL <- paste(tmpItemL, collapse=" ")
        #### Create the "BY" statement for the sub-scale
        tmpModl1 <- paste0(tmpScale," BY ",tmpItemL,";")
        #### Add this string as a single list element to a temporary list
        tmpModel <- c(tmpModel, tmpModl1)
        #### Add the sub-scale name to another temporary list
        tmpSubsc <- c(tmpSubsc, tmpScale)
      }
    }
    #### Use a loop to set each sub-scale's variance to 1
    for(j in 1:length(tmpSubsc)){
      tmpSubsc[j] <- paste0(tmpSubsc[j],"@1;") 
    }
    #### Create full model statement
    model1 <- c(tmpModel, "", tmpSubsc)
  }
  #### Add improvement(s)<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ### Pooled CFA improvements
  tmpImprv <- c()
  for(j in 1:nrow(PoolImpr)){
    if(scale %in% PoolImpr$Scale[j]){
      tmpImprv1 <- PoolImpr$PooledImprovement[j]
      tmpImprv1 <- unlist(strsplit(tmpImprv1, "\n"))
      tmpImprv2 <- PoolImpr$CCCPImprovement[j]
      tmpImprv2 <- unlist(strsplit(tmpImprv2, "\n"))
    } else {
      tmpImprv1 <- NA
      tmpImprv2 <- NA
    }
    tmpImprv <- c(tmpImprv, tmpImprv1, tmpImprv2)
    tmpImprv <- tmpImprv[!is.na(tmpImprv)]
  }
  model1 <- c(model1, tmpImprv)
  ### Create Scale directory
  dir.create(paste(dirCFAs, scale, sep="\\"), showWarnings=T, recursive=T)#Replace with "dirCFAs"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ### Set the working directory to the created Scale directory
  setwd(file.path(dirCFAs, scale))#Replace with "dirCFAs"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
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
