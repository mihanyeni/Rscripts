#!usr/bin/env Rscript

# Syntax author:   Justin Wild, Ph.D.         #
# email:           justin.wild@iea-hamburg.de #
# Affiliation:     IEA Hamburg                #
# Syntax language: R (3.5.1)                  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Check
# Project:         CFA Scaling                #
# Last update:     15-Nov-2019                # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Check

####################################################################################################

# Purpose: Create pooled CFA script for MPlus
# Note:    This script relies on a study/project "setup" file and data set preparation

# Package dependencies: MplusAutomation 0.7.3 #
#                       openxlsx (4.1.2)      #
#                       (data.table 1.12.0)   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Check

####################################################################################################

# Drop: dirSetup, selectionListScale # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Clean

run.CFA <- function(nameStudy,dirRoot,dirAnalysisPhase,dirGroup,selectionGroup,dirAnalysis,selectionAnalysis,dirRound,selectionIDVars,selectionGroupCodeVar,selectionStrat,selectionCluster,selectionWeight,selectionGlobalDropCountry,selectionGlobalDropLevel,selectionGlobalDropPop,selectionAdjudicateVar,selectionAdjudicateCode,selectionCountryCodeVar,selectionLevelCodeVar,selectionTargetLevelCode,selectionPopCodeVar,selectionImprove,selectionFS,selectionMplusAuto,selectionSuffix,selectionListScalePI,selectionScale,selectionScaleDropItems,selectionScaleDropSpecifics,selectionScaleDropCountry,selectionScaleDropLevel,selectionScaleDropPop){ # <<<<<<<<<<<<<< Clean
  
  # 1: Prepare names for directory paths, load datasets and files, & prepare drop selections <<<<<<< See below section # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Prepare drop selection, line 41

  # Directory paths
  ## Full path for Mplus input files
  dirCFAs <- paste0(dirRoot,"/",dirAnalysisPhase,"4Validation/",dirGroup,dirAnalysis,dirRound)
  dirLogs <- paste0(dirRoot,"/",dirAnalysisPhase,"4Validation/",dirGroup,dirAnalysis,dirRound,"Logs")

  # Load datasets and files
  ## Scale dataset named "dfScale"
  load(paste0(dirRoot,"/4Rscripts/SupportFiles/dfScale_",gsub(" ","",selectionGroup),".Rda")) # <<<< Change to XL
  ## Country dataset named "dfCntry"
  load(paste0(dirRoot,"/4Rscripts/SupportFiles/dfCntry_",gsub(" ","",selectionGroup),".Rda")) # <<<< Change to XL
  ## Model improvements file
  if(selectionImprove=="Yes"){
    dfImprv <- read.xlsx(paste0(dirRoot,"/3Documentation/modelImprovements.xlsx"), sheet=selectionGroup, rowNames=F, colNames=T)
  }
  # Prepare drop selections # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Should we do this in the SOURCE_LOGS script?...simply print them
  selectionGlobalDropCountry  <- unlist(strsplit(selectionGlobalDropCountry,"; "))
  selectionGlobalDropLevel    <- unlist(strsplit(selectionGlobalDropLevel,"; "))
  selectionGlobalDropPop      <- unlist(strsplit(selectionGlobalDropPop,"; "))
  selectionScaleDropCountry   <- unlist(strsplit(selectionScaleDropCountry,"; "))
  selectionScaleDropLevel     <- unlist(strsplit(selectionScaleDropLevel,"; "))
  selectionScaleDropPop       <- unlist(strsplit(selectionScaleDropPop, "; "))
  ##################################################################################################
  
  # 2: Manipulate data frames to create additional variables, sub-data frames, etc.
  # Country data frame
  dfCntryCleaned <- dfCntry
  # If some countries are excluded delete them from the country file
  if(paste(selectionGlobalDropCountry,collapse=" ")!="None"){
    for(row in (nrow(dfCntryCleaned)-1):1){
      for(cnt in 1:length(selectionGlobalDropCountry)){
        if(dfCntryCleaned$CountryAlpha3[row]==selectionGlobalDropCountry[cnt]){
          dfCntryCleaned <- dfCntryCleaned[-row,]
        }
        if(dfCntryCleaned$CountryAlpha3[nrow(dfCntryCleaned)]==selectionGlobalDropCountry[cnt]){
          dfCntryCleaned <- dfCntryCleaned[-nrow(dfCntryCleaned),]
        }
      }
    }
  }
  if(paste(selectionScaleDropCountry,collapse=" ")!="None" & nrow(dfCntryCleaned)!=0){
    if(nrow(dfCntryCleaned)==1){
      for(cnt in 1:length(selectionScaleDropCountry)){
        if(dfCntryCleaned$CountryAlpha3[1]==selectionScaleDropCountry[cnt]){
          dfCntryCleaned <- dfCntryCleaned[-1,]
        }
      }
    } else {
      for(row in (nrow(dfCntryCleaned)-1):1){
        for(cnt in 1:length(selectionScaleDropCountry)){
          if(dfCntryCleaned$CountryAlpha3[row]==selectionScaleDropCountry[cnt]){
            dfCntryCleaned <- dfCntryCleaned[-row,]
          }
          if(dfCntryCleaned$CountryAlpha3[nrow(dfCntryCleaned)]==selectionScaleDropCountry[cnt]){
            dfCntryCleaned <- dfCntryCleaned[-nrow(dfCntryCleaned),]
          }
        }
      }
    }
  }
  # If some levels are excluded delete them from the country file
  if(paste(selectionGlobalDropLevel,collapse=" ")!="None" & nrow(dfCntryCleaned)!=0){
    if(nrow(dfCntryCleaned)==1){
      for(lvl in 1:length(selectionGlobalDropLevel)){
        if(dfCntryCleaned$Level[1]==selectionGlobalDropLevel[lvl]){
          dfCntryCleaned <- dfCntryCleaned[-1,]
        }
      }
    } else {
      for(row in (nrow(dfCntryCleaned)-1):1){
        for(lvl in 1:length(selectionGlobalDropLevel)){
          if(dfCntryCleaned$Level[row]==selectionGlobalDropLevel[lvl]){
            dfCntryCleaned <- dfCntryCleaned[-row,]
          }
          if(dfCntryCleaned$Level[nrow(dfCntryCleaned)]==selectionGlobalDropLevel[lvl]){
            dfCntryCleaned <- dfCntryCleaned[-nrow(dfCntryCleaned),]
          }
        }
      }
    }
  }
  if(paste(selectionScaleDropLevel,collapse=" ")!="None" & nrow(dfCntryCleaned)!=0){
    if(nrow(dfCntryCleaned)==1){
      for(lvl in 1:length(selectionScaleDropLevel)){
        if(dfCntryCleaned$Level[1]==selectionScaleDropLevel[lvl]){
          dfCntryCleaned <- dfCntryCleaned[-1,]
        }
      }
    } else {
      for(row in (nrow(dfCntryCleaned)-1):1){
        for(lvl in 1:length(selectionScaleDropLevel)){
          if(dfCntryCleaned$Level[row]==selectionScaleDropLevel[lvl]){
            dfCntryCleaned <- dfCntryCleaned[-row,]
          }
          if(dfCntryCleaned$Level[nrow(dfCntryCleaned)]==selectionScaleDropLevel[lvl]){
            dfCntryCleaned <- dfCntryCleaned[-nrow(dfCntryCleaned),]
          }
        }
      }
    }
  }
  # If some populations are excluded delete them from the country file
  if(paste(selectionGlobalDropPop,collapse=" ")!="None" & nrow(dfCntryCleaned)!=0){
    if(nrow(dfCntryCleaned)==1){
      for(pop in 1:length(selectionGlobalDropPop)){
        if(dfCntryCleaned$Pop[1]==selectionGlobalDropPop[pop]){
          dfCntryCleaned <- dfCntryCleaned[-1,]
        }
      }
    } else {
      for(row in (nrow(dfCntryCleaned)-1):1){
        for(pop in 1:length(selectionGlobalDropPop)){
          if(dfCntryCleaned$Pop[row]==selectionGlobalDropPop[pop]){
            dfCntryCleaned <- dfCntryCleaned[-row,]
          }
          if(dfCntryCleaned$Pop[nrow(dfCntryCleaned)]==selectionGlobalDropPop[pop]){
            dfCntryCleaned <- dfCntryCleaned[-nrow(dfCntryCleaned),]
          }
        }
      }
    }
  }
  if(paste(selectionScaleDropPop,collapse=" ")!="None" & nrow(dfCntryCleaned)!=0){
    if(nrow(dfCntryCleaned)==1){
      for(pop in 1:length(selectionScaleDropPop)){
        if(dfCntryCleaned$Pop[1]==selectionScaleDropPop[pop]){
          dfCntryCleaned <- dfCntryCleaned[-1,]
        }
      }
    } else {
      for(row in (nrow(dfCntryCleaned)-1):1){
        for(pop in 1:length(selectionScaleDropPop)){
          if(dfCntryCleaned$Pop[row]==selectionScaleDropPop[pop]){
            dfCntryCleaned <- dfCntryCleaned[-row,]
          }
          if(dfCntryCleaned$Pop[nrow(dfCntryCleaned)]==selectionScaleDropPop[pop]){
            dfCntryCleaned <- dfCntryCleaned[-nrow(dfCntryCleaned),]
          }
        }
      }
    }
  } # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Stop and produce warning is nrow(dfCntryCleaned)==0
  # Write additional Mplus syntax variables
  ## Country identifier
  dfCntryCleaned$CountrySyntax <- paste0(dfCntryCleaned$CountryID,"=",dfCntryCleaned$CountryAlpha3)
  if(!is.na(dfCntry$LevelID[1])){ #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< What's the error/warning we get here? This looks ok... <<<<<<<<<<<<<<<<<<<
    ## Level identifier
    dfCntryCleaned$LevelSyntax   <- paste0(dfCntryCleaned$LevelID,"=",dfCntryCleaned$Level)
    ## Population identifier
    dfCntryCleaned$PopSyntax     <- paste0(dfCntryCleaned$PopID,"=",dfCntryCleaned$Pop)
    ## String of all participating countries of a particular level
    tempLevel <- aggregate(CountrySyntax ~ LevelID, data=dfCntryCleaned, paste, collapse=", ")
    colnames(tempLevel) <- c("LevelID","AllCountriesSyntax")
    dfCntryCleaned <- merge(dfCntryCleaned, tempLevel, all.x=T)
    ## String of all participating levels of a particular country
    tempCntry <- aggregate(LevelSyntax ~ CountryID, data=dfCntryCleaned, paste, collapse=", ")
    colnames(tempCntry) <- c("CountryID","AllLevelsSyntax")
    dfCntryCleaned <- merge(dfCntryCleaned, tempCntry, all.x=T)
  }
  ##################################################################################################
  
  # 3: Create function to write Mplus code for scales # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Change to 4
  ## Create Mplus syntax loop
  writeMplus <- function(title, cases, group, auxil, invar, outpt, sdata, selectionIDVars, selectionGroupCodeVar, selectionCountryCodeVar, selectionLevelCodeVar, selectionPopCodeVar, selectionStrat, selectionCluster, selectionWeight, selectionScale, ccode, cntry, path1, path2, estmt, items, itemU, lcode, modin, phant, catgr, suffx){
    mplusSyntax <- # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Order things in the function call about the way they are ordered below
      c('TITLE:',
        '<tabSpace><01.cfa.ttl>.', #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< For each replacement, add numbering and reference below
        '',
        'DATA:',
        '<tabSpace>FILE IS "<02.pth.one>',
        '<tabSpace><03.pth.two><04.scale..><05.phantom>.dat";',
        '',
        'VARIABLE:',
        '<tabSpace>NAMES ARE <06.id.var.><07.id.grp.> <08.id.cntr> <09.id.lev.><10.id.pop.><11.v.strat><12.v.clust><13.var.wgt>', # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Think about spacing here
        '<tabSpace><14.itm.nms>;',
        '',
        '<tabSpace>USEVARIABLES ARE <15.use.itm>;', #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Think about statement order: necessary, then optional
        '<tabSpace>MISSING ARE <15.use.itm> (<16.missing>);',
        '<tabSpace><17.stm.cat>',
        '',
        '<tabSpace><18.stm.sel>', #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Optional
        '<tabSpace><19.stm.grp>',
        '',
        '<tabSpace><20.stm.aux>',
        '',
        '<tabSpace>STRATIFICATION IS <11.v.strat>;', #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Necessary
        '<tabSpace>CLUSTER IS <12.v.clust>;',
        '<tabSpace>WEIGHT IS <13.var.wgt>;',
        '',
        'ANALYSIS:',
        '<tabSpace>TYPE IS COMPLEX;',
        '<tabSpace>ESTIMATOR IS <21.estimtr>;',
        '<tabSpace><22.stm.inv>',
        '',
        'MODEL:',
        '',# At line 31 of the syntax insert the model statement
        '',
        'OUTPUT:',
        '<tabSpace><23.stm.out>;',
        '',
        '')# At line 36 of the syntax insert the "SAVEDATA" statement
    
    # Replace CFA analysis statements from manipulated variables specified below
    mplusSyntax <- gsub("<01.cfa.ttl>", title, mplusSyntax)
    mplusSyntax <- gsub("<18.stm.sel>", cases, mplusSyntax) # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 18. REPLACEMENT
    mplusSyntax <- gsub("<19.stm.grp>", group, mplusSyntax) # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 19. Replacement
    mplusSyntax <- gsub("<20.stm.aux>", auxil, mplusSyntax) # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 20. REPLACEMENT
    mplusSyntax <- gsub("<22.stm.inv>", invar, mplusSyntax) # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 22. REPLACEMENT
    mplusSyntax <- gsub("<23.stm.out>", outpt, mplusSyntax) # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 23. REPLACEMENT
    mplusSyntax <- gsub("<16.missing>", outpt, mplusSyntax) # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 16. REPLACEMENT
    ## Insert the "SAVEDATA" statement
    mplusSyntax <- c(mplusSyntax[1:35],sdata)

    # Replace CFA analysis scale specific statements... #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Add numbering, mentioned above, here <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    ## ...from user selection
    if(!is.na(selectionIDVars)){ #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Check these are OK (also, lines 245, 252, 257) <<<<<<<<<<<<<<<<<<<<<<<<<<<
      # 6. ID variables <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 6. REPLACEMENT & other places
      mplusSyntax <- gsub("<06.id.var.>", paste0(selectionIDVars," "), mplusSyntax)
    } else {
      mplusSyntax <- gsub("<06.id.var.>", "", mplusSyntax)
    }
    # 7. ID group variable <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 7. REPLACEMENT & other places
    mplusSyntax <- gsub("<07.id.grp.>", selectionGroupCodeVar, mplusSyntax)
    # 8. Country ID variable <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 8. REPLACEMENT & other places
    mplusSyntax <- gsub("<08.id.cntr>", selectionCountryCodeVar, mplusSyntax)
    # 9. Level ID variable <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 9. REPLACEMENT & other places
    # 10. Pop ID variable <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 10. REPLACEMENT & other places
    if(!is.na(selectionLevelCodeVar)){ #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      mplusSyntax <- gsub("<09.id.lev.>", paste0(selectionLevelCodeVar," "), mplusSyntax)
      mplusSyntax <- gsub("<10.id.pop.>", paste0(selectionPopCodeVar," "), mplusSyntax)
    } else {
      mplusSyntax <- gsub("<09.id.lev.>", "", mplusSyntax)
      mplusSyntax <- gsub("<10.id.pop.>", "", mplusSyntax)
    }
    if(!is.na(selectionStrat)){ #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      mplusSyntax <- gsub("<11.v.strat>", paste0(selectionStrat," "), mplusSyntax) #<<<<<<<<<<<<<<<< 11. REPLACEMENT: Got to put space in sometimes, take out others...
    } else {
      mplusSyntax <- gsub("<11.v.strat>", "", mplusSyntax)
    }
    if(!is.na(selectionCluster)){ #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      mplusSyntax <- gsub("<12.v.clust>", paste0(selectionCluster," "), mplusSyntax) #<<<<<<<<<<<<<< 12. REPLACEMENT: Got to put space in sometimes, take out others...
    } else {
      mplusSyntax <- gsub("<12.v.clust>", "", mplusSyntax)
    }
    mplusSyntax <- gsub("<13.var.wgt>", selectionWeight, mplusSyntax) # <<<<<<<<<<<<<<<<<<<<<<<<<<<< 13. REPLACEMENT
    # 4. Scale name <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 4. REPLACEMENT & other places
    mplusSyntax <- gsub("<04.scale..>", selectionScale, mplusSyntax)
    
    ## ...from manipulated variables specified below
    mplusSyntax <- gsub("<cntry.code>", ccode, mplusSyntax)
    mplusSyntax <- gsub("<cntry.name>", cntry, mplusSyntax)
    # 2. Path one <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 2. REPLACEMENT 
    mplusSyntax <- gsub("<02.pth.one>", path1, mplusSyntax)
    # 3. Path two <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 3. REPLACEMENT 
    mplusSyntax <- gsub("<03.pth.two>", path2, mplusSyntax)
    mplusSyntax <- gsub("<21.estimtr>", estmt, mplusSyntax) # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 21. REPLACEMENT
    mplusSyntax <- gsub("<14.itm.nms>", items, mplusSyntax) # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 14. REPLACEMENT
    mplusSyntax <- gsub("<15.use.itm>", itemU, mplusSyntax) # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 15. REPLACEMENT
    mplusSyntax <- gsub("<level.code>", lcode, mplusSyntax)
    mplusSyntax <- gsub("<mod.indics>", modin, mplusSyntax)
    mplusSyntax <- gsub("<05.phantom>", phant, mplusSyntax) # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 5. REPLACEMENT
    mplusSyntax <- gsub("<17.stm.cat>", catgr, mplusSyntax) # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 17. REPLACEMENT
    mplusSyntax <- gsub("<suffix.opt>", suffx, mplusSyntax)
    # Insert the model statement
    if(selectionAnalysis=="Pooled CFA" | selectionAnalysis=="Single Population CFA"){
      if(selectionFS=="Yes"){
        mplusSyntax <- c(mplusSyntax[1:30],model,mplusSyntax[32:38])
      } else {
        mplusSyntax <- c(mplusSyntax[1:30],model,mplusSyntax[32:36])
      }
    } else {
      mplusSyntax <- c(mplusSyntax[1:30],model,mplusSyntax[32:36])
    }
    # Make the Mplus syntax look 'nice'
    ## Remove extra lines in syntax
    for(line in length(mplusSyntax):2){
      if((mplusSyntax[line]=="" & mplusSyntax[line-1]=="") | (mplusSyntax[line]=="" & mplusSyntax[line-1]=="<tabSpace>") | (mplusSyntax[line]=="<tabSpace>" & mplusSyntax[line-1]=="<tabSpace>") | (mplusSyntax[line]=="<tabSpace>" & mplusSyntax[line-1]=="") | is.na(mplusSyntax[line])){
        mplusSyntax <- mplusSyntax[-line]
      }
    }
    ## Ensure no lines are greater than 90 characters (Mplus cannot read lines longer than 90 characters)
    mplusSyntax <- paste(strwrap(mplusSyntax, 90, simplify=T), collapse="\n")
    ## Replace <tabSpace> with tabSp
    mplusSyntax <- gsub("<tabSpace>", tabSp, mplusSyntax)
    
    writeLines(text=mplusSyntax, con=filename)
  }
  ##################################################################################################
  
  # 4: Manipulate remaining varibales & write Mplus files to respective scale folders
  # Create a new country data frame with Mplus syntax for measurement invariance testing
  ## Cross-countries
  if(selectionAnalysis=="Cross-country MGCFA with MI testing"){
    # Copy country data frame
    cntryMGCFA <- dfCntryCleaned
    # Limit data frame to the level numeric variable and a Mplus string of all countries in each population
    cntryMGCFA <- cntryMGCFA[,c("LevelID","AllCountriesSyntax")]
    # Get unique cases of populations and the countries in each population
    cntryMGCFA <- unique(cntryMGCFA)
    # Remove deleted populations
    cntryMGCFA$RemoveCountriesSyntax <- NA
    if(paste(selectionGlobalDropCountry,collapse=" ")=="None" & paste(selectionScaleDropCountry,collapse=" ")=="None" & paste(selectionGlobalDropLevel,collapse=" ")=="None" & paste(selectionScaleDropLevel,collapse=" ")=="None" & paste(selectionGlobalDropPop,collapse=" ")=="None" & paste(selectionScaleDropPop,collapse=" ")=="None"){
      cntryMGCFA$RemoveCountriesSyntax <- ""
    } else {
      listCountryAlpha <- c()
      if(paste(selectionGlobalDropCountry,collapse=" ")!="None"){ 
        for(cnt in 1:length(selectionGlobalDropCountry)){
          listCountryAlpha <- c(listCountryAlpha, selectionGlobalDropCountry[cnt])
        }
        cntryMGCFA$RemoveCountriesSyntax <- paste(listCountryAlpha, collapse=" ")
      }
      if(paste(selectionScaleDropCountry,collapse=" ")!="None"){
        for(cnt in 1:length(selectionScaleDropCountry)){
          listCountryAlpha <- c(listCountryAlpha, selectionScaleDropCountry[cnt])
        }
        cntryMGCFA$RemoveCountriesSyntax <- paste(listCountryAlpha, collapse=" ")
      }
      for(row in 1:nrow(cntryMGCFA)){
        if(paste(selectionGlobalDropPop,collapse=" ")!="None"){
          for(pop in 1:length(selectionGlobalDropPop)){
            droppedCntry <- unlist(strsplit(selectionGlobalDropPop[pop], "_"))[1]
            droppedLevel <- unlist(strsplit(selectionGlobalDropPop[pop], "_"))[2]
            if(droppedLevel==paste0(selectionLevelCodeVar,cntryMGCFA$LevelID[row]) & !(droppedCntry %in% listCountryAlpha)){
              listCountryAlpha <- c(listCountryAlpha, droppedCntry)
            }
          }
          cntryMGCFA$RemoveCountriesSyntax[row] <- paste(listCountryAlpha, collapse=" ")
        }
        if(paste(selectionScaleDropPop,collapse=" ")!="None"){
          for(pop in 1:length(selectionScaleDropPop)){
            droppedCntry <- unlist(strsplit(selectionScaleDropPop[pop], "_"))[1]
            droppedLevel <- unlist(strsplit(selectionScaleDropPop[pop], "_"))[2]
            if(droppedLevel==paste0(selectionLevelCodeVar,cntryMGCFA$LevelID[row]) & !(droppedCntry %in% listCountryAlpha)){
              listCountryAlpha <- c(listCountryAlpha, droppedCntry)
            }
          }
          cntryMGCFA$RemoveCountriesSyntax[row] <- paste(listCountryAlpha, collapse=" ")
        }
      }
    }
  }
  ## Cross-levels
  if(selectionAnalysis=="Cross-level MGCFA with MI testing"){
    # Copy country data
    cntryMGCFA <- dfCntryCleaned
    # Create a loop to delete countries with only one population level
    for(row in nrow(cntryMGCFA):1){
      # If the two string variables are identical, then only one population was measured in the country
      if(identical(cntryMGCFA$LevelSyntax[row], cntryMGCFA$AllLevelsSyntax[row])){
        cntryMGCFA <- cntryMGCFA[-row,]
      }
    }
    # Remove single level data, so that unique observations will be only those countries with multiple populations
    cntryMGCFA <- cntryMGCFA[,c("CountryAlpha3","CountryID","AllLevelsSyntax")]
    cntryMGCFA <- unique(cntryMGCFA)
    # Remove deleted populations
    cntryMGCFA$RemoveLevelsSyntax <- NA
    if(paste(selectionGlobalDropCountry,collapse=" ")=="None" & paste(selectionScaleDropCountry,collapse=" ")=="None" & paste(selectionGlobalDropLevel,collapse=" ")=="None" & paste(selectionScaleDropLevel,collapse=" ")=="None" & paste(selectionGlobalDropPop,collapse=" ")=="None" & paste(selectionScaleDropPop,collapse=" ")=="None"){
      cntryMGCFA$RemoveLevelsSyntax <- ""
    } else {
      listLevel <- c()
      if(paste(selectionGlobalDropLevel,collapse=" ")!="None"){ 
        for(lvl in 1:length(selectionGlobalDropLevel)){
          listLevel <- c(listLevel, selectionGlobalDropLevel[lvl])
        }
        cntryMGCFA$RemoveLevelsSyntax <- paste(listLevel, collapse=" ")
      }
      if(paste(selectionScaleDropLevel,collapse=" ")!="None"){
        for(lvl in 1:length(selectionScaleDropLevel)){
          listLevel <- c(listLevel, selectionScaleDropLevel[lvl])
        }
        cntryMGCFA$RemoveLevelsSyntax <- paste(listLevel, collapse=" ")
      }
      for(row in 1:nrow(cntryMGCFA)){
        if(paste(selectionGlobalDropPop,collapse=" ")!="None"){
          for(pop in 1:length(selectionGlobalDropPop)){
            droppedCntry <- unlist(strsplit(selectionGlobalDropPop[pop], "_"))[1]
            droppedLevel <- unlist(strsplit(selectionGlobalDropPop[pop], "_"))[2]
            if(droppedCntry==cntryMGCFA$CountryAlpha3[row] & !(droppedLevel %in% listLevel)){
              listLevel <- c(listLevel, droppedLevel)
            }
          }
          cntryMGCFA$RemoveLevelsSyntax[row] <- paste(listLevel, collapse=" ")
        }
        if(paste(selectionScaleDropPop,collapse=" ")!="None"){
          for(pop in 1:length(selectionScaleDropPop)){
            droppedCntry <- unlist(strsplit(selectionGlobalDropPop[pop], "_"))[1]
            droppedLevel <- unlist(strsplit(selectionGlobalDropPop[pop], "_"))[2]
            if(droppedCntry==cntryMGCFA$CountryAlpha3[row] & !(droppedLevel %in% listLevel)){
              listLevel <- c(listLevel, droppedLevel)
            }
          }
          cntryMGCFA$RemoveLevelsSyntax[row] <- paste(listLevel, collapse=" ")
        }
      }
    }
  }
  ################################################
  
  # Analysis specific statements
  if(selectionAnalysis=="Pooled CFA"){
    # 1. Title replacement <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 1. REPLACEMENT
    if(!is.na(as.numeric(selectionTargetLevelCode))){
      title <- paste0(nameStudy,": Pooled CFA for <04.scale..> (<09.id.lev.> ",as.numeric(selectionTargetLevelCode),")")
    } else {
      title <- paste0(nameStudy,": Pooled CFA for <04.scale..>")
    }
    group <- ""
    if(paste(selectionGlobalDropCountry,collapse=" ")=="None" & paste(selectionScaleDropCountry,collapse=" ")=="None" & paste(selectionGlobalDropLevel,collapse=" ")=="None" & paste(selectionScaleDropLevel,collapse=" ")=="None" & paste(selectionGlobalDropPop,collapse=" ")=="None" & paste(selectionScaleDropPop,collapse=" ")=="None" & selectionAdjudicateVar=="None"){
      if(!is.na(as.numeric(selectionTargetLevelCode))){
        cases <- paste0("SUBPOPULATION IS (<09.id.lev.> EQ ",as.numeric(selectionTargetLevelCode),");")
      } else {
        cases <- ""
      }
    } else {
      if(!is.na(as.numeric(selectionTargetLevelCode))){
        cases <- paste0("SUBPOPULATION IS (<09.id.lev.> EQ ",as.numeric(selectionTargetLevelCode))
      } else {
        cases <- "SUBPOPULATION IS ("
      }
      if(selectionAdjudicateVar!="None"){        
        cases <- paste0(cases," AND ",selectionAdjudicateVar," EQ ",selectionAdjudicateCode)
      }
      if(paste(selectionGlobalDropCountry,collapse=" ")!="None"){ 
        listGDC <- c()
        for(cnt in 1:length(selectionGlobalDropCountry)){
          cases <- paste0(cases," AND <08.id.cntr> NE ",selectionGlobalDropCountry[cnt])
          listGDC <- c(listGDC, selectionGlobalDropCountry[cnt])
        }
      }
      if(paste(selectionScaleDropCountry,collapse=" ")!="None"){
        listSDC <- c()
        for(cnt in 1:length(selectionScaleDropCountry)){
          cases <- paste0(cases," AND <08.id.cntr> NE ",selectionScaleDropCountry[cnt])
          listSDC <- c(listSDC, selectionScaleDropCountry[cnt])
        }
      }
      if(!is.na(as.numeric(selectionTargetLevelCode))){
        if(paste(selectionGlobalDropPop,collapse=" ")!="None"){
          for(pop in 1:length(selectionGlobalDropPop)){
            droppedCntry <- unlist(strsplit(selectionGlobalDropPop[pop], "_"))[1]
            droppedLevel <- unlist(strsplit(selectionGlobalDropPop[pop], "_"))[2]
            if(droppedLevel==paste0(selectionLevelCodeVar,selectionTargetLevelCode) & !(droppedCntry %in% listGDC) & !(droppedCntry %in% listSDC)){
              cases <- paste0(cases," AND <08.id.cntr> NE ",droppedCntry)
            }
          }
        }
        if(paste(selectionScaleDropPop,collapse=" ")!="None"){
          for(pop in 1:length(selectionScaleDropPop)){
            droppedCntry <- unlist(strsplit(selectionScaleDropPop[pop], "_"))[1]
            droppedLevel <- unlist(strsplit(selectionScaleDropPop[pop], "_"))[2]
            if(droppedLevel==paste0(selectionLevelCodeVar,selectionTargetLevelCode) & !(droppedCntry %in% listGDC) & !(droppedCntry %in% listSDC)){
              cases <- paste0(cases," AND <08.id.cntr> NE ",droppedCntry)
            }
          }
        }
      } else {
        if(paste(selectionGlobalDropLevel,collapse=" ")!="None"){
          for(lvl in 1:length(selectionGlobalDropLevel)){
            cases <- paste0(cases," AND <09.id.lev.> NE ",selectionGlobalDropLevel[lvl])
          }
        }
        if(paste(selectionScaleDropLevel,collapse=" ")!="None"){
          for(lvl in 1:length(selectionScaleDropLevel)){
            cases <- paste0(cases," AND <09.id.lev.> NE ",selectionScaleDropLevel[lvl])
          }
        }
        if(paste(selectionGlobalDropPop,collapse=" ")!="None"){
          for(pop in 1:length(selectionGlobalDropPop)){
            cases <- paste0(cases," AND <10.id.pop.> NE ",selectionGlobalDropPop[pop])
          }
        }
        if(paste(selectionScaleDropPop,collapse=" ")!="None"){
          for(pop in 1:length(selectionScaleDropPop)){
            cases <- paste0(cases," AND <10.id.pop.> NE ",selectionScaleDropPop[pop])
          }
        }
      }
      cases <- paste0(cases,");")
    }
    outpt <- "SAMPSTAT STANDARDIZED RESIDUAL TECH1 FSDETERMINACY SVALUES <mod.indics>"
    if(selectionFS=="Yes"){
      auxil <- "AUXILIARY ARE <06.id.var.><07.id.grp.> <08.id.cntr> <09.id.lev.> <10.id.pop.>;"
      sdata <- c("SAVEDATA:","<tabSpace>FILE IS FactorScores_<04.scale..>_PooledCFA<suffix.opt>.dat;","<tabSpace>SAVE IS FSCORES;")
    } else {
      auxil <- ""
      sdata <- ""
    }
  }
  if(selectionAnalysis=="Single Population CFA"){
    title <- paste0(nameStudy,": CFA for <04.scale..> - <cntry.name> for <09.id.lev.> <level.code>")
    group <- ""
    if(selectionAdjudicateVar!="None"){
      cases <- paste0("SUBPOPULATION IS (",selectionAdjudicateVar," EQ ",selectionAdjudicateCode)
      cases <- paste0(cases," AND <08.id.cntr> EQ <cntry.code> AND <09.id.lev.> EQ <level.code>);")
    } else {
      cases <- "SUBPOPULATION IS (<08.id.cntr> EQ <cntry.code> AND <09.id.lev.> EQ <level.code>);"
    }
    outpt <- "SAMPSTAT STANDARDIZED RESIDUAL TECH1 FSDETERMINACY SVALUES <mod.indics>"
    if(selectionFS=="Yes"){
      auxil <- "AUXILIARY ARE <06.id.var.><07.id.grp.> <08.id.cntr> <09.id.lev.> <10.id.pop.>;"
      sdata <- c("SAVEDATA:","<tabSpace>FILE IS FactorScores_<04.scale..>_<cntry.name>_<09.id.lev.><level.code><suffix.opt>.dat;","<tabSpace>SAVE IS FSCORES;")
    } else {
      auxil <- ""
      sdata <- ""
    }
  }
  if(selectionAnalysis=="Cross-country MGCFA with MI testing"){
    title <- paste0(nameStudy,": MG CFA for <04.scale..> in the <09.id.lev.> <level.code> level - Cross-Country, Single Level (Measurement Invariance testing)")
    auxil <- ""
    group <- "GROUPING IS <08.id.cntr> (<cntry.code>);"
    if(paste(selectionGlobalDropCountry,collapse=" ")=="None" & paste(selectionScaleDropCountry,collapse=" ")=="None" & paste(selectionGlobalDropPop,collapse=" ")=="None" & paste(selectionScaleDropPop,collapse=" ")=="None" & selectionAdjudicateVar=="None"){
      cases <- "USEOBSERVATION IS (<09.id.lev.> EQ <level.code>);"
    } else {
      cases <- "USEOBSERVATION IS (<09.id.lev.> EQ <level.code>"
      if(selectionAdjudicateVar!="None"){        
        cases <- paste0(cases," AND ",selectionAdjudicateVar," EQ ",selectionAdjudicateCode)
      }
      cases <- paste0(cases,");")
    }
    outpt <- "SAMPSTAT RESIDUAL TECH1"
    sdata <- ""
  }
  if(selectionAnalysis=="Cross-level MGCFA with MI testing"){
    title <- paste0(nameStudy,": MG CFA for <04.scale..> in <cntry.name> - Cross-Level, Single Country (Measurement Invariance testing)")
    auxil <- ""
    group <- "GROUPING IS <09.id.lev.> (<level.code>);"
    if(paste(selectionGlobalDropLevel,collapse=" ")=="None" & paste(selectionScaleDropLevel,collapse=" ")=="None" & paste(selectionGlobalDropPop,collapse=" ")=="None" & paste(selectionScaleDropPop,collapse=" ")=="None" & selectionAdjudicateVar=="None"){
      cases <- "USEOBSERVATION IS (<08.id.cntr> EQ <cntry.code>);"
    } else {
      cases <- "USEOBSERVATION IS (<08.id.cntr> EQ <cntry.code>"
      if(selectionAdjudicateVar!="None"){        
        cases <- paste0(cases," AND ",selectionAdjudicateVar," EQ ",selectionAdjudicateCode)
      }
      cases <- paste0(cases,");")
    }
    outpt <- "SAMPSTAT RESIDUAL TECH1"
    sdata <- ""
  }
  ################################################
  
  # Housekeeping variables
  ## Set directory path variables
  path1 <- paste0(dirRoot,"/",dirAnalysisPhase)
  path2 <- paste0("1Data/2MplusScaleData/",dirGroup)
  ## Set suffix variable
  if(selectionSuffix=="None"){
    suffx <- ""
  } else {
    suffx <- paste0("_",selectionSuffix)
  }
  ## Set phantom indicator variable
  phantomScales <- unlist(strsplit(selectionListScalePI, "; "))
  if(selectionScale %in% phantomScales){
    phant <- "_PI"
  } else {
    phant <- ""
  }
  # Set spacing variable to make the Mplus syntax look 'nice'
  tabSp <- "    "
  ################################################
  
  # Scale specific variables
  ## Get the row of the scale
  for(row in 1:nrow(dfScale)){
    if(selectionScale %in% dfScale$Scale[row]){
      ind <- row
    }
  }
  ## Create scale name
  scale <- dfScale$Scale[ind]
  ## Create item list
  items <- dfScale$Items[ind]
  ## Delete items not needed
  if(selectionScaleDropItems[1]!="None"){
    # Separate out dropped items
    dropItems <- unlist(strsplit(selectionScaleDropItems, "; "))
    # Create a list of the items in the scale
    itemU <- unlist(strsplit(items, " "))
    # Scan the "dropItems" vector for items to drop
    for(itm in dropItems){
      # If an item in the vector "dropItems" is in the item list of scales...
      if(itm %in% itemU){
        # ...get the index of that item
        indexItem <- which(itemU %in% itm)
        # ...delete it from the item list
        itemU <- itemU[-c(indexItem)]
      } else {next}
    }
    itemU <- paste0(itemU, collapse=" ") 
  } else {
    itemU <- items
  }
  ## Create 'Categorical' Mplus statement for cateogrical or mixed scale
  if(dfScale$Type[ind]=="Categorical"){
    catgr <- paste0("CATEGORICAL ARE ",itemU,";")
    outpt <- gsub("FSDETERMINACY ", "", outpt)
  } else if(dfScale$Type[ind]=="Mixed"){
    catgr <- paste0("CATEGORICAL ARE ",dfScale$CatVarForMixed[ind],";")
    outpt <- gsub("FSDETERMINACY ", "", outpt)
  } else if(!is.na(dfScale$isMD[ind]) & dfScale$isMD[ind]=="Yes"){ #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< New, see line 654, looks good! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    for(row in 1:nrow(dfScale)){
      if(((!is.na(dfScale$inMD1[row]) & dfScale$inMD1[row]==dfScale$Scale[ind]) | (!is.na(dfScale$inMD2[row]) & dfScale$inMD2[row]==dfScale$Scale[ind])) & dfScale$Type[row]=="Categorical"){
        itemCatFull <- unlist(strsplit(dfScale$Items[row]," "))
        itemCat     <- c()
        for(itm in 1:length(itemCatFull)){
          if(grepl(itemCatFull[itm],itemU)){
            itemCat <- c(itemCat, itemCatFull[itm])
          }
        }
        itemCat <- paste(itemCat, collapse=" ")
        catgr <- paste0("CATEGORICAL ARE ",itemCat,";") #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< What if MD scale includes two categorical/mixed scales? Fix this <<<<<<<<<
        outpt <- gsub("FSDETERMINACY ", "", outpt)
      }
      if(((!is.na(dfScale$inMD1[row]) & dfScale$inMD1[row]==dfScale$Scale[ind]) | (!is.na(dfScale$inMD2[row]) & dfScale$inMD2[row]==dfScale$Scale[ind])) & dfScale$Type[row]=="Mixed"){
        itemCatFull <- unlist(strsplit(dfScale$CatVarForMixed[row]," "))
        itemCat     <- c()
        for(itm in 1:length(itemCatFull)){
          if(grepl(itemCatFull[itm],itemU)){
            itemCat <- c(itemCat, itemCatFull[itm])
          }
        }
        catgr <- paste0("CATEGORICAL ARE ",itemCat,";")
        outpt <- gsub("FSDETERMINACY ", "", outpt)
      } 
    }
  } else {
    catgr <- ""
  }
  ## Create estimator Mplus variable
  if(dfScale$Type[ind]=="Continuous"){
    estmt <- "MLR"
  } else {
    estmt <- "WLSMV"
  }
  ## Create modification indices output request Mplus variable
  if(selectionAnalysis=="Pooled CFA" | selectionAnalysis=="Single Population CFA"){
    if(dfScale$Type[ind]=="Continuous"){
      modin <- "MODINDICES(3.84)"
    } else {
      modin <- "MODINDICES(ALL)"
    }
  } else {
    modin <- ""
  }
  ## Create invariance test request Mplus variable
  if(selectionAnalysis=="Pooled CFA" | selectionAnalysis=="Single Population CFA"){
    invar <- ""
  } else {
    if(dfScale$Type[ind]=="Continuous"){
      invar <- "MODEL IS CONFIGURAL METRIC SCALAR;"
    } else {
      invar <- "MODEL IS CONFIGURAL SCALAR;"
    }
  }
  #######################
  
  # Create model statement
  ## UNIDIMENSIONAL SCALES
  ## If the scale is not multidimensional...
  if(is.na(dfScale$isMD[ind]) | dfScale$isMD[ind]=="No"){ #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Doesn't acknowledge categorical in mixed scales <<<<<<<<<<<<<<<<<<<<<<<<<<
    # Create a list of the scale variables
    itemL <- unlist(strsplit(itemU, " "))
    # Add "*" after the first element of the scale variables (freeing the parameter)
    itemL[1] <- paste0(itemL[1], "*")
    # Recombine the list of scale variables into a string
    itemL <- paste(itemL, collapse=" ")
    # Create "BY" statement for the scale
    modelBY <- paste0(scale, " BY ", itemL, ";")
    # Create fixed scale variance statement
    modelSV <- paste0(scale, "@1;")
    # Create full model statement
    modelBASE <- c(paste0("<tabSpace>",modelBY), paste0("<tabSpace>",modelSV))
  } else {
    ## MULTIDIMENSIONAL SCALES
    # Create temporary lists to store model lines and subscale information
    tempModel <- c()
    tempSubsc <- c()
    # Use a loop to generate the model statement (list of lines)
    for(row in 1:nrow(dfScale)){
      # If the MD scale name is in the "inMD" column...
      if(dfScale$inMD1[row] %in% scale | dfScale$inMD2[row] %in% scale){
        # Get the sub-scale name
        tempScale <- dfScale$Scale[row]
        # Get the sub-scale variables
        tempItems <- dfScale$Items[row]
        # Delete items not needed
        if(selectionScaleDropItems[1]!="None"){
          dropItems <- unlist(strsplit(selectionScaleDropItems, "; "))
          # Create a list of the items in the scale
          tempItemU <- unlist(strsplit(tempItems, " "))
          # Scan the "dropItems" vector for items to drop
          for(itm in dropItems){
            # If an item in the vector "dropItems" is in the item list of scales...
            if(itm %in% tempItemU){
              # ...get the index of that item
              indexItem <- which(tempItemU %in% itm)
              # ...delete it from the item list
              tempItemU <- tempItemU[-c(indexItem)]
            } else {next}
          }
        } else {
          tempItemU <- unlist(strsplit(tempItems, " "))
        }
        # Add "*" after the first element of the sub-scale variables (freeing the parameter)
        tempItemU[1] <- paste0(tempItemU[1], "*")
        # Recombine the list of sub-scale variables into a string
        tempItemU <- paste(tempItemU, collapse=" ")
        # Create the "BY" statement for the sub-scale
        tempModelBY <- paste0(tempScale," BY ",tempItemU,";")
        # Add this string as a single list element to a temporary list
        tempModel <- c(tempModel, tempModelBY)
        # Add the sub-scale name to another temporary list
        tempSubsc <- c(tempSubsc, tempScale)
      }
    }
    # Use a loop to set each sub-scale's variance to 1
    for(sub in 1:length(tempSubsc)){
      tempSubsc[sub] <- paste0(tempSubsc[sub],"@1;") 
    }
    # Create full model statement
    modelBASE <- c(paste0("<tabSpace>",tempModel), paste0("<tabSpace>",tempSubsc))
  }
  # Add improvement(s)
  if(selectionImprove=="Yes"){
    for(row in 1:nrow(dfImprv)){
      if(scale %in% dfImprv$Scale[row]){ #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Check on names for 'modelImprovement' file <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        tempImprv <- dfImprv$PooledImprovement[row] #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< See line 174 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        if(!is.na(tempImprv)){
          tempImprv <- unlist(strsplit(tempImprv, "\n"))
          for(imp in 1:length(tempImprv)){
            tempImprv[imp] <- paste0("<tabSpace>",tempImprv[imp])
          }
          tempImprv <- c("<tabSpace>!Pooled Improvement(s)",tempImprv)
        } else {
          tempImprv <- ""
        }
      }
    }
  } else {
    tempImprv <- ""
  }
  modelBASE <- c(modelBASE, "", tempImprv)
  # Create Scale directory
  dir.create(paste0(dirCFAs,scale), showWarnings=T, recursive=T)
  # Set the working directory to the created Scale directory
  setwd(file.path(dirCFAs, scale))
  # Create further variables if needed
  if(selectionAnalysis=="Pooled CFA"){
    model <- modelBASE
    ccode <- ""
    cntry <- ""
    lcode <- ""
    filename <- paste0(scale,"_PooledCFA",suffx,".inp")
    # Use the function to create the Mplus input in the directory
    writeMplus(title, cases, group, auxil, invar, outpt, sdata, selectionIDVars, selectionGroupCodeVar, selectionCountryCodeVar, selectionLevelCodeVar, selectionPopCodeVar, selectionStrat, selectionCluster, selectionWeight, selectionScale, ccode, cntry, path1, path2, estmt, items, itemU, lcode, modin, phant, catgr, suffx)
  }
  if(selectionAnalysis=="Single Population CFA"){
    # Get specific population item drop information
    if(selectionScaleDropSpecifics[1]!="None"){
      # Separate out the population and items as a single character
      scaleDropSpecific <- unlist(strsplit(selectionScaleDropSpecifics, "; "))
      scaleDropSpecificPop <- c()
      scaleDropSpecificItm <- c()
      for(pop in 1:length(scaleDropSpecific)){
        scaleDropSpecificPop <- c(scaleDropSpecificPop, unlist(strsplit(scaleDropSpecific[pop], ": "))[1])
        scaleDropSpecificItm <- c(scaleDropSpecificItm, unlist(strsplit(scaleDropSpecific[pop], ": "))[2])
      }
    }
    # Add country specific variables
    for(crow in 1:nrow(dfCntryCleaned)){
      # Set the "cntry", "ccode", "level", and "lcode" variables
      cntry <- dfCntryCleaned$CountryAlpha3[crow]
      ccode <- dfCntryCleaned$CountryID[crow]
      level <- dfCntryCleaned$Level[crow]
      lcode <- dfCntryCleaned$LevelID[crow]
      # Delete items for population specific selections
      if(selectionScaleDropSpecifics[1]!="None"){ #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< change 'temp' to 'mod' or 'MOD' <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        for(pop in 1:length(scaleDropSpecificPop)){
          if(cntry %in% scaleDropSpecificPop[pop] | level %in% scaleDropSpecificPop[pop] | paste0(cntry,"_",level) %in% scaleDropSpecificPop[pop]){
            if(is.na(dfScale$isMD[ind]) | dfScale$isMD[ind]=="No"){ #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Change all 'no', 'yes', etc. to "to.upper"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
              tempItemL <- gsub("*","",itemL,fixed=T)
              tempItemL <- unlist(strsplit(tempItemL," "))
              tempRemove <- unlist(strsplit(scaleDropSpecificItm[pop], ", "))
              for(itm in length(tempItemL):1){
                if(tempItemL[itm] %in% tempRemove){
                  tempItemL <- tempItemL[-itm]
                }
              }
              # Change 'itemU' variable
              itemUMOD <- paste(tempItemL, collapse=" ")
              # Change 'model' statment
              tempItemL[1] <- paste0(tempItemL[1],"*")
              tempItemL <- paste(tempItemL, collapse=" ")
              modelMOD <- gsub(itemL,tempItemL,modelBASE,fixed=T)
              # Change 'categorical statement'
              if(dfScale$Type[ind]=="Categorical"){
                catgrMOD <- paste0("CATEGORICAL ARE ",itemUMOD,";")
              } else if(dfScale$Type[ind]=="Mixed"){
                tempItemCat <- unlist(strsplit(itemUMOD," "))
                useItemCat  <- c()
                listCatItem <- unlist(strsplit(dfScale$CatVarForMixed[ind]," "))
                for(itm in 1:length(listCatItem)){
                  if(listCatItem[itm] %in% tempItemCat){
                    useItemCat <- c(useItemCat, listCatItem[itm])
                  }
                }
                catgrMOD <- paste0("CATEGORICAL ARE ",paste(useItemCat, collapse=" "),";")
              }
            } else if(!is.na(dfScale$isMD[ind]) & dfScale$isMD[ind]=="Yes"){
              # Change 'itemU' variable
              tempRemove <- unlist(strsplit(scaleDropSpecificItm[pop], ", ")) #<<<<<<<<<<<<<<<<<<<<< Add line in Mplus syntax about which items are deleted <<<<<<<<<<<<<<<<<<<
              tempItemU  <- unlist(strsplit(itemU, " "))
              for(itm in length(tempItemU):1){
                if(tempItemU[itm] %in% tempRemove){
                  tempItemU <- tempItemU[-itm]
                }
              }
              itemUMOD <- paste(tempItemU, collapse=" ")
              # Change 'model' statement
              # Create temporary lists to store model lines and subscale information
              tempModel <- c()
              tempSubsc <- c()
              modelMOD <- modelBASE
              # Use a loop to generate the model statement (list of lines)
              for(row in 1:nrow(dfScale)){
                # If the MD scale name is in the "inMD" column...
                if(dfScale$inMD1[row] %in% scale | dfScale$inMD2[row] %in% scale){
                  # Get the sub-scale name
                  tempScale <- dfScale$Scale[row]
                  # Get the sub-scale variables
                  tempItems <- dfScale$Items[row]
                  # Delete items not needed
                  if(selectionScaleDropItems[1]!="None"){
                    dropItems <- unlist(strsplit(selectionScaleDropItems, "; "))
                    # Create a list of the items in the scale
                    tempItemU <- unlist(strsplit(tempItems, " "))
                    # Scan the "dropItems" vector for items to drop
                    for(itm in dropItems){
                      # If an item in the vector "dropItems" is in the item list of scales...
                      if(itm %in% tempItemU){
                        # ...get the index of that item
                        indexItem <- which(tempItemU %in% itm)
                        # ...delete it from the item list
                        tempItemU <- tempItemU[-c(indexItem)]
                      } else {next}
                    }
                  } else {
                    tempItemU <- unlist(strsplit(tempItems, " "))
                  }
                  # New code <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                  modItemU <- tempItemU
                  for(itm in length(modItemU):1){
                    if(modItemU[itm] %in% tempRemove){
                      modItemU <- modItemU[-itm]
                    }
                  }
                  #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                  # Modified code <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                  if(!identical(modItemU,character(0))){
                    # Add "*" after the first element of the sub-scale variables (freeing the parameter)
                    modItemU[1] <- paste0(modItemU[1], "*")
                    tempItemU[1] <- paste0(tempItemU[1], "*")
                    # Recombine the list of sub-scale variables into a string
                    modItemU <- paste(modItemU, collapse=" ")
                    tempItemU <- paste(tempItemU, collapse=" ")
                    # Modify 'model' statement
                    modelMOD <- gsub(tempItemU,modItemU,modelMOD,fixed=T)
                  } else {
                    for(line in length(modelMOD):1){
                      if(grepl(tempScale,modelMOD[line])){
                        modelMOD[line] <- ""
                      }
                    }
                  }
                }
              }
              #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
              # Change 'categorical' statment
              for(row in 1:nrow(dfScale)){
                if(((!is.na(dfScale$inMD1[row]) & dfScale$inMD1[row]==dfScale$Scale[ind]) | (!is.na(dfScale$inMD2[row]) & dfScale$inMD2[row]==dfScale$Scale[ind])) & (dfScale$Type[row]=="Categorical" | dfScale$Type[row]=="Mixed")){
                  tempItemCat <- unlist(strsplit(itemCat," "))
                  tempRemove <- unlist(strsplit(scaleDropSpecificItm[pop], ", "))
                  for(itm in length(tempItemCat):1){
                    if(tempItemCat[itm] %in% tempRemove){
                      tempItemCat <- tempItemCat[-itm]
                    }
                  }
                  if(identical(tempItemCat,character(0))){
                    catgrMOD <- ""
                  } else {
                    catgrMOD <- paste0("CATEGORICAL ARE ",paste(tempItemCat, collapse=" "),";")
                  }
                }
              }
            }
            #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            break
          } else {
            catgrMOD <- catgr
            itemUMOD <- itemU
          }
        }
      }
      # Add country specific improvement(s)
      tempImprv2 <- ""
      if(selectionImprove=="Yes"){
        for(irow in 1:nrow(dfImprv)){
          if(scale %in% dfImprv$Scale[irow] & cntry %in% dfImprv$Country[irow] & level %in% dfImprv$Level[irow]){ # See line 174 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            tempImprv2 <- dfImprv$SinglePopImprovement[irow] #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< See line 174 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            tempImprv2 <- unlist(strsplit(tempImprv2, "\n"))
            for(imp in 1:length(tempImprv2)){
              tempImprv2[imp] <- paste0("<tabSpace>",tempImprv2[imp])
            }
            tempImprv2 <- c(paste0("<tabSpace>!",cntry," ",level," Specific Improvement(s)"),tempImprv2)
            break
          }
        }
      }
      if(selectionScaleDropSpecifics!="None"){
        for(pop in 1:length(scaleDropSpecificPop)){
          if(cntry %in% scaleDropSpecificPop[pop] | level %in% scaleDropSpecificPop[pop] | paste0(cntry,"_",level) %in% scaleDropSpecificPop[pop]){
            model <- c(modelMOD, "", tempImprv2)
            break
          } else {
            model <- c(modelBASE, "", tempImprv2)
          }
        }
      } else {
        model <- c(modelBASE, "", tempImprv2)
        catgrMOD <- catgr
        itemUMOD <- itemU
      }
      filename <- paste0(scale,"_",cntry,"_",level,suffx,".inp")
      # Use the function to create the Mplus input in the directory
      writeMplus(title, cases, group, auxil, invar, outpt, sdata, selectionIDVars, selectionGroupCodeVar, selectionCountryCodeVar, selectionLevelCodeVar, selectionPopCodeVar, selectionStrat, selectionCluster, selectionWeight, selectionScale, ccode, cntry, path1, path2, estmt, items, itemUMOD, lcode, modin, phant, catgrMOD, suffx)
    }
  }
  if(selectionAnalysis=="Cross-country MGCFA with MI testing"){
    # Add country specific variables
    for(crow in 1:nrow(cntryMGCFA)){
      # Set the "level", "lcode" and "ccode" variables
      level <- paste0(selectionLevelCodeVar,cntryMGCFA$Level[crow])
      lcode <- cntryMGCFA$LevelID[crow]
      ccode <- cntryMGCFA$AllCountriesSyntax[crow]
      # Change the "cases" variable if needed
      if(cntryMGCFA$RemoveCountriesSyntax[crow]!=""){
        casesMod <- gsub(");", "", cases)
        listCountryAlpha <- unlist(strsplit(cntryMGCFA$RemoveCountriesSyntax[crow]," "))
        listCountry      <- c()
        for(cnt in 1:length(listCountryAlpha)){
          for(row in 1:nrow(dfCntry)){
            if(listCountryAlpha[cnt] %in% dfCntry$CountryAlpha3[row]){
              listCountry <- c(listCountry, dfCntry$CountryID[row])
            }
          }
        }
        listCountry <- unique(listCountry)
        for(cnt in 1:length(listCountry)){
          listCountry[cnt] <- paste0("<08.id.cntr> NE ", listCountry[cnt])
        }
        listCountry <- paste(listCountry, collapse=" AND ")
        casesMod <- paste0(casesMod, " AND ", listCountry, ");")
      } else {
        casesMod <- cases
      }
      # Add country specific improvement(s)
      tempImprv2 <- ""
      if(selectionImprove=="Yes"){
        for(irow in 1:nrow(dfImprv)){
          if(scale %in% dfImprv$Scale[irow] & level %in% dfImprv$Level[irow]){ #<<<<<<<<<<<<<<<<<<<< See line 174 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            tempImprv2 <- dfImprv$CrossCountryImprovement[irow] #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< See line 174 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            tempImprv2 <- unlist(strsplit(tempImprv2, "\n"))
            for(imp in 1:length(tempImprv2)){
              tempImprv2[imp] <- paste0("<tabSpace>",tempImprv2[imp])
            }
            tempImprv2 <- c(paste0("<tabSpace>!",level," Specific Improvement(s)"),tempImprv2)
            break
          }
        }
      }
      model <- c(modelBASE, "", tempImprv2)
      cntry <- ""
      filename <- paste0(scale,"_",level,"_CrossCountry",suffx,".inp")
      # Use the function to create the Mplus input in the directory
      writeMplus(title, casesMod, group, auxil, invar, outpt, sdata, selectionIDVars, selectionGroupCodeVar, selectionCountryCodeVar, selectionLevelCodeVar, selectionPopCodeVar, selectionStrat, selectionCluster, selectionWeight, selectionScale, ccode, cntry, path1, path2, estmt, items, itemU, lcode, modin, phant, catgr, suffx)
    }
  }
  if(selectionAnalysis=="Cross-level MGCFA with MI testing"){
    # Add country specific variables
    for(crow in 1:nrow(cntryMGCFA)){
      # Set the "cntry", "ccode", and "lcode" variables
      cntry <- cntryMGCFA$CountryAlpha3[crow]
      ccode <- cntryMGCFA$CountryID[crow]
      lcode <- cntryMGCFA$AllLevelsSyntax[crow]
      # Change the "cases" variable if needed
      if(cntryMGCFA$RemoveLevelsSyntax[crow]!=""){
        casesMod <- gsub(");", "", cases)
        listLevelFull <- unlist(strsplit(cntryMGCFA$RemoveLevelsSyntax[crow]," "))
        listLevel     <- c()
        for(lvl in 1:length(listLevelFull)){
          for(row in 1:nrow(dfCntry)){
            if(listLevelFull[lvl] %in% dfCntry$Level[row]){
              listLevel <- c(listLevel, dfCntry$LevelID[row])
            }
          }
        }
        listLevel <- unique(listLevel)
        for(lvl in 1:length(listLevel)){
          listLevel[lvl] <- paste0("<09.id.lev.> NE ", listLevel[lvl])
        }
        listLevel <- paste(listLevel, collapse=" AND ")
        casesMod <- paste0(casesMod, " AND ", listLevel, ");")
      } else {
        casesMod <- cases
      }
      # Add country specific improvement(s)
      tempImprv2 <- ""
      if(selectionImprove=="Yes"){
        for(irow in 1:nrow(dfImprv)){
          if(scale %in% dfImprv$Scale[irow] & cntry %in% dfImprv$Country[irow]){ #<<<<<<<<<<<<<<<<<< See line 174 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            tempImprv2 <- dfImprv$CrossLevelImprovement[irow] #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< See line 174 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            tempImprv2 <- unlist(strsplit(tempImprv2, "\n"))
            for(imp in 1:length(tempImprv2)){
              tempImprv2[imp] <- paste0("<tabSpace>",tempImprv2[imp])
            }
            tempImprv2 <- c(paste0("<tabSpace>!",cntry," Specific Improvement(s)"),tempImprv2)
            break
          }
        }
      }
      model <- c(modelBASE, "", tempImprv2)
      modin <- ""
      filename <- paste0(scale,"_",cntry,"_CrossLevel",suffx,".inp")
      # Use the function to create the Mplus input in the directory
      writeMplus(title, casesMod, group, auxil, invar, outpt, sdata, selectionIDVars, selectionGroupCodeVar, selectionCountryCodeVar, selectionLevelCodeVar, selectionPopCodeVar, selectionStrat, selectionCluster, selectionWeight, selectionScale, ccode, cntry, path1, path2, estmt, items, itemU, lcode, modin, phant, catgr, suffx)
    }
  }
  ##################################################################################################
  
  # 5: Run all the Mplus input files recursively to generate CFA output
  if(selectionMplusAuto=="Yes"){
    setwd(paste0(dirCFAs,"/",scale))
    system.time({runModels(target=getwd(),replaceOutfile="modifiedDate",recursive=T)})
  }
}
# END





#####
