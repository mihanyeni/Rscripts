#!usr/bin/Rscript

# Syntax author:   ******************           #
# email:           **************************   #
# Affiliation:     ***********                  #
# Syntax language: R (3.4.4)                    #
# Project:         ***************************  #
# Analysis:        ***************              #
# Last update:     07-Jan-2019                  #

# Title:   CFA and MGCFA script generator
# Purpose: Create Mplus input & output for scales for various analyses

###################################################################################################

# This script is the located in the following two directories:
#   Z:\***************\3_MS\Analysis\03Implementation\1PreliminaryAnalysis\ValidationScript
#   Z:\***************\3_MS\Analysis\03Implementation\2FinalAnalysis\ValidationScript

# The script relies on support files created by the "0b" R scripts found in the same folder

###################################################################################################

run.CFA <- function(dir1Intro,dir2Group,dir3CFAnl,dir4Round,useWeight,scaleList=NULL,dropItems=NULL,suffix=NULL,excludeCNTRY=NULL,includeImprove=F,savedata=F,useMplusAutomation=T){
  
  # 1: Prepare names for directory paths and files
  ## Directory paths
  ### Full support files path (e.g.country data, scale data, improvement data, MG CFA data)
  dirFSupp <- paste0(dir1Intro,"/",dir2Group,"/2PRG/2Validation/SupportFiles")
  ### Full CFA / MG CFA path for where to put Mplus input files
  dirFCFAs <- paste0(dir1Intro,"/",dir2Group,"/4Validation/",dir3CFAnl,"/",dir4Round)
  
  ## Files # NAMEs MAY CHANGE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ### Scale file
  nameScale <- paste0("scaleData_",dir2Group,".Rda")
  ### Improvement file
  if(isTRUE(includeImprove)){
    nameImprv <- paste0("modelImprovements_",dir2Group,".xlsx")
  }
  ### Country file
  if(dir3CFAnl!="01CFAPooled"){
    nameCntry <- paste0("countryData_",dir2Group,".Rda")
  }
  #################################################################################################
  
  # 2: Prepare data and lists
  ## Read in the support files
  setwd(dirFSupp)
  load(nameScale)
  if(dir3CFAnl!="01CFAPooled"){
    load(nameCntry)
    # If some countries are excluded...
    if(!is.null(excludeCNTRY)){
      # Delete them from the country file 
      for(row in nrow(cntryData):1){
        for(cnt in 1:length(excludeCNTRY)){
          if(cntryData$CountryCode[row]==excludeCNTRY[cnt]){
            cntryData <- cntryData[-row,]
          }
        }
      }
      # Rewrite variables with Mplus strings
      ## Select only variables needed
      cntryData <- cntryData[,c("Country","CountryAlpha3","CountryCode","ISCED","GroupName","IDEXCLUDE")] # MAY REMOVE "IDEXCLUDE" <<<<<<<<<<<<<<<<<<
      ## ISCED string: code and name
      cntryData$ISCEDstring <- ifelse(cntryData$ISCED==1,"1=ISCEDU3",ifelse(cntryData$ISCED==2,"2=ISCED0.2","empty"))
      ## String of all ISCED levels for the particular country
      tempCntry <- aggregate(ISCEDstring ~ Country, data=cntryData, paste, collapse=", ", subset=cntryData$IDEXCLUDE==0) # MAY REMOVE "IDEXCLUDE" <<<
      colnames(tempCntry) <- c("Country","AllISCED")
      cntryData <- merge(cntryData, tempCntry, all.x=T)
      ## Country string: code and name
      cntryData$CountryString <- paste0(cntryData$CountryCode,"=",cntryData$Country)
      ## String of all countries for the particular ISCED level
      tempISCED <- aggregate(CountryString ~ ISCED, data=cntryData, paste, collapse=", ", subset=cntryData$IDEXCLUDE==0) # MAY REMOVE "IDEXCLUDE" <<<
      colnames(tempISCED) <- c("ISCED","AllCountries")
      cntryData <- merge(cntryData, tempISCED, all.x=T)
    }
  }
  #
  if(isTRUE(includeImprove)){
    modelImprv <- read.xlsx(nameImprv, sheet="Improvements", colNames=T)
  }
  #################################################################################################
  
  # 3: Create function to write Mplus code for scales
  ## Create Mplus syntax loop
  writeMplus <- function(auxil,cases,catgr,ccode,clust,cntry,estmt,sdata,group,idvar,invar,invSt,isced,items,itemU,model,modif,outpt,path1,path2,scale,strat,title,weigt){
    mplusSyntax  <-
      c('TITLE:',
        '<tabSpace><cfa.titl>.',
        '',
        'DATA: FILE IS',
        '<tabSpace>"<dta.pth1>',
        '<tabSpace><dta.pth2><scl.name>.dat";',
        '',
        'VARIABLE:',
        '<tabSpace>NAMES ARE <id.varbl> IDCNTRY ISCED <stratify> <clusters> <use.wght>',
        '<tabSpace><itm.nams> SUPERGROUPID;',
        '',
        '<tabSpace>USEVARIABLES ARE <use.itms>;',
        '<tabSpace><categrcl>',
        '<tabSpace>MISSING ARE <use.itms> (9);',
        '',
        '<tabSpace><auxiliar>',
        '<tabSpace><grouping>',
        '',
        '<tabSpace><use.case>;',
        '',
        '<tabSpace>WEIGHT IS <use.wght>;',
        '<tabSpace>STRATIFICATION IS <stratify>;',
        '<tabSpace>CLUSTER IS <clusters>;',
        '',
        'ANALYSIS:',
        '<tabSpace>TYPE IS COMPLEX;',
        '<tabSpace>ESTIMATOR = <estimatr>;',
        '<tabSpace><inv.stmt>',
        '',
        'MODEL:',
        '',# This is line 31 of the syntax, where we will place the model statement
        '',
        'OUTPUT:',
        '<tabSpace><output.s>;',
        '',
        '')# This is line 36 of the syntax, where we will place the 'savedata' statement
    
    # Replace CFA analysis specific statments
    ## Replace <cfa.titl> with title
    mplusSyntax <- gsub("<cfa.titl>", title, mplusSyntax)
    
    ## Replace <auxiliar> with group
    mplusSyntax <- gsub("<auxiliar>", auxil, mplusSyntax)
    
    ## Replace <grouping> with group
    mplusSyntax <- gsub("<grouping>", group, mplusSyntax)
    
    ## Replace <use.case> with cases
    mplusSyntax <- gsub("<use.case>", cases, mplusSyntax)
    
    ## Replace <inv.stmt> with invSt
    mplusSyntax <- gsub("<inv.stmt>", invSt, mplusSyntax)
    
    ## Replace <output.s> with outpt
    mplusSyntax <- gsub("<output.s>", outpt, mplusSyntax)
    
    ## Insert the 'savedata' statment
    mplusSyntax <- c(mplusSyntax[1:35],sdata)
    
    # Replace specific wording or variables
    ## Replace <categrcl> with catgr
    mplusSyntax <- gsub("<categrcl>", catgr, mplusSyntax)
    
    ## Replace <clusters> with clust
    mplusSyntax <- gsub("<clusters>", clust, mplusSyntax)
    
    # Replace <cntry.cd> with ccode
    mplusSyntax <- gsub("<cntry.cd>", ccode, mplusSyntax)
    
    # Replace <cntr.nam> with cntry
    mplusSyntax <- gsub("<cntr.nam>", cntry, mplusSyntax)
    
    ## Replace <dta.pth1> with path1
    mplusSyntax <- gsub("<dta.pth1>", path1, mplusSyntax)

    ## Replace <dta.pth2> with path2
    mplusSyntax <- gsub("<dta.pth2>", path2, mplusSyntax)
    
    ## Replace <estimatr> with estmt
    mplusSyntax <- gsub("<estimatr>", estmt, mplusSyntax)
    
    ## Replace <id.varbl> with idvar
    mplusSyntax <- gsub("<id.varbl>", idvar, mplusSyntax)
    
    ## Replace <inv.test> with invar
    mplusSyntax <- gsub("<inv.test>", invar, mplusSyntax)
    
    ## Replace <isced.nm> with isced
    mplusSyntax <- gsub("<isced.nm>", isced, mplusSyntax)
    
    ## Replace <itm.nams> with items
    mplusSyntax <- gsub("<itm.nams>", items, mplusSyntax)
    
    ## Replace <mod.inds> with modif
    mplusSyntax <- gsub("<mod.inds>", modif, mplusSyntax)
    
    ## Replace <scl.name> with scale
    mplusSyntax <- gsub("<scl.name>", scale, mplusSyntax)
    
    ## Replace <stratify> with strat
    mplusSyntax <- gsub("<stratify>", strat, mplusSyntax)
    
    ## Replace <suf.optn> with suffix
    mplusSyntax <- gsub("<suf.optn>", suffix, mplusSyntax)
    
    ## Replace <use.itms> with itemU
    mplusSyntax <- gsub("<use.itms>", itemU, mplusSyntax)
    
    ## Replace <use.wght> with weigt
    mplusSyntax <- gsub("<use.wght>", weigt, mplusSyntax)
    
    # Insert the model statement
    if(dir3CFAnl=="01CFAPooled" | dir3CFAnl=="02CFASingleCntSinglePop"){
      if(isTRUE(savedata)){
        mplusSyntax <- c(mplusSyntax[1:30],model,mplusSyntax[32:38])
      } else {
        mplusSyntax <- c(mplusSyntax[1:30],model,mplusSyntax[32:36])
      }
    } else {
      mplusSyntax <- c(mplusSyntax[1:30],model,mplusSyntax[32:36])
    }
    # Remove extra spaces in syntax
    for(line in length(mplusSyntax):2){
      if((mplusSyntax[line]=="" & mplusSyntax[line-1]=="") | (mplusSyntax[line]=="" & mplusSyntax[line-1]=="<tabSpace>") | (mplusSyntax[line]=="<tabSpace>" & mplusSyntax[line-1]=="<tabSpace>") | (mplusSyntax[line]=="<tabSpace>" & mplusSyntax[line-1]=="")){
        mplusSyntax <- mplusSyntax[-line]
      }
    }
    # Ensure no lines are greater than 90 characters
    mplusSyntax <- paste(strwrap(mplusSyntax, 90, simplify=T), collapse="\n")
    
    # Replace <tabSpace> with tabSp
    mplusSyntax <- gsub("<tabSpace>", tabSp, mplusSyntax)
    
    writeLines(text=mplusSyntax, con=filename)
  }
  #################################################################################################
  
  # 4: Write Mplus files to respective scale folders
  ## Analysis specific options
  ### Create a new country dataset with only countries that have multiple populations
  if(dir3CFAnl=="03MGCFASingleCntCrossPopMI"){
    # Copy country data
    cntryMGCFA <- cntryData
    # Create a loop to delete countries with only one population level
    for(row in nrow(cntryMGCFA):1){
      # If the two string variables are identical, then only one population was measured in the country
      if(identical(cntryMGCFA$ISCEDstring[row], cntryMGCFA$AllISCED[row])){
        cntryMGCFA <- cntryMGCFA[-row,]
      }
    }
    # Create a loop to delete countries selected for exclusion
    for(row in nrow(cntryMGCFA):1){
      # If the country & population should be excluded, exclude it
      if(cntryMGCFA$IDEXCLUDE[row]==1){  # MAY REMOVE "IDEXCLUDE" <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        cntryMGCFA <- cntryMGCFA[-row,]
      }
    }
    # Remove single population data, so that unique observations will be only those countries with multiple populations
    cntryMGCFA <- cntryMGCFA[,c("CountryAlpha3","CountryCode","AllISCED")]
    cntryMGCFA <- unique(cntryMGCFA)
  }
  ### Create a new country dataset with only populations that have multiple countries
  if(dir3CFAnl=="04MGCFASinglePopCrossCntMI"){
    # Copy country dataset
    cntryMGCFA <- cntryData
    # Limit data set to ISCED numeric values and a Mplus string of all countries in each population
    cntryMGCFA <- cntryMGCFA[,c("ISCED","AllCountries")]
    # Get unique cases of populations and the countries in each population
    cntryMGCFA <- unique(cntryMGCFA)
    # Remove TALIS-PISA link
    for(row in 1:nrow(cntryMGCFA)){
      if(is.na(cntryMGCFA$AllCountries[row])){
        cntryMGCFA <- cntryMGCFA[-row,]
      }
    }
  }
  ## Set fixed replacement variables
  ### Set the title, used cases, invariance, output, factor score statements
  if(dir3CFAnl=="01CFAPooled"){
    title <- "Pooled CFA for <scl.name> (ISCED 2)"
    group <- ""
    if(is.null(excludeCNTRY)){
      cases <- "SUBPOPULATION IS (ISCED EQ 2)"
    } else {
      cases <- "SUBPOPULATION IS (ISCED EQ 2"
    }
    if(!is.null(excludeCNTRY)){
      for(cnt in 1:length(excludeCNTRY)){
        cases <- paste0(cases," AND IDCNTRY NE ",excludeCNTRY[cnt])
      }
      cases <- paste0(cases,")")
    }
    invSt <- ""
    outpt <- "SAMPSTAT STANDARDIZED RESIDUAL TECH1 FSDETERMINACY SVALUES <mod.inds>"
    if(isTRUE(savedata)){
      auxil <- "AUXILIARY ARE <id.varbl> IDCNTRY ISCED;"
      sdata <- c("SAVEDATA:","<tabSpace>FILE IS FactorScores_<scl.name>_PooledCFA<suf.optn>.dat;","<tabSpace>SAVE IS FSCORES;")
    } else {
      auxil <- ""
      sdata <- ""
    }
  }
  if(dir3CFAnl=="02CFASingleCntSinglePop"){
    title <- "CFA for <scl.name> - <cntr.nam> for population ISCED <isced.nm>"
    group <- ""
    cases <- "SUBPOPULATION IS (ISCED EQ <isced.nm> AND IDCNTRY EQ <cntry.cd>)"
    invSt <- ""
    outpt <- "SAMPSTAT STANDARDIZED RESIDUAL TECH1 FSDETERMINACY SVALUES <mod.inds>"
    if(isTRUE(savedata)){
      auxil <- "AUXILIARY ARE <id.varbl> IDCNTRY ISCED;"
      sdata <- c("SAVEDATA:","<tabSpace>FILE IS FactorScores_<scl.name>_<cntr.nam>_ISCED<isced.nm><suf.optn>.dat;","<tabSpace>SAVE IS FSCORES;")
    } else {
      auxil <- ""
      sdata <- ""
    }
  }
  if(dir3CFAnl=="03MGCFASingleCntCrossPopMI"){
    title <- "MG CFA for <scl.name> in <cntr.nam> - Single Country, Cross-Population (Measurement Invariance testing)"
    auxil <- ""
    group <- "GROUPING IS ISCED (<isced.nm>);"
    cases <- "USEOBSERVATION IS (IDCNTRY EQ <cntry.cd>)"
    invSt <- "MODEL = <inv.test>;"
    outpt <- "SAMPSTAT RESIDUAL TECH1"
    sdata <- ""
  }
  if(dir3CFAnl=="04MGCFASinglePopCrossCntMI"){
    title <- "MG CFA for <scl.name> in the ISCED <isced.nm> population - Cross-Country, Single Population (Measurement Invariance testing)"
    auxil <- ""
    group <- "GROUPING IS IDCNTRY (<cntry.cd>);"
    cases <- "USEOBSERVATION IS (ISCED EQ <isced.nm>)"
    invSt <- "MODEL = <inv.test>;"
    outpt <- "SAMPSTAT RESIDUAL TECH1"
    sdata <- ""
  }
  ### Set correct path folders for the analysis
  path1 <- paste0(dir1Intro,"/")
  path2 <- paste0(dir2Group,"/1Data/3Validation/1AddedSuperGroupID/")
  ### Set spacing variable for "nice looking" Mplus code
  tabSp <- "     "
  ### Set weight variable
  weigt <- useWeight
  ### Set ID, stratification, and cluster variables
  if(dir2Group== "Leader"){
    idvar <- "IDCENTRE"
    strat <- "BRRCZONE"
    clust <- "BRRCREP"
  }
  if(dir2Group== "Staff"){
    idvar <- "IDSTAFF"
    strat <- "BRRSZONE"
    clust <- "BRRSREP"
  }
  ### Set suffix option
  if(is.null(suffix)){
    suffix <- ""
  } else {
    suffix <- paste0("_",suffix)
  }
  ## Create scale folders and replacement strings in the syntax for each respective scale
  setwd(dirFCFAs)
  ### Get only the scales in the scale list
  indexScale <- c()
  if(!is.null(scaleList)){
    for(row in 1:nrow(scaleData)){
      if(scaleData$Scale[row] %in% scaleList){
        indexScale <- c(indexScale, rownames(scaleData)[row])
      }
    }
  } else {
    scaleList <- scaleData$Scale
    indexScale <- 1:nrow(scaleData)
  }
  indexScale <- as.numeric(indexScale)
  ### Begin loop for settings specifics for each scale
  for(ind in indexScale){
    # Create scale name
    scale <- scaleData$Scale[ind]
    # Create item list
    items <- scaleData$Items[ind]
    # Delete items not needed
    if(!is.null(dropItems)){
      ## Create a list of the items in the scale
      itemU <- unlist(strsplit(items, " "))
      ## Scan the dropItems vector for items to drop
      for(itm in dropItems){
        ## If an item in the vector dropItems is in the item list of scales...
        if(itm %in% itemU){
          ## ...get the index of that item
          indexItem <- which(itemU %in% itm)
          ## ...delete it from the item list
          itemU <- itemU[-c(indexItem)]
        } else {next}
      }
      itemU <- paste0(itemU, collapse=" ") 
    } else {
      itemU <- items
    }
    # Create "Categorical" Mplus statement
    if(!is.na(scaleData$Categorical[ind])){
      catgr <- paste0(scaleData$Categorical[ind],itemU,";")
      # Modify output statement based on scale
      outpt <- gsub("FSDETERMINACY ", "", outpt)
    } else {
      catgr <- ""
    }
    # Create estimator type Mplus statement
    estmt <- scaleData$Estimator[ind]
    if(dir3CFAnl=="01CFAPooled" | dir3CFAnl=="02CFASingleCntSinglePop"){
      # Create modification indices output request Mplus statement
      modif <- scaleData$ModIndices[ind]
    } else {
      # Create modification indices output request Mplus statement
      invar <- scaleData$Invariance[ind]
    }
    # Create model statement
    # UNIDIMENSIONAL SCALES
    # If the scale is not multidimensional...
    if(is.na(scaleData$isMD[ind])){
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
      # MULTIDIMENSIONAL SCALES
      # Create temporary lists to store model lines and subscale information
      tempModel <- c()
      tempSubsc <- c()
      # Use a loop to generate the model statement (list of lines)
      for(row in 1:nrow(scaleData)){
        # If the MD scale name is in the "inMD" column...
        if(scaleData$inMD[row] %in% scale){
          # Get the sub-scale name
          tempScale <- scaleData$Scale[row]
          # Get the sub-scale variables
          tempItems <- scaleData$Items[row]
          # Delete items not needed
          if(!is.null(dropItems)){
            ## Create a list of the items in the scale
            tempItemU <- unlist(strsplit(tempItems, " "))
            ## Scan the dropItems vector for items to drop
            for(itm in dropItems){
              ## If an item in the vector dropItems is in the item list of scales...
              if(itm %in% tempItemU){
                ## ...get the index of that item
                indexItem <- which(tempItemU %in% itm)
                ## ...delete it from the item list
                tempItemU <- tempItemU[-c(indexItem)]
              } else {next}
            }
          } else {
            tempItemU <- unlist(strsplit(tempItems, ""))
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
    if(isTRUE(includeImprove)){
      for(row in 1:nrow(modelImprv)){
        if(scale %in% modelImprv$Scale[row]){
          tempImprv <- modelImprv$PooledImprovement[row]
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
    dir.create(paste(dirFCFAs, scale, sep="/"), showWarnings=T, recursive=T)
    # Set the working directory to the created Scale directory
    setwd(file.path(dirFCFAs, scale))
    # Create further variables if needed
    if(dir3CFAnl=="01CFAPooled"){
      model <- modelBASE
      ccode <- ""
      cntry <- ""
      invar <- ""
      isced <- ""
      filename <- paste0(scale,"_PooledCFA",suffix,".inp")
      # Use the function to create the Mplus input in the directory
      writeMplus(auxil,cases,catgr,ccode,clust,cntry,estmt,sdata,group,idvar,invar,invSt,isced,items,itemU,model,modif,outpt,path1,path2,scale,strat,title,weigt)
    }
    if(dir3CFAnl=="02CFASingleCntSinglePop"){
      # Add country specific variables
      for(crow in 1:nrow(cntryData)){
        # Set the "cntry", "isced", and "ccode" variables
        cntry <- cntryData$CountryAlpha3[crow]
        isced <- cntryData$ISCED[crow]
        ccode <- cntryData$CountryCode[crow]
        # Add country specific improvement(s)
        tempImprv2 <- ""
        if(isTRUE(includeImprove)){
          for(irow in 1:nrow(modelImprv)){
            if(scale %in% modelImprv$Scale[irow] & cntry %in% modelImprv$Country[irow]){
              tempImprv2 <- modelImprv$CountryImprovement[irow]
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
        invar <- ""
        filename <- paste0(scale,"_",cntry,"_ISCED",isced,suffix,".inp")
        # Use the function to create the Mplus input in the directory
        writeMplus(auxil,cases,catgr,ccode,clust,cntry,estmt,sdata,group,idvar,invar,invSt,isced,items,itemU,model,modif,outpt,path1,path2,scale,strat,title,weigt)
      }
    }
    if(dir3CFAnl=="03MGCFASingleCntCrossPopMI"){
      # Add country specific variables
      for(crow in 1:nrow(cntryMGCFA)){
        # Set the "cntry", "isced", and "ccode" variables
        cntry <- cntryMGCFA$CountryAlpha3[crow]
        isced <- cntryMGCFA$AllISCED[crow]
        ccode <- cntryMGCFA$CountryCode[crow]
        # Add country specific improvement(s)
        tempImprv2 <- ""
        if(isTRUE(includeImprove)){
          for(irow in 1:nrow(modelImprv)){
            if(scale %in% modelImprv$Scale[irow] & cntry %in% modelImprv$Country[irow]){
              tempImprv2 <- modelImprv$CountryImprovement[irow]
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
        modif <- ""
        filename <- paste0(scale,"_",cntry,"_CrossPopulation",suffix,".inp")
        # Use the function to create the Mplus input in the directory
        writeMplus(auxil,cases,catgr,ccode,clust,cntry,estmt,sdata,group,idvar,invar,invSt,isced,items,itemU,model,modif,outpt,path1,path2,scale,strat,title,weigt)
      }
    }
    if(dir3CFAnl=="04MGCFASinglePopCrossCntMI"){
      # Add country specific variables
      for(crow in 1:nrow(cntryMGCFA)){
        # Set the "isced" and "ccode" variables
        isced <- cntryMGCFA$ISCED[crow]
        ccode <- cntryMGCFA$AllCountries[crow]
        # Add country specific improvement(s)
        tempImprv2 <- ""
        if(isTRUE(includeImprove)){
          for(irow in 1:nrow(modelImprv)){
            if(scale %in% modelImprv$Scale[irow] & isced %in% modelImprv$Population[irow]){
              tempImprv2 <- modelImprv$PopulationImprovement[irow]
              tempImprv2 <- unlist(strsplit(tempImprv2, "\n"))
              for(imp in 1:length(tempImprv2)){
                tempImprv2[imp] <- paste0("<tabSpace>",tempImprv2[imp])
              }
              tempImprv2 <- c(paste0("<tabSpace>!ISCED ",isced," Specific Improvement(s)"),tempImprv2)
              break
            }
          }
        }
        model <- c(modelBASE, "", tempImprv2)
        cntry <- ""
        modif <- ""
        filename <- paste0(scale,"_ISCED",isced,"_CrossCountry",suffix,".inp")
        # Use the function to create the Mplus input in the directory
        writeMplus(auxil,cases,catgr,ccode,clust,cntry,estmt,sdata,group,idvar,invar,invSt,isced,items,itemU,model,modif,outpt,path1,path2,scale,strat,title,weigt)
      }
    }
  }
  #################################################################################################
  
  # 5: Run all the Mplus input files recursively to generate CFA output
  if(isTRUE(useMplusAutomation)){
    for(scl in 1:length(scaleList)){
      setwd(paste0(dirFCFAs,"/",scaleList[scl]))
      system.time({runModels(target=getwd(),replaceOutfile="modifiedDate")})
    }
  }
}

# END





#####
