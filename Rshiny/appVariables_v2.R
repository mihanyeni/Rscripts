#!usr/bin/Rscript

nameStudy                   <- "Test"
dirRawData                  <- "Z:/LSCE/General/Analysis/03Preparation/00_RShinyTesting"
dirSource                   <- "Z:/LSCE/General/Analysis/03Preparation/00_RShinyTesting/SourceScripts"
dirSetup                    <- "Z:/LSCE/General/Analysis/03Preparation/00_RShinyTesting"
dirRoot                     <- "Z:/LSCE/General/Analysis/03Preparation/00_RShinyTesting/Studies/3S/05Implementation"
dirReport                   <- "Z:/LSCE/General/Analysis/03Preparation/00_RShinyTesting/Studies/3S/06Reporting"

# D drive ##########################################################################################
dirRaw                      <- "D:/3_LSCE/00_ShinyTesting"
dirSource                   <- "D:/3_LSCE/00_ShinyTesting/SourceScripts"
dirSetup                    <- "D:/3_LSCE/00_ShinyTesting"
dirRoot                     <- "D:/3_LSCE/00_ShinyTesting/Studies/3S/05Implementation"
dirReport                   <- "D:/3_LSCE/00_ShinyTesting/Studies/3S/06Reporting"
####################################################################################################

selectionCountryCodeVar     <- "IDCNTRY"
selectionLevelCodeVar       <- "ISCED"
selectionTargetLevelCode    <- "2"
selectionPopCodeVar         <- "IDPOP"

selectionAdjudicateVar      <- "None"
selectionAdjudicateCode     <- "1"
selectionWeight             <- "SENWGT0"
selectionIDVars             <- "IDBOOK IDCLASS"

selectionAnalysisPhase      <- "Preliminary Analysis"
selectionAnalysis           <- "Single Population CFA"
selectionRound              <- "Improved Models"
selectionGroup              <- "Staff"
# datasets
#dfScale <- read.xlsx("Z:/LSCE/General/Analysis/03Preparation/00_RShinyTesting/setup_TEST.xlsx", sheet=paste0("Scale_",gsub(" ","",selectionGroup)), startRow=1, colNames=T, rowNames=F)
#save(dfScale, file=paste0("Z:/LSCE/General/Analysis/03Preparation/00_RShinyTesting/Studies/3S/05Implementation/4Rscripts/SupportFiles/dfScale_",selectionGroup,".Rda"))
load(paste0("Z:/LSCE/General/Analysis/03Preparation/00_RShinyTesting/Studies/3S/05Implementation/4Rscripts/SupportFiles/dfCntry_",selectionGroup,".Rda"))
#dfCntry <- read.xlsx("Z:/LSCE/General/Analysis/03Preparation/00_RShinyTesting/setup_TEST.xlsx", sheet="Country", startRow=1, colNames=T, rowNames=F)
#save(dfCntry, file=paste0("Z:/LSCE/General/Analysis/03Preparation/00_RShinyTesting/Studies/3S/05Implementation/4Rscripts/SupportFiles/dfCntry_",selectionGroup,".Rda"))
load(paste0("Z:/LSCE/General/Analysis/03Preparation/00_RShinyTesting/Studies/3S/05Implementation/4Rscripts/SupportFiles/dfScale_",selectionGroup,".Rda"))

load("D:/3_LSCE/00_ShinyTesting/Studies/3S/05Implementation/4Rscripts/SupportFiles/dfCntry_Staff.Rda")
load("D:/3_LSCE/00_ShinyTesting/Studies/3S/05Implementation/4Rscripts/SupportFiles/dfScale_Staff.Rda")

selectionGroupCodeVar       <- "IDSTAFF"
selectionStrat              <- "BRRSZONE"
selectionCluster            <- "BRRZREP"

dirAnalysisPhase            <- "1PreliminaryAnalysis/"
dirGroup                    <- "1Staff/"
dirAnalysis                 <- "2SinglePopulationCFA/"
dirRound                    <- "2ImprovedModels/"

selectionImprove            <- "No"
selectionFS                 <- "Yes"
selectionMplusAuto          <- "No"

selectionGlobalDropCountry  <- "None"
selectionGlobalDropLevel    <- "None"
selectionGlobalDropPop      <- "None"

selectionListScale          <- "S1MD2"
selectionListScalePI        <- "None"

selectionScale              <- "S1MD2"
selectionScaleDropItems     <- c("item3D")
selectionScaleDropSpecifics <- "AUS: item2A, item3A, item3C; DEU_ISCED2: item2D, item3A; DNK_ISCED2: item3E; KOR_ISCED1: item2C"
selectionScaleDropCountry   <- "None"
selectionScaleDropLevel     <- "None"
selectionScaleDropPop       <- "KOR_ISCED3"

selectionFilename           <- "SinglePopulationCFA_S1GOV"
selectionSuffix             <- "addCatFix7"

# END





#####
