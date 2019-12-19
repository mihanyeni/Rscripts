#!usr/bin/Rscript

# datasets
dfScale <- read.xlsx("D:/3_LSCE/00_ShinyTesting/setup.xlsx", sheet="Scale_Teacher", rowNames=F, colNames=T)
dfCntry <- read.xlsx("D:/3_LSCE/00_ShinyTesting/setup.xlsx", sheet="Country", rowNames=F, colNames=T)

nameStudy                   <- "LSCE"
dirRawData                  <- "Z:/LSCE/General/Analysis/03Preparation/1RawData"
dirSource                   <- "Z:/LSCE/General/Analysis/04Implementation/4Rscripts/SourceScripts"
dirSetup                    <- "Z:/LSCE/General/Analysis/03Preparation"
dirRoot                     <- "Z:/LSCE/General/Analysis/04Implementation"
dirReport                   <- "Z:/LSCE/General/Analysis/05Reporting"

selectionCountryCodeVar     <- "IDCNTRY"
selectionLevelCodeVar       <- NA
selectionTargetLevelCode    <- NA
selectionPopCodeVar         <- NA

selectionAdjudicateVar      <- "None"
selectionWeight             <- "SENWGT0"

selectionAnalysisPhase      <- "Preliminary Analysis"
selectionAnalysis           <- "Pooled CFA"
selectionRound              <- "First Models"
selectionGroup              <- "Student Assessment"
selectionGroupCodeVar       <- "IDSTUD"
selectionStrat              <- NA
selectionCluster            <- NA

dirAnalysisPhase            <- "1PreliminaryAnalysis/"
dirGroup                    <- "1StudentAssessment/"
dirAnalysis                 <- "1PooledCFA/"
dirRound                    <- "1FirstModels/"

selectionImprove            <- FALSE
selectionFS                 <- FALSE
selectionMplusAuto          <- FALSE

selectionGlobalDropCountry  <- "None"
selectionGlobalDropLevel    <- "None"
selectionGlobalDropPop      <- "None"

selectionListScale          <- "EMP"
selectionListScalePI        <- "EMP"

selectionScale              <- "EMP"
selectionScaleDropItems     <- "None"
selectionScaleDropSpecifics <- "None"
selectionScaleDropCountry   <- "None"
selectionScaleDropLevel     <- "None"
selectionScaleDropPop       <- "None"

selectionFilename           <- "PooledCFA_EMP"
selectionSuffix             <- "v1"




