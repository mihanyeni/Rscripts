#!usr/bin/Rscript

run.source.logs <- function(nameStudy, dirRawData, dirRoot, dirReport, dirSource, dirSetup, selectionAnalysisPhase, selectionGroup, selectionAnalysis, selectionRound, selectionGroupCodeVar, selectionStrat, selectionCluster, selectionWeight, selectionIDVars, selectionGlobalDropCountry, selectionGlobalDropLevel, selectionGlobalDropPop, selectionAdjudicateVar, selectionAdjudicateCode, selectionCountryCodeVar, selectionLevelCodeVar, selectionTargetLevelCode, selectionPopCodeVar, selectionImprove, selectionFS, selectionMplusAuto, selectionSuffix, selectionListScale, selectionListScalePI, dirAnalysisPhase, dirGroup, dirAnalysis, dirRound, selectionScale, selectionScaleDropItems, selectionScaleDropSpecifics, selectionScaleDropCountry, selectionScaleDropLevel, selectionScaleDropPop, selectionFilename, selectionFilenameDate){
  inputScript <-
    c("**************************************************",
      "**********      Study information       **********",
      "**************************************************",
      "",
      "Study:                              <nameStudy.>",
      "Raw data folder:                    <dirRawData>",
      "R source scripts folder:            <dirSource.>",
      "Setup file folder:                  <dirSetup..>",
      "Root implementation folder:         <dirRoot...>",
      "Reporting folder:                   <dirReport.>",
      "",
      "Country numeric code variable:      <cntry.cd.v>",
      "Level numeric code variable:        <level.cd.v>",
      "Numeric code for target level:      <trgt.lvl.c>",
      "Population numeric code variable:   <pop.code.v>",
      "",
      "**************************************************",
      "*****    User defined & derived variables    *****",
      "**************************************************",
      "",
      "Adjudication variable:              <adjudicate>",
      "Numeric code for adjudicated cases  <adjud.code>",
      "Weight variable:                    <weight....>",
      "ID variables:                       <id.variabl>",
      "",
      "Analysis phase:                     <analys.phs>",
      "Analysis:                           <analysis..>",
      "Round:                              <round.....>",
      "Group:                              <groupname.>",
      "   Group ID derived variable:       <group.id.v>",
      "   Stratification derived variable: <stratifica>",
      "   Cluster derived variable:        <cluster...>",
      "",
      "Global - drop countries:            <g.drop.cnt>",
      "Global - drop levels:               <g.drop.lvl>",
      "Global - drop populations:          <g.drop.pop>",
      "",
      "Scale List:                         <scale.list>",
      "Scales with Phantom Indicators:     <scale.pi..>",
      "Scale:                              <scale.....>",
      "Scale - drop items:                 <s.drop.itm>",
      "Scale - drop items, pop. specific:  <s.drop.spe>",
      "Scale - drop countries:             <s.drop.cnt>",
      "Scale - drop levels:                <s.drop.lvl>",
      "Scale - drop populations:           <s.drop.pop>",
      "",
      "Include improvements:               <improve...>",
      "Generate factor scores:             <factor.sco>",
      "Generate Mplus output:              <mplus.Auto>",
      "",
      "**************************************************",
      "**********   Filenames & directories    **********",
      "**************************************************",
      "",
      "Base filename:                      <filename..>",
      "Filename suffix:                    <suffix....>",
      "Mplus input filename:               <created.fn>",
      "MPlus input file directory:         <created.dr>",
      "Log filename:                       <dated.fnam>",
      "Log file directory:                 <log.dir...>")
  # Replace values #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< rearrange <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  inputScript <- gsub("<nameStudy.>", nameStudy, inputScript)
  inputScript <- gsub("<dirRawData>", dirRawData, inputScript)
  inputScript <- gsub("<dirRoot...>", dirRoot, inputScript)
  inputScript <- gsub("<dirReport.>", dirReport, inputScript)
  inputScript <- gsub("<dirSource.>", dirSource, inputScript)
  inputScript <- gsub("<dirSetup..>", dirSetup, inputScript)
  inputScript <- gsub("<analys.phs>", selectionAnalysisPhase, inputScript)
  inputScript <- gsub("<groupname.>", selectionGroup, inputScript)
  inputScript <- gsub("<analysis..>", selectionAnalysis, inputScript)
  inputScript <- gsub("<round.....>", selectionRound, inputScript)
  inputScript <- gsub("<group.id.v>", selectionGroupCodeVar, inputScript)
  inputScript <- gsub("<stratifica>", selectionStrat, inputScript)
  inputScript <- gsub("<cluster...>", selectionCluster, inputScript)
  inputScript <- gsub("<weight....>", selectionWeight, inputScript)
  inputScript <- gsub("<id.variabl>", selectionIDVars, inputScript)
  inputScript <- gsub("<g.drop.cnt>", selectionGlobalDropCountry, inputScript)
  inputScript <- gsub("<g.drop.lvl>", selectionGlobalDropLevel, inputScript)
  inputScript <- gsub("<g.drop.pop>", selectionGlobalDropPop, inputScript)
  inputScript <- gsub("<adjudicate>", selectionAdjudicateVar, inputScript)
  inputScript <- gsub("<adjud.code>", selectionAdjudicateCode, inputScript)
  inputScript <- gsub("<cntry.cd.v>", selectionCountryCodeVar, inputScript)
  inputScript <- gsub("<level.cd.v>", selectionLevelCodeVar, inputScript)
  inputScript <- gsub("<trgt.lvl.c>", selectionTargetLevelCode, inputScript)
  inputScript <- gsub("<pop.code.v>", selectionPopCodeVar, inputScript)
  inputScript <- gsub("<improve...>", selectionImprove, inputScript)
  inputScript <- gsub("<factor.sco>", selectionFS, inputScript)
  inputScript <- gsub("<mplus.Auto>", selectionMplusAuto, inputScript)
  inputScript <- gsub("<suffix....>", selectionSuffix, inputScript)
  inputScript <- gsub("<scale.list>", selectionListScale, inputScript)
  inputScript <- gsub("<scale.pi..>", selectionListScalePI, inputScript)
  inputScript <- gsub("<scale.....>", selectionScale, inputScript)
  inputScript <- gsub("<s.drop.itm>", selectionScaleDropItems, inputScript)
  inputScript <- gsub("<s.drop.spe>", selectionScaleDropSpecifics, inputScript)
  inputScript <- gsub("<s.drop.cnt>", selectionScaleDropCountry, inputScript)
  inputScript <- gsub("<s.drop.lvl>", selectionScaleDropLevel, inputScript)
  inputScript <- gsub("<s.drop.pop>", selectionScaleDropPop, inputScript)
  inputScript <- gsub("<filename..>", selectionFilename, inputScript)
  inputScript <- gsub("<created.fn>", paste0(selectionFilename,"_",selectionSuffix), inputScript)
  inputScript <- gsub("<created.dr>", paste0(dirRoot,"/",dirAnalysisPhase,"4Validation/",dirGroup,dirAnalysis,dirRound), inputScript)
  inputScript <- gsub("<dated.fnam>", selectionFilenameDate, inputScript)
  inputScript <- gsub("<log.dir...>", paste0(dirRoot,"/",dirAnalysisPhase,"4Validation/",dirGroup,dirAnalysis,dirRound,"Logs"), inputScript)
  # Write lines
  dirLogs <- paste0(dirRoot,"/",dirAnalysisPhase,"4Validation/",dirGroup,dirAnalysis,dirRound,"/Logs")
  setwd(dirLogs)
  writeLines(text=inputScript, con=paste0(selectionFilenameDate,".txt"))
}

# END





#####
