.ModelAnalysisPlot <- function(...){
  if(requireNamespace("AtlasVisualization", quietly=TRUE)){
    ns <- asNamespace("AtlasVisualization")
    if(exists("ModelAnalysisPlot", envir=ns, inherits=FALSE)){
      return(get("ModelAnalysisPlot", envir=ns)(...))
    }
    if(exists("MofaModelAnalysis", envir=ns, inherits=FALSE)){
      warning(
        "Using deprecated AtlasVisualization::MofaModelAnalysis(). ",
        "Reload AtlasVisualization from the local source to use ModelAnalysisPlot().",
        call.=FALSE
      )
      return(get("MofaModelAnalysis", envir=ns)(...))
    }
  }

  if(exists("ModelAnalysisPlot", envir=.GlobalEnv, inherits=FALSE)){
    return(get("ModelAnalysisPlot", envir=.GlobalEnv)(...))
  }
  if(exists("MofaModelAnalysis", envir=.GlobalEnv, inherits=FALSE)){
    warning(
      "Using deprecated global MofaModelAnalysis(). ",
      "Reload AtlasVisualization from the local source to use ModelAnalysisPlot().",
      call.=FALSE
    )
    return(get("MofaModelAnalysis", envir=.GlobalEnv)(...))
  }

  stop(
    "Could not find ModelAnalysisPlot. Reload AtlasVisualization before atlastoolsV2, ",
    "for example devtools::load_all('/home/rstudio/git/AtlasTools/atlasvisualization') ",
    "then devtools::load_all('/home/rstudio/git/AtlasTools/atlastoolsV2').",
    call.=FALSE
  )
}
