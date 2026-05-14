#' @title generate SOM map with code values in nodes.
#' @name SomMapCodes
#' @description This function generate a SOM map with in each node a diagram
#' with factor values.
#' @param data.som (.rds) RSS map with information to plot
#' @param path.plot.save path to save figure
#' @param as.pdf (bool) if generate and save pdf figure
#' @param as.png (bool) if generated and save png figure
#' @param file.name name of the file to generate
#' @param title title of the figure
#' @param width width of the figure
#' @param height height of the figure
#' @export


SomMapCodes <- function(data.som, path.plot.save, as.pdf=T, as.png=F,
                        file.name="Codes", title="Codes", width=7, height=7){

  require(kohonen)

  fn = file.name

  if(as.pdf){
    pdf(file.path(path.plot.save, paste0(fn, ".pdf")),
        width = width,
        height = height)
  } else if (as.png){
    png(file.path(path.plot.save, paste0(fn, ".png")),
        width=600,
        height=350)
  }

  par(mfrow = c(1, 1))
  plot(data.som, type="codes", main=title, codeRendering="segments")
  dev.off()
}
